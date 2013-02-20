namespace CreepGrow

open System
open Microsoft.FSharp.Core
open Microsoft.FSharp.NativeInterop
open OpenTK
open OpenTK.Graphics
open OpenTK.Graphics.OpenGL

/// Represents an OpenGL VBO.
type VBO = uint32

/// Contains functions for constructing and manipulating VBO's.
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module VBO =

    /// Creates an empty VBO.
    let create () =
        let mutable vbo = 0u : VBO
        GL.GenBuffers (1, &vbo)
        vbo

    /// Creates a set of empty VBO's.
    let createArray count =
        let vbos = Array.zeroCreate<VBO> count
        GL.GenBuffers (count, vbos)
        vbos

    /// Binds a buffer to a specified target.
    let bind target (vbo : VBO) =
        GL.BindBuffer (target, vbo)

    /// Fills a currently-bound buffer with static data.
    let fill target (data : 'a[]) =
        GL.BufferData (target, nativeint data.Length * nativeint sizeof<'a>, data, BufferUsageHint.StaticDraw)

    /// Contains functions for mapping VBO's.
    module Map =

        /// Maps a VBO to fill it for the first time.
        let fill target size =
            GL.BufferData (target, size, nativeint 0, BufferUsageHint.DynamicDraw)
            GL.MapBuffer (target, BufferAccess.WriteOnly)

        /// Maps a VBO to replace its contents.
        let replace target size =
            if Extensions.mapBufferRange.Enabled then
                let access = BufferAccessMask.MapInvalidateBufferBit ||| BufferAccessMask.MapWriteBit
                GL.MapBufferRange (target, nativeint 0, size, access)
            else
                GL.BufferData (target, size, nativeint 0, BufferUsageHint.DynamicDraw)
                GL.MapBuffer (target, BufferAccess.WriteOnly)

        /// Maps a VBO to append to its contents.
        let append target start size =
            if Extensions.mapBufferRange.Enabled then
                let access = BufferAccessMask.MapUnsynchronizedBit ||| BufferAccessMask.MapWriteBit
                GL.MapBufferRange (target, start, size - start, access)
            else
                GL.MapBuffer (target, BufferAccess.WriteOnly) - start
                
    /// Unmaps a VBO.
    let unmap target = GL.UnmapBuffer target |> ignore

    /// Contains functions related to vertex buffers.
    module Vertex =

        /// Binds a VBO to ArrayBuffer.
        let bind (vbo : VBO) = bind BufferTarget.ArrayBuffer vbo

        /// Fills the currently-bound vertex buffer with static vertex data.
        let fill (data : 'a[]) = fill BufferTarget.ArrayBuffer data

        /// Draws the contents of the currently-bound vertex buffer.
        let draw mode first count =
            GL.DrawArrays (mode, first, count)

        /// Sets the interleaved vertex format for next drawing operation.
        let setInterleavedFormat (format : InterleavedArrayFormat) =
            GL.InterleavedArrays (format, 0, IntPtr.Zero)

    /// Contains functions related to index buffers.
    module Index =

        /// Binds a VBO to ElementArrayBuffer.
        let bind (vbo : VBO) = bind BufferTarget.ElementArrayBuffer vbo

        /// Fills the currently-bound index buffer with static index data.
        let fill (data : 'a[]) = fill BufferTarget.ElementArrayBuffer data

        /// Draws the contents of the currently-bound index buffer.
        let draw elementsType mode count =
            GL.DrawElements (mode, count, elementsType, IntPtr.Zero)

    /// Contains stream types that write to VBO's.
    module Stream =

        /// A vertex stream that writes to a VBO.
        type Vertex<'a when 'a : unmanaged> (destination : nativeptr<'a>, rangeEnd : nativeint) =
            inherit Stream.Vertex<'a> ()
            let mutable destination = destination
            new (map : nativeint -> nativeint, size : nativeint) = 
                let destination = map size
                Vertex<'a> (NativePtr.ofNativeInt destination, destination + size)

            override this.Write () =
                let nDestination = NativePtr.add destination 1
                if NativePtr.toNativeInt nDestination > rangeEnd then raise Stream.Vertex.Overflow
                NativePtr.write destination this.Next
                this.Index <- this.Index + 1u
                destination <- nDestination

        /// An index stream that writes to a VBO.
        type Index (destination : nativeptr<uint32>, rangeEnd : nativeint) =
            inherit Stream.Index ()
            let mutable destination = destination
            new (map : nativeint -> nativeint, size : nativeint) = 
                let destination = map size
                Index (NativePtr.ofNativeInt destination, destination + size)

            /// The amount of indices written to this stream.
            [<DefaultValue>] val mutable Count : int

            override this.Write index =
                let nDestination = NativePtr.add destination 1
                if NativePtr.toNativeInt nDestination > rangeEnd then raise Stream.Index.Overflow
                NativePtr.write destination index
                this.Count <- this.Count + 1
                destination <- nDestination

    /// Destroys a VBO.
    let destroy (vbo : VBO) =
        let mutable vbo = vbo
        GL.DeleteBuffers (1, &vbo)

    /// Destroys a set of VBO's.
    let destroyArray (vbos : VBO[]) =
        GL.DeleteBuffers (vbos.Length, vbos)