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

    /// A stream for writing or appending items to a VBO.
    type Stream<'a when 'a : unmanaged> (target, size : uint32, index : uint32, destination : nativeptr<'a>) =
        let size = size
        let mutable index = index
        let mutable destination = destination

        /// Gets the size of the underlying buffer for this stream.
        member this.Size = size

        /// Gets the index of the next item to be written to this stream.
        member this.Index = index

        /// Closes this stream.
        member this.Finish () = GL.UnmapBuffer target

        interface Geometry.Stream<'a> with
            member this.Write item =
                if index < size then
                    NativePtr.write destination item
                    destination <- NativePtr.add destination 1
                    index <- index + 1u
                    true
                else false

        interface Geometry.Stream.Indexed<'a, uint32> with
            member this.Index = index

    /// Contains functions for constructing VBO write streams.
    module Stream =

        /// Creates a stream to fill a currently-bound VBO.
        let fill target size =
            let bufferSize = nativeint size * nativeint sizeof<'a>
            GL.BufferData (target, bufferSize, nativeint 0, BufferUsageHint.DynamicDraw)
            let destination = GL.MapBuffer (target, BufferAccess.WriteOnly)
            Stream<'a> (target, size, 0u, NativePtr.ofNativeInt destination)

        /// Creates a stream to the replace the contents of a currently-bound VBO.
        let replace target size =
            let bufferSize = nativeint size * nativeint sizeof<'a>
            if Extensions.mapBufferRange.Enabled then
                let access = BufferAccessMask.MapInvalidateBufferBit ||| BufferAccessMask.MapWriteBit
                let destination = GL.MapBufferRange (target, nativeint 0, bufferSize, access)
                Stream<'a> (target, size, 0u, NativePtr.ofNativeInt destination)
            else
                GL.BufferData (target, bufferSize, nativeint 0, BufferUsageHint.DynamicDraw)
                let destination = GL.MapBuffer (target, BufferAccess.WriteOnly)
                Stream<'a> (target, size, 0u, NativePtr.ofNativeInt destination)

        /// Creates a stream to append the contents of a currently-bound VBO.
        let append target size index =
            let bufferOffset = nativeint index * nativeint sizeof<'a>
            if Extensions.mapBufferRange.Enabled then
                let access = BufferAccessMask.MapUnsynchronizedBit ||| BufferAccessMask.MapWriteBit
                let destination = GL.MapBufferRange (target, bufferOffset, nativeint (size - index) * nativeint sizeof<'a>, access)
                Stream<'a> (target, size, index, NativePtr.ofNativeInt destination)
            else
                let destination = GL.MapBuffer (target, BufferAccess.WriteOnly)
                Stream<'a> (target, size, index, NativePtr.add (NativePtr.ofNativeInt destination) (int index)) 

    /// Contains various vertex types and functions related to vertex buffers.
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

    /// Destroys a VBO.
    let destroy (vbo : VBO) =
        let mutable vbo = vbo
        GL.DeleteBuffers (1, &vbo)

    /// Destroys a set of VBO's.
    let destroyArray (vbos : VBO[]) =
        GL.DeleteBuffers (vbos.Length, vbos)