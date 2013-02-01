namespace CreepGrow

open System
open Microsoft.FSharp.Core
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

    /// Contains various vertex types and functions related to vertex buffers.
    module Vertex =
        type [<Struct>] T2fN3fV3f =
            val mutable UV : Vector2
            val mutable Normal : Vector3
            val mutable Position : Vector3

        type [<Struct>] T2fC4fN3fV3f =
            val mutable UV : Vector2
            val mutable Color : Color4
            val mutable Normal : Vector3
            val mutable Position : Vector3

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