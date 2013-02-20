namespace CreepGrow

open Microsoft.FSharp.NativeInterop
open OpenTK
open OpenTK.Graphics
open CreepGrow.Util
open CreepGrow.Quantity
open CreepGrow.Convert

/// Contains various vertex types.
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

/// Contains functions and types related to geometry streams.
module Stream =

    /// A write-only stream that accepts vertex data.
    type [<AbstractClass>] Vertex<'a> () =

        /// Describes the next vertex to be written to this stream.
        [<DefaultValue>] val mutable Next : 'a
        
        /// The index of the next vertex to be written to this stream. This advances after
        /// every write.
        [<DefaultValue>] val mutable Index : uint32

        /// Writes a vertex to this stream.
        abstract Write : unit -> unit
    
    /// Contains functions and types related to vertex streams.
    module Vertex =

        /// A vertex stream accepting vertices with texture coordinates.
        type Textured = Vertex<Vertex.T2fN3fV3f>

        /// An exception raised when a vertex stream overflows.
        exception Overflow

    /// A write-only stream that accepts index data.
    type [<AbstractClass>] Index () =

        /// Writes an index to this stream.
        abstract Write : uint32 -> unit

    /// Contains functions and types related to index streams.
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module Index =

        /// An exception raised when an index stream overflows.
        exception Overflow

/// Contains functions for writing geometry.
module Geometry =
        
    /// Writes a vertex to the given stream.
    let inline writeVertex vertex (vertices : #Stream.Vertex<'a>) =
        vertices.Next <- vertex
        vertices.Write ()

    /// Writes an index to the given stream.
    let inline writeIndex (index : uint32) (indices : #Stream.Index) =
        indices.Write index

    /// Identifies a curve of vertices. If the last vertex repeats the first, the curve is closed.
    type [<Struct>] Curve (``base`` : uint32, count : int) =
            
        /// The base vertex for this curve.
        member this.Base = ``base``

        /// The number of vertices in this curve.
        member this.Count = count

    /// Writes a circular ring to the given stream.
    let writeRing count normal normalOffset position positionOffset v (vertices : #Stream.Vertex.Textured) =
        let ``base`` = vertices.Index
        let thetaDelta = 2.0 * float pi / float count
        let uDelta = 1.0f / float32 count
        vertices.Next.UV.Y <- v
        for i = 0 to count do
            let theta = thetaDelta * float i
            let dir = vec2<1> (cos theta) (sin theta)
            vertices.Next.UV.X <- uDelta * float32 i
            vertices.Next.Normal <- Vector3.toOpenTKf (normalOffset + (normal : Matrix23<1>) * dir)
            vertices.Next.Position <- Vector3.toOpenTKf (positionOffset + (position : Matrix23<'u>) * dir)
            vertices.Write ()
        Curve (``base``, count + 1)

    /// Contains triangle-strip primitive writing functions.
    module Strips =

        /// Writes a surface connecting two curves to the given stream.
        let writeSurface (a : Curve) (b : Curve) (indices : #Stream.Index) =
            writeIndex (b.Base) indices
            if a.Count = b.Count then
                for i = 0 to a.Count - 1 do
                    writeIndex (b.Base + uint32 i) indices
                    writeIndex (a.Base + uint32 i) indices
            elif a.Count > b.Count then
                for i = 0 to a.Count - 1 do
                    writeIndex (b.Base + uint32 (i * b.Count / a.Count)) indices
                    writeIndex (a.Base + uint32 i) indices
            else
                for i = 0 to b.Count - 1 do
                    writeIndex (b.Base + uint32 i) indices
                    writeIndex (a.Base + uint32 (i * a.Count / b.Count)) indices
            writeIndex (a.Base + uint32 a.Count - 1u) indices