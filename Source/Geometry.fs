namespace CreepGrow

open Microsoft.FSharp.NativeInterop
open OpenTK
open OpenTK.Graphics
open CreepGrow.Util

/// Contains types for various vertex formats.
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

/// Contains functions for writing geometry.
module Geometry =

    /// A write-only stream that accepts geometry data.
    type Stream<'a> =

        /// Tries writing an item to this stream, returning true on success or false if there
        /// is an overflow.
        abstract Write : 'a -> bool

    /// Contains types and functions related to geometry streams.
    module Stream =

        /// A geometry stream where each written item is associated with a sequential index.
        type Indexed<'a, 'b> =
            inherit Stream<'a>

            /// The index of the next item to be written to this stream.
            abstract Index : 'b
    
    /// An exception raised when the vertex buffer overflows.
    exception VertexOverflow

    /// An exception raised when the index buffer overflows.
    exception IndexOverflow

    /// Writes a vertex to the given stream.
    let inline writeVertex (vertex : 'a byref) (vertices : #Stream<'a>) =
        if not (vertices.Write vertex) then raise VertexOverflow

    /// Writes an index to the given stream.
    let inline writeIndex (index : 'a) (indices : #Stream<'a>) =
        if not (indices.Write index) then raise IndexOverflow

    /// Identifies a ring of vertices.
    type [<Struct>] Ring<'a> (``base`` : 'a, count : int) =
            
        /// The base vertex for this ring.
        member this.Base = ``base``

        /// The number of vertices in this ring.
        member this.Count = count

    /// Writes a circular ring to the given stream.
    let inline writeRing count center radius xDir yDir zDir normalX normalR v (vertices : Stream.Indexed<Vertex.T2fN3fV3f, 'a>) =
        let ``base`` = vertices.Index
        let thetaDelta = 2.0f * float32 pi / float32 count
        let uDelta = 1.0f / float32 count
        let baseNormal = (xDir : Vector3) * normalX
        let mutable vertex = Vertex.T2fN3fV3f ()
        for i = 0 to count - 1 do
            let theta = thetaDelta * float32 i
            let u = uDelta * float32 i
            let dir = (yDir : Vector3) * cos theta + (zDir : Vector3) * sin theta
            vertex.Position <- center + dir * radius
            vertex.Normal <- baseNormal + dir * normalR
            vertex.UV.X <- u
            vertex.UV.Y <- v
            writeVertex &vertex vertices

    /// Contains triangle-strip primitive writing functions.
    module Strips =

        /// Writes a tube connecting two rings to the given stream.
        let writeTube (a : Ring<'a>) (b : Ring<'a>) (indices : Stream<'a>) =
            let mutable i = 0
            