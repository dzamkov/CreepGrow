namespace CreepGrow

open System
open System.Collections.Generic
open OpenTK

/// A vector in three-dimensional space.
type Vector = Vector3d

/// A point in three-dimensional space.
type Point = Vector3d

/// Contains simple shape types used to describe the bounds of an object or a region.
module Bounds =

    /// An axis-aligned bounding box.
    type [<Struct>] Box =

        /// The minimum point in this bounding box.
        val Min : Point

        /// The maximum point in this bounding box.
        val Max : Point

        new (min, max) = { Min = min; Max = max }
        new (vertex) = { Min = vertex; Max = vertex }

        /// Gets the union of two bounding boxes.
        static member (||.) (a : Box, b : Box) =
            Box (Vector (min a.Min.X b.Min.X,
                         min a.Min.Y b.Min.Y, 
                         min a.Min.Z b.Min.Z),
                 Vector (max a.Max.X b.Max.X,
                         max a.Max.Y b.Max.Y, 
                         max a.Max.Z b.Max.Z))

        /// Gets the union of a bounding box and a point.
        static member (|.) (a : Box, b : Vector) =
            Box (Vector (min a.Min.X b.X,
                         min a.Min.Y b.Y, 
                         min a.Min.Z b.Z),
                 Vector (max a.Max.X b.X,
                         max a.Max.Y b.Y, 
                         max a.Max.Z b.Z))

        /// Gets a null bounding box, one that contains no points and does not influence
        /// a union of bounding boxes.
        static member Null =
            Box (Vector (infinity, infinity, infinity),
                 Vector (-infinity, -infinity, -infinity))

        /// Determines whether this bounding box contains the given point.
        member this.Contains (point : Point) =
            point.X >= this.Min.X && point.Y >= this.Min.Y && point.Z >= this.Min.Z &&
            point.X <= this.Max.X && point.Y <= this.Max.Y && point.Z <= this.Max.Z

/// A collection of spatial items organized by location.
type Container<'a> =
    interface
    end

/// Contains functions and types related to containers.
module Container =

    /// A container that organizes individual items in terms of their 
    /// absolute location. (Used for picking, culling objects to be rendered, etc.)
    type Absolute<'a> =
        inherit Container<'a>

        /// Iterates over all items in this container.
        abstract IterateItems : ('a -> unit) -> unit

    /// A container that organizes pairs of items in terms of their relative
    /// locations. (Used for finding intersections, objects that are near each other, etc.)
    type Pairwise<'a> =
        inherit Container<'a>

        /// Iterates over all unique pairs of items in this container.
        abstract IteratePairs : ('a -> 'a -> unit) -> unit

    /// Iterates over all items in a container.
    let iterItems f (container : Absolute<'a>) = container.IterateItems f

    /// Iterates over all pairs of items in a container.
    let iterPairs f (container : Pairwise<'a>) = container.IteratePairs f

    /// Contains functions that select all items from a container that meet certain criteria
    /// allowing for false positives.
    module Filter =
                    
        /// A container specialized for bounding box filter.
        type Box<'a> =
            inherit Absolute<'a>
            abstract IterateBox : Bounds.Box -> ('a -> unit) -> unit

        /// A container specialized for intersections.
        type Intersection<'a> =
            inherit Pairwise<'a>
            abstract IterateIntersections : ('a -> 'a -> unit) -> unit

        /// Iterates over all items in a container that are within the given bounding box.
        let iterBox f (container : Absolute<'a>) (box : Bounds.Box) =
            match container with
            | :? Box<'a> as container -> container.IterateBox box f
            | container -> container.IterateItems f

        /// Iterates over all possibly-intersecting pairs of items in a container.
        let iterIntersections f (container : Pairwise<'a>) =
            match container with
            | :? Intersection<'a> as container -> container.IterateIntersections f
            | container -> container.IteratePairs f
            
    /// Contains types and functions related to bin-based containers.
    module Bin =

        /// Identifies a bin.
        type [<Struct>] Index =
            val X : int
            val Y : int
            val Z : int
            new (x, y, z) = {X = x; Y = y; Z = z}

        /// Identifies a range of bins.
        type [<Struct>] Range =
            val Min : Index
            val Max : Index
            new (min, max) = {Min = min; Max = max}
            
            /// Gets the union of two ranges.
            static member (||.) (a : Range, b : Range) =
                Range (Index (min a.Min.X b.Min.X,
                              min a.Min.Y b.Min.Y, 
                              min a.Min.Z b.Min.Z),
                       Index (max a.Max.X b.Max.X,
                              max a.Max.Y b.Max.Y, 
                              max a.Max.Z b.Max.Z))

            /// Gets the length (in bins) of this range.
            member this.Length = this.Max.X - this.Min.X + 1

            /// Gets the width (in bins) of this range.
            member this.Width = this.Max.Y - this.Min.Y + 1

            /// Gets the height (in bins) of this range.
            member this.Height = this.Max.Z - this.Min.Z + 1

        /// An identifier and storage structure for items.
        type Item<'a> (value : 'a, range : Range) =
            let mutable range = range

            /// Gets the value of this item.
            member this.Value = value

            /// Gets or sets the range of bins this item occupies.
            member this.Range
                with get () = range
                and set value = range <- value

        /// A bin in a bin space.
        type Bin<'a> = List<Item<'a>>

        /// A container that organizes items using equal-sized bins.
        type Container<'a> (size : Vector) =
            let mutable bins = null
            let mutable min = Index ()

            /// Gets the size of the organizational bins in this container.
            member this.Size = size

            /// Gets the bin range for this container
            member this.Range =
                Range (min, Index (min.X + Array3D.length1 bins, 
                                   min.Y + Array3D.length2 bins, 
                                   min.Z + Array3D.length3 bins))

            /// Removes all items and bins in this container.
            member this.Reset () = bins <- null

            /// Removes all items and bins in this container, and sets up a new range of bins.
            member this.Reset (range : Range) =
                bins <- Array3D.zeroCreate range.Length range.Width range.Height
                for nI = 0 to Array3D.length1 bins - 1 do
                    for nJ = 0 to Array3D.length2 bins - 1 do
                        for nK = 0 to Array3D.length3 bins - 1 do
                            bins.[nI, nJ, nK] <- List<Item<'a>> ()

            /// Expands the range of this container, leaving all current items and bins unchanged.
            member this.Expand (range : Range) =
                let nBins = Array3D.zeroCreate range.Length range.Width range.Height
                for nI = 0 to Array3D.length1 bins - 1 do
                    let pI = nI + range.Min.X - min.X
                    for nJ = 0 to Array3D.length2 bins - 1 do
                        let pJ = nI + range.Min.Y - min.Y
                        for nK = 0 to Array3D.length3 bins - 1 do
                            let pK = nI + range.Min.Z - min.Z
                            if pI >= 0 && pI < Array3D.length1 bins &&
                                pJ >= 0 && pJ < Array3D.length2 bins &&
                                pK >= 0 && pK < Array3D.length3 bins
                            then nBins.[nI, nJ, nK] <- bins.[pI, pJ, pK]
                            else nBins.[nI, nJ, nK] <- List<Item<'a>> ()
                bins <- nBins
                min <- range.Min

            /// Ensures this container contains the given range of bins.
            member this.Include (range : Range) =
                if bins = null then this.Reset range
                else
                    let cRange = this.Range
                    if range.Min.X < cRange.Min.X || 
                       range.Min.Y < cRange.Min.Y || 
                       range.Min.Z < cRange.Min.Z ||
                       range.Max.X > cRange.Max.X || 
                       range.Max.Y > cRange.Max.Y || 
                       range.Max.Z > cRange.Max.Z
                    then this.Expand (cRange ||. range)

            interface Absolute<'a> with
                member this.IterateItems f = ()

    /// A bin-based container that is both absolute and pairwise.
    type Bin<'a> = Bin.Container<'a>