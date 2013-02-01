namespace CreepGrow

open System
open System.Collections.Generic
open Quantity

/// A vector in three-dimensional space.
type Vector = Vector3d

/// A point in three-dimensional space.
type Point = Vector3d

/// Contains simple shape types used to describe the bounds of an object or a region.
module Bounds =

    /// An axis-aligned bounding box.
    type [<Struct>] Box<[<Measure>] 'u> =

        /// The minimum point in this bounding box.
        val Min : Vector3<'u>

        /// The maximum point in this bounding box.
        val Max : Vector3<'u>

        new (min, max) = { Min = min; Max = max }

        /// Gets the union of two bounding boxes.
        static member (||.) (a : Box<'u>, b : Box<'u>) =
            Box (Vector3 (min a.Min.X b.Min.X,
                          min a.Min.Y b.Min.Y, 
                          min a.Min.Z b.Min.Z),
                 Vector3 (max a.Max.X b.Max.X,
                          max a.Max.Y b.Max.Y, 
                          max a.Max.Z b.Max.Z))

        /// Gets the union of a bounding box and a point.
        static member (|.) (a : Box<'u>, b : Vector3<'u>) =
            Box (Vector3 (min a.Min.X b.X,
                          min a.Min.Y b.Y, 
                          min a.Min.Z b.Z),
                 Vector3 (max a.Max.X b.X,
                          max a.Max.Y b.Y, 
                          max a.Max.Z b.Z))

        /// Determines whether this bounding box contains the given point.
        member this.Contains (point : Vector3<'u>) =
            point.X >= this.Min.X && point.Y >= this.Min.Y && point.Z >= this.Min.Z &&
            point.X <= this.Max.X && point.Y <= this.Max.Y && point.Z <= this.Max.Z

    /// Contains functions for constructing and manipulating bounding boxes.
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module Box =

        /// Constructs a bounding box that contains all points between the given minimum
        /// and maximum points.
        let create min max = Box<'u> (min, max)

        /// Constructs a bounding box containing the given point.
        let point point = create point point

        /// Constructs a bounding box containing the given points.
        let points (points : seq<Vector3<'u>>) =
            let mutable cMin = vec3 infinity infinity infinity
            let mutable cMax = vec3 -infinity -infinity -infinity
            for point in points do
                cMin.X <- min cMin.X point.X
                cMin.Y <- min cMin.Y point.Y
                cMin.Z <- min cMin.Z point.Z
                cMax.X <- max cMax.X point.X
                cMax.Y <- max cMax.Y point.Y
                cMax.Z <- max cMax.Z point.Z
            create cMin cMax

        /// Constructs a bounding box containing a sphere.
        let sphere (center : Vector3<'u>) radius =
            let min = Vector3.create (center.X - radius) (center.Y - radius) (center.Z - radius)
            let max = Vector3.create (center.X + radius) (center.Y + radius) (center.Z + radius)
            create min max

        /// Constructs a bounding box that contains no points.
        let nil () = create (vec3 infinity infinity infinity) (vec3 -infinity -infinity -infinity)

        /// Expands a bounding box to include the given point.
        let insert (point : Vector3<'u>) (box : Box<'u>) = (box |. point)

        /// Gets the union of two bounding boxes.
        let union (a : Box<'u>) (b : Box<'u>) = (a ||. b)

        /// Determines whether the given bounding box contains the given point.
        let contains (box : Box<'u>) point = box.Contains point

/// Contains functions and types related to containers.
module Container =

    /// Contains types and functions related to bin-based containers.
    module Bin =

        /// Identifies a bin.
        type [<Struct>] Index =
            val X : int
            val Y : int
            val Z : int
            new (x, y, z) = {X = x; Y = y; Z = z}

        /// Finds the index for the bin the given point occupies.
        let pointIndex (binSize : Vector3<'u>) (point : Vector3<'u>) =
            Index (floor (point.X / binSize.X) |> int,
                   floor (point.Y / binSize.Y) |> int,
                   floor (point.Z / binSize.Z) |> int)

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

            /// Indicates whether this range contains the bin with the given index.
            member this.Contains (index : Index) =
                index.X >= this.Min.X && index.Y >= this.Min.Y && index.Z >= this.Min.Z &&
                index.X <= this.Max.X && index.Y <= this.Max.Y && index.Z <= this.Max.Z

            /// Gets the length (in bins) of this range.
            member this.Length = this.Max.X - this.Min.X + 1

            /// Gets the width (in bins) of this range.
            member this.Width = this.Max.Y - this.Min.Y + 1

            /// Gets the height (in bins) of this range.
            member this.Height = this.Max.Z - this.Min.Z + 1

        /// Finds the range of bins a bounding box occupies.
        let boxRange (binSize : Vector3<'u>) (box : Bounds.Box<'u>) =
            Range (pointIndex binSize box.Min, pointIndex binSize box.Max)

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
        type Container<'a, [<Measure>] 'u> (binSize : Vector3<'u>) =
            let mutable bins = null
            let mutable offset = Index ()

            static let getBin (bins : Bin<'a>[,,]) (offset : Index) (index : Index) =
                bins.[index.X - offset.X, index.Y - offset.Y, index.Z - offset.Z]

            static let setBin (bins : Bin<'a>[,,]) (offset : Index) (index : Index) bin =
                bins.[index.X - offset.X, index.Y - offset.Y, index.Z - offset.Z] <- bin

            static let insertItem (bins : Bin<'a>[,,]) min index item =
                let cur = getBin bins min index
                if cur = null then
                    let cur = List<Item<'a>> ()
                    cur.Add item
                    setBin bins min index cur
                else cur.Add item

            static let removeItem (bins : Bin<'a>[,,]) min index item =
                (getBin bins min index).Remove item |> ignore

            /// Gets the size of the organizational bins in this container.
            member this.BinSize = binSize

            /// Gets the bin range for this container
            member this.Range =
                Range (offset, Index (offset.X + Array3D.length1 bins - 1, 
                                      offset.Y + Array3D.length2 bins - 1, 
                                      offset.Z + Array3D.length3 bins - 1))

            /// Removes all items and bins in this container.
            member this.Reset () = bins <- null

            /// Removes all items and bins in this container, and sets up a new range of bins.
            member this.Reset (range : Range) =
                bins <- Array3D.zeroCreate range.Length range.Width range.Height
                offset <- range.Min

            /// Expands the range of this container, leaving all current items and bins unchanged.
            member this.Expand (nRange : Range) =
                let nBins = Array3D.zeroCreate nRange.Length nRange.Width nRange.Height
                let oRange = this.Range
                for x = nRange.Min.X to nRange.Max.X do
                    for y = nRange.Min.Y to nRange.Max.Y do
                        for z = nRange.Min.Z to nRange.Max.Z do
                            let index = Index (x, y, z)
                            if oRange.Contains index then
                                getBin bins offset index |> setBin nBins nRange.Min index
                bins <- nBins
                offset <- nRange.Min

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

            /// Inserts an item into this container.
            member this.Insert (value : 'a, range : Range) =
                this.Include range
                let item = Item<'a> (value, range)
                for x = range.Min.X to range.Max.X do
                    for y = range.Min.Y to range.Max.Y do
                        for z = range.Min.Z to range.Max.Z do
                            let index = Index (x, y, z)
                            insertItem bins offset index item

            /// Inserts an item into this container.
            member this.Insert (value : 'a, bounds : Bounds.Box<'u>) =
                this.Insert (value, boxRange binSize bounds)

            /// Updates the range of an item in this container.
            member this.Update (item : Item<'a>, nRange : Range) =
                this.Include nRange
                let pRange = item.Range
                for x = nRange.Min.X to nRange.Max.X do
                    for y = nRange.Min.Y to nRange.Max.Y do
                        for z = nRange.Min.Z to nRange.Max.Z do
                            let index = Index (x, y, z)
                            if not (pRange.Contains index) then
                                insertItem bins offset index item
                for x = pRange.Min.X to pRange.Max.X do
                    for y = pRange.Min.Y to pRange.Max.Y do
                        for z = pRange.Min.Z to pRange.Max.Z do
                            let index = Index (x, y, z)
                            if not (nRange.Contains index) then
                                removeItem bins offset index item

            /// Updates the bounding box of an item in this container.
            member this.Update (item : Item<'a>, nBounds : Bounds.Box<'u>) =
                this.Update (item, boxRange binSize nBounds)

            /// Removes an item from this container.
            member this.Remove (item : Item<'a>) =
                let range = item.Range
                for x = range.Min.X to range.Max.X do
                    for y = range.Min.Y to range.Max.Y do
                        for z = range.Min.Z to range.Max.Z do
                            let index = Index (x, y, z)
                            removeItem bins offset index item

            /// Iterates over all items in this container.
            member this.IterateItems f =
                for i = 0 to Array3D.length1 bins - 1 do
                    for j = 0 to Array3D.length2 bins - 1 do
                        for k = 0 to Array3D.length3 bins - 1 do
                            let cur = bins.[i, j, k]
                            if cur <> null then
                                for item in cur do
                                    let iMin = item.Range.Min
                                    if i + offset.X = iMin.X && // Prevent duplicates by only
                                       j + offset.Y = iMin.Y && // returning an item at its minimum bin.
                                       k + offset.Z = iMin.Z
                                    then f item

            /// Iterates over all pairs of items that share a bin in this container.
            member this.IterateIntersections f =
                for i = 0 to Array3D.length1 bins - 1 do
                    for j = 0 to Array3D.length2 bins - 1 do
                        for k = 0 to Array3D.length3 bins - 1 do
                            let cur = bins.[i, j, k]
                            if cur <> null then
                                for l = 0 to cur.Count - 1 do
                                    for m = l + 1 to cur.Count - 1 do
                                        let lItem = cur.[l]
                                        let mItem = cur.[m]
                                        let lMin = lItem.Range.Min
                                        let mMin = mItem.Range.Min
                                        if i + offset.X = max lMin.X mMin.X && // Prevent duplicates by only
                                           j + offset.Y = max lMin.Y mMin.Y && // returning an intersection at
                                           k + offset.Z = max lMin.Z mMin.Z    // its minimum bin.
                                        then f lItem mItem

    /// A bin-based container that is both absolute and pairwise.
    type Bin<'a, [<Measure>] 'u> = Bin.Container<'a, 'u>

    /// Creates an empty bin-based container.
    let bin binSize = new Bin.Container<'a, 'u> (binSize)