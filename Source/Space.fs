namespace CreepGrow

open OpenTK

/// An axis-aligned bounding box.
type [<Struct>] BoundingBox =

    /// The minimum point in this bounding box.
    val Min : Vector3d

    /// The maximum point in this bounding box.
    val Max : Vector3d

    new (min, max) = { Min = min; Max = max }
    new (vertex) = { Min = vertex; Max = vertex }

    /// Gets the union of two bounding boxes such that if either contains a point, the union contains the point.
    static member (||.) (a : BoundingBox, b : BoundingBox) =
        BoundingBox (
            Vector3d (
                min a.Min.X b.Min.X,
                min a.Min.Y b.Min.Y, 
                min a.Min.Z b.Min.Z),
            Vector3d (
                max a.Max.X b.Max.X,
                max a.Max.Y b.Max.Y, 
                max a.Max.Z b.Max.Z))
        

/// A collection of spatial objects organized by their location.
type [<AbstractClass>] Space<'a> () =
    class
    end