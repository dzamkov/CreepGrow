module CreepGrow.Quantity

open Microsoft.FSharp.Core

/// Distance in meters.
[<Measure>] type m

/// Time in seconds.
[<Measure>] type s

/// Mass in kilograms
[<Measure>] type kg

/// Force measured in Newtons.
[<Measure>] type N = kg m / s ^ 2

/// A scalar quantity.
type Scalar<[<Measure>] 'a> = float<'a>

/// A two-dimensional vector quantity.
type [<Struct>] Vector2<[<Measure>] 'a> =
    new (x, y) = { X = x; Y = y }
    val mutable X : Scalar<'a>
    val mutable Y : Scalar<'a>
    static member (+) (a : Vector2<'a>, b : Vector2<'a>) =
        Vector2<'a> (a.X + b.X, a.Y + b.Y)
    static member (-) (a : Vector2<'a>, b : Vector2<'a>) =
        Vector2<'a> (a.X - b.X, a.Y - b.Y)
    static member (*) (a : Vector2<'a>, b : Scalar<'b>) =
        Vector2<'a 'b> (a.X * b, a.Y * b)
    static member (*) (a : Scalar<'a>, b : Vector2<'b>) =
        Vector2<'a 'b> (a * b.X, a * b.Y)
    static member (*) (a : Vector2<'a>, b : Vector2<'b>) =
        (a.X * b.X + a.Y * b.Y) : Scalar<'a 'b>
    static member (/) (a : Vector2<'a>, b : Scalar<'b>) =
        Vector2<'a / 'b> (a.X / b, a.Y / b)

/// A three-dimensional vector quantity.
type [<Struct>] Vector3<[<Measure>] 'a> =
    new (x, y, z) = { X = x; Y = y; Z = z }
    val mutable X : Scalar<'a>
    val mutable Y : Scalar<'a>
    val mutable Z : Scalar<'a>
    static member (+) (a : Vector3<'a>, b : Vector3<'a>) =
        Vector3<'a> (a.X + b.X, a.Y + b.Y, a.Z + b.Z)
    static member (-) (a : Vector3<'a>, b : Vector3<'a>) =
        Vector3<'a> (a.X - b.X, a.Y - b.Y, a.Z - b.Z)
    static member (*) (a : Vector3<'a>, b : Scalar<'b>) =
        Vector3<'a 'b> (a.X * b, a.Y * b, a.Z * b)
    static member (*) (a : Scalar<'a>, b : Vector3<'b>) =
        Vector3<'a 'b> (a * b.X, a * b.Y, a * b.Z)
    static member (*) (a : Vector3<'a>, b : Vector3<'b>) =
        (a.X * b.X + a.Y * b.Y + a.Z * b.Z) : Scalar<'a 'b>
    static member (/) (a : Vector3<'a>, b : Scalar<'b>) =
        Vector3<'a / 'b> (a.X / b, a.Y / b, a.Z / b)

/// A matrix quantity that projects from two-dimensional space to three-dimensional space.
type [<Struct>] Matrix23<[<Measure>] 'a> =
    new (x, y) = { X = x; Y = y }
    val mutable X : Vector3<'a>
    val mutable Y : Vector3<'a>
    static member (+) (a : Matrix23<'a>, b : Matrix23<'a>) =
        Matrix23<'a> (a.X + b.X, a.Y + b.Y)
    static member (-) (a : Matrix23<'a>, b : Matrix23<'a>) =
        Matrix23<'a> (a.X - b.X, a.Y - b.Y)
    static member (*) (a : Matrix23<'a>, b : Scalar<'b>) =
        Matrix23<'a 'b> (a.X * b, a.Y * b)
    static member (*) (a : Scalar<'a>, b : Matrix23<'b>) =
        Matrix23<'a 'b> (a * b.X, a * b.Y)
    static member (*) (a : Matrix23<'a>, b : Vector2<'b>) =
       (a.X * b.X + a.Y * b.Y) : Vector3<'a 'b>
    static member (/) (a : Matrix23<'a>, b : Scalar<'b>) =
        Matrix23<'a / 'b> (a.X / b, a.Y / b)

/// Contains functions related to two-dimensional vectors.
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Vector2 =

    /// Constructs a vector.
    let create x y = Vector2<'u> (x, y)

    /// Constructs a vector from circular coordinates. Note that here, phi means the angle above
    /// or below the XY plane.
    let circular theta (radius : Scalar<'u>) =
        create (radius * cos theta)
               (radius * sin theta)

    /// Gets the square of the length of a vector.
    let inline sqrLen (vec : Vector2<'u>) : Scalar<'u 'u> = vec.X * vec.X + vec.Y * vec.Y

    /// Gets the length of a vector.
    let len (vec : Vector2<'u>) : Scalar<'u> = sqrt (sqrLen vec)

    /// Gets the direction of a vector.
    let dir (vec : Vector2<'u>) = vec / len vec

/// Contains functions related to three-dimensional vectors.
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Vector3 =

    /// Constructs a vector.
    let create x y z = Vector3<'u> (x, y, z)

    /// Constructs a vector from spherical coordinates. Note that here, phi means the angle above
    /// or below the XY plane.
    let spherical theta phi (radius : Scalar<'u>) =
        create (radius * cos phi * cos theta)
               (radius * cos phi * sin theta)
               (radius * sin phi)

    /// Constructs a vector from from cylindrical coordinates.
    let cylindrical theta (radius : Scalar<'u>) (z : Scalar<'u>) =
        create (radius * cos theta)
               (radius * sin theta)
               z

    /// Gets the square of the length of a vector.
    let inline sqrLen (vec : Vector3<'u>) : Scalar<'u 'u> = vec.X * vec.X + vec.Y * vec.Y + vec.Z * vec.Z

    /// Gets the length of a vector.
    let len (vec : Vector3<'u>) : Scalar<'u> = sqrt (sqrLen vec)

    /// Gets the direction of a vector.
    let dir (vec : Vector3<'u>) = vec / len vec

    /// Computes the cross product of two vectors.
    let cross (a : Vector3<'a>) (b : Vector3<'b>) =
        create (a.Y * b.Z - a.Z * b.Y)
               (a.Z * b.X - a.X * b.Z)
               (a.X * b.Y - a.Y * b.X)

/// Contains functions related to two by three matrices.
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Matrix23 =

    /// Constructs a matrix.
    let create x y = Matrix23<'u> (x, y)

/// Constructs a scalar with the given measure
let inline scalar<[<Measure>] 'u> value : Scalar<'u> = 
    LanguagePrimitives.FloatWithMeasure<'u> value

/// Constructs a two-dimensional vector of the given measure.
let inline vec2<[<Measure>] 'u> x y = Vector2.create (scalar<'u> x) (scalar<'u> y)

/// Constructs a three-dimensional vector of the given measure.
let inline vec3<[<Measure>] 'u> x y z = Vector3.create (scalar<'u> x) (scalar<'u> y) (scalar<'u> z)