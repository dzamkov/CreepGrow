module CreepGrow.Quantity

open Microsoft.FSharp.Core
open OpenTK

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

/// Constructs a scalar with the given measure
let inline scalar<[<Measure>] 'u> value : Scalar<'u> = 
    LanguagePrimitives.FloatWithMeasure<'u> value

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

/// Contains functions for three-dimensional vectors.
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

    /// Gets the length of a vector.
    let len (vec : Vector3<'u>) : Scalar<'u> = sqrt (vec.X * vec.X + vec.Y * vec.Y + vec.Z * vec.Z)

    /// Gets the square of the length of a vector.
    let sqrLen (vec : Vector3<'u>) : Scalar<'u 'u> = vec.X * vec.X + vec.Y * vec.Y + vec.Z * vec.Z

    /// Gets the direction of a vector.
    let dir (vec : Vector3<'u>) = vec / len vec

    /// Computes the cross product of two vectors.
    let cross (a : Vector3<'a>) (b : Vector3<'b>) =
        create (a.Y * b.Z - a.Z * b.Y)
               (a.Z * b.X - a.X * b.Z)
               (a.X * b.Y - a.Y * b.X)


    /// Converts a vector quantity to an OpenGL vector.
    let toOpenGL (vec : Vector3<'u>) = Vector3d (float vec.X, float vec.Y, float vec.Z)

/// Constructs a three-dimensional vector of the given measure.
let inline vec3<[<Measure>] 'u> x y z = Vector3.create (scalar<'u> x) (scalar<'u> y) (scalar<'u> z)