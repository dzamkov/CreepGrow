module CreepGrow.Convert

open OpenTK
open Quantity

module Vector2 =
    let inline toOpenTKf (vec : Vector2<'u>) =
        OpenTK.Vector2 (float32 vec.X, float32 vec.Y)
    let inline toOpenTKd (vec : Vector2<'u>) =
        OpenTK.Vector2d (float vec.X, float vec.Y)

module Vector3 =
    let inline toOpenTKf (vec : Vector3<'u>) =
        OpenTK.Vector3 (float32 vec.X, float32 vec.Y, float32 vec.Z)
    let inline toOpenTKd (vec : Vector3<'u>) =
        OpenTK.Vector3d (float vec.X, float vec.Y, float vec.Z)