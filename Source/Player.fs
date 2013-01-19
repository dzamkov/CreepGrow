namespace CreepGrow

open System
open Quantity

/// Describes a player.
type Player () =
    let mutable theta = 0.0
    let mutable phi = 0.0
    let mutable position = vec3<m> 0.0 0.0 0.0
    static let eyeHeight = scalar<m> 1.6
    static let up = vec3<1> 0.0 0.0 1.0

    /// The angle on the Z axis the player is looking towards.
    [<DefaultValue>] val mutable Theta : float

    /// The angle above the XY plane the player is looking towards.
    [<DefaultValue>] val mutable Phi : float

    /// The position of the player.
    [<DefaultValue>] val mutable Position : Vector3<m>

/// Contains functions related to the player.
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Player =

    /// The direction going above a player.
    let upDir = vec3<1> 0.0 0.0 1.0

    /// Gets the direction a player is looking towards.
    let eyeDir (player : Player) = Vector3.spherical player.Theta player.Phi 1.0

    /// Gets the direction going in front of a player.
    let fowardDir (player : Player) = Vector3.cylindrical player.Theta 1.0 0.0

     /// Gets the direction going behind a player.
    let backDir (player : Player) = Vector3.cylindrical player.Theta -1.0 0.0

    /// Gets the direction going to the left of a player.
    let leftDir (player : Player) = Vector3.cross upDir (fowardDir player)

    /// Gets the direction going to the right of a player.
    let rightDir (player : Player) = Vector3.cross (fowardDir player) upDir

    /// Gets the position at the base of a player.
    let footPos (player : Player) = player.Position

    /// The height of a player's eyes above the his feet.
    let eyeHeight = scalar<m> 1.6

    /// Gets the position of a player's eyes.
    let eyePos (player : Player) = footPos player + upDir * eyeHeight

    /// The maximum movement rate of a player.
    let moveRate = scalar<m/s> 3.0

    /// Updates a player.
    let update (player : Player) w a s d (time : Scalar<s>) =
        let movement = moveRate * time
        if w then player.Position <- player.Position + fowardDir player * movement
        if a then player.Position <- player.Position + leftDir player * movement
        if s then player.Position <- player.Position + backDir player * movement
        if d then player.Position <- player.Position + rightDir player * movement
        player.Position.Z <- 0.0<m>