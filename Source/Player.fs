namespace CreepGrow

open System

open OpenTK

/// Represents a player.
type Player () =
    let mutable theta = 0.0
    let mutable phi = 0.0
    let mutable position = Vector3d (0.0, 0.0, 0.0)
    static let eyeHeight = 1.6
    static let up = Vector3d (0.0, 0.0, 1.0)

    /// Gets or sets the angle on the Z axis the player is looking towards.
    member this.Theta
        with get () = theta
        and set value = theta <- value

    /// Gets or sets the angle above the XY plane the player is looking towards.
    member this.Phi
        with get () = phi
        and set value = phi <- value |> max (Math.PI * -0.4) |> min (Math.PI * 0.4)

    /// Gets the direction the player is looking towards.
    member this.EyeDirection = 
        Vector3d (
            cos theta * cos phi,
            sin theta * cos phi,
            sin phi)

    /// Gets the direction the player is facing.
    member this.FowardDirection =
        Vector3d (cos theta, sin theta, 0.0)

    /// Gets the direction going above the player.
    member this.UpDirection = up

    /// Gets the direction going to the left of the player.
    member this.LeftDirection = 
        Vector3d.Normalize (Vector3d.Cross (this.UpDirection, this.FowardDirection))

    /// Gets or sets the position of the player's feet. 
    member this.FootPosition
        with get () = position
        and set value = position <- value

    /// Gets or sets the position of the player's eyes.
    member this.EyePosition
        with get () = position + Vector3d (0.0, 0.0, eyeHeight)
        and set value = position <- value - Vector3d (0.0, 0.0, eyeHeight)
