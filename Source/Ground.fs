namespace CreepGrow

open OpenTK
open OpenTK.Graphics
open OpenTK.Graphics.OpenGL

/// Describes a level ground surface that blends into the skybox below the horizon.
type Ground = {
   
        /// The seamless texture the ground is based on.
        Texture : Texture

        /// The length of the texture in world units.
        Scale : double

        /// The angle below the camera at which the ground is completely opaque.
        Max : double

        /// The angle below the camera at which the ground is visible.
        FuzzyMax : double
    }

/// Contains functions for constructing and manipulating grounds.
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Ground =

    /// Creates a ground with the given properties.
    let create texture scale max fuzzyMax = {
            Texture = texture
            Scale = scale
            Max = max
            FuzzyMax = fuzzyMax
        }

    /// Prepares a ground for rendering.
    let prepare (ground : Ground) (color : Color4) (resolution : Resolution) =
        let length = -1.0 / tan ground.FuzzyMax
        let delta = length / double (resolution + 1)
        let pointCount = (resolution + 2) * 2
        let points = Array2D.zeroCreate pointCount pointCount
        for i = 0 to resolution + 1 do
            let x = length - delta * double i
            for j = 0 to resolution + 1 do
                let y = length - delta * double j
                let distance = sqrt (x * x + y * y)
                let angle = atan (-1.0 / distance)
                let alpha = (ground.FuzzyMax - angle) / (ground.FuzzyMax - ground.Max)
                let alpha = alpha |> max 0.0 |> min 1.0 |> float32
                let invert i = pointCount - i - 1
                points.[i, j] <- (-x, -y, alpha)
                points.[invert i, j] <- (x, -y, alpha)
                points.[i, invert j] <- (-x, y, alpha)
                points.[invert i, invert j] <- (x, y, alpha)

        let renderDefault () =
            let outputPoint (x : double, y, alpha) =
                GL.Color4 (color.R, color.G, color.B, color.A * alpha)
                GL.TexCoord2 (x, y)
                GL.Vertex2 (x, y)
            Texture.bind2D ground.Texture
            GL.Begin BeginMode.Quads
            GL.Normal3 (0.0, 0.0, 1.0)
            for i = 0 to pointCount - 2 do
                for j = 0 to pointCount - 2 do
                    outputPoint points.[i, j]
                    outputPoint points.[i + 1, j]
                    outputPoint points.[i + 1, j + 1]
                    outputPoint points.[i, j + 1]
            GL.End ()

        let render (camera : Vector3d) () =
            GL.MatrixMode (MatrixMode.Modelview)
            GL.PushMatrix ()
            GL.Translate (camera.X, camera.Y, 0.0)
            GL.Scale (camera.Z, camera.Z, 1.0)
            GL.MatrixMode (MatrixMode.Texture)
            GL.PushMatrix ()
            GL.Translate (camera.X / ground.Scale, camera.Y / ground.Scale, 0.0)
            GL.Scale (camera.Z / ground.Scale, camera.Z / ground.Scale, 1.0)
            renderDefault ()
            GL.MatrixMode (MatrixMode.Texture)
            GL.PopMatrix ()
            GL.MatrixMode (MatrixMode.Modelview)
            GL.PopMatrix ()

        render : Vector3d -> ColorRender

    /// Destroys a ground.
    let destroy (ground : Ground) = 
        Texture.destroy ground.Texture