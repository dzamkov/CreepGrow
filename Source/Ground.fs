namespace CreepGrow

open OpenTK
open OpenTK.Graphics
open OpenTK.Graphics.OpenGL
open CreepGrow.Util

/// Describes a level ground surface that blends into the skybox below the horizon.
type Ground = {
   
        /// The seamless texture the ground is based on.
        Texture : Texture

        /// The base color of the ground.
        Color : Color4

        /// The length of the texture in world units.
        Scale : double

        /// The angle below the camera at which the ground is completely opaque.
        Max : double

        /// The angle below the camera at which the ground is visible.
        FuzzyMax : double

        /// The resolution of the ground.
        Resolution : Resolution
    }

/// Contains functions for constructing and manipulating grounds.
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Ground =

    /// Creates a ground with the given properties.
    let create texture color scale max fuzzyMax resolution = {
            Texture = texture
            Color = color
            Scale = scale
            Max = max
            FuzzyMax = fuzzyMax
            Resolution = resolution
        }

    /// Loads a ground from a file using the given settings.
    let loadFile path color scale max fuzzyMax resolution =
        let texture = Texture.create ()
        Texture.bind2D texture
        Texture.setMipmap TextureTarget.Texture2D true
        Texture.readFile Texture.Format.rgb path
        Texture.setWrap TextureTarget.Texture2D TextureWrapMode.Repeat TextureWrapMode.Repeat
        Texture.setFilter TextureTarget.Texture2D TextureMinFilter.LinearMipmapLinear TextureMagFilter.Linear
        create texture color scale max fuzzyMax resolution

    /// Prepares a ground for rendering.
    let prepare (ground : Ground) =
        let color = ground.Color
        let resolution = ground.Resolution
        let edgeVertexCount = (resolution + 2) * 2
        let totalVertexCount = edgeVertexCount * edgeVertexCount

        let vertices = Array.zeroCreate totalVertexCount
        let vertexIndex (i : int) (j : int) = uint16 (i + (j * edgeVertexCount))
        let setVertex i j (x : float) (y : float) (alpha : float) =
            let mutable vertex = Vertex.T2fC4fN3fV3f ()
            vertex.UV <- Vector2 (float32 x, float32 y)
            vertex.Color <- Color4 (color.R, color.G, color.B, color.A * float32 alpha)
            vertex.Normal <- Vector3 (0.0f, 0.0f, 1.0f)
            vertex.Position <- Vector3 (float32 x, float32 y, 0.0f)
            vertices.[int (vertexIndex i j)] <- vertex

        let length = -1.0 / tan ground.FuzzyMax
        let delta = length / float (resolution + 1)
        for i = 0 to resolution + 1 do
            let x = length - delta * double i
            for j = 0 to resolution + 1 do
                let y = length - delta * double j
                let distance = sqrt (x * x + y * y)
                let angle = atan (-1.0 / distance)
                let alpha = (ground.FuzzyMax - angle) / (ground.FuzzyMax - ground.Max)
                let alpha = alpha |> max 0.0 |> min 1.0
                let invert i = edgeVertexCount - i - 1
                setVertex i j -x -y alpha
                setVertex (invert i) j x -y alpha
                setVertex i (invert j) -x y alpha
                setVertex (invert i) (invert j) x y alpha

        let quadCount = (edgeVertexCount - 1) * (edgeVertexCount - 1)
        let indexCount = quadCount * 4
        let indices = Array.zeroCreate indexCount
        let mutable cur = 0
        for i = 0 to edgeVertexCount - 2 do
            for j = 0 to edgeVertexCount - 2 do
            indices.[cur + 0] <- vertexIndex i j
            indices.[cur + 1] <- vertexIndex (i + 1) j
            indices.[cur + 2] <- vertexIndex (i + 1) (j + 1)
            indices.[cur + 3] <- vertexIndex i (j + 1)
            cur <- cur + 4

        let VBOs = VBO.createArray 2

        let vertexVBO = VBOs.[0]
        VBO.Vertex.bind vertexVBO
        VBO.Vertex.fill vertices

        let indexVBO = VBOs.[1]
        VBO.Index.bind indexVBO
        VBO.Index.fill indices

        let drawDefault () =
            Texture.bind2D ground.Texture
            VBO.Vertex.bind vertexVBO
            VBO.Index.bind indexVBO
            VBO.Vertex.setInterleavedFormat InterleavedArrayFormat.T2fC4fN3fV3f
            VBO.Index.draw DrawElementsType.UnsignedShort BeginMode.Quads indexCount

        let draw (camera : Vector3d) () =
            GL.MatrixMode (MatrixMode.Modelview)
            GL.PushMatrix ()
            GL.Translate (camera.X, camera.Y, 0.0)
            GL.Scale (camera.Z, camera.Z, 1.0)
            GL.MatrixMode (MatrixMode.Texture)
            GL.PushMatrix ()
            GL.Translate (camera.X / ground.Scale, camera.Y / ground.Scale, 0.0)
            GL.Scale (camera.Z / ground.Scale, camera.Z / ground.Scale, 1.0)
            drawDefault ()
            GL.MatrixMode (MatrixMode.Texture)
            GL.PopMatrix ()
            GL.MatrixMode (MatrixMode.Modelview)
            GL.PopMatrix ()

        let destroy () = VBO.destroyArray VBOs

        (draw : Vector3d -> ColorDraw), 
        (destroy : Destroy)

    /// Destroys a ground.
    let destroy (ground : Ground) = 
        Texture.destroy ground.Texture