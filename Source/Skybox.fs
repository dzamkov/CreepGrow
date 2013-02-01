namespace CreepGrow

open System.IO

open OpenTK
open OpenTK.Graphics
open OpenTK.Graphics.OpenGL

/// Represents a skybox.
type Skybox = {
        Foward : Texture
        Right : Texture
        Back : Texture
        Left : Texture
        Up : Texture
    }

/// Contains functions for constructing and manipulating skyboxes.
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Skybox =

    /// Creates a skybox from the given textures.
    let create foward right back left up = {
            Foward = foward
            Right = right
            Back = back
            Left = left
            Up = up
        }

    /// Loads a skybox from a directory that contains all image parts.
    let loadDirectory (path : string) =
        let prepareFile path =
            let texture = Texture.create ()
            Texture.bind2D texture
            Texture.readFile Texture.Format.rgb path
            Texture.setWrap TextureTarget.Texture2D TextureWrapMode.ClampToEdge TextureWrapMode.ClampToEdge
            Texture.setFilter TextureTarget.Texture2D TextureMinFilter.Linear TextureMagFilter.Linear
            texture
        let loadPart names =
            Seq.pick (fun path ->
                let name = Path.GetFileNameWithoutExtension path
                if Seq.exists (fun n -> n = name) names then Some path
                else None) (Directory.EnumerateFiles path) 
                |> prepareFile
        {
            Foward = loadPart ["F"; "Foward"]
            Right = loadPart ["R"; "Right"]
            Back = loadPart ["B"; "Back"]
            Left = loadPart ["L"; "Left"]
            Up = loadPart ["U"; "Up"]
        }
        
    /// Renders the given skybox as 2-unit-long cube centered at the origin using the current
    /// OpenGL state.
    let render (skybox : Skybox) =
        Texture.bind2D skybox.Foward
        GL.Begin BeginMode.Quads
        GL.TexCoord2 (0.0, 0.0)
        GL.Vertex3 (1.0, 1.0, 1.0)
        GL.TexCoord2 (0.0, 1.0)
        GL.Vertex3 (1.0, 1.0, -1.0)
        GL.TexCoord2 (1.0, 1.0)
        GL.Vertex3 (1.0, -1.0, -1.0)
        GL.TexCoord2 (1.0, 0.0)
        GL.Vertex3 (1.0, -1.0, 1.0)
        GL.End ()

        Texture.bind2D skybox.Right
        GL.Begin BeginMode.Quads
        GL.TexCoord2 (0.0, 0.0)
        GL.Vertex3 (1.0, -1.0, 1.0)
        GL.TexCoord2 (0.0, 1.0)
        GL.Vertex3 (1.0, -1.0, -1.0)
        GL.TexCoord2 (1.0, 1.0)
        GL.Vertex3 (-1.0, -1.0, -1.0)
        GL.TexCoord2 (1.0, 0.0)
        GL.Vertex3 (-1.0, -1.0, 1.0)
        GL.End ()

        Texture.bind2D skybox.Back
        GL.Begin BeginMode.Quads
        GL.TexCoord2 (0.0, 0.0)
        GL.Vertex3 (-1.0, -1.0, 1.0)
        GL.TexCoord2 (0.0, 1.0)
        GL.Vertex3 (-1.0, -1.0, -1.0)
        GL.TexCoord2 (1.0, 1.0)
        GL.Vertex3 (-1.0, 1.0, -1.0)
        GL.TexCoord2 (1.0, 0.0)
        GL.Vertex3 (-1.0, 1.0, 1.0)
        GL.End ()

        Texture.bind2D skybox.Left
        GL.Begin BeginMode.Quads
        GL.TexCoord2 (0.0, 0.0)
        GL.Vertex3 (-1.0, 1.0, 1.0)
        GL.TexCoord2 (0.0, 1.0)
        GL.Vertex3 (-1.0, 1.0, -1.0)
        GL.TexCoord2 (1.0, 1.0)
        GL.Vertex3 (1.0, 1.0, -1.0)
        GL.TexCoord2 (1.0, 0.0)
        GL.Vertex3 (1.0, 1.0, 1.0)
        GL.End ()

        Texture.bind2D skybox.Up
        GL.Begin BeginMode.Quads
        GL.TexCoord2 (0.0, 0.0)
        GL.Vertex3 (-1.0, 1.0, 1.0)
        GL.TexCoord2 (0.0, 1.0)
        GL.Vertex3 (1.0, 1.0, 1.0)
        GL.TexCoord2 (1.0, 1.0)
        GL.Vertex3 (1.0, -1.0, 1.0)
        GL.TexCoord2 (1.0, 0.0)
        GL.Vertex3 (-1.0, -1.0, 1.0)
        GL.End ()

    /// Destroys a skybox (by destroying all of its textures).
    let destroy (skybox : Skybox) =
        Texture.destroy skybox.Foward
        Texture.destroy skybox.Right
        Texture.destroy skybox.Back
        Texture.destroy skybox.Left
        Texture.destroy skybox.Up