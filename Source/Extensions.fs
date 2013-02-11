namespace CreepGrow

open OpenTK
open OpenTK.Graphics
open OpenTK.Graphics.OpenGL

module GL =

    /// The current OpenGL version.
    let version = GL.GetString StringName.Version

    /// The list of extensions supported by the current driver.
    let all = (GL.GetString StringName.Extensions).Split [| ' ' |]

/// Contains information about an extension.
type Extension (supported : bool) =
    let mutable enabled = supported
    new (name) = Extension (GL.all |> Array.exists (fun n -> n = name))
    new (names) = Extension (names |> Seq.forall (fun name -> GL.all |> Array.exists (fun n -> n = name)))

    /// Indicates whether this extension is supported.
    member this.Supported = supported

    /// Gets or sets whether this extension is enabled.
    member this.Enabled
        with get () = enabled
        and set value = enabled <- value

/// Contains information about extensions possibly used by this program.
module Extensions =
    let mapBufferRange = Extension "GL_ARB_map_buffer_range"
    let fragmentShader = Extension [| "GL_ARB_fragment_program"; "GL_ARB_fragment_shader" |]