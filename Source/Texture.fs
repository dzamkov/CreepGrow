namespace CreepGrow

open System
open System.Collections.Generic
open System.Drawing
open System.Drawing.Imaging

open Microsoft.FSharp.Core

open OpenTK
open OpenTK.Graphics
open OpenTK.Graphics.OpenGL

/// Represents an OpenGL texture.
type Texture = int

/// Contains functions for constructing and manipulating textures.
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Texture =

    /// Creates a empty texture.
    let create () = GL.GenTexture () : Texture

    /// Binds a texture.
    let bind target (texture : Texture) =
        GL.BindTexture (target, texture)

    /// Binds a 2D texture.
    let bind2D texture = bind TextureTarget.Texture2D texture

    /// Identifies a possible pixel format for a texture.
    type Format = {
            System : Imaging.PixelFormat
            GL : OpenGL.PixelFormat
            PixelType : PixelType
            Internal : PixelInternalFormat
        }

    /// Contains possible pixel formats.
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module Format =
        
        /// A generic rgb format.
        let rgb = { 
                System = Imaging.PixelFormat.Format24bppRgb
                GL = OpenGL.PixelFormat.Bgr
                PixelType = PixelType.UnsignedByte
                Internal = PixelInternalFormat.Rgb
            }

    /// Loads a texture from a bitmap.
    let loadBitmap (format : Format) (bitmap : Bitmap) =
        let texture = create ()
        let rect = Rectangle (0, 0, bitmap.Width, bitmap.Height)
        let data = bitmap.LockBits (rect, ImageLockMode.ReadOnly, format.System)
        bind2D texture
        GL.TexImage2D (TextureTarget.Texture2D, 0, format.Internal, data.Width, data.Height, 0, 
            format.GL, format.PixelType, data.Scan0)
        bitmap.UnlockBits data
        texture

    /// Loads a texture from a file.
    let loadFile (format : Format) (path : string) =
        use bitmap = new Bitmap (path)
        loadBitmap format bitmap

    /// Generates mipmaps for the currently-bound texture.
    let generateMipmap (target : TextureTarget) =
        GL.Ext.GenerateMipmap (target |> int |> enum)

    /// Sets the wrap mode for the currently bound texture.
    let setWrap target (s : TextureWrapMode) (t : TextureWrapMode) =
        GL.TexParameter (target, TextureParameterName.TextureWrapS, (int)s)
        GL.TexParameter (target, TextureParameterName.TextureWrapT, (int)t)

    /// Sets the filter mode for the currently bound texture.
    let setFilter target (min : TextureMinFilter) (mag : TextureMagFilter) =
        GL.TexParameter (target, TextureParameterName.TextureMinFilter, (int)min)
        GL.TexParameter (target, TextureParameterName.TextureMagFilter, (int)mag)

    /// Prepares the currently bound texture to be rendered using default settings.
    let setup target =
        generateMipmap target
        setWrap target TextureWrapMode.Repeat TextureWrapMode.Repeat
        setFilter target TextureMinFilter.LinearMipmapLinear TextureMagFilter.Linear

    /// Loads and prepares the a texture from the given bitmap.
    let setupBitmap format bitmap =
        let texture = loadBitmap format bitmap
        setup TextureTarget.Texture2D
        texture

    /// Loads and prepares the a texture from the given file.
    let setupFile format path =
        let texture = loadFile format path
        setup TextureTarget.Texture2D
        texture

    /// Destroys a texture.
    let destroy (texture : Texture) = GL.DeleteTexture texture