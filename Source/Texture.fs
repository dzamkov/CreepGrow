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

    /// Reads a bitmap into the currently-bound 2D texture.
    let readBitmap (format : Format) (bitmap : Bitmap) =
        let rect = Rectangle (0, 0, bitmap.Width, bitmap.Height)
        let data = bitmap.LockBits (rect, ImageLockMode.ReadOnly, format.System)
        GL.TexImage2D (TextureTarget.Texture2D, 0, format.Internal, data.Width, data.Height, 0, 
            format.GL, format.PixelType, data.Scan0)
        bitmap.UnlockBits data

    /// Reads image data from a file into the currently-bound 2D texture.
    let readFile (format : Format) (path : string) =
        use bitmap = new Bitmap (path)
        readBitmap format bitmap

    /// Generates mipmaps for the currently-bound texture.
    let generateMipmap (target : TextureTarget) =
        GL.Ext.GenerateMipmap (target |> int |> enum)

    /// Sets whether mipmaps are automatically generated for the currently-bound texture.
    let setMipmap target generate =
        let mode = if generate then Boolean.True else Boolean.False
        GL.TexParameter (target, TextureParameterName.GenerateMipmap, int mode)

    /// Sets the wrap mode for the currently-bound texture.
    let setWrap target (s : TextureWrapMode) (t : TextureWrapMode) =
        GL.TexParameter (target, TextureParameterName.TextureWrapS, int s)
        GL.TexParameter (target, TextureParameterName.TextureWrapT, int t)

    /// Sets the filter mode for the currently-bound texture.
    let setFilter target (min : TextureMinFilter) (mag : TextureMagFilter) =
        GL.TexParameter (target, TextureParameterName.TextureMinFilter, int min)
        GL.TexParameter (target, TextureParameterName.TextureMagFilter, int mag)

    /// Destroys a texture.
    let destroy (texture : Texture) = GL.DeleteTexture texture