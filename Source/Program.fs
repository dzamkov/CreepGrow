module CreepGrow.Program

open System
open System.Collections.Generic
open System.Drawing
open System.Windows.Forms

open OpenTK
open OpenTK.Graphics
open OpenTK.Graphics.OpenGL
open OpenTK.Input

open Quantity

let rand = Random ()

type [<AllowNullLiteral>] Game () =
    let player = Player ()
    let creep = Creep.init ()
    let drawGround, _ = Ground.prepare (Resources.ground.Force ())

    member this.Initialize () =
        GL.Enable EnableCap.DepthTest
        GL.Enable EnableCap.CullFace
        GL.Enable EnableCap.ColorMaterial
        GL.Enable EnableCap.Lighting
        GL.Enable EnableCap.Light0
        GL.Enable EnableCap.Light1

        GL.TexEnv (TextureEnvTarget.TextureEnv,
            TextureEnvParameter.TextureEnvMode,
            float32 TextureEnvMode.Modulate)

        GL.ShadeModel ShadingModel.Smooth
        GL.ColorMaterial (MaterialFace.Front, ColorMaterialParameter.AmbientAndDiffuse)

        GL.Light (LightName.Light0, LightParameter.Position, Vector4.Normalize (Vector4 (1.0f, 0.0f, 1.0f, 0.0f)))
        GL.Light (LightName.Light0, LightParameter.Ambient, Color4 (0.6f, 0.6f, 0.6f, 1.0f))
        GL.Light (LightName.Light0, LightParameter.Diffuse, Color4 (1.0f, 1.0f, 1.0f, 1.0f))
        GL.Light (LightName.Light0, LightParameter.Specular, Color4 (1.0f, 1.0f, 1.0f, 1.0f))

        GL.Light (LightName.Light1, LightParameter.Position, Vector4.Normalize (Vector4 (-1.0f, -0.0f, -1.0f, 0.0f)))
        GL.Light (LightName.Light1, LightParameter.Ambient, Color4 (0.0f, 0.0f, 0.0f, 0.0f))
        GL.Light (LightName.Light1, LightParameter.Diffuse, Color4 (-0.5f, -0.5f, -0.5f, 0.0f))
        GL.Light (LightName.Light1, LightParameter.Specular, Color4 (0.0f, 0.0f, 0.0f, 0.0f))

    member this.Render (aspectRatio : float) =
        GL.Clear (ClearBufferMask.DepthBufferBit)

        GL.MatrixMode MatrixMode.Projection
        let mutable proj = Matrix4d.CreatePerspectiveFieldOfView (1.2, aspectRatio, 0.1, 1000.0)
        GL.LoadMatrix &proj

        let eyeDir = Player.eyeDir player |> Vector3.toOpenGL
        let eyePos = Player.eyePos player |> Vector3.toOpenGL
        let upDir = Player.upDir |> Vector3.toOpenGL
        let mutable view = Matrix4d.LookAt (Vector3d (0.0, 0.0, 0.0), eyeDir, upDir)
        GL.MultMatrix &view

        GL.Disable (EnableCap.DepthTest)
        GL.Disable (EnableCap.Lighting)
        GL.Enable (EnableCap.Texture2D)
        GL.Color3 (1.0, 1.0, 1.0)

        Skybox.render (Resources.Skybox.day.Force ())

        GL.Translate -eyePos

        GL.Enable (EnableCap.DepthTest)
        GL.Enable (EnableCap.Texture2D)
        GL.Enable (EnableCap.Blend)
        GL.BlendFunc (BlendingFactorSrc.SrcAlpha, BlendingFactorDest.OneMinusSrcAlpha)
        GL.BlendEquation (BlendEquationMode.FuncAdd)

        drawGround eyePos ()

        GL.ColorMask (false, false, false, false)
        GL.Begin BeginMode.Quads
        GL.Vertex3 (-1000.0, -1000.0, 0.0)
        GL.Vertex3 (1000.0, -1000.0, 0.0)
        GL.Vertex3 (1000.0, 1000.0, 0.0)
        GL.Vertex3 (-1000.0, 1000.0, 0.0)
        GL.End ()
        GL.ColorMask (true, true, true, true)

    member this.Update (keyboard : KeyboardDevice) dX dY (time : Scalar<s>) =
        Creep.updateTree (vec3 0.0 0.0 1.0) (vec3 1.0 0.0 0.0) creep 0.0 time
        let isDown key = keyboard.[key]
        Player.update player dX dY (isDown Key.W) (isDown Key.A) (isDown Key.S) (isDown Key.D) time
        
type Window () =
    inherit GameWindow (640, 480, GraphicsMode.Default, "CreepGrow")
    let mutable game = null
    
    member this.KeyDown key =
        if key = Key.Escape then
            this.Close ()

    override this.OnLoad args =
        this.Keyboard.KeyDown.Add (fun args -> this.KeyDown args.Key)
        this.VSync <- VSyncMode.Adaptive
        this.WindowState <- WindowState.Maximized
        game <- Game ()

    override this.OnRenderFrame args =
        game.Render (float this.Width / float this.Height)
        this.SwapBuffers ()

    override this.OnResize args =
        GL.Viewport (0, 0, this.Width, this.Height)

    override this.OnUpdateFrame args =
        if this.Focused then
            Cursor.Hide ()
            let center = 
                Point (
                    this.Bounds.Left + this.Bounds.Width / 2, 
                    this.Bounds.Top + this.Bounds.Height / 2)
            let cursor = Cursor.Position
            let dX = float (cursor.X - center.X) / 300.0
            let dY = float (cursor.Y - center.Y) / 300.0
            Cursor.Position <- center
            game.Update this.Keyboard dX dY (scalar<s> args.Time)
        else Cursor.Show ()

[<EntryPoint>]
let main argv = 
    (new Window ()).Run ()
    0