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

/// A joint on a tendril of the creep.
type Joint (radius : double, center : Vector3d) as this =
    let mutable radius = radius
    let mutable center = center
    let mutable previous = this
    let following = List<Joint> ()

    /// Gets or sets the radius of this joint.
    member this.Radius
        with get () = radius
        and set value = radius <- value

    /// Gets or sets point at the center of this joint.
    member this.Center
        with get () = center
        and set value = center <- value

    /// Gets or sets the previous joint for this joint.
    member this.Previous
        with get () = previous
        and set value = previous <- value

    /// Gets the joints directly following this joint.
    member this.Following = following

    /// Indicates whether this joint is a root.
    member this.Root = (previous = this)

    /// Indicates whether this joint is a leader.
    member this.Leader = (radius = 0.0)

    /// The direction this joint is facing.
    member this.Direction =
        let mutable dif = this.Center - this.Previous.Center
        for next in following do
            dif <- dif + (next.Center - this.Center) * (next.Radius + 1.0) / this.Radius
        Vector3d.Normalize dif

    /// Approximates the volume of the segments following this joint.
    member this.Volume =
        let mutable volume = 0.0
        for next in following do
            let h = (next.Center - this.Center).Length
            let v = (1.0 / 3.0) * Math.PI * h * (this.Radius * this.Radius + this.Radius * next.Radius + next.Radius * next.Radius)
            volume <- volume + v
        volume

    /// Gets the derivative of volume with respect to radius.
    member this.DVolume =
        let mutable dVolume = 0.0
        for next in following do
            let h = (next.Center - this.Center).Length
            let dV = (1.0 / 3.0) * Math.PI * h * (2.0 * this.Radius + next.Radius)
            dVolume <- dVolume + dV
        dVolume

    /// Grows the creep by the given amount of volume.
    member this.Grow (volume : double) =
        if this.Leader then
            let pcenter = previous.Center
            let dif = center - pcenter
            let dis = dif.Length
            if dis > 0.7 then
                // Split the leader segment.
                let newLeader = Joint (0.0, center)
                center <- pcenter + dif * 0.5
                radius <- previous.Radius * 0.5
                Joint.Connect (this, newLeader)
                newLeader.Grow (volume)
            else
                let dir = Vector3d.Normalize (dif / dis + Vector3d (rand.NextDouble () - 0.5, rand.NextDouble () - 0.5, rand.NextDouble () - 0.5) * 2.0 + Vector3d (0.0, 0.0, -center.Z * 0.3))
                center <- center + dir * volume * 20.0
        else
            let taken = volume * 0.01 * (0.1 / (radius + 0.1))
            
            if rand.NextDouble () < 0.00005 && previous <> this then
                let pcenter = previous.Center
                let dif = center - pcenter
                let dis = dif.Length
                let dir = Vector3d.Normalize (dif / dis + Vector3d (rand.NextDouble () - 0.5, rand.NextDouble () - 0.5, rand.NextDouble () - 0.5) * 1.3)
                let newLeader = Joint (0.0, center + dir * 0.1)
                Joint.Connect (this, newLeader)

            let volume = volume - taken
            radius <- radius + taken / this.DVolume
            for next in following do
                let taken = volume / double following.Count
                next.Grow (taken)

    /// Connects two joints with a segment. The "next" joint must be a root.
    static member Connect (previous : Joint, next : Joint) =
        previous.Following.Add next
        next.Previous <- previous

type Window () =
    inherit GameWindow (640, 480, GraphicsMode.Default, "CreepGrow")
    let player = Player ()
    let creepRoot = Joint (0.01, Vector3d (0.0, 0.0, 0.0))
    let creepLeader = Joint (0.0, Vector3d (0.0, 0.1, 0.0))
    do Joint.Connect (creepRoot, creepLeader)

    member this.KeyDown key =
        if key = Key.Escape then
            this.Close ()

    override this.OnLoad args =
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

        GL.Light (LightName.Light0, LightParameter.Position, Vector4.Normalize (Vector4 (1.0f, 0.0f, 1.0f, 1.0f)))
        GL.Light (LightName.Light0, LightParameter.Ambient, Color4 (0.6f, 0.6f, 0.6f, 1.0f))
        GL.Light (LightName.Light0, LightParameter.Diffuse, Color4 (1.0f, 1.0f, 1.0f, 1.0f))
        GL.Light (LightName.Light0, LightParameter.Specular, Color4 (1.0f, 1.0f, 1.0f, 1.0f))

        GL.Light (LightName.Light1, LightParameter.Position, Vector4.Normalize (Vector4 (-1.0f, -0.0f, -1.0f, 1.0f)))
        GL.Light (LightName.Light1, LightParameter.Ambient, Color4 (0.0f, 0.0f, 0.0f, 0.0f))
        GL.Light (LightName.Light1, LightParameter.Diffuse, Color4 (-0.5f, -0.5f, -0.5f, 0.0f))
        GL.Light (LightName.Light1, LightParameter.Specular, Color4 (0.0f, 0.0f, 0.0f, 0.0f))


        this.Keyboard.KeyDown.Add (fun args -> this.KeyDown args.Key)

        this.WindowState <- WindowState.Maximized

    override this.OnRenderFrame args =
        GL.ClearColor (0.0f, 0.7f, 1.0f, 1.0f)
        GL.Clear (ClearBufferMask.ColorBufferBit ||| ClearBufferMask.DepthBufferBit)

        GL.MatrixMode MatrixMode.Projection
        let mutable proj = Matrix4d.CreatePerspectiveFieldOfView (1.2, float this.Width / float this.Height, 0.1, 1000.0)
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

        let points = List<Vector3d> ()
        let tris = List<int * int * int> ()

        let joint count radius center zDir xDir =
            let yDir = Vector3d.Normalize (Vector3d.Cross (xDir, zDir))
            let xDir = Vector3d.Normalize (Vector3d.Cross (zDir, yDir))
            let delta = Math.PI * 2.0 / double count
            let fPoint = points.Count
            for i = 0 to count - 1 do
                let ang = delta * double i
                let x = cos ang * radius
                let y = sin ang * radius
                points.Add (center + xDir * x + yDir * y)
            fPoint, xDir

        let connect count aPoint bPoint =
            for i = 0 to count - 1 do
                tris.Add (aPoint + i, bPoint + i, aPoint + (i + 1) % count)
                tris.Add (aPoint + (i + 1) % count, bPoint + i, bPoint + (i + 1) % count)

        let taper count fPoint last =
            let lPoint = points.Count
            points.Add last
            for i = 0 to count - 1 do
                tris.Add (fPoint + i, lPoint, fPoint + (i + 1) % count)

        let tendril count xDir (cur : Joint) =
            let rec tendril count lPoint xDir (cur : Joint) =
                if cur.Leader then taper count lPoint cur.Center
                else
                    let cPoint, xDir = joint count cur.Radius cur.Center cur.Direction xDir
                    connect count lPoint cPoint
                    tendril count cPoint xDir cur.Following.[0]
                    for i = 1 to cur.Following.Count - 1 do tendril count cPoint xDir cur.Following.[i]
            let lPoint, xDir = joint count cur.Radius cur.Center cur.Direction xDir
            for next in cur.Following do tendril count lPoint xDir next
        tendril 10 (Vector3d (1.0, 0.0, 0.0)) creepRoot

        let normals = Array.zeroCreate points.Count
        for (a, b, c) in tris do
            let normal = Vector3d.Cross (points.[b] - points.[a], points.[c] - points.[a])
            normals.[a] <- normals.[a] + normal
            normals.[b] <- normals.[b] + normal
            normals.[c] <- normals.[c] + normal
        for i = 0 to normals.Length - 1 do
            normals.[i].Normalize ()

        GL.Enable (EnableCap.DepthTest)
        GL.Enable (EnableCap.Texture2D)
        GL.Enable (EnableCap.Blend)
        GL.BlendFunc (BlendingFactorSrc.SrcAlpha, BlendingFactorDest.OneMinusSrcAlpha)
        GL.BlendEquation (BlendEquationMode.FuncAdd)

        let renderGround = Ground.prepare (Resources.ground.Force ()) (Color4 (1.2f, 1.2f, 1.2f, 1.0f)) 10
        renderGround eyePos ()

        GL.ColorMask (false, false, false, false)
        GL.Begin BeginMode.Quads
        GL.Vertex3 (-1000.0, -1000.0, 0.0)
        GL.Vertex3 (1000.0, -1000.0, 0.0)
        GL.Vertex3 (1000.0, 1000.0, 0.0)
        GL.Vertex3 (-1000.0, 1000.0, 0.0)
        GL.End ()
        GL.ColorMask (true, true, true, true)

        GL.Enable (EnableCap.Lighting)
        GL.Disable (EnableCap.Texture2D)
        GL.Color3 (0.2, 0.0, 0.5)
        GL.Material (MaterialFace.Front, MaterialParameter.Specular, Color4 (0.3f, 0.1f, 0.1f, 1.0f))
        GL.Material (MaterialFace.Front, MaterialParameter.Shininess, 8)
        GL.Begin BeginMode.Triangles
        for (a, b, c) in tris do
            GL.Normal3 normals.[a]
            GL.Vertex3 points.[a]
            GL.Normal3 normals.[b]
            GL.Vertex3 points.[b]
            GL.Normal3 normals.[c]
            GL.Vertex3 points.[c]
        GL.End ()

        
        

        this.SwapBuffers ()

    override this.OnResize args =
        GL.Viewport (0, 0, this.Width, this.Height)

    override this.OnUpdateFrame args =
        let updateTime = scalar<s> args.Time

        creepRoot.Grow (0.8 * args.Time)
        if this.Focused then
            Cursor.Hide ()
            let center = 
                Point (
                    this.Bounds.Left + this.Bounds.Width / 2, 
                    this.Bounds.Top + this.Bounds.Height / 2)
            let cursor = Cursor.Position
            let dX = cursor.X - center.X
            let dY = cursor.Y - center.Y
            Cursor.Position <- center
            player.Theta <- player.Theta - double dX / 300.0
            player.Phi <- player.Phi - double dY / 300.0
        else Cursor.Show ()

        let isDown key = this.Keyboard.[key]
        Player.update player (isDown Key.W) (isDown Key.A) (isDown Key.S) (isDown Key.D) updateTime

[<EntryPoint>]
let main argv = 
    (new Window ()).Run ()
    0