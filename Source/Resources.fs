module CreepGrow.Resources

open OpenTK
open OpenTK.Graphics
open OpenTK.Graphics.OpenGL

let ground = lazy (
    let texture = Texture.setupFile Texture.Format.rgb "Resources/Ground.png"
    Ground.create texture 20.0 -0.05 -0.0015)

module Skybox =
    let day = lazy (Skybox.loadDirectory "Resources/Skybox/Day")