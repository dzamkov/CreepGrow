module CreepGrow.Resources

open OpenTK
open OpenTK.Graphics
open OpenTK.Graphics.OpenGL

let ground = lazy (Ground.loadFile "Resources/Ground.png" (Color4 (1.2f, 1.2f, 1.2f, 1.0f)) 20.0 -0.05 -0.0015 6)

module Skybox =
    let day = lazy (Skybox.loadDirectory "Resources/Skybox/Day")