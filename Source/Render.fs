namespace CreepGrow

/// A resolution parameter for a rendering operation that specifies how many
/// extra primitives are used in rendering. A value of 0 gives the minimum needed
/// to render the object or effect. As the value increases, the effect will appear
/// smoother.
type Resolution = int

/// A procedure that renders some object or effect to the current graphics
/// context using the current state except when the state needs to be
/// changed as part of the effect. If the state is changed, it does not need
/// to be restored.
type Render = unit -> unit

/// A procedure that renders a colored/textured object or effect.
type ColorRender = Render

/// A procedure that renders the depth profile of an object.
type DepthRender = Render

/// A procedure that renders the shadow volume of an object.
type ShadowRender = Render

/// A procedure that destroys resources that are no longer needed.
type Destroy = unit -> unit