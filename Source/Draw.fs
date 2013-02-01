namespace CreepGrow

/// A resolution parameter for a drawing operation that specifies how many
/// extra primitives are used in drawing. A value of 0 gives the minimum needed
/// to render the object or effect. As the value increases, the effect will appear
/// smoother.
type Resolution = int

/// A procedure that draws some object or effect to the current graphics
/// context using the current state except when the state needs to be
/// changed as part of the effect. If the state is changed, it does not need
/// to be restored.
type Draw = unit -> unit

/// A procedure that draws a colored/textured object or effect.
type ColorDraw = Draw

/// A procedure that draws the depth profile of an object.
type DepthDraw = Draw

/// A procedure that draws the shadow volume of an object.
type ShadowDraw = Draw

/// A procedure that destroys resources that are no longer needed.
type Destroy = unit -> unit