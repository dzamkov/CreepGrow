namespace CreepGrow

/// Contains functions and types related to "Creep", the tentacle monster thing.
module Creep =

    /// Describes a joint on a tendril. Note that joints act as the primitive physics
    /// units for the Creep, having velocity and mass.
    type Joint () =

        /// The ideal radius of the joint.
        [<DefaultValue>] val mutable Radius : float

        /// The point at the center of the joint.
        [<DefaultValue>] val mutable Center : Point

        /// The velocity of the joint.
        [<DefaultValue>] val mutable Velocity : Vector

        /// The mass of the joint. Joint mass is calculated by distributing the mass of segments
        /// to their attached joints.
        [<DefaultValue>] val mutable Mass : float

        /// The preceding joint connected to this joint, or None if this is a root joint.
        [<DefaultValue>] val mutable Preceding : Joint option

        /// The following joints connection to this joint.
        [<DefaultValue>] val mutable Following : Joint list

    /// The density of creep material.
    let density = 1000.0

    /// Connects two joints. Note that the second joint must be a root.
    let connect (preceding : Joint) (following : Joint) =
        preceding.Following <- following :: preceding.Following
        following.Preceding <- Some preceding