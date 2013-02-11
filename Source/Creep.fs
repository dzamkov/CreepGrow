namespace CreepGrow

open System
open Util
open Quantity

/// Contains functions and types related to "Creep", the tentacle monster thing.
module Creep =

    /// The density of a creep tendril, excluding the goo that normally
    /// flows through it.
    let baseDensity = scalar<kg/m^2/m> 500.0

    /// The density of the goo that flows through creep tendrils.
    let gooDensity = scalar<kg/m^3> 1000.0

    /// The stiffness of a creep tendril (given as its spring constant).
    let stiffness = scalar<N/m> 0.1

    /// The bounciness of creep that hits the ground.
    let groundBounce = 0.7

    /// The bounciness of creep that hits other creep.
    let creepBounce = 0.3

    /// Describes a joint on a tendril.
    type Joint () =

        /// The point at the center of the joint (absolute).
        [<DefaultValue>] val mutable Center : Vector3<m>

        /// The velocity of the joint (absolute).
        [<DefaultValue>] val mutable Velocity : Vector3<m/s>

        /// The mass attributed to this joint.
        [<DefaultValue>] val mutable Mass : Scalar<kg>

        /// The radius of the exterior of this joint.
        [<DefaultValue>] val mutable Radius : Scalar<m>

        /// The inner area of this joint through which creep-goo can flow.
        [<DefaultValue>] val mutable Area : Scalar<m^2>

    /// Updates an individual creep joint.
    let updateJoint (joint : Joint) time =
        joint.Center <- joint.Center + joint.Velocity * time
        if joint.Center.Z - joint.Radius < scalar<m> 0.0 then
            joint.Center.Z <- joint.Radius
            joint.Velocity.Z <- groundBounce * abs joint.Velocity.Z
        else joint.Velocity.Z <- joint.Velocity.Z - scalar<m/s^2> 9.8 * time

    /// Describes a feature on a tendril. 
    type Feature (start : Joint) =
        
        /// The joint at the beginning of this feature.
        member this.Start = start

    /// Describes a segment on a tendril.
    type Segment (start, following : Feature) =
        inherit Feature (start)

        /// The feature following this segment.
        member this.Following = following

        /// The joint at the end of this segment.
        member this.End = following.Start

        /// The target length of this segment.
        [<DefaultValue>] val mutable Length : Scalar<m>

        /// The segment that precedes this segment, or null if
        /// not applicable.
        [<DefaultValue>] val mutable Preceding : Segment

    /// Applies a spring force along a segment.
    let applySpring (length : Scalar<m>) (s : Joint) (e : Joint) dis (dir : Vector3<1>) (time : Scalar<s>) =
        let impulse = dir * (stiffness * (length - dis) * time)
        s.Velocity <- s.Velocity + impulse / s.Mass
        e.Velocity <- e.Velocity - impulse / e.Mass

    /// Updates a feature and all of its descendants.
    let rec updateTree xDir rDir (feature : Feature) inflow time =
        let zDir = Vector3.dir (Vector3.cross xDir rDir)
        let yDir = Vector3.cross zDir xDir
        updateJoint feature.Start time
        match feature with
        | :? Segment as segment ->
            let s = segment.Start
            let e = segment.End
            let dif = e.Center - s.Center
            let dis = Vector3.len dif
            let dir = dif / dis

            segment.Length <- segment.Length * (1.1 ** float time)
            applySpring segment.Length s e dis dir time
            updateTree dir yDir segment.Following inflow time
        | _ -> ()

    /// Creates an initial greep growth, returning the root feature.
    let init () =
        let s = Joint ()
        s.Center <- vec3<m> 0.0 0.0 0.0
        s.Mass <- scalar<kg> 0.4
        s.Radius <- scalar<m> 0.2

        let e = Joint ()
        e.Center <- vec3<m> 0.1 0.1 1.0
        e.Mass <- scalar<kg> 0.1

        let tail = Feature e
        let seg = Segment (s, tail)
        seg.Length <- scalar<m> 1.0
        seg

    /// Contains functions and types related to the visual aspects of creep.
    (* module Visual =
        open VBO

        /// Contains functions related to joint geometry.
        module Joint =

            /// Cached information about a joint in a VBO.
            type Cache (ring : Geometry.Ring) =

                /// The ring for this joint.
                member this.Ring = ring

        /// Writes the geometry of a feature and all of its descendents to the given streams.
        let rec writeTree xDir rDir (feature : Feature) (vertices : Stream<Vertex.T2fN3fV3f>) (indices : Stream<uint32>) =
            let zDir = Vector3.dir (Vector3.cross xDir rDir)
            let yDir = Vector3.cross zDir xDir
            match feature with
            | :? Segment as segment ->
                let s = segment.Start
                let e = segment.End
                let dif = e.Center - s.Center
                let dis = Vector3.len dif
                let dir = dif / dis *)