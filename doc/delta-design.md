# Design of the Delta printer

## Goals

* Very stiff frame, each face has to be resistant aginst folding (rectangle -> parallelogram)
* Reasonable cheap bill of materials - leveraging off-shelf material + 3d printed rest
* Requiring minimum tools and machining to put together in precise way with correct orientation
* Modular, easy to scale up

## Aluminium rectangle printer

* Square aluminum profiles creating "windows", each corner fixed with 2 screws
* Each corner fixing screw hole is drilled simultaneously through horizontal/vertical frame
which are clamped together in fixed position for drilling
* After holes are drilled (and position of each component marked), clamps are released
* Each vertical corner of the printer is created by joining together two vertical aluminium 
rectangles with 3d printed triangle brackets (push-through interference fit) which also
serve as rail (openbeam 20x20 V-slot) mounting points
* Motors/endstops are mounted on opposite end of the openbeam rail extrusions
* Second set of 3d printed brackets from the bottom serve as print-bed supports (they are longer)
* Shoulder screw will be used for very precise alignment
* Horizontal profiles will be of the L or U type for greater stifness
* Delta rod joints made using Iglidur threaded ball bearing ends

## Tubular spaceframe delta printer

* Fully triangulated frame, outer structure consisting of steel/aluminium tubes connected by 
3D printed/Injection Molded quasi-hemispherical nodes
* Very stiff through load elements (tubes) working stressed in tension/compression, not bending
* Scalable and modular through different tube diameters/lengths, connector nodes are automatically
generated based on tube diameter and overall height/width
* Linear rails implemented with square anodised aluminium profiles with ligthweight closed
sliding carriages riding on IGUS tribo bearing elements (adhesive tribo-tape strips)
