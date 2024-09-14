import Graphics.Implicit
import Control.Lens 
import Linear.V3

--outer dimensions
x = 70 
y = 45
z = 45

wall = 2 
bevel = wall * 2 

insert_rad = 2.8
screw_rad  = 2
insert_height = 8

square_clone_mv :: ℝ2 -> SymbolicObj3-> SymbolicObj3
square_clone_mv mv shape
	= union 
		[ shape
		, translate (V3 (mv ^. _x) (mv ^. _y) 0) shape 
		, translate (V3 (mv ^. _x) 0          0) shape 
		, translate (V3 0          (mv ^. _y) 0) shape 
		] 

donut :: ℝ -> ℝ -> ℝ -> SymbolicObj3
donut ir or h
	= difference ( cylinder or h )
		[ cylinder ir h ] 

mk_box :: ℝ -> ℝ3 -> SymbolicObj3
mk_box bevel dimensions 
	= withRounding bevel  $ cube False dimensions 

wall2 = wall * 2 
insert_wall = insert_rad + wall
mk_4_insert = square_clone_mv (V2 (x - (2*insert_wall)) (y - (2*insert_wall)))


mk_into_pillars :: ℝ -> SymbolicObj3 ->  SymbolicObj3
mk_into_pillars h single_pillar 
 =	let
 	pillar4x = translate (V3 insert_wall insert_wall 0 )
		$ mk_4_insert single_pillar 
	in
	union 
		[ translate (V3 0 0 wall  ) pillar4x 
		, translate (V3 0 0 (z - (h) )) pillar4x
		]

all_pillars = translate (V3 0 0 0	)
		(mk_into_pillars (insert_height + wall2) $ cylinder insert_wall ( insert_height + wall2 ) )
	
--this is assembled upside down
workpeice_box = difference --differnce trims excess from bottom (top)
	( unionR wall [ mk_box bevel (V3 x y z) ,	all_pillars ] )
	[ translate (V3 0 0   z ) $ cube False (V3 x y z) 
	, translate (V3 0 0 (-z)) $ cube False (V3 x y z)
	]

screw_hole  = cylinder screw_rad (wall  * 2 )
screw_holes = translate (V3 insert_wall insert_wall (- wall) )
	 $ mk_4_insert screw_hole

all_insert_holes = mk_into_pillars (insert_height + wall)$ cylinder insert_rad (insert_height + wall)

threading_cuts = union
	[ screw_holes
	, all_insert_holes
	]
cutbox = translate (pure wall) 
	$ mk_box (bevel - wall)  (V3 (x - wall2) (y - wall2) (z - wall2))

cut = differenceR (insert_wall*2) cutbox [all_pillars] 
	--insert_wall is for ease of printing

cavity_box = difference workpeice_box
	[ cut
	, threading_cuts
	]

lid_cutter =  union
	[ translate (V3 0 0 (-z + insert_height )) $ cut
	, cube False (V3 x y wall )
	]

d_type_back_len = z / 2 
d_type_screw_rad  = 3.2/2
d_type_screw_dist = 11 + d_type_screw_rad
d_type_backplate = translate (V3 d_type_back_len 0 0 )
	$ cube True (V3 d_type_back_len 31 13)
d_type_screw = rotate3 (V3 0 (pi/2) 0) 
	$ cylinder d_type_screw_rad (d_type_back_len / 2)
d_type_pins = cube True (V3 d_type_back_len 18 9.5)

d_type = translate (V3 0 (y/2) (z/2)) $ union 
	[	d_type_backplate 
	,	d_type_pins 
	,	translate (V3 0 d_type_screw_dist 0) d_type_screw 
	,	translate (V3 0 (-d_type_screw_dist) 0) d_type_screw 
	]

wire_hole = translate (V3 x (y/2) (wall)) $ withRounding wall (cube True (V3 30 5 5 ))


lid = intersect [cavity_box ,lid_cutter]
box = difference cavity_box [lid_cutter]
box_w_connectors = difference box
	[ d_type
	, wire_hole
	]


main = sequence [ writeBinSTL 0.5 "box_2_lid.stl" lid, writeBinSTL 0.5 "box_2_box.stl" box_w_connectors ]
