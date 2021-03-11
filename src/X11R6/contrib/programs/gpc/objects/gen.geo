%-----------------------------------------------------------------------
|	Sample File ngca1.geo
|	Defines a index_polygons3 cube
|
|	That is tranformed by the contents of Matrix entry 0
|
|	Sample BIF Geometry File
-----------------------------------------------------------------------%

begin_structure 3;

apply_to_global3 10;

gen_sphere3 20 20;
end_structure ;

begin_structure 31;
	interior_lighting 4;
	surface_properties .5 1. 1. 1. 1. 1. 5 0;
	interior_shading 3;
end_structure ;

begin_structure 32;
call_structure 31;
execute_structure 3;
end_structure ;

begin_structure 33;
	interior_lighting 4;
	surface_properties .5 1. 1. 1. 1. 1. 5 0;
	interior_shading 3;
	execute_structure 3;
end_structure ;

begin_structure 34;
interior_color 0 1 0;
end_structure ;



