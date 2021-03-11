%-----------------------------------------------------------------------
|	Sample File ngca1.geo
|	Defines a index_polygons3 cube
|
|	That is tranformed by the contents of Matrix entry 0
|
|	Sample BIF Geometry File
-----------------------------------------------------------------------%

begin_structure 2;

apply_to_global3 0;

quad_mesh3
	2 5
	 1  1  1
	-1  1  1
	-1 -1  1
	 1 -1  1
	 1  1  1
	 1  1 -1
	-1  1 -1
	-1 -1 -1
	 1 -1 -1
	 1  1 -1

	facet_normals
	{
		 0  1  0
		-1  0  0
		 0 -1  0
		 1  0  0
	}

	facet_colors
	{
		0.5 0.0 1.0
		1.0 0.5 0.0
		0.0 1.0 0.5
		1.0 1.0 1.0
	}
	vertex_colors
	{
		0.0 0.5 1.0
		0.0 1.0 0.5
		0.5 0.5 1.0
		0.5 1.0 0.5
		0.0 0.5 1.0
		1.0 0.5 1.0
		1.0 1.0 0.5
		0.0 1.0 0.0
		1.0 0.0 1.0
		1.0 0.5 1.0
	}

	vertex_normals
	{
		 1  1  1
		-1  1  1
		-1 -1  1
		 1 -1  1
		 1  1  1
		 1  1 -1
		-1  1 -1
		-1 -1 -1
		 1 -1 -1
		 1  1 -1
	}
;

end_structure ;

