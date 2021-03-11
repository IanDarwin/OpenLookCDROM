
%-----------------------------------------------------------------------
|	Sample File tri.geo
|	Defines a triangle3 hollow cube
|
|	That is tranformed by the contents of Matrix entry 20
|
|	Sample BIF Geometry File
-----------------------------------------------------------------------%

begin_structure 20;

apply_to_global3 20;

triangle3
	 1  1  1
	-1  1  1
	 1  1 -1
	-1  1 -1
	 1 -1 -1
	-1 -1 -1
	 1 -1  1
	-1 -1  1
	 1  1  1
	-1  1  1

	facet_normals
	{
		 0  1  0
		 0  1  0
		 0  0 -1
		 0  0 -1
		 0 -1  0
		 0 -1  0
		 0  0  1
		 0  0  1
	}

	facet_colors
	{
		0.7 0.7 0.7
		1.0 0.0 0.0
		0.0 1.0 0.0
		0.0 0.0 1.0
		1.0 1.0 0.0
		1.0 0.0 1.0
		0.0 1.0 1.0
		1.0 1.0 1.0
	}
	vertex_colors
	{
		0.7 0.7 0.7
		1.0 0.0 0.0
		0.0 1.0 0.0
		0.0 0.0 1.0
		1.0 1.0 0.0
		1.0 0.0 1.0
		0.0 1.0 1.0
		1.0 1.0 1.0
		1.0 0.5 0.0
		1.0 0.0 0.5
	}

	vertex_normals
	{
		 1  1  1
		-1  1  1
		 1  1 -1
		-1  1 -1
		 1 -1 -1
		-1 -1 -1
		 1 -1  1
		-1 -1  1
		 1  1  1
		-1  1  1
	}
;

end_structure ;

