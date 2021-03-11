%-----------------------------------------------------------------------
|	Sample File ngca1.geo
|	Defines a index_polygons3 cube
|
|	That is tranformed by the contents of Matrix entry 0
|
|	Sample BIF Geometry File
-----------------------------------------------------------------------%

begin_structure 1;

apply_to_global3 0;

interior_shading 2;
index_polygons3
	 1  1  1
	-1  1  1
	-1 -1  1
	 1 -1  1
	 1  1 -1
	 1 -1 -1
	-1 -1 -1
	-1  1 -1

	facet_connectivity
	{
		{ 1 7 6 2 } % -X face %
		{ 0 1 2 3 } % +Z face %
		{ 4 0 3 5 } % +X face %
		{ 1 7 4 0 } % +Y face %
		{ 4 5 6 7 } % -Z face %
		{ 5 3 2 6 } % -Y face %
	}

	facet_normals
	{
		-1  0  0
		 0  0  1
		 1  0  0
		 0  1  0
		 0  0 -1
		 0 -1  0
	}
%
	facet_colors
	{
		0.0 0.5 1.0
		0.0 1.0 0.5
		0.5 0.5 1.0
		0.5 1.0 0.5
		1.0 0.5 1.0
		1.0 1.0 0.5
	}
%
	vertex_colors
	{
		0.0 0.5 1.0
		0.0 1.0 0.5
		0.5 0.5 1.0
		0.5 1.0 0.5
		1.0 0.5 1.0
		1.0 1.0 0.5
		0.0 1.0 0.0
		1.0 0.0 1.0
	}
%
	vertex_normals
	{
		 1  1  1
		-1  1  1
		-1 -1  1
		 1 -1  1
		 1  1 -1
		 1 -1 -1
		-1 -1 -1
		-1  1 -1
	}
%
;

end_structure ;

