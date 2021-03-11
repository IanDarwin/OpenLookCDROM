%                              %
%	Sub cube               %
%                              %
BEGIN_STRUCTURE 111 ;

% +Z face                      %
INTERIOR_COLOR_INDEX 1 ;
POLYGON3
	-0.3 -0.3  0.3
	 0.3 -0.3  0.3
	 0.3  0.3  0.3
	-0.3  0.3  0.3

	VERTEX_COLORS
	{
		1. 0. 0.
		1. 0. 0.
		1. 0. 0.
		1. 0. 0.
	}

;

% -Z face                      %
INTERIOR_COLOR_INDEX 2 ;
POLYGON3
	 0.3 -0.3 -0.3
	-0.3 -0.3 -0.3
	-0.3  0.3 -0.3
	 0.3  0.3 -0.3

	VERTEX_COLORS
	{
		0  1. 0.
		0  1. 0.
		0  1. 0.
		0  1. 0.
	}

;

% -X face                     %
INTERIOR_COLOR_INDEX 3 ;
POLYGON3

	-0.3 -0.3 -0.3
	-0.3 -0.3  0.3
	-0.3  0.3  0.3
	-0.3  0.3 -0.3


	VERTEX_COLORS
	{
		1. 1. 0.
		1. 1. 0.
		1. 1. 0.
		1. 1. 0.
	}

;

% +X face                      %
INTERIOR_COLOR_INDEX 4 ;
POLYGON3

	 0.3 -0.3  0.3
	 0.3 -0.3 -0.3
	 0.3  0.3 -0.3
	 0.3  0.3  0.3


	VERTEX_COLORS
	{
		0. 0. 1.
		0. 0. 1.
		0. 0. 1.
		0. 0. 1.
	}

;

% -Y face                      %
INTERIOR_COLOR_INDEX 5 ;
POLYGON3

	-0.3 -0.3 -0.3
	 0.3 -0.3 -0.3
	 0.3 -0.3  0.3
	-0.3 -0.3  0.3


	VERTEX_COLORS
	{
		1 0. 1.
		1 0. 1.
		1 0. 1.
		1 0. 1.
	}

;
% +Y face                      %
INTERIOR_COLOR_INDEX 6 ;
POLYGON3

	-0.3  0.3  0.3
	 0.3  0.3  0.3
	 0.3  0.3 -0.3
	-0.3  0.3 -0.3


	VERTEX_COLORS
	{
		0 1. 1.
		0 1. 1.
		0 1. 1.
		0 1. 1.
	}

;

END_STRUCTURE
;

BEGIN_STRUCTURE	91 ;
CONCAT_MATRIX3  10 30 POSTCONCAT ; % NEW Translation   %
CONCAT_MATRIX3  11 40 PRECONCAT ;  % NEW Rotations     %
CONCAT_MATRIX3  30 50 REPLACE ;    % Copy to workspace %
CONCAT_MATRIX3  40 50 PRECONCAT ;  % Compose           %
APPLY_TO_LOCAL3 50 REPLACE ;
CALL_STRUCTURE  111 ;
END_STRUCTURE     ;

BEGIN_STRUCTURE	92 ;
CONCAT_MATRIX3  12 31 POSTCONCAT ; % NEW Translation   %
CONCAT_MATRIX3  13 41 PRECONCAT ;  % NEW Rotations     %
CONCAT_MATRIX3  31 51 REPLACE ;    % Copy to workspace %
CONCAT_MATRIX3  41 51 PRECONCAT ;  % Compose           %
APPLY_TO_LOCAL3 51 REPLACE ;
CALL_STRUCTURE  111 ;
END_STRUCTURE     ;

BEGIN_STRUCTURE	93 ;
CONCAT_MATRIX3  14 32 POSTCONCAT ; % NEW Translation   %
CONCAT_MATRIX3  15 42 PRECONCAT ;  % NEW Rotations     %
CONCAT_MATRIX3  32 52 REPLACE ;    % Copy to workspace %
CONCAT_MATRIX3  42 52 PRECONCAT ;  % Compose           %
APPLY_TO_LOCAL3 52 REPLACE ;
CALL_STRUCTURE  111 ;
END_STRUCTURE     ;

BEGIN_STRUCTURE	94 ;
CONCAT_MATRIX3  16 33 POSTCONCAT ; % NEW Translation   %
CONCAT_MATRIX3  17 43 PRECONCAT ;  % NEW Rotations     %
CONCAT_MATRIX3  33 53 REPLACE ;    % Copy to workspace %
CONCAT_MATRIX3  43 53 PRECONCAT ;  % Compose           %
APPLY_TO_LOCAL3 53 REPLACE ;
CALL_STRUCTURE  111 ;
END_STRUCTURE     ;

BEGIN_STRUCTURE	95 ;
CONCAT_MATRIX3  18 34 POSTCONCAT ; % NEW Translation   %
CONCAT_MATRIX3  19 44 PRECONCAT ;  % NEW Rotations     %
CONCAT_MATRIX3  34 54 REPLACE ;    % Copy to workspace %
CONCAT_MATRIX3  44 54 PRECONCAT ;  % Compose           %
APPLY_TO_LOCAL3 54 REPLACE ;
CALL_STRUCTURE  111 ;
END_STRUCTURE     ;

BEGIN_STRUCTURE	96 ;
CONCAT_MATRIX3  20 35 POSTCONCAT ; % NEW Translation   %
CONCAT_MATRIX3  21 45 PRECONCAT ;  % NEW Rotations     %
CONCAT_MATRIX3  35 55 REPLACE ;    % Copy to workspace %
CONCAT_MATRIX3  45 55 PRECONCAT ;  % Compose           %
APPLY_TO_LOCAL3 55 REPLACE ;
CALL_STRUCTURE  111 ;
END_STRUCTURE     ;


