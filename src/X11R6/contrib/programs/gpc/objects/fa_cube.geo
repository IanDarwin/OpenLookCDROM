%                                                                      %
% BIF Geometry test file                                               %
% Polygon with data primitives and attributes                          %
%                                                                      %

% Structure 0 defines color indices                                    %

% OBJECT 1: Unit cube with no optional groups                          %
BEGIN_STRUCTURE
1
;

% +Z face                                                              %
INTERIOR_COLOR_INDEX 1 ;
FILL_AREA_SET3
{
-0.5 -0.5  0.5
 0.5 -0.5  0.5
 0.5  0.5  0.5
-0.5  0.5  0.5
}
{
-0.4  0.4  0.5
 0.4  0.4  0.5
 0.4 -0.4  0.5
-0.4 -0.4  0.5
}
;

INTERIOR_COLOR_INDEX
2
;
% -Z face                                                              %
FILL_AREA_SET3
{
 0.5 -0.5 -0.5
-0.5 -0.5 -0.5
-0.5  0.5 -0.5
 0.5  0.5 -0.5
}
{
 0.4  0.4 -0.5
-0.4  0.4 -0.5
-0.4 -0.4 -0.5
 0.4 -0.4 -0.5
}
;

INTERIOR_COLOR_INDEX
3
;
% -X face                                                              %
FILL_AREA_SET3
{
-0.5 -0.5 -0.5
-0.5 -0.5  0.5
-0.5  0.5  0.5
-0.5  0.5 -0.5
}
{
-0.5  0.4 -0.4
-0.5  0.4  0.4
-0.5 -0.4  0.4
-0.5 -0.4 -0.4
}
;

INTERIOR_COLOR_INDEX
4
;
% +X face                                                              %
FILL_AREA_SET3
{
 0.5 -0.5  0.5
 0.5 -0.5 -0.5
 0.5  0.5 -0.5
 0.5  0.5  0.5
}
{
 0.5  0.4  0.4
 0.5  0.4 -0.4
 0.5 -0.4 -0.4
 0.5 -0.4  0.4
}
;

INTERIOR_COLOR_INDEX
5
;
% -Y face                                                              %
FILL_AREA_SET3
{
-0.5 -0.5 -0.5
 0.5 -0.5 -0.5
 0.5 -0.5  0.5
-0.5 -0.5  0.5
}
{
-0.4 -0.5  0.4
 0.4 -0.5  0.4
 0.4 -0.5 -0.4
-0.4 -0.5 -0.4
}
;

INTERIOR_COLOR_INDEX
6
;
% +Y face                                                              %
FILL_AREA_SET3
{
-0.5  0.5  0.5
 0.5  0.5  0.5
 0.5  0.5 -0.5
-0.5  0.5 -0.5
}
{
-0.4  0.5 -0.4
 0.4  0.5 -0.4
 0.4  0.5  0.4
-0.4  0.5  0.4
}
;

% Close OBJECT 1                                                       %
END_STRUCTURE
;

