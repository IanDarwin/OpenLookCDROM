BEGIN_STRUCTURE 110 ;
%------------------------------------%
% Initialize the Motion Channels     %
%------------------------------------%

IDENTITY3  0 ; %  Workspace          %
IDENTITY3  1 ; %  Global Channel     %
IDENTITY3  2 ; %  Global Rate        %
IDENTITY3  3 ; %  Body   Channel     %
IDENTITY3  4 ; %  Body   Pre-Rate    %
IDENTITY3  5 ; %  Body   Post-Rate   %
IDENTITY3  6 ; %  Missle Rate        %
IDENTITY3 34 ; %  Centered Body      %
IDENTITY3 35 ; %  Main Rotor Channel %
IDENTITY3 61 ; %  Tail Rotor Channel %

%------------------------------------%
% Initial Position                   %
%------------------------------------%
% Uncentered Body %
TRANSLATE3   3 0. -700. 0. REPLACE;
ROTATE_XYZ3  3 0. 90. 90.  PRECONCAT;
ROTATE3      3 60. Y_AXIS PRECONCAT ;

% Motion Rates %
ROTATE3      4 -2. Y_AXIS   REPLACE ; % Body Pre-Rate          %
TRANSLATE3   5  0. 17. 0.   REPLACE ; % Body Post-Rate         %
TRANSLATE3   6 -40. 0. 0.   REPLACE ; % Missle Rate            %
IDENTITY3    7 ;                      %  Missle 1 Plume Rate   %
IDENTITY3    8 ;                      %  Missle 2 Plume Rate   %
IDENTITY3    9 ;                      %  Missle 3 Plume Rate   %
IDENTITY3   10 ;                      %  Missle 4 Plume Rate   %
ROTATE3     36 30. Z_AXIS   REPLACE ; %  Main Rotor Rates      %
ROTATE3     62 50. Y_AXIS   REPLACE ; %  Tail Rotor Rates      %
end_structure;

begin_structure 100;
% "Acquire " %
% Set rates to quick pivot turn %
ROTATE3      4  .5 Y_AXIS   REPLACE ;
ROTATE3      5  4. Y_AXIS   REPLACE ;
end_structure;

begin_structure 101;
% "Fire One!!!" %
% Set rates to slow turn %
IDENTITY3    4;
ROTATE3      5  2. Y_AXIS   REPLACE ;
% Initialize Plume Growth %
SCALE3      73  3.0  3.0  3.0  REPLACE ;
SCALE3       7  1.1  1.1  1.1   REPLACE ;
% Initialize the missle position %
CONCAT_MATRIX3 34 52 REPLACE;
end_structure;

begin_structure 102;
% "Fire Two!!!" %
% Initialize Plume Growth %
SCALE3      75  3.0  3.0  3.0  REPLACE ;
SCALE3       9  1.1  1.1  1.1   REPLACE ;
% Initialize the missle position %
CONCAT_MATRIX3 34 54 REPLACE;
end_structure;

begin_structure 103;
% "Acquire No. 2 " %
% Set rates to quick pivot turn %
ROTATE3      4  .5 X_AXIS   REPLACE ;
ROTATE3      5  4. Y_AXIS   REPLACE ;
end_structure;

begin_structure 104;
% "Fire Three !!!" %
% Initialize the missle position %
CONCAT_MATRIX3 34 53 REPLACE;
% Set rates to slow turn %
IDENTITY3    4;
ROTATE3      5  2. Y_AXIS   REPLACE ;
% Initialize Plume Growth %
SCALE3      74  3.0  3.0  3.0  REPLACE ;
SCALE3       8  1.1  1.1  1.1   REPLACE ;
end_structure;

begin_structure 105;
% "Acquire No. 3 " %
% Set rates to quick pivot turn %
ROTATE3      4  .5 Y_AXIS     REPLACE ;
TRANSLATE3   5  0. 0. -4.   PRECONCAT ;
ROTATE3      5  4. Y_AXIS     REPLACE ;
end_structure;

begin_structure 106;
% "Fire Four!!!" %
% Set rates to quick pivot turn %
TRANSLATE3   4  -18. 0. 5. REPLACE ;
IDENTITY3    5  ;
% Initialize the missle position %
CONCAT_MATRIX3 34 55 REPLACE;
% Initialize Plume Growth %
SCALE3      76  3.0  3.0  3.0  REPLACE ;
SCALE3      10  1.1  1.1  1.1   REPLACE ;
end_structure;
