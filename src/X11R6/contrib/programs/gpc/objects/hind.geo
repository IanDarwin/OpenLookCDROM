% Groups of Helicopter Parts %

% The Body %

% Mi24 "Hind" Attack  Helicopter %
% Mi24 Body %
BEGIN_STRUCTURE 70;
	% Update position %
	CONCAT_MATRIX3  4 3 PRECONCAT;
	CONCAT_MATRIX3  5 3 POSTCONCAT;
	CONCAT_MATRIX3  3 34 REPLACE;
	% Centering transformation %
	TRANSLATE3      34 -350. 0. 0.      PRECONCAT;
	APPLY_TO_LOCAL3 34                  REPLACE  ;

	CALL_STRUCTURE 40 ;
	CALL_STRUCTURE 41 ;
	CALL_STRUCTURE 42 ;
	CALL_STRUCTURE 43 ;
	CALL_STRUCTURE 44 ;
	CALL_STRUCTURE 45 ;
	CALL_STRUCTURE 46 ;
	CALL_STRUCTURE 47 ;
	CALL_STRUCTURE 48 ;
	CALL_STRUCTURE 49 ;
	CALL_STRUCTURE 50 ;
	CALL_STRUCTURE 51 ;
	CALL_STRUCTURE 56 ;
	CALL_STRUCTURE 57 ;
	CALL_STRUCTURE 58 ;
	CALL_STRUCTURE 59 ;
	CALL_STRUCTURE 60 ;
	CALL_STRUCTURE 64 ;
	CALL_STRUCTURE 65 ;
	CALL_STRUCTURE 66 ;
END_STRUCTURE;


% The Attached missles %
BEGIN_STRUCTURE 71;
	CALL_STRUCTURE 52 ;
	CALL_STRUCTURE 53 ;
	CALL_STRUCTURE 54 ;
	CALL_STRUCTURE 55 ;
END_STRUCTURE;

% The moving rotors %
BEGIN_STRUCTURE 72;
	% >>> Main Rotor <<< %
	% Update position    %
	CONCAT_MATRIX3  36   35            PRECONCAT;
	
	% Build matrix       %
	CONCAT_MATRIX3  34    0            REPLACE;
	TRANSLATE3       0  268.08 0. 0.   PRECONCAT;
	CONCAT_MATRIX3  35    0            PRECONCAT;
	TRANSLATE3       0 -268.08 0. 0.   PRECONCAT;
	APPLY_TO_LOCAL3  0                 REPLACE;
	
	% Display Main Rotor %
	CALL_STRUCTURE 35 ;
	CALL_STRUCTURE 36 ;
	CALL_STRUCTURE 37 ;
	CALL_STRUCTURE 38 ;
	CALL_STRUCTURE 39 ;

	% >>> Tail Rotor <<< %
	% Update position    %
	CONCAT_MATRIX3  62   61            PRECONCAT;

	% Build matrix       %
	CONCAT_MATRIX3  34    0              REPLACE;
	TRANSLATE3       0  706.70 0.  70.62 PRECONCAT;
	CONCAT_MATRIX3  61    0              PRECONCAT;
	TRANSLATE3       0 -706.70 0. -70.62 PRECONCAT;
	APPLY_TO_LOCAL3  0                   REPLACE;

	% Display Tail Rotor %
	CALL_STRUCTURE 61 ;
	CALL_STRUCTURE 62 ;
	CALL_STRUCTURE 63 ;
END_STRUCTURE ;


% The moving missles %

% >>>> Missle #1 <<<< %
BEGIN_STRUCTURE 73;
	% Update position    %
	CONCAT_MATRIX3    6   52            PRECONCAT;
	APPLY_TO_LOCAL3  52                   REPLACE;
	
	% Display Missle #1 %
	CALL_STRUCTURE 52 ;

	% Display Plume %
	% Place and Orient %
	LOCAL_TRANSFORMATION3
		{    1.0    0.0     0.0  349.0 }
		{    0.0    1.0     0.0  167.2 }
		{    0.0    0.0     1.0  -43.0 }
		{    0.0    0.0     0.0    1.0 }
		PRECONCAT
	;
	% Scale %
	CONCAT_MATRIX3    7 73              PRECONCAT;
	APPLY_TO_LOCAL3  73                 PRECONCAT;
	
	CALL_STRUCTURE 68;
	

END_STRUCTURE;

% >>>> Missle #2 <<<< %
BEGIN_STRUCTURE 74;
	% Update position    %
	CONCAT_MATRIX3    6   53            PRECONCAT;
	APPLY_TO_LOCAL3  53                   REPLACE;
	
	% Display Missle #2 %
	CALL_STRUCTURE 53 ;

	% Display Plume %
	% Place and Orient %
	LOCAL_TRANSFORMATION3
		{    1.0    0.0     0.0  349.0 }
		{    0.0    1.0     0.0 -167.2 }
		{    0.0    0.0     1.0  -43.0 }
		{    0.0    0.0     0.0    1.0 }
		PRECONCAT
	;
	% Scale %
	CONCAT_MATRIX3    8 74              PRECONCAT;
	APPLY_TO_LOCAL3  74                 PRECONCAT;
	
	CALL_STRUCTURE 68;
	

END_STRUCTURE;

% >>>> Missle #3 <<<< %
BEGIN_STRUCTURE 75;
	% Update position    %
	CONCAT_MATRIX3    6   54            PRECONCAT;
	APPLY_TO_LOCAL3  54                   REPLACE;
	
	% Display Missle #3 %
	CALL_STRUCTURE 54 ;

	% Display Plume %
	% Place and Orient %
	LOCAL_TRANSFORMATION3
		{    1.0    0.0     0.0  349.0 }
		{    0.0    1.0     0.0  150.0 }
		{    0.0    0.0     1.0  -43.0 }
		{    0.0    0.0     0.0    1.0 }
		PRECONCAT
	;
	% Scale %
	CONCAT_MATRIX3    9 75              PRECONCAT;
	APPLY_TO_LOCAL3  75                 PRECONCAT;
	
	CALL_STRUCTURE 68;

END_STRUCTURE;

% >>>> Missle #4 <<<< %
BEGIN_STRUCTURE 76;
	% Update position    %
	CONCAT_MATRIX3    6   55            PRECONCAT;
	APPLY_TO_LOCAL3  55                   REPLACE;
	
	% Display Missle #4 %
	CALL_STRUCTURE 55 ;

	% Display Plume %
	% Place and Orient %
	LOCAL_TRANSFORMATION3
		{    1.0    0.0     0.0  349.0 }
		{    0.0    1.0     0.0 -150.0 }
		{    0.0    0.0     1.0  -43.0 }
		{    0.0    0.0     0.0    1.0 }
		PRECONCAT
	;
	% Scale %
	CONCAT_MATRIX3   10 76              PRECONCAT;
	APPLY_TO_LOCAL3  76                 PRECONCAT;
	
	CALL_STRUCTURE 68;

END_STRUCTURE;

