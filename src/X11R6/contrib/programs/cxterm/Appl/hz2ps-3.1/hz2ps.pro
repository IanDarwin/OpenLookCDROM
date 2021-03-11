%%BeginResource hz2ps-prolog 3.0 0

/HZimage        % stack: hex-string (HZbm x HZbm dot matrix)
{
	/HZstr exch def
	gsave
	currentpoint translate
	HZpoint dup scale
	HZbm dup 1 [HZbm 0 0 HZbm neg 0 HZbm] {HZstr} image
	grestore
} def
	
/HZshow        % stack: hex-string
{
	HZimage
	HZskip 0 rmoveto
} def

/HZvshow        % stack: hex-string
{
	HZimage
	0 HZskip neg rmoveto
} def

/HZimageR        % stack: hex-string (HZbm x HZbm dot matrix)
{
	/HZstr exch def
	gsave
	currentpoint translate
	HZpoint dup scale
	HZbm dup 1 [0 HZbm neg HZbm neg 0 HZbm HZbm] {HZstr} image
	grestore
} def
	
/HZvshowR        % stack: hex-string
{
	HZimageR
	0 HZskip neg rmoveto
} def

/HZpath		% stack: --
{
	newpath
	0 0 moveto
	1 0 rlineto
	0 1 rlineto
	-1 0 rlineto
	closepath
} def

/HZimageQ        % stack: hex-string (HZbm x HZbm dot matrix)
{
	/HZstr exch def
	gsave
	currentpoint translate
	HZpoint dup scale
	HZpath clip
	0.25 0.2 translate
	HZbm dup 1 [HZbm 0 0 HZbm neg 0 HZbm] {HZstr} image
	grestore
} def
	
/HZvshowQ	% stack: hex-string
{
	HZimageQ
	0 HZskip neg rmoveto
} def

/Eshow			% stack: string
{
	0 Edescender rmoveto
	show
	0 Edescender neg rmoveto
} def

/Evshow			% stack: string
{
	halfHZpoint HZskip rmoveto	% center line and adjust for vshow
	0 Evskip Epoint sub rmoveto
	Evskip exch vshow
	0 Epoint Evskip sub rmoveto
	halfHZpoint neg HZskip neg rmoveto
} def

/halfHZpoint {HZpoint 2 div} def

/vshow			% stack: num string
{
	/thestring exch def
	/lineskip exch def
	thestring
	{
		/charcode exch def
		/thechar ( ) dup 0 charcode put def
		0 lineskip neg rmoveto
		gsave
		thechar stringwidth pop 2 div neg 0 rmoveto
		thechar show
		grestore
	} forall
} def

/chooseFont		% stack: typeface-name num
{
	exch
	findfont 
	exch scalefont setfont 
} def

/dash			% stack: num
{
	gsave
	HZpoint mul 0 exch rmoveto
	HZskip 0 rlineto
	HZpoint 25 div setlinewidth
	stroke
	grestore
	HZskip 0 rmoveto
} def

/vdash			% stack: num
{
	gsave
	HZpoint mul HZskip rmoveto
	0 HZskip neg rlineto
	HZpoint 25 div setlinewidth
	stroke
	grestore
	0 HZskip neg rmoveto
} def

/prPageNo		% stack: string
{
	gsave
	pnLeft pnBottom moveto
	show
	grestore
} def

/beginpage		% stack: --
{
	/pgsave save def
} def

/endpage		% stack: --
{
	pgsave restore
	showpage
} def

%%EndResource
