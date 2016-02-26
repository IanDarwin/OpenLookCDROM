# include "stdio.h"
# define U(x) x
# define NLSTATE yyprevious=YYNEWLINE
# define BEGIN yybgin = yysvec + 1 +
# define INITIAL 0
# define YYLERR yysvec
# define YYSTATE (yyestate-yysvec-1)
# define YYOPTIM 1
# define YYLMAX BUFSIZ
# define output(c) putc(c,yyout)
# define input() (((yytchar=yysptr>yysbuf?U(*--yysptr):getc(yyin))==10?(yylineno++,yytchar):yytchar)==EOF?0:yytchar)
# define unput(c) {yytchar= (c);if(yytchar=='\n')yylineno--;*yysptr++=yytchar;}
# define yymore() (yymorfg=1)
# define ECHO fprintf(yyout, "%s",yytext)
# define REJECT { nstr = yyreject(); goto yyfussy;}
int yyleng; extern char yytext[];
int yymorfg;
extern char *yysptr, yysbuf[];
int yytchar;
FILE *yyin = {stdin}, *yyout = {stdout};
extern int yylineno;
struct yysvf { 
	struct yywork *yystoff;
	struct yysvf *yyother;
	int *yystops;};
struct yysvf *yyestate;
extern struct yysvf yysvec[], *yybgin;
# define YYNEWLINE 10
yylex(){
int nstr; extern int yyprevious;
/* Parser for GIL files. */
#include <stdlib.h>
#include "x.tab.h"
extern int tokval;
while((nstr = yylook()) >= 0)
yyfussy: switch(nstr){
case 0:
if(yywrap()) return(0); break;
case 1:
	return GIL_HDR;
break;
case 2:
	{ return LEFT; }
break;
case 3:
	{ return RIGHT; }
break;
case 4:
	{ 
		if (yytext[yyleng-1] == '\\')
			yymore();
		else {
			tokval = (int)strdup(yytext);
			return QSTRING;
		}
		}
break;
case 5:
	{ return NIL; }
break;
case 6:
{ return TRUE; }
break;
case 7:
{ tokval = (int)strdup(yytext); return OBJ_NAME; }
break;
case 8:
	{ tokval = atoi(yytext); return NUMBER; }
break;
case 9:
;
break;
case 10:
		return ACTION;
break;
case 11:
	return ACTIONS;
break;
case 12:
	return ANCHOR_OBJECT;
break;
case 13:
	return ANCHOR_POINT;
break;
case 14:
	return ARG_TYPE;
break;
case 15:
return BACKGROUND_COLOR;
break;
case 16:
	return BUTTON_TYPE;
break;
case 17:
	return CHOICE_COLORS;
break;
case 18:
return CHOICE_LABEL_TYPES;
break;
case 19:
	return CHOICES;
break;
case 20:
return COLUMN_ALIGNMENT;
break;
case 21:
	return COLUMNS;
break;
case 22:
	return CONSTANT_WIDTH;
break;
case 23:
return DEFAULT_DROP_SITE;
break;
case 24:
	return DONE_HANDLER;
break;
case 25:
	return DROPPABLE;
break;
case 26:
	return EVENT_HANDLER;
break;
case 27:
return FOREGROUND_COLOR;
break;
case 28:
		return FROM;
break;
case 29:
	return FUNCTION_TYPE;
break;
case 30:
	return GROUP_TYPE;
break;
case 31:
		return HEIGHT;
break;
case 32:
		return HELP;
break;
case 33:
return HORIZONTAL_OFFSET;
break;
case 34:
return HORIZONTAL_SPACING;
break;
case 35:
	return ICON_FILE;
break;
case 36:
	return ICON_LABEL;
break;
case 37:
	return ICON_MASK_FILE;
break;
case 38:
return INITIAL_LIST_GLYPHS;
break;
case 39:
return INITIAL_LIST_VALUES;
break;
case 40:
return INITIAL_SELECTIONS;
break;
case 41:
	return INITIAL_STATE;
break;
case 42:
	return INITIAL_VALUE;
break;
case 43:
		return LABEL;
break;
case 44:
	return LABEL_TYPE;
break;
case 45:
	return LAYOUT_TYPE;
break;
case 46:
	return MAX_VALUE;
break;
case 47:
	return MEMBERS;
break;
case 48:
		return MENU;
break;
case 49:
	return MIN_VALUE;
break;
case 50:
return MULTIPLE_SELECTIONS;
break;
case 51:
		return NAME;
break;
case 52:
	return NOTIFY_HANDLER;
break;
case 53:
		return OWNER;
break;
case 54:
		return PINNED;
break;
case 55:
	return READ_ONLY;
break;
case 56:
return REFERENCE_POINT;
break;
case 57:
	return RESIZABLE;
break;
case 58:
	return ROW_ALIGNMENT;
break;
case 59:
		return ROWS;
break;
case 60:
return SELECTION_REQUIRED;
break;
case 61:
	return SETTING_TYPE;
break;
case 62:
	return SHOW_BORDER;
break;
case 63:
	return SHOW_FOOTER;
break;
case 64:
	return STORED_LENGTH;
break;
case 65:
	return TEXT_TYPE;
break;
case 66:
		return TITLE;
break;
case 67:
		return TO;
break;
case 68:
		return TYPE;
break;
case 69:
	return USER_DATA;
break;
case 70:
	return VALUE_LENGTH;
break;
case 71:
return VALUE_UNDERLINED;
break;
case 72:
	return VALUE_X;
break;
case 73:
	return VALUE_Y;
break;
case 74:
return VERTICAL_OFFSET;
break;
case 75:
return VERTICAL_SPACING;
break;
case 76:
		return WHEN;
break;
case 77:
		return WIDTH;
break;
case 78:
		return X;
break;
case 79:
		return Y;
break;
case 80:
		return ACTIVE;
break;
case 81:
	return ALPHANUMERIC;
break;
case 82:
	return BASE_WINDOW;
break;
case 83:
	return BOTTOM_EDGES;
break;
case 84:
		return BUTTON;
break;
case 85:
		return COLUMN;
break;
case 86:
	return CONTROL_AREA;
break;
case 87:
	return EXCLUSIVE;
break;
case 88:
		return GROUP;
break;
case 89:
	return HORIZONTAL;
break;
case 90:
return HORIZONTAL_CENTERS;
break;
case 91:
	return INACTIVE;
break;
case 92:
	return INVISIBLE;
break;
case 93:
		return LABELS;
break;
case 94:
	return LEFT_EDGES;
break;
case 95:
		return NORMAL;
break;
case 96:
	return NORTH_WEST;
break;
case 97:
	return NUMERIC;
break;
case 98:
		return OPEN;
break;
case 99:
	return POPUP_WINDOW;
break;
case 100:
		return ROW;
break;
case 101:
	return SCROLLING_LIST;
break;
case 102:
	return SETTING;
break;
case 103:
		return STRING;
break;
case 104:
	return TEXT_FIELD;
break;
case 105:
	return TEXT_PANE;
break;
case 106:
	return TOP_EDGES;
break;
case 107:
	return USER_DEFINED;
break;
case 108:
return VERTICAL_CENTERS;
break;
case 109:
	return VISIBLE;
break;
case -1:
break;
default:
fprintf(yyout,"bad switch yylook %d",nstr);
} return(0); }
/* end of yylex */
int yyvstop[] = {
0,

9,
0,

2,
0,

3,
0,

8,
0,

7,
0,

7,
0,

7,
0,

4,
0,

78,
0,

79,
0,

7,
0,

6,
0,

67,
0,

5,
7,
0,

100,
0,

28,
0,

32,
0,

48,
0,

51,
0,

98,
0,

59,
0,

68,
0,

76,
0,

88,
0,

43,
0,

53,
0,

66,
0,

77,
0,

1,
0,

10,
0,

80,
0,

84,
0,

85,
0,

31,
0,

93,
0,

95,
0,

54,
0,

103,
0,

11,
0,

19,
0,

21,
0,

47,
0,

97,
0,

102,
0,

72,
0,

73,
0,

109,
0,

14,
0,

91,
0,

25,
0,

87,
0,

35,
0,

92,
0,

46,
0,

49,
0,

55,
0,

57,
0,

105,
0,

65,
0,

106,
0,

69,
0,

30,
0,

89,
0,

36,
0,

44,
0,

94,
0,

96,
0,

104,
0,

82,
0,

16,
0,

45,
0,

62,
0,

63,
0,

81,
0,

13,
0,

83,
0,

86,
0,

24,
0,

99,
0,

61,
0,

107,
0,

70,
0,

12,
0,

17,
0,

26,
0,

29,
0,

41,
0,

42,
0,

58,
0,

64,
0,

22,
0,

37,
0,

52,
0,

101,
0,

56,
0,

74,
0,

15,
0,

20,
0,

27,
0,

71,
0,

108,
0,

75,
0,

23,
0,

33,
0,

18,
0,

90,
0,

34,
0,

40,
0,

60,
0,

38,
0,

39,
0,

50,
0,
0};
# define YYTYPE int
struct yywork { YYTYPE verify, advance; } yycrank[] = {
0,0,	0,0,	1,0,	1,0,	
1,0,	1,0,	1,0,	1,0,	
1,0,	1,0,	1,3,	0,0,	
1,0,	1,0,	1,0,	1,0,	
1,0,	1,0,	1,0,	1,0,	
1,0,	1,0,	1,0,	1,0,	
1,0,	1,0,	1,0,	1,0,	
1,0,	1,0,	1,0,	1,0,	
1,0,	0,0,	1,0,	1,4,	
1,0,	1,0,	1,0,	1,0,	
1,0,	1,5,	1,6,	1,0,	
1,0,	1,0,	1,0,	1,0,	
1,0,	1,7,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	1,8,	
1,9,	1,0,	1,0,	1,0,	
1,0,	1,0,	1,10,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	4,4,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	4,4,	7,7,	7,7,	
7,7,	7,7,	7,7,	7,7,	
7,7,	7,7,	7,7,	7,7,	
1,0,	1,0,	1,0,	1,0,	
9,36,	1,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	4,13,	0,0,	
0,0,	0,0,	0,0,	1,11,	
0,0,	0,0,	0,0,	16,46,	
11,37,	1,12,	0,0,	0,0,	
4,4,	0,0,	16,47,	0,0,	
1,0,	1,0,	1,0,	1,0,	
1,0,	2,0,	2,0,	2,0,	
2,0,	2,0,	2,0,	2,0,	
2,0,	4,4,	20,56,	2,0,	
2,0,	2,0,	2,0,	2,0,	
2,0,	2,0,	2,0,	2,0,	
2,0,	2,0,	2,0,	2,0,	
2,0,	2,0,	2,0,	2,0,	
2,0,	2,0,	2,0,	2,0,	
18,51,	2,0,	18,52,	2,0,	
2,0,	2,0,	2,0,	2,0,	
2,5,	2,6,	2,0,	2,0,	
2,0,	2,0,	2,0,	2,0,	
12,38,	19,53,	23,61,	27,72,	
19,54,	31,84,	23,62,	19,55,	
12,38,	27,73,	2,8,	2,9,	
2,0,	2,0,	2,0,	2,0,	
2,0,	8,14,	8,15,	8,16,	
8,17,	8,18,	8,19,	8,20,	
8,21,	8,22,	33,88,	33,89,	
8,23,	8,24,	8,25,	8,26,	
8,27,	12,38,	8,28,	8,29,	
8,30,	8,31,	8,32,	8,33,	
8,34,	8,35,	14,39,	2,0,	
2,0,	2,0,	2,0,	12,38,	
2,0,	15,43,	22,59,	14,40,	
25,67,	14,41,	29,76,	21,57,	
29,77,	14,42,	36,90,	29,78,	
17,48,	22,60,	2,11,	15,44,	
12,10,	21,58,	25,68,	26,70,	
2,12,	15,45,	17,49,	29,79,	
25,69,	17,50,	26,71,	2,0,	
2,0,	2,0,	2,0,	2,0,	
10,10,	10,10,	10,10,	10,10,	
10,10,	10,10,	10,10,	10,10,	
10,10,	10,10,	10,10,	10,10,	
10,10,	10,10,	10,10,	10,10,	
10,10,	10,10,	10,10,	10,10,	
10,10,	10,10,	10,10,	10,10,	
10,10,	10,10,	37,91,	39,92,	
40,93,	41,94,	10,10,	42,95,	
10,10,	10,10,	10,10,	10,10,	
10,10,	10,10,	10,10,	10,10,	
10,10,	10,10,	10,10,	10,10,	
10,10,	10,10,	10,10,	10,10,	
10,10,	10,10,	10,10,	10,10,	
10,10,	10,10,	10,10,	10,10,	
10,10,	10,10,	24,63,	28,74,	
30,80,	43,96,	24,64,	32,85,	
30,81,	44,98,	24,65,	32,86,	
45,99,	28,75,	30,82,	32,87,	
46,100,	47,101,	48,103,	47,102,	
49,104,	43,97,	24,66,	50,105,	
30,83,	51,106,	52,107,	53,108,	
54,109,	55,110,	56,111,	57,112,	
58,114,	59,115,	57,113,	60,116,	
61,119,	62,121,	63,122,	64,123,	
64,124,	65,125,	66,126,	60,117,	
67,127,	68,128,	69,130,	68,129,	
70,131,	71,132,	72,133,	73,134,	
74,135,	75,138,	76,139,	77,140,	
60,118,	74,136,	78,142,	61,120,	
80,145,	79,143,	81,146,	77,141,	
79,144,	82,147,	83,148,	84,149,	
85,150,	86,151,	74,137,	87,152,	
88,153,	89,154,	90,155,	92,156,	
93,157,	94,158,	95,159,	96,160,	
97,161,	98,162,	99,163,	100,164,	
101,165,	102,166,	102,167,	103,168,	
104,169,	105,170,	106,171,	107,172,	
108,173,	109,174,	110,175,	111,176,	
112,177,	113,178,	114,179,	115,180,	
116,181,	117,182,	118,183,	119,184,	
120,185,	121,186,	122,187,	123,188,	
124,189,	125,190,	126,191,	127,192,	
128,193,	129,195,	130,196,	131,197,	
132,198,	133,199,	134,200,	128,194,	
135,201,	136,202,	137,203,	138,204,	
139,206,	140,207,	141,208,	142,209,	
143,210,	144,211,	145,212,	146,213,	
147,214,	148,215,	149,216,	150,217,	
151,218,	152,219,	153,220,	154,221,	
155,222,	156,223,	157,225,	158,226,	
159,227,	160,228,	161,229,	162,230,	
156,224,	163,231,	164,232,	165,233,	
166,234,	167,235,	168,236,	169,237,	
170,238,	171,239,	172,240,	173,241,	
175,242,	176,243,	177,244,	179,245,	
180,246,	181,247,	182,248,	183,249,	
184,250,	185,251,	186,252,	187,253,	
188,254,	190,255,	191,256,	193,257,	
194,258,	195,259,	196,260,	198,261,	
199,262,	200,263,	201,264,	202,265,	
203,266,	204,267,	206,268,	207,269,	
208,270,	209,271,	210,272,	211,273,	
212,274,	138,205,	213,275,	214,276,	
216,277,	217,279,	218,280,	219,281,	
221,282,	222,283,	223,284,	224,285,	
225,286,	226,287,	227,288,	228,289,	
229,290,	230,291,	231,292,	232,293,	
233,294,	234,295,	235,296,	236,297,	
237,298,	238,299,	239,300,	240,301,	
241,302,	242,303,	243,304,	244,305,	
245,306,	246,307,	247,310,	248,311,	
249,312,	250,313,	251,315,	246,308,	
246,309,	252,316,	253,317,	254,318,	
255,319,	256,320,	257,321,	258,322,	
259,323,	260,324,	262,325,	263,326,	
264,327,	265,328,	216,278,	266,329,	
267,330,	268,331,	269,332,	270,333,	
271,334,	272,336,	273,337,	274,338,	
271,335,	276,341,	277,342,	278,343,	
279,344,	280,345,	281,346,	284,347,	
286,348,	274,339,	287,349,	288,350,	
289,351,	274,340,	290,352,	291,353,	
292,354,	293,355,	294,357,	295,359,	
296,360,	297,361,	298,362,	299,363,	
300,364,	301,365,	302,366,	303,367,	
304,368,	306,369,	307,370,	308,371,	
309,372,	310,373,	311,374,	312,375,	
313,376,	315,377,	316,378,	317,379,	
318,380,	319,381,	320,382,	250,314,	
322,383,	323,384,	324,385,	326,386,	
327,387,	328,388,	329,389,	330,390,	
331,391,	332,392,	333,393,	334,394,	
335,395,	336,396,	338,397,	339,398,	
340,399,	341,400,	342,401,	343,402,	
344,403,	345,407,	346,408,	348,409,	
349,410,	349,411,	350,412,	351,413,	
352,414,	344,404,	353,415,	354,416,	
344,405,	344,406,	357,419,	359,420,	
355,417,	360,421,	361,422,	362,423,	
363,424,	364,425,	365,426,	293,356,	
294,358,	355,418,	366,427,	367,428,	
368,429,	369,430,	370,431,	371,432,	
372,433,	373,434,	374,435,	375,436,	
376,437,	377,438,	378,439,	379,440,	
381,441,	382,442,	383,443,	384,444,	
386,445,	387,446,	388,447,	389,448,	
390,449,	391,450,	392,451,	393,452,	
394,453,	395,454,	396,455,	397,456,	
398,457,	399,458,	400,459,	401,460,	
402,461,	403,462,	404,463,	407,464,	
409,465,	410,466,	411,467,	413,468,	
414,469,	415,470,	416,471,	417,472,	
418,473,	419,474,	420,475,	421,476,	
422,477,	423,478,	424,479,	425,480,	
426,481,	427,482,	428,483,	429,484,	
430,485,	431,486,	432,487,	433,488,	
435,489,	436,492,	437,493,	438,494,	
439,495,	440,496,	441,497,	435,490,	
442,498,	443,499,	435,491,	444,500,	
445,501,	446,502,	447,503,	448,504,	
449,505,	450,506,	451,507,	452,508,	
453,509,	454,510,	455,511,	456,512,	
457,513,	458,514,	459,515,	460,516,	
461,517,	462,518,	463,519,	464,520,	
465,521,	466,522,	467,523,	468,524,	
469,525,	470,526,	471,527,	472,528,	
473,529,	474,530,	475,531,	476,532,	
477,533,	478,534,	480,535,	482,536,	
483,537,	484,538,	485,539,	487,540,	
488,541,	489,542,	490,543,	491,545,	
493,546,	494,547,	495,548,	498,549,	
499,550,	500,551,	501,552,	503,553,	
505,554,	506,555,	507,556,	508,557,	
509,558,	490,544,	510,559,	511,560,	
512,561,	517,562,	518,563,	519,564,	
520,565,	521,568,	522,569,	523,570,	
524,571,	525,572,	526,573,	527,574,	
528,575,	529,576,	530,577,	531,578,	
520,566,	532,579,	533,580,	534,581,	
520,567,	535,582,	536,583,	537,584,	
539,585,	541,586,	542,587,	543,588,	
544,589,	545,590,	547,591,	549,592,	
551,593,	552,594,	553,595,	554,596,	
555,597,	556,598,	557,599,	558,600,	
559,601,	560,602,	562,603,	563,604,	
564,605,	565,606,	566,607,	567,608,	
568,609,	569,610,	570,611,	571,612,	
573,613,	575,614,	576,615,	577,616,	
578,617,	579,618,	580,619,	581,620,	
582,621,	583,622,	584,623,	585,624,	
586,627,	587,628,	588,629,	589,630,	
590,631,	592,632,	593,633,	594,634,	
595,635,	596,636,	597,637,	585,625,	
598,638,	599,639,	602,640,	585,626,	
603,641,	604,642,	605,643,	606,644,	
607,645,	608,646,	610,647,	612,648,	
614,649,	615,650,	616,651,	617,652,	
619,653,	621,654,	622,655,	623,656,	
624,657,	625,658,	626,659,	627,660,	
628,661,	629,662,	630,663,	631,664,	
632,665,	633,666,	635,667,	636,668,	
637,669,	638,670,	640,671,	643,672,	
644,673,	645,674,	646,675,	648,676,	
650,677,	651,678,	652,679,	653,680,	
655,681,	657,682,	658,683,	659,684,	
660,685,	661,686,	662,688,	665,689,	
666,690,	667,691,	669,692,	670,693,	
672,694,	673,695,	674,696,	675,697,	
676,698,	677,699,	678,700,	680,701,	
661,687,	681,702,	682,703,	683,704,	
684,705,	686,706,	687,707,	688,708,	
689,709,	691,710,	693,711,	694,712,	
695,713,	696,714,	697,715,	698,716,	
699,717,	700,718,	701,719,	702,720,	
703,721,	704,722,	705,723,	706,724,	
707,725,	708,726,	709,727,	711,728,	
712,729,	713,730,	715,731,	717,732,	
719,733,	721,734,	722,735,	723,736,	
724,737,	725,738,	726,739,	727,740,	
728,741,	732,742,	734,743,	736,744,	
737,745,	738,746,	739,747,	740,748,	
741,749,	745,750,	746,751,	748,752,	
0,0};
struct yysvf yysvec[] = {
0,	0,	0,
yycrank+-1,	0,		0,	
yycrank+-128,	yysvec+1,	0,	
yycrank+0,	0,		yyvstop+1,
yycrank+-72,	0,		0,	
yycrank+0,	0,		yyvstop+3,
yycrank+0,	0,		yyvstop+5,
yycrank+34,	0,		yyvstop+7,
yycrank+96,	0,		0,	
yycrank+25,	0,		0,	
yycrank+191,	0,		yyvstop+9,
yycrank+11,	yysvec+10,	yyvstop+11,
yycrank+-175,	0,		yyvstop+13,
yycrank+0,	0,		yyvstop+15,
yycrank+119,	0,		0,	
yycrank+128,	0,		0,	
yycrank+11,	0,		0,	
yycrank+135,	0,		0,	
yycrank+42,	0,		0,	
yycrank+66,	0,		0,	
yycrank+24,	0,		0,	
yycrank+130,	0,		0,	
yycrank+127,	0,		0,	
yycrank+81,	0,		0,	
yycrank+217,	0,		0,	
yycrank+131,	0,		0,	
yycrank+131,	0,		0,	
yycrank+74,	0,		0,	
yycrank+214,	0,		0,	
yycrank+131,	0,		0,	
yycrank+215,	0,		0,	
yycrank+66,	0,		0,	
yycrank+222,	0,		0,	
yycrank+98,	0,		0,	
yycrank+0,	0,		yyvstop+17,
yycrank+0,	0,		yyvstop+19,
yycrank+161,	0,		0,	
yycrank+174,	yysvec+10,	yyvstop+21,
yycrank+0,	0,		yyvstop+23,
yycrank+167,	0,		0,	
yycrank+172,	0,		0,	
yycrank+186,	0,		0,	
yycrank+184,	0,		0,	
yycrank+218,	0,		0,	
yycrank+205,	0,		0,	
yycrank+208,	0,		0,	
yycrank+217,	0,		0,	
yycrank+221,	0,		0,	
yycrank+228,	0,		0,	
yycrank+222,	0,		0,	
yycrank+224,	0,		0,	
yycrank+236,	0,		0,	
yycrank+239,	0,		0,	
yycrank+225,	0,		0,	
yycrank+229,	0,		0,	
yycrank+231,	0,		0,	
yycrank+231,	0,		0,	
yycrank+238,	0,		0,	
yycrank+230,	0,		0,	
yycrank+234,	0,		0,	
yycrank+250,	0,		0,	
yycrank+250,	0,		0,	
yycrank+247,	0,		0,	
yycrank+230,	0,		0,	
yycrank+242,	0,		0,	
yycrank+243,	0,		0,	
yycrank+246,	0,		0,	
yycrank+247,	0,		0,	
yycrank+243,	0,		0,	
yycrank+249,	0,		0,	
yycrank+259,	0,		0,	
yycrank+251,	0,		0,	
yycrank+252,	0,		0,	
yycrank+251,	0,		0,	
yycrank+267,	0,		0,	
yycrank+246,	0,		0,	
yycrank+252,	0,		0,	
yycrank+259,	0,		0,	
yycrank+259,	0,		0,	
yycrank+262,	0,		0,	
yycrank+252,	0,		0,	
yycrank+258,	0,		0,	
yycrank+265,	0,		yyvstop+25,
yycrank+266,	0,		0,	
yycrank+278,	0,		0,	
yycrank+272,	0,		0,	
yycrank+267,	0,		0,	
yycrank+268,	0,		0,	
yycrank+283,	0,		0,	
yycrank+285,	0,		0,	
yycrank+310,	0,		0,	
yycrank+0,	yysvec+10,	yyvstop+27,
yycrank+282,	0,		0,	
yycrank+284,	0,		0,	
yycrank+285,	0,		0,	
yycrank+295,	0,		0,	
yycrank+284,	0,		0,	
yycrank+291,	0,		0,	
yycrank+277,	0,		0,	
yycrank+278,	0,		0,	
yycrank+290,	0,		0,	
yycrank+279,	0,		0,	
yycrank+282,	0,		0,	
yycrank+302,	0,		0,	
yycrank+299,	0,		0,	
yycrank+289,	0,		0,	
yycrank+292,	0,		0,	
yycrank+295,	0,		0,	
yycrank+303,	0,		0,	
yycrank+296,	0,		0,	
yycrank+307,	0,		0,	
yycrank+290,	0,		0,	
yycrank+305,	0,		0,	
yycrank+297,	0,		0,	
yycrank+305,	0,		0,	
yycrank+301,	0,		0,	
yycrank+313,	0,		0,	
yycrank+297,	0,		0,	
yycrank+309,	0,		0,	
yycrank+314,	0,		0,	
yycrank+305,	0,		0,	
yycrank+301,	0,		0,	
yycrank+373,	0,		0,	
yycrank+321,	0,		0,	
yycrank+303,	0,		0,	
yycrank+376,	0,		0,	
yycrank+306,	0,		0,	
yycrank+322,	0,		0,	
yycrank+315,	0,		0,	
yycrank+320,	0,		0,	
yycrank+325,	0,		0,	
yycrank+317,	0,		0,	
yycrank+327,	0,		0,	
yycrank+319,	0,		0,	
yycrank+313,	0,		0,	
yycrank+332,	0,		0,	
yycrank+332,	0,		0,	
yycrank+329,	0,		0,	
yycrank+390,	0,		yyvstop+30,
yycrank+325,	0,		0,	
yycrank+336,	0,		0,	
yycrank+322,	0,		0,	
yycrank+320,	0,		0,	
yycrank+326,	0,		0,	
yycrank+336,	0,		0,	
yycrank+326,	0,		0,	
yycrank+335,	0,		0,	
yycrank+399,	0,		0,	
yycrank+344,	0,		0,	
yycrank+332,	0,		0,	
yycrank+330,	0,		0,	
yycrank+332,	0,		0,	
yycrank+344,	0,		0,	
yycrank+340,	0,		0,	
yycrank+335,	0,		0,	
yycrank+407,	0,		0,	
yycrank+342,	0,		0,	
yycrank+357,	0,		0,	
yycrank+344,	0,		0,	
yycrank+340,	0,		0,	
yycrank+354,	0,		0,	
yycrank+413,	0,		0,	
yycrank+348,	0,		0,	
yycrank+350,	0,		0,	
yycrank+363,	0,		0,	
yycrank+354,	0,		0,	
yycrank+348,	0,		0,	
yycrank+351,	0,		0,	
yycrank+349,	0,		0,	
yycrank+422,	0,		0,	
yycrank+356,	0,		0,	
yycrank+353,	0,		0,	
yycrank+353,	0,		0,	
yycrank+368,	0,		0,	
yycrank+0,	0,		yyvstop+32,
yycrank+356,	0,		0,	
yycrank+361,	0,		0,	
yycrank+370,	0,		0,	
yycrank+0,	0,		yyvstop+34,
yycrank+353,	0,		0,	
yycrank+431,	0,		0,	
yycrank+361,	0,		0,	
yycrank+373,	0,		0,	
yycrank+364,	0,		0,	
yycrank+372,	0,		0,	
yycrank+364,	0,		0,	
yycrank+437,	0,		0,	
yycrank+365,	0,		0,	
yycrank+383,	0,		0,	
yycrank+0,	0,		yyvstop+36,
yycrank+367,	0,		0,	
yycrank+381,	0,		0,	
yycrank+0,	0,		yyvstop+38,
yycrank+390,	0,		0,	
yycrank+384,	0,		0,	
yycrank+387,	0,		0,	
yycrank+376,	0,		0,	
yycrank+0,	0,		yyvstop+40,
yycrank+377,	0,		0,	
yycrank+391,	0,		0,	
yycrank+381,	0,		0,	
yycrank+449,	0,		0,	
yycrank+381,	0,		0,	
yycrank+374,	0,		0,	
yycrank+400,	0,		0,	
yycrank+0,	0,		yyvstop+42,
yycrank+390,	0,		0,	
yycrank+400,	0,		0,	
yycrank+395,	0,		0,	
yycrank+456,	0,		0,	
yycrank+401,	0,		0,	
yycrank+393,	0,		0,	
yycrank+459,	0,		0,	
yycrank+405,	0,		0,	
yycrank+406,	0,		0,	
yycrank+0,	0,		yyvstop+44,
yycrank+463,	0,		0,	
yycrank+408,	0,		0,	
yycrank+405,	0,		0,	
yycrank+413,	0,		0,	
yycrank+0,	0,		yyvstop+46,
yycrank+408,	0,		0,	
yycrank+462,	0,		0,	
yycrank+404,	0,		0,	
yycrank+414,	0,		0,	
yycrank+406,	0,		0,	
yycrank+403,	0,		0,	
yycrank+397,	0,		0,	
yycrank+405,	0,		0,	
yycrank+401,	0,		0,	
yycrank+412,	0,		0,	
yycrank+412,	0,		0,	
yycrank+422,	0,		0,	
yycrank+414,	0,		0,	
yycrank+428,	0,		0,	
yycrank+415,	0,		0,	
yycrank+419,	0,		0,	
yycrank+424,	0,		0,	
yycrank+432,	0,		0,	
yycrank+485,	0,		0,	
yycrank+416,	0,		0,	
yycrank+418,	0,		0,	
yycrank+428,	0,		0,	
yycrank+489,	0,		yyvstop+48,
yycrank+419,	0,		0,	
yycrank+425,	0,		0,	
yycrank+435,	0,		0,	
yycrank+433,	0,		0,	
yycrank+442,	0,		0,	
yycrank+435,	0,		0,	
yycrank+496,	0,		yyvstop+50,
yycrank+426,	0,		0,	
yycrank+444,	0,		0,	
yycrank+449,	0,		0,	
yycrank+433,	0,		0,	
yycrank+451,	0,		0,	
yycrank+437,	0,		0,	
yycrank+442,	0,		0,	
yycrank+506,	0,		0,	
yycrank+431,	0,		0,	
yycrank+448,	0,		0,	
yycrank+0,	0,		yyvstop+52,
yycrank+454,	0,		0,	
yycrank+510,	0,		0,	
yycrank+445,	0,		0,	
yycrank+456,	0,		0,	
yycrank+462,	0,		0,	
yycrank+452,	0,		0,	
yycrank+453,	0,		0,	
yycrank+446,	0,		0,	
yycrank+453,	0,		0,	
yycrank+466,	0,		0,	
yycrank+465,	0,		0,	
yycrank+463,	0,		0,	
yycrank+465,	0,		0,	
yycrank+0,	0,		yyvstop+54,
yycrank+469,	0,		0,	
yycrank+470,	0,		0,	
yycrank+471,	0,		0,	
yycrank+527,	0,		0,	
yycrank+474,	0,		0,	
yycrank+466,	0,		0,	
yycrank+0,	0,		yyvstop+56,
yycrank+0,	0,		yyvstop+58,
yycrank+460,	0,		yyvstop+60,
yycrank+0,	0,		yyvstop+62,
yycrank+459,	0,		0,	
yycrank+533,	0,		0,	
yycrank+467,	0,		0,	
yycrank+469,	0,		0,	
yycrank+477,	0,		0,	
yycrank+538,	0,		0,	
yycrank+539,	0,		yyvstop+64,
yycrank+540,	0,		0,	
yycrank+541,	0,		yyvstop+66,
yycrank+477,	0,		0,	
yycrank+480,	0,		0,	
yycrank+473,	0,		0,	
yycrank+493,	0,		0,	
yycrank+493,	0,		0,	
yycrank+488,	0,		0,	
yycrank+488,	0,		0,	
yycrank+483,	0,		0,	
yycrank+484,	0,		0,	
yycrank+480,	0,		0,	
yycrank+0,	0,		yyvstop+68,
yycrank+487,	0,		0,	
yycrank+493,	0,		0,	
yycrank+502,	0,		0,	
yycrank+503,	0,		0,	
yycrank+483,	0,		0,	
yycrank+494,	0,		0,	
yycrank+505,	0,		0,	
yycrank+488,	0,		0,	
yycrank+0,	0,		yyvstop+70,
yycrank+560,	0,		0,	
yycrank+506,	0,		0,	
yycrank+499,	0,		0,	
yycrank+493,	0,		0,	
yycrank+501,	0,		0,	
yycrank+502,	0,		0,	
yycrank+0,	0,		yyvstop+72,
yycrank+493,	0,		0,	
yycrank+568,	0,		0,	
yycrank+515,	0,		0,	
yycrank+0,	0,		yyvstop+74,
yycrank+496,	0,		0,	
yycrank+506,	0,		0,	
yycrank+507,	0,		0,	
yycrank+520,	0,		0,	
yycrank+514,	0,		0,	
yycrank+515,	0,		0,	
yycrank+516,	0,		0,	
yycrank+519,	0,		0,	
yycrank+512,	0,		0,	
yycrank+513,	0,		0,	
yycrank+580,	0,		0,	
yycrank+0,	0,		yyvstop+76,
yycrank+521,	0,		0,	
yycrank+530,	0,		0,	
yycrank+507,	0,		0,	
yycrank+526,	0,		0,	
yycrank+533,	0,		0,	
yycrank+530,	0,		0,	
yycrank+524,	0,		0,	
yycrank+536,	0,		0,	
yycrank+533,	0,		0,	
yycrank+0,	0,		yyvstop+78,
yycrank+526,	0,		0,	
yycrank+525,	0,		0,	
yycrank+537,	0,		0,	
yycrank+522,	0,		0,	
yycrank+530,	0,		0,	
yycrank+541,	0,		0,	
yycrank+527,	0,		0,	
yycrank+549,	0,		0,	
yycrank+0,	0,		yyvstop+80,
yycrank+549,	0,		0,	
yycrank+0,	0,		yyvstop+82,
yycrank+531,	0,		0,	
yycrank+604,	0,		0,	
yycrank+605,	0,		0,	
yycrank+541,	0,		0,	
yycrank+544,	0,		0,	
yycrank+556,	0,		0,	
yycrank+536,	0,		0,	
yycrank+541,	0,		0,	
yycrank+549,	0,		0,	
yycrank+539,	0,		0,	
yycrank+545,	0,		0,	
yycrank+554,	0,		0,	
yycrank+565,	0,		0,	
yycrank+549,	0,		0,	
yycrank+564,	0,		0,	
yycrank+621,	0,		0,	
yycrank+559,	0,		0,	
yycrank+547,	0,		0,	
yycrank+553,	0,		0,	
yycrank+567,	0,		0,	
yycrank+554,	0,		0,	
yycrank+0,	0,		yyvstop+84,
yycrank+555,	0,		0,	
yycrank+572,	0,		0,	
yycrank+573,	0,		0,	
yycrank+571,	0,		0,	
yycrank+0,	0,		yyvstop+86,
yycrank+571,	0,		0,	
yycrank+569,	0,		0,	
yycrank+579,	0,		0,	
yycrank+571,	0,		0,	
yycrank+577,	0,		0,	
yycrank+571,	0,		0,	
yycrank+571,	0,		0,	
yycrank+638,	0,		yyvstop+88,
yycrank+570,	0,		0,	
yycrank+574,	0,		0,	
yycrank+578,	0,		0,	
yycrank+586,	0,		0,	
yycrank+578,	0,		0,	
yycrank+577,	0,		0,	
yycrank+589,	0,		0,	
yycrank+575,	0,		0,	
yycrank+590,	0,		0,	
yycrank+592,	0,		0,	
yycrank+584,	0,		0,	
yycrank+0,	0,		yyvstop+90,
yycrank+0,	0,		yyvstop+92,
yycrank+587,	0,		0,	
yycrank+0,	0,		yyvstop+94,
yycrank+595,	0,		0,	
yycrank+599,	0,		0,	
yycrank+587,	0,		0,	
yycrank+0,	0,		yyvstop+96,
yycrank+589,	0,		0,	
yycrank+600,	0,		0,	
yycrank+601,	0,		0,	
yycrank+581,	0,		0,	
yycrank+592,	0,		0,	
yycrank+607,	0,		0,	
yycrank+597,	0,		0,	
yycrank+661,	0,		0,	
yycrank+610,	0,		0,	
yycrank+608,	0,		0,	
yycrank+609,	0,		0,	
yycrank+609,	0,		0,	
yycrank+601,	0,		0,	
yycrank+611,	0,		0,	
yycrank+603,	0,		0,	
yycrank+619,	0,		0,	
yycrank+603,	0,		0,	
yycrank+619,	0,		0,	
yycrank+616,	0,		0,	
yycrank+617,	0,		0,	
yycrank+612,	0,		0,	
yycrank+0,	0,		yyvstop+98,
yycrank+612,	0,		0,	
yycrank+620,	0,		0,	
yycrank+610,	0,		0,	
yycrank+602,	0,		0,	
yycrank+623,	0,		0,	
yycrank+624,	0,		0,	
yycrank+625,	0,		0,	
yycrank+683,	0,		0,	
yycrank+614,	0,		0,	
yycrank+634,	0,		0,	
yycrank+622,	0,		0,	
yycrank+612,	0,		0,	
yycrank+633,	0,		0,	
yycrank+634,	0,		0,	
yycrank+626,	0,		0,	
yycrank+634,	0,		0,	
yycrank+628,	0,		0,	
yycrank+623,	0,		0,	
yycrank+640,	0,		0,	
yycrank+625,	0,		0,	
yycrank+641,	0,		0,	
yycrank+635,	0,		0,	
yycrank+643,	0,		0,	
yycrank+644,	0,		0,	
yycrank+631,	0,		0,	
yycrank+650,	0,		0,	
yycrank+643,	0,		0,	
yycrank+639,	0,		0,	
yycrank+650,	0,		0,	
yycrank+706,	0,		0,	
yycrank+638,	0,		0,	
yycrank+647,	0,		0,	
yycrank+649,	0,		0,	
yycrank+655,	0,		0,	
yycrank+645,	0,		0,	
yycrank+654,	0,		0,	
yycrank+646,	0,		0,	
yycrank+651,	0,		0,	
yycrank+662,	0,		0,	
yycrank+656,	0,		0,	
yycrank+643,	0,		0,	
yycrank+649,	0,		0,	
yycrank+650,	0,		0,	
yycrank+657,	0,		0,	
yycrank+0,	0,		yyvstop+100,
yycrank+666,	0,		0,	
yycrank+0,	0,		yyvstop+102,
yycrank+667,	0,		0,	
yycrank+652,	0,		0,	
yycrank+668,	0,		0,	
yycrank+662,	0,		0,	
yycrank+0,	0,		yyvstop+104,
yycrank+663,	0,		0,	
yycrank+727,	0,		0,	
yycrank+668,	0,		0,	
yycrank+673,	0,		0,	
yycrank+678,	0,		0,	
yycrank+0,	0,		yyvstop+106,
yycrank+675,	0,		0,	
yycrank+665,	0,		0,	
yycrank+663,	0,		0,	
yycrank+0,	0,		yyvstop+108,
yycrank+0,	0,		yyvstop+110,
yycrank+664,	0,		0,	
yycrank+664,	0,		0,	
yycrank+671,	0,		0,	
yycrank+682,	0,		0,	
yycrank+0,	0,		yyvstop+112,
yycrank+738,	0,		0,	
yycrank+0,	0,		yyvstop+114,
yycrank+675,	0,		0,	
yycrank+740,	0,		0,	
yycrank+741,	0,		0,	
yycrank+666,	0,		0,	
yycrank+687,	0,		0,	
yycrank+689,	0,		0,	
yycrank+681,	0,		0,	
yycrank+692,	0,		0,	
yycrank+0,	0,		yyvstop+116,
yycrank+0,	0,		yyvstop+118,
yycrank+0,	0,		yyvstop+120,
yycrank+0,	0,		yyvstop+122,
yycrank+683,	0,		0,	
yycrank+691,	0,		0,	
yycrank+694,	0,		0,	
yycrank+697,	0,		0,	
yycrank+692,	0,		0,	
yycrank+697,	0,		0,	
yycrank+689,	0,		0,	
yycrank+755,	0,		0,	
yycrank+682,	0,		0,	
yycrank+701,	0,		0,	
yycrank+702,	0,		0,	
yycrank+693,	0,		0,	
yycrank+704,	0,		0,	
yycrank+703,	0,		0,	
yycrank+702,	0,		0,	
yycrank+708,	0,		0,	
yycrank+699,	0,		0,	
yycrank+710,	0,		0,	
yycrank+705,	0,		0,	
yycrank+769,	0,		0,	
yycrank+694,	0,		0,	
yycrank+0,	0,		yyvstop+124,
yycrank+771,	0,		yyvstop+126,
yycrank+0,	0,		yyvstop+128,
yycrank+715,	0,		0,	
yycrank+703,	0,		0,	
yycrank+711,	0,		0,	
yycrank+723,	0,		0,	
yycrank+713,	0,		0,	
yycrank+0,	0,		yyvstop+130,
yycrank+721,	0,		0,	
yycrank+0,	0,		yyvstop+132,
yycrank+722,	0,		0,	
yycrank+0,	0,		yyvstop+134,
yycrank+724,	0,		0,	
yycrank+714,	0,		0,	
yycrank+714,	0,		0,	
yycrank+726,	0,		0,	
yycrank+720,	0,		0,	
yycrank+715,	0,		0,	
yycrank+718,	0,		0,	
yycrank+717,	0,		0,	
yycrank+718,	0,		0,	
yycrank+730,	0,		0,	
yycrank+0,	0,		yyvstop+136,
yycrank+733,	0,		0,	
yycrank+719,	0,		0,	
yycrank+722,	0,		0,	
yycrank+736,	0,		0,	
yycrank+736,	0,		0,	
yycrank+727,	0,		0,	
yycrank+741,	0,		0,	
yycrank+742,	0,		0,	
yycrank+726,	0,		0,	
yycrank+744,	0,		0,	
yycrank+0,	0,		yyvstop+138,
yycrank+729,	0,		0,	
yycrank+0,	0,		yyvstop+140,
yycrank+731,	0,		0,	
yycrank+738,	0,		0,	
yycrank+737,	0,		0,	
yycrank+748,	0,		0,	
yycrank+752,	0,		0,	
yycrank+738,	0,		0,	
yycrank+737,	0,		0,	
yycrank+751,	0,		0,	
yycrank+754,	0,		0,	
yycrank+742,	0,		0,	
yycrank+756,	0,		0,	
yycrank+751,	0,		0,	
yycrank+741,	0,		0,	
yycrank+757,	0,		0,	
yycrank+743,	0,		0,	
yycrank+743,	0,		0,	
yycrank+0,	0,		yyvstop+142,
yycrank+753,	0,		0,	
yycrank+754,	0,		0,	
yycrank+744,	0,		0,	
yycrank+753,	0,		0,	
yycrank+755,	0,		0,	
yycrank+761,	0,		0,	
yycrank+767,	0,		0,	
yycrank+768,	0,		0,	
yycrank+0,	0,		yyvstop+144,
yycrank+0,	0,		yyvstop+146,
yycrank+754,	0,		0,	
yycrank+772,	0,		0,	
yycrank+769,	0,		0,	
yycrank+766,	0,		0,	
yycrank+765,	0,		0,	
yycrank+774,	0,		0,	
yycrank+780,	0,		0,	
yycrank+0,	0,		yyvstop+148,
yycrank+762,	0,		0,	
yycrank+0,	0,		yyvstop+150,
yycrank+768,	0,		0,	
yycrank+0,	0,		yyvstop+152,
yycrank+765,	0,		0,	
yycrank+836,	0,		0,	
yycrank+773,	0,		0,	
yycrank+767,	0,		0,	
yycrank+0,	0,		yyvstop+154,
yycrank+839,	0,		0,	
yycrank+0,	0,		yyvstop+156,
yycrank+771,	0,		0,	
yycrank+775,	0,		0,	
yycrank+786,	0,		0,	
yycrank+787,	0,		0,	
yycrank+787,	0,		0,	
yycrank+778,	0,		0,	
yycrank+783,	0,		0,	
yycrank+847,	0,		0,	
yycrank+794,	0,		0,	
yycrank+793,	0,		0,	
yycrank+794,	0,		0,	
yycrank+795,	0,		0,	
yycrank+796,	0,		0,	
yycrank+0,	0,		yyvstop+158,
yycrank+793,	0,		0,	
yycrank+783,	0,		0,	
yycrank+785,	0,		0,	
yycrank+788,	0,		0,	
yycrank+0,	0,		yyvstop+160,
yycrank+798,	0,		0,	
yycrank+0,	0,		yyvstop+162,
yycrank+0,	0,		yyvstop+164,
yycrank+798,	0,		0,	
yycrank+788,	0,		0,	
yycrank+790,	0,		0,	
yycrank+807,	0,		0,	
yycrank+0,	0,		yyvstop+166,
yycrank+799,	0,		0,	
yycrank+0,	0,		yyvstop+168,
yycrank+792,	0,		0,	
yycrank+808,	0,		0,	
yycrank+806,	0,		0,	
yycrank+796,	0,		0,	
yycrank+0,	0,		yyvstop+170,
yycrank+804,	0,		0,	
yycrank+0,	0,		yyvstop+172,
yycrank+803,	0,		0,	
yycrank+812,	0,		0,	
yycrank+818,	0,		0,	
yycrank+815,	0,		0,	
yycrank+814,	0,		0,	
yycrank+802,	0,		0,	
yycrank+0,	0,		yyvstop+174,
yycrank+0,	0,		yyvstop+176,
yycrank+820,	0,		0,	
yycrank+806,	0,		0,	
yycrank+811,	0,		0,	
yycrank+0,	0,		yyvstop+178,
yycrank+806,	0,		0,	
yycrank+806,	0,		0,	
yycrank+0,	0,		yyvstop+180,
yycrank+814,	0,		0,	
yycrank+824,	0,		0,	
yycrank+825,	0,		0,	
yycrank+822,	0,		0,	
yycrank+817,	0,		0,	
yycrank+808,	0,		0,	
yycrank+820,	0,		0,	
yycrank+0,	0,		yyvstop+182,
yycrank+826,	0,		0,	
yycrank+822,	0,		0,	
yycrank+818,	0,		0,	
yycrank+820,	0,		0,	
yycrank+837,	0,		0,	
yycrank+0,	0,		yyvstop+184,
yycrank+829,	0,		0,	
yycrank+841,	0,		0,	
yycrank+834,	0,		0,	
yycrank+824,	0,		0,	
yycrank+0,	0,		yyvstop+186,
yycrank+825,	0,		0,	
yycrank+0,	0,		yyvstop+188,
yycrank+837,	0,		0,	
yycrank+842,	0,		0,	
yycrank+830,	0,		0,	
yycrank+829,	0,		0,	
yycrank+836,	0,		0,	
yycrank+833,	0,		0,	
yycrank+836,	0,		0,	
yycrank+833,	0,		0,	
yycrank+834,	0,		0,	
yycrank+837,	0,		0,	
yycrank+851,	0,		0,	
yycrank+852,	0,		0,	
yycrank+849,	0,		0,	
yycrank+834,	0,		0,	
yycrank+848,	0,		0,	
yycrank+846,	0,		0,	
yycrank+853,	0,		0,	
yycrank+0,	0,		yyvstop+190,
yycrank+845,	0,		0,	
yycrank+860,	0,		0,	
yycrank+846,	0,		0,	
yycrank+0,	0,		yyvstop+192,
yycrank+859,	0,		0,	
yycrank+0,	0,		yyvstop+194,
yycrank+862,	0,		0,	
yycrank+0,	0,		yyvstop+196,
yycrank+863,	0,		0,	
yycrank+0,	0,		yyvstop+198,
yycrank+851,	0,		0,	
yycrank+850,	0,		0,	
yycrank+857,	0,		0,	
yycrank+856,	0,		0,	
yycrank+852,	0,		0,	
yycrank+860,	0,		0,	
yycrank+860,	0,		0,	
yycrank+871,	0,		0,	
yycrank+0,	0,		yyvstop+200,
yycrank+0,	0,		yyvstop+202,
yycrank+0,	0,		yyvstop+204,
yycrank+858,	0,		0,	
yycrank+0,	0,		yyvstop+206,
yycrank+859,	0,		0,	
yycrank+0,	0,		yyvstop+208,
yycrank+872,	0,		0,	
yycrank+872,	0,		0,	
yycrank+876,	0,		0,	
yycrank+863,	0,		0,	
yycrank+869,	0,		0,	
yycrank+880,	0,		0,	
yycrank+0,	0,		yyvstop+210,
yycrank+0,	0,		yyvstop+212,
yycrank+0,	0,		yyvstop+214,
yycrank+866,	0,		0,	
yycrank+867,	0,		0,	
yycrank+0,	0,		yyvstop+216,
yycrank+868,	0,		0,	
yycrank+0,	0,		yyvstop+218,
yycrank+0,	0,		yyvstop+220,
yycrank+0,	0,		yyvstop+222,
yycrank+0,	0,		yyvstop+224,
0,	0,	0};
struct yywork *yytop = yycrank+983;
struct yysvf *yybgin = yysvec+1;
char yymatch[] = {
00  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,011 ,011 ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
011 ,01  ,'"' ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
'0' ,'0' ,'0' ,'0' ,'0' ,'0' ,'0' ,'0' ,
'0' ,'0' ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,'A' ,'A' ,'A' ,'A' ,'A' ,'A' ,'A' ,
'A' ,'A' ,'A' ,'A' ,'A' ,'A' ,'A' ,'A' ,
'A' ,'A' ,'A' ,'A' ,'A' ,'A' ,'A' ,'A' ,
'A' ,'A' ,'A' ,01  ,01  ,01  ,01  ,'A' ,
01  ,'A' ,'A' ,'A' ,'A' ,'A' ,'A' ,'A' ,
'A' ,'A' ,'A' ,'A' ,'A' ,'A' ,'A' ,'A' ,
'A' ,'A' ,'A' ,'A' ,'A' ,'A' ,'A' ,'A' ,
'A' ,'A' ,'A' ,01  ,01  ,01  ,01  ,01  ,
0};
char yyextra[] = {
0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,
0};
#ifndef lint
static	char ncform_sccsid[] = "@(#)ncform 1.6 88/02/08 SMI"; /* from S5R2 1.2 */
#endif

int yylineno =1;
# define YYU(x) x
# define NLSTATE yyprevious=YYNEWLINE
char yytext[YYLMAX];
struct yysvf *yylstate [YYLMAX], **yylsp, **yyolsp;
char yysbuf[YYLMAX];
char *yysptr = yysbuf;
int *yyfnd;
extern struct yysvf *yyestate;
int yyprevious = YYNEWLINE;
yylook(){
	register struct yysvf *yystate, **lsp;
	register struct yywork *yyt;
	struct yysvf *yyz;
	int yych, yyfirst;
	struct yywork *yyr;
# ifdef LEXDEBUG
	int debug;
# endif
	char *yylastch;
	/* start off machines */
# ifdef LEXDEBUG
	debug = 0;
# endif
	yyfirst=1;
	if (!yymorfg)
		yylastch = yytext;
	else {
		yymorfg=0;
		yylastch = yytext+yyleng;
		}
	for(;;){
		lsp = yylstate;
		yyestate = yystate = yybgin;
		if (yyprevious==YYNEWLINE) yystate++;
		for (;;){
# ifdef LEXDEBUG
			if(debug)fprintf(yyout,"state %d\n",yystate-yysvec-1);
# endif
			yyt = yystate->yystoff;
			if(yyt == yycrank && !yyfirst){  /* may not be any transitions */
				yyz = yystate->yyother;
				if(yyz == 0)break;
				if(yyz->yystoff == yycrank)break;
				}
			*yylastch++ = yych = input();
			yyfirst=0;
		tryagain:
# ifdef LEXDEBUG
			if(debug){
				fprintf(yyout,"char ");
				allprint(yych);
				putchar('\n');
				}
# endif
			yyr = yyt;
			if ( (int)yyt > (int)yycrank){
				yyt = yyr + yych;
				if (yyt <= yytop && yyt->verify+yysvec == yystate){
					if(yyt->advance+yysvec == YYLERR)	/* error transitions */
						{unput(*--yylastch);break;}
					*lsp++ = yystate = yyt->advance+yysvec;
					goto contin;
					}
				}
# ifdef YYOPTIM
			else if((int)yyt < (int)yycrank) {		/* r < yycrank */
				yyt = yyr = yycrank+(yycrank-yyt);
# ifdef LEXDEBUG
				if(debug)fprintf(yyout,"compressed state\n");
# endif
				yyt = yyt + yych;
				if(yyt <= yytop && yyt->verify+yysvec == yystate){
					if(yyt->advance+yysvec == YYLERR)	/* error transitions */
						{unput(*--yylastch);break;}
					*lsp++ = yystate = yyt->advance+yysvec;
					goto contin;
					}
				yyt = yyr + YYU(yymatch[yych]);
# ifdef LEXDEBUG
				if(debug){
					fprintf(yyout,"try fall back character ");
					allprint(YYU(yymatch[yych]));
					putchar('\n');
					}
# endif
				if(yyt <= yytop && yyt->verify+yysvec == yystate){
					if(yyt->advance+yysvec == YYLERR)	/* error transition */
						{unput(*--yylastch);break;}
					*lsp++ = yystate = yyt->advance+yysvec;
					goto contin;
					}
				}
			if ((yystate = yystate->yyother) && (yyt= yystate->yystoff) != yycrank){
# ifdef LEXDEBUG
				if(debug)fprintf(yyout,"fall back to state %d\n",yystate-yysvec-1);
# endif
				goto tryagain;
				}
# endif
			else
				{unput(*--yylastch);break;}
		contin:
# ifdef LEXDEBUG
			if(debug){
				fprintf(yyout,"state %d char ",yystate-yysvec-1);
				allprint(yych);
				putchar('\n');
				}
# endif
			;
			}
# ifdef LEXDEBUG
		if(debug){
			fprintf(yyout,"stopped at %d with ",*(lsp-1)-yysvec-1);
			allprint(yych);
			putchar('\n');
			}
# endif
		while (lsp-- > yylstate){
			*yylastch-- = 0;
			if (*lsp != 0 && (yyfnd= (*lsp)->yystops) && *yyfnd > 0){
				yyolsp = lsp;
				if(yyextra[*yyfnd]){		/* must backup */
					while(yyback((*lsp)->yystops,-*yyfnd) != 1 && lsp > yylstate){
						lsp--;
						unput(*yylastch--);
						}
					}
				yyprevious = YYU(*yylastch);
				yylsp = lsp;
				yyleng = yylastch-yytext+1;
				yytext[yyleng] = 0;
# ifdef LEXDEBUG
				if(debug){
					fprintf(yyout,"\nmatch ");
					sprint(yytext);
					fprintf(yyout," action %d\n",*yyfnd);
					}
# endif
				return(*yyfnd++);
				}
			unput(*yylastch);
			}
		if (yytext[0] == 0  /* && feof(yyin) */)
			{
			yysptr=yysbuf;
			return(0);
			}
		yyprevious = yytext[0] = input();
		if (yyprevious>0)
			output(yyprevious);
		yylastch=yytext;
# ifdef LEXDEBUG
		if(debug)putchar('\n');
# endif
		}
	}
yyback(p, m)
	int *p;
{
if (p==0) return(0);
while (*p)
	{
	if (*p++ == m)
		return(1);
	}
return(0);
}
	/* the following are only used in the lex library */
yyinput(){
	return(input());
	}
yyoutput(c)
  int c; {
	output(c);
	}
yyunput(c)
   int c; {
	unput(c);
	}
