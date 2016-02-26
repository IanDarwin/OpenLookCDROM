#ifndef robot_DEFINED
#define robot_DEFINED
extern	char	inst[];
extern	Panel	colour_panel;
extern	Panel	history_panel;
extern	Panel	text_editor_panel;
extern	Panel	axes_panel;
extern	Panel	zoom_panel;
extern	Panel	split_panel;
extern	Panel	specials_panel;
extern	Display	*dpy;

extern Bool	not_open_look;
extern	void	hide_me();


#define NO_PANELS	1	/* number of control panels */
#define	ROBOT_WIDTH 	720	 /* width of panels */
#define	MY_CANVAS_WIDTH 	612	 /* width of plotting area */
#define	MY_CANVAS_HEIGHT	792	/* height of the plotting area */
#define PANEL0_HEIGHT	55
#define PANEL1_HEIGHT	30
#define PANEL2_HEIGHT	30
#define TEXT_HEIGHT	120

#define NMENUS	9		/* no. of menus allowed */
#define NSUBMENUS 6		/* number of sub (pull-right) menus used */

#define	ARROW_MENU 0		/* identify sub-menus */
#define	FONT_MENU 1		/* identify sub-menus */
#define	TEXT_SIZE_MENU 2	/* identify sub-menus */
#define	TEXT_STYLE_MENU 3	/* identify sub-menus */
#define	LINE_STYLE_MENU 4	/* identify sub-menus */
#define	PLOT_MODE_MENU 5	/* identify sub-menus */

#define NMAIN	9		/* no. of main buttons allowed */
#define MAXARGS 20	/* maximum no. of command line arguments */
#define ILENGTH	80	/* length of character string expected by ROBOT */
#define	FAIL	-999	/* returned by a procedure which has troubles */

/* Robot only allows maxmod models to be fitted at once (defined
   in fitcom, the number below must be at least that large */
#define MAX_MODELS 30	


#define FIT_NAME_LENGTH	12	/* used in fit_models structure */
#define FIT_PARAMETER_LENGTH	10
#define	NO_PARAMETERS		6
#define	NO_MODEL_TYPES	12

#define NO_AD_PANELS 7	/* number of sub-panels in the Astro D panel */



/* these need to be replaced with link lists!!! */
#define ALIASMAX 20

struct alias_text
{
	char	text[ILENGTH];
};

extern	struct	alias_text	alias_name[];
extern	int	alias_number;



#define HELPMAX	198

struct	help_text
{
	char	text[ILENGTH];
};

extern	struct	help_text	help_name[];
extern	int	help_number;

struct	robot
{
int	multiclick;
};

extern	struct robot Robot;

#define exchange(a, b, c) if( strcmp ((a), (b)) == 0) strcpy((a),(c))
#define streq(a, b) ( strcmp((a), (b)) == 0)
#define streqn(a, b) ( strncmp((a), (b), strlen(b)) == 0)
#define strne(a, b) ( strcmp((a), (b)) != 0)

/* standardize on name of Dismiss button */
#define DISMISS "Dismiss"


#endif /* robot_DEFINED */
