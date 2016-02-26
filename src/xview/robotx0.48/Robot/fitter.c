/* routines associated with the fitting control panel for Robot */
#include <xview/xview.h>
#include <xview/frame.h>
#include <xview/cursor.h>
#include <xview/panel.h>
#include <xview/canvas.h>
#include <xview/xv_xrect.h>
#include <xview/cms.h>
#include <xview/textsw.h>
#include <xview/server.h>
#include <xview/seln.h>
#include <xview/notice.h>
#include <xview/notify.h>
#include <xview/xv_error.h>

#include "robot.h"

#ifdef USA
#define NORMALISATION "Normalization"
#else
#define NORMALISATION "Normalisation"
#endif



#define	DEFAULT_NAME	"Parameter value:"
#define	THE_PANEL_WIDTH	350

#define DESCRIPTION_LENGTH	55


static	int maxmod;	/* maximum number of models as defined in
			 * FORTRAN include file fitcom */

/* Fitting control */
Frame		fit_frame;
Panel_item	fit_list, replace_button;
Panel_item	undelete_button;
Panel_item	npar_setter;
void		get_text_(), do_fit(), add_model();
void		delete_model(), duplicate_model(), model_select();
void		undelete_model();
void		replace_parameters();
void		show_parameters(), model_copy();
void		save_parameters();
void		npar_set_proc();
int		current_selection = -999;	/* global variable! */
int		get_selected_row();


/* the models which can be used and a description of them */
struct models
{
	char	name[12];
	char	description[DESCRIPTION_LENGTH];
	int	npars;
};
static	struct models	model[NO_MODEL_TYPES];

/* description of parameters */
#define	MAXPNAME	30
#define TLENGTH 14

struct	names
{
	char	text[TLENGTH];
};

struct names	pname[MAXPNAME];
static	int	npnames = 0;

/* models selected for fitting */
struct fit_models
{
	char	name[FIT_NAME_LENGTH];
	char	description[DESCRIPTION_LENGTH];
	int	npars;
	float	parameters[FIT_PARAMETER_LENGTH];
	short	fix_free[FIT_PARAMETER_LENGTH];
};
static	struct	fit_models	fit_model[MAX_MODELS];
static	struct	fit_models	deleted_model;

/* panels for specifying parameters */
struct fit_panels
{
	Panel_item	text;
	Panel_item	fix_free;
};
struct	fit_panels	fit_panel[NO_PARAMETERS];

char	temp_text[MAXPNAME];	/* polynomial coefficient names */


void
make_pname(name)
char	*name;
/* adds parameter name to list */
{

	if(npnames >= MAXPNAME){
		fprintf(stderr, "error in make_pname\n");
	}
	else{
		if(strlen(name) + 1 > TLENGTH){
		 fprintf(stderr, "Error in make_pname: name too long\n");
		 fprintf(stderr, "%s\n", name);
		}
		strncpy(pname[npnames].text, name,
				MIN(TLENGTH-1, strlen(name)));
		npnames++;
	}
}



/* A window to control CURFIT */
void
fit_frame_create(frame, fit_main_panel)
Frame	frame;
Panel	fit_main_panel;
{
int	j;

	maxmod = getmm_();
	if(maxmod > MAX_MODELS){
		fprintf(stderr, "maxmod in 'fitcom' is larger than\n");
		fprintf(stderr, "MAX_MODELS in robot.h\n");
		fprintf(stderr, "Source needs editing!!!!!\n");
	}

	fit_frame = xv_create(frame, FRAME_CMD,
	FRAME_LABEL, "Fit Controller",
				FRAME_SHOW_FOOTER,	TRUE,
				FRAME_CMD_PUSHPIN_IN, TRUE,
				NULL);
	fit_main_panel = xv_get(fit_frame, FRAME_CMD_PANEL);
	xv_set(fit_main_panel, XV_WIDTH, THE_PANEL_WIDTH, NULL);
	

	j = 0;
/* This really ought to be integrated with the CRVFIT routine
 * to make sure things stay consistent */
	strcpy(model[j].name, "Gauss");
	strcpy(model[j].description,
		"a1*exp(((x-a2)/a3)**2/2.)");
	make_pname(NORMALISATION);
	make_pname("Mean");
	make_pname("Sigma");
	model[j].npars = 3; j++;

	strcpy(model[j].name, "Exponential"); 
	strcpy(model[j].description,
		"x < a2, 0; x >= t2, a1*exp((x-a2)/a3");
	make_pname(NORMALISATION);
	make_pname("Start");
	make_pname("Decay");
	model[j].npars = 3; j++;

	strcpy(model[j].name, "Polynomial");
		make_pname("Coefficients");
	strcpy(model[j].description,
		"a1 + a2*x + a3*x**2 + ...");
	model[j].npars = 1; j++;

	strcpy(model[j].name, "User");
	make_pname("Parameters");
	strcpy(model[j].description,
		"User Defined");
	model[j].npars = 1; j++;

	strcpy(model[j].name, "Lorentz");
	strcpy(model[j].description,
		"a1*a3**2*0.25/(((x-a2)**2)*a3**2*0.25)");
	make_pname(NORMALISATION);
	make_pname("Mean");
	make_pname("Width");
	model[j].npars = 3; j++;

	strcpy(model[j].name, "Tophat");
	make_pname(NORMALISATION);
	make_pname("Start");
	make_pname("Width");
	model[j].npars = 3; j++;

	strcpy(model[j].name, "Powerlaw");
	make_pname(NORMALISATION);
	make_pname("Power");
	model[j].npars = 2; j++;

	strcpy(model[j].name, "Sine");
	make_pname("Amplitude");
	make_pname("Phase");
	make_pname("Period");
	model[j].npars = 3; j++;

	strcpy(model[j].name, "Triangle");
	make_pname("Height");
	make_pname("Start");
	make_pname("HWZI");
	model[j].npars = 3; j++;

	strcpy(model[j].name, "Blackbody");
	make_pname(NORMALISATION);
	make_pname("kT");
	model[j].npars = 2; j++;

	strcpy(model[j].name, "Orbit");
	make_pname("Amplitude");
	make_pname("Phase");
	make_pname("Period");
	make_pname("Mean");
	make_pname("eccentricity");
	make_pname("omega");
	model[j].npars = 6; j++;

	strcpy(model[j].name, "last");


	j = 0;
	while(strne(model[j].name,"last")){
	xv_create(fit_main_panel, PANEL_BUTTON,
			XV_HELP_DATA,	helper(model[j].name),
			PANEL_LABEL_STRING, model[j].name,
			PANEL_NOTIFY_PROC,	add_model,
			NULL);
	j++;
	}


	xv_set(fit_main_panel, PANEL_LAYOUT, PANEL_VERTICAL, NULL);

	fit_list = xv_create(fit_main_panel, PANEL_LIST,
			PANEL_LIST_DISPLAY_ROWS,	10,
			PANEL_LIST_WIDTH,		120,
			PANEL_NOTIFY_PROC,		model_select,
			NULL);
 
	xv_create(fit_main_panel, PANEL_BUTTON,
			XV_HELP_DATA,	"robot:delete_model",
			PANEL_LABEL_STRING, "Delete Model",
			PANEL_NOTIFY_PROC,	delete_model,
			NULL);

	xv_set(fit_main_panel, PANEL_LAYOUT, PANEL_HORIZONTAL, NULL);

	undelete_button = xv_create(fit_main_panel, PANEL_BUTTON,
			XV_HELP_DATA,	"robot:undelete_model",
			PANEL_LABEL_STRING, "Undelete Model",
			PANEL_NOTIFY_PROC,	undelete_model,
			PANEL_INACTIVE,	TRUE,
			NULL);

	xv_set(fit_main_panel, PANEL_LAYOUT, PANEL_VERTICAL, NULL);

	xv_create(fit_main_panel, PANEL_BUTTON,
			XV_HELP_DATA,	"robot:duplicate_model",
			PANEL_LABEL_STRING, "Duplicate Model",
			PANEL_NOTIFY_PROC,	duplicate_model,
			NULL);
	xv_set(fit_main_panel, PANEL_LAYOUT, PANEL_HORIZONTAL, NULL);

	npar_setter = xv_create(fit_main_panel, PANEL_NUMERIC_TEXT,
				PANEL_LABEL_STRING, "No of parameters",
				PANEL_NOTIFY_PROC,	npar_set_proc,
				PANEL_MIN_VALUE, 0,
				PANEL_MAX_VALUE, NO_PARAMETERS,
				PANEL_VALUE_DISPLAY_LENGTH, 3,
				XV_SHOW,	FALSE,
				NULL);

/* set up panel items for specifying parameters and whether
 * they are fixed or free */
	xv_set(fit_main_panel, PANEL_LAYOUT, PANEL_VERTICAL, NULL);
	for(j = 0; j < NO_PARAMETERS; j++){
		fit_panel[j].text = xv_create(fit_main_panel, PANEL_TEXT,
				PANEL_LABEL_STRING, DEFAULT_NAME,
				PANEL_LABEL_X,	5,
				PANEL_VALUE_X,	120,
				PANEL_VALUE,	" ",
				PANEL_VALUE_DISPLAY_LENGTH, 10,
				PANEL_INACTIVE,	TRUE,
				NULL);
	xv_set(fit_main_panel, PANEL_LAYOUT, PANEL_HORIZONTAL, NULL);
		fit_panel[j].fix_free = xv_create(fit_main_panel, PANEL_CHOICE,
					XV_HELP_DATA, "robot:fix_free",
					PANEL_CHOICE_STRINGS,
						"Free", "Fixed", NULL,
					PANEL_INACTIVE, TRUE,
					NULL);
	xv_set(fit_main_panel, PANEL_LAYOUT, PANEL_VERTICAL, NULL);
	}

	replace_button = xv_create(fit_main_panel, PANEL_BUTTON,
			XV_HELP_DATA,	"robot:replace_parameters",
			PANEL_LABEL_STRING, 
				"Replace Parameters with Fit Results",
			PANEL_NOTIFY_PROC, replace_parameters,
			PANEL_INACTIVE,	TRUE,
			NULL);

	xv_set(fit_main_panel, PANEL_DEFAULT_ITEM,
		xv_create(fit_main_panel, PANEL_BUTTON,
			XV_HELP_DATA,	"robot:do_fit",
			PANEL_LABEL_STRING, "Do Fit!",
			PANEL_NOTIFY_PROC,	do_fit,
			NULL),
				NULL);
if(not_open_look){
	
           xv_set(fit_main_panel,
		PANEL_LAYOUT, PANEL_HORIZONTAL,
		NULL);

	   xv_create(fit_main_panel, PANEL_BUTTON,
		XV_HELP_DATA,		"robot:dismiss",
		PANEL_LABEL_STRING,	DISMISS,
		PANEL_NOTIFY_PROC,	hide_me,
		NULL);
}

	window_fit(fit_main_panel);
	window_fit(fit_frame);
}


void
show_fitter()
{
	xv_set(fit_frame, XV_SHOW, TRUE, NULL);
	xv_set(fit_frame, FRAME_CMD_PUSHPIN_IN, TRUE, NULL);

}

void
do_fit()
{
static char Blank[] = "END";
char	buff[ILENGTH];
int	i, j, nrows;
int	itext;
/* ensure models are up to date */
	save_parameters();
/* First check we have some models to fit! */
	nrows = xv_get(fit_list, PANEL_LIST_NROWS);
	if(nrows < 1){
		toarkt_("No models specified!");
		return;
	}
	xv_set(replace_button, PANEL_INACTIVE, FALSE, NULL);
	for(i = 0; i < nrows; i++){
		strcpy(buff, fit_model[i].name);
		buffset_(buff);
		strcpy(buff, "");
		for(j = 0; j < fit_model[i].npars; j++){
		   itext = strlen(buff);
		   if(fit_model[i].fix_free[j]  == 1)
		     sprintf(buff + itext, " (%g) ", 
				fit_model[i].parameters[j]);
		    else 
		     sprintf(buff + itext, " %g ", 
				fit_model[i].parameters[j]);
		}
		    buffset_(buff);
	}
/* avoid f2c version of FORTRAN code messing up out string */
	strcpy(buff, Blank);
	buffset_(buff);
	strcpy(inst,"CURFIT");
	to_robot();
}


void
replace_parameters()
/* change parameter values to those resulting from the fit */
{
int	parameter_number, nrows;
int	i, j;
float	value;
float	error; /* not used here */
	nrows = xv_get(fit_list, PANEL_LIST_NROWS);
	parameter_number = 0;
	for(i = 0; i < nrows; i++){
		for (j = 0; j < fit_model[i].npars; j++){
			parameter_number++;
			getpar_(&parameter_number, &value, &error);
			fit_model[i].parameters[j] = value;
		}
	}
	show_parameters();
}
			



void
add_model(item, event)
Panel_item	item;
Event		*event;
{
	int nrows;
	char	label[20];
	int	find_model();

	save_parameters();

	nrows = xv_get(fit_list, PANEL_LIST_NROWS);
	if(test_models() == FAIL) return;
	strcpy(label, (char *) xv_get(item, PANEL_LABEL_STRING));
	xv_set(fit_list, PANEL_LIST_STRING, nrows, label,
			 PANEL_LIST_SELECT, nrows, TRUE,
		NULL);
	strcpy(fit_model[nrows].name, label);
	fit_model[nrows].npars = model[find_model(label)].npars;
	show_parameters();
}

int
test_models()
/* check whether we have too many models */
{
int	nrows;
	nrows = xv_get(fit_list, PANEL_LIST_NROWS);
	if(nrows >= maxmod || nrows >= MAX_MODELS) {
		toarkt_("Maximum number of allowed models exceeded!");
		return(FAIL);
	}
	else
		return(1);
}
	


int
find_model(name)
char	*name;
{
int j;
	for(j = 0; j < NO_MODEL_TYPES; j++){
		if(streq(name, model[j].name))
		return(j);
	}
	return (FAIL);
}
	



void
duplicate_model()
{
int	selected_row, nrows;
	save_parameters();
	nrows = xv_get(fit_list, PANEL_LIST_NROWS);
	if(nrows == 0){
		toarkt_("Nothing to duplicate!");
		return;
	}

	if(test_models() == FAIL) return;
	selected_row = get_selected_row();
	model_copy(&fit_model[nrows], &fit_model[selected_row]);
	xv_set(fit_list, PANEL_LIST_STRING, nrows, fit_model[nrows].name,
			 PANEL_LIST_SELECT, nrows, TRUE,
		NULL);
	show_parameters();
}

int
get_selected_row()
{
int	nrows, i;
	nrows = xv_get(fit_list, PANEL_LIST_NROWS);
	for (i = 0; i < nrows; i++) {
		if (xv_get(fit_list, PANEL_LIST_SELECTED, i)) {
			return(i);
			
		}
	}
}

void
delete_model()
{
int	selected_row, nrows, i;

	nrows = xv_get(fit_list, PANEL_LIST_NROWS);
	if(nrows == 0){
		toarkt_("Nothing to delete!");
		return;
	}
	selected_row = get_selected_row();
	xv_set(fit_list, PANEL_LIST_DELETE, selected_row, NULL);
	if (selected_row > 0){
		xv_set(fit_list,
		       PANEL_LIST_SELECT, selected_row - 1, TRUE,
		       NULL);
		}
/* store model in case we need it again */
	model_copy(&deleted_model, &fit_model[selected_row]);

/* remove model from list of models to be fitted too */
	for (i = selected_row; i <= nrows; i++){
		model_copy(&fit_model[i], &fit_model[i+1]);
	}
	strcpy(fit_model[nrows].name, " ");

	xv_set(undelete_button, PANEL_INACTIVE, FALSE, NULL);

	show_parameters();
}

void
undelete_model()
{
int	nrows;

	save_parameters();
	nrows = xv_get(fit_list, PANEL_LIST_NROWS);
	if(test_models() == FAIL) return;
	model_copy(&fit_model[nrows], &deleted_model);
	xv_set(fit_list, PANEL_LIST_STRING, nrows, fit_model[nrows].name,
			 PANEL_LIST_SELECT, nrows, TRUE,
		NULL);
	show_parameters();
}


/* copies parameters of model no. 2 onto model no. 1 */
/* (could this all be done automatically???) */
void
model_copy(model1, model2)
struct fit_models	*model1, *model2;
{
int 	i;
	save_parameters();
	strcpy(model1->name, model2->name);
	strcpy(model1->description, model2->description);
	model1->npars = model2->npars;
	for (i = 0; i < FIT_PARAMETER_LENGTH; i++){
		model1->parameters[i] = model2->parameters[i];
		model1->fix_free[i] = model2->fix_free[i];
	}
	show_parameters();
}
	

void
model_select(item, string, client_data, op, event)
Panel_item	item;
char		*string;
caddr_t		client_data;
Panel_list_op	op;
Event		*event;
{
/*	printf("item %s selected\n", string); */
	if(op  == PANEL_LIST_OP_SELECT);
	if(op  == PANEL_LIST_OP_DESELECT){
/*		printf("deselected number was %d\n", current_selection); */
		save_parameters();
	}
	show_parameters();
}


void
save_parameters()
{
int	i;
/* save parameters to structure */
	if(current_selection < 0) return;
	if(streq(fit_model[current_selection].name, "Polynomial") ||
		streq(fit_model[current_selection].name, "User"))
		fit_model[current_selection].npars = 
			xv_get(npar_setter, PANEL_VALUE);
	for (i = 0; i < fit_model[current_selection].npars; i++){
		sscanf((char *)xv_get(fit_panel[i].text, PANEL_VALUE),
				"%g", &fit_model[current_selection].parameters[i]);
		fit_model[current_selection].fix_free[i] 
			= xv_get(fit_panel[i].fix_free, PANEL_VALUE);
		}
}

/* display current values of selected model */
void
show_parameters()
{
int i, j, k, selected_row;
char	label[20];
	j = 0;
/* which item is selected? */
	selected_row = get_selected_row();
	current_selection = selected_row;
/* only need to specify the number of parameters for a polynomial */	
	if(streq(fit_model[selected_row].name, "Polynomial") ||
		streq(fit_model[selected_row].name, "User")){
		xv_set(npar_setter, XV_SHOW, TRUE, NULL);
		xv_set(npar_setter, 
			PANEL_VALUE, fit_model[selected_row].npars,
			NULL);
	}
	else
		xv_set(npar_setter, XV_SHOW, FALSE, NULL);


	for (i = 0; i < fit_model[selected_row].npars; i++) {
		sprintf(label, "%g", fit_model[selected_row].parameters[i]);
		xv_set(fit_panel[i].text, PANEL_INACTIVE, FALSE, 
				PANEL_VALUE, label,
				NULL);
		xv_set(fit_panel[i].fix_free, PANEL_INACTIVE, FALSE, NULL);
		xv_set(fit_panel[i].fix_free, PANEL_INACTIVE, FALSE, NULL);
		xv_set(fit_panel[i].fix_free, PANEL_VALUE, 
			fit_model[selected_row].fix_free[i], NULL);}
	for (i = fit_model[selected_row].npars; i < NO_PARAMETERS; i++){
		if(!(xv_get(fit_panel[i].text, PANEL_INACTIVE))) {
		  xv_set(fit_panel[i].text, PANEL_INACTIVE, TRUE, NULL);
		  xv_set(fit_panel[i].text, 
			PANEL_VALUE, 0,
			PANEL_LABEL_STRING, DEFAULT_NAME,
			NULL);
		  xv_set(fit_panel[i].fix_free, PANEL_INACTIVE, TRUE, NULL);
		}
		if(xv_get(fit_panel[i].fix_free, PANEL_VALUE) != 0)
			xv_set(fit_panel[i].fix_free, PANEL_VALUE, 
			    0, NULL);
	}


	
/* find the instructions to go with this model */
	i = 0;

	while(strne(fit_model[j].name, "last")){
	if(streq(model[j].name, fit_model[selected_row].name)){
		for(k = 0; k < fit_model[selected_row].npars; k++) {
		    if(streq(fit_model[selected_row].name, "Polynomial") ||
			streq(fit_model[selected_row].name, "User")){
			sprintf(temp_text, "Coefficient %d", k+1);
		    	xv_set(fit_panel[k].text,
			 	PANEL_LABEL_STRING, temp_text, 
			 	NULL);
		    }
		    else
		    	xv_set(fit_panel[k].text,
			 	PANEL_LABEL_STRING, pname[i+k].text, 
			 	NULL);
		}
		break;}
	i = i + model[j].npars;
	j++;

	}
}




/* set the number of parameters to be used
 * only for use for polynomial and (eventually) the user supplied model
 * where we don't automatically know how many parameters there are
 */
void
npar_set_proc()
{
int	ipar, selected_row;
	ipar = xv_get(npar_setter, PANEL_VALUE);
/* which item is selected? */
	selected_row = get_selected_row();
	fit_model[selected_row].npars = ipar;
	show_parameters();
}

