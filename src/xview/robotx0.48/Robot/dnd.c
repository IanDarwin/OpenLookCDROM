/* drag and drop routines */
/* based on the demonstartion routine included in openwin 3.0 */
#include <xview/xview.h>
#include <xview/panel.h>
#include <xview/textsw.h>
#include <xview/dragdrop.h>
#include <xview/xv_xrect.h>

#include "robot.h"

#define FILE_NAME_ATOM			        0	
#define _SUN_AVAILABLE_TYPES_ATOM	        1	
#define XA_STRING_ATOM			        2	
#define TOTAL_ATOMS		                3

#define ICON_SIZE	16  /* was 32 in original */		

void	drop_proc();
Drag_drop		drag_object; /* The drag object */
struct
{
	Atom	atom;
	char	*name;
} atom_list[TOTAL_ATOMS] = 
{
	{0,	"FILE_NAME"},
	{0,	"_SUN_AVAILABLE_TYPES"},
	{0,	"XA_STRING"},
};
/* 
 *  DnD_init: Create a drop site, and a drag object.
 */
void
DnD_init(server, panel)
Xv_Server server;
Panel	panel;
{
	Xv_opaque	drop_glyph;
	Xv_opaque	busy_glyph;

        static unsigned short   drop_icon[] = {
#include "drop_site.icon"
        };
        static unsigned short   busy_icon[] = {
#include "busy_site.icon"
        };

	int	i;        

	for(i = 0; i < TOTAL_ATOMS; i++)
	{
		atom_list[i].atom = xv_get(server,
					SERVER_ATOM, 
					atom_list[i].name);
	}

	atom_list[XA_STRING_ATOM].atom = XA_STRING;
	drag_object = xv_create(panel, DRAGDROP, NULL);

        drop_glyph = xv_create(XV_NULL, SERVER_IMAGE,
                SERVER_IMAGE_BITS, drop_icon,
                SERVER_IMAGE_DEPTH, 1,
                XV_WIDTH, ICON_SIZE,
                XV_HEIGHT, ICON_SIZE,
                NULL);

        busy_glyph = xv_create(XV_NULL, SERVER_IMAGE,
                SERVER_IMAGE_BITS, busy_icon,
                SERVER_IMAGE_DEPTH, 1,
                XV_WIDTH, ICON_SIZE,
                XV_HEIGHT, ICON_SIZE,
                NULL);

	xv_create(panel,                        PANEL_DROP_TARGET,
			XV_HELP_DATA,		"robot:dragon_droppings",
			PANEL_DROP_DND,		drag_object,
			PANEL_DROP_GLYPH,	drop_glyph,
			PANEL_DROP_BUSY_GLYPH,	busy_glyph,
			PANEL_NOTIFY_PROC,	drop_proc,
			PANEL_DROP_FULL,	TRUE,
			NULL); 
}
 
 
/* drop_proc: Setup the drag operation and handle the drop.
 *
 */

void
drop_proc(item, value, event)
Xv_opaque	item;
int		value;
Event		*event;
{
	Selection_requestor	sel_req;
        static void             get_primary_selection();

	sel_req = xv_get(item, PANEL_DROP_SEL_REQ);
        

	/* printf("sel_req = %X\n", sel_req); */
	switch(event_action(event))
	{
	case	ACTION_DRAG_MOVE:	/* they are moving the object */
		/* printf("drag move\n"); */
		get_primary_selection(sel_req);		
		break;

	case	ACTION_DRAG_COPY:	/* they are copying the object */
		/* printf("drag copy\n"); */
		get_primary_selection(sel_req);		
		break;

	case	LOC_DRAG:
		break;
	default:
		printf("unknown event %d\n", event_action(event));
	}

} 
static void
get_primary_selection(sel_req)
Selection_requestor sel_req;
{
	long            length;
	int             format;
	char		*sel_string;
	char		*string;
	Atom		*list;
	int		i;

	list = NULL;
	xv_set(sel_req, SEL_TYPE, atom_list[_SUN_AVAILABLE_TYPES_ATOM].atom, 0);
	list = (Atom *) xv_get(sel_req, SEL_DATA, &length, &format);
	if (length == SEL_ERROR)
	{
		printf("*** Unable to get target list.\n");
	}
	else
	{
		/* printf("length = %d format = %d\n", length, format); */
		while(*list)
		{
			/* printf("list = %X\n", list); */
			for(i = 0; i < TOTAL_ATOMS; i++)
			{
				if(*list == atom_list[i].atom)
				{
					/* printf("supports %d %s\n", i,
							 atom_list[i].name); */
					break;
				}
			}
			list++;
		}
	}
	xv_set(sel_req, SEL_TYPE, atom_list[FILE_NAME_ATOM].atom, 0);
	string = (char *) xv_get(sel_req, SEL_DATA, &length, &format);
	if (length != SEL_ERROR)
	{
		/* printf("length = %d format = %d\n", length, format); */
		/* Create a NULL-terminated version of 'string' */
		sel_string = (char *) calloc(1, length + 1);
		strncpy(sel_string, string, length);
		/* check if it's a ".dat" file */
		if(streqn(string+strlen(string)-4, ".dat")){
 			sprintf(inst, "plotfile %s", string);
		}
		else{
			sprintf(inst, "file %s", string);
		}
		to_robot();

		return;
	}
	else
	{
		printf("*** Unable to get FILE_NAME_ATOM selection.\n");
	}

	xv_set(sel_req, SEL_TYPE, atom_list[XA_STRING_ATOM].atom, 0);
	string = (char *) xv_get(sel_req, SEL_DATA, &length, &format);
	if (length != SEL_ERROR)
	{
		/* printf("length = %d format = %d\n", length, format); */
		/* Create a NULL-terminated version of 'string' */
		sel_string = (char *) calloc(1, length + 1);
		strncpy(sel_string, string, length);
		/* printf("string is %s\n", string); */
		sprintf(inst, "file %s", string);
		/* printf("inst (2) is %s", inst); */
		to_robot();

	}
	else
	{
		printf("*** Unable to get XA_STRING_ATOM selection.\n");
	}
} 


