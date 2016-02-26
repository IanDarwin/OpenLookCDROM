%{
/*
 *	(c) Copyright 1991 Sun Microsystems, Inc.  All rights reserved.
 *	See LEGAL_NOTICE file for terms of the license.
 *
 *	@(#) treetool.l 1.2 92/01/28 
 */

typedef enum {
	T_EOF = 0, 
	T_STR, 
	T_LPAR, 
	T_RPAR,
	T_FONT,
	T_ICON,
	T_COLOR,
} Token;

int lineno = 1;

%}

%START COMMENT
%START STRING

%%
<COMMENT>[^\n]*			;
<COMMENT>"\n"			{ lineno++; BEGIN 0; }
^"#"				{ BEGIN COMMENT; } 

\"\"				{ yytext[0] = '\0';return(T_STR); }
<STRING>[^"]*			{ return(T_STR); }
<STRING>\"			{ BEGIN 0; }
\"				{ BEGIN STRING; }

"("				{ return(T_LPAR); }
")"				{ return(T_RPAR); }
[\r\n]				{ lineno++ ; }
[\t ]				;
[^\r\n\t\" ()]+			{ return(T_STR); }

%%

#include <xview/frame.h>
#include <xview/openmenu.h>
#include <xview/scrollbar.h>
#include <sspkg/canshell.h> 
#include <sspkg/tree.h>
#include <sspkg/rectobj.h> 
#include <sspkg/drawobj.h> 
 

void	read_node_descr();
int	read_node();
Rectobj	add_leaf();
void	set_horizontal();
void	set_vertical();
void	resize();

Tree		tree = 0;
Canvas_shell	shell;


main(argc, argv)
	int	argc;
	char	*argv[];
{
	Frame		frame;
	Menu		menu;
	Scrollbar	vscroll;
	Scrollbar	hscroll;
 
	xv_init(XV_INIT_ARGC_PTR_ARGV, &argc, argv, NULL);
 
	frame = (Frame) xv_create(XV_NULL, FRAME,
			FRAME_LABEL, argv[0],
			XV_WIDTH, 400,
			XV_HEIGHT, 300,
			NULL);

	menu = (Menu) xv_create(XV_NULL, MENU_COMMAND_MENU,
			MENU_GEN_PIN_WINDOW, frame, "Layout",
			MENU_ITEM,
				MENU_STRING, "Horizontal",
				MENU_ACTION_PROC, set_horizontal,
				NULL,
			MENU_ITEM,
				MENU_STRING, "Vertical",
				MENU_ACTION_PROC, set_vertical,
				NULL,
			NULL);

	shell = (Canvas_shell) xv_create(frame, CANVAS_SHELL, 
			CANVAS_RESIZE_PROC, resize,
			RECTOBJ_MENU, menu,
			NULL);

	vscroll = (Scrollbar) xv_create(shell, SCROLLBAR,
			SCROLLBAR_DIRECTION, SCROLLBAR_VERTICAL,
			SCROLLBAR_SPLITTABLE, TRUE,
			SCROLLBAR_PIXELS_PER_UNIT, 10,
			NULL);

	hscroll = (Scrollbar) xv_create(shell, SCROLLBAR,
			SCROLLBAR_DIRECTION, SCROLLBAR_HORIZONTAL,
			SCROLLBAR_SPLITTABLE, TRUE,
			SCROLLBAR_PIXELS_PER_UNIT, 10,
			NULL);

	tree = (Tree) xv_create(shell, TREE,
			XV_WIDTH, 400,
			XV_HEIGHT, 300,
			NULL);


	while( read_node(tree) )
		;

	xv_set(shell,	
		CANVAS_MIN_PAINT_WIDTH, xv_get(tree, XV_WIDTH),
		CANVAS_MIN_PAINT_HEIGHT, xv_get(tree, XV_HEIGHT),
		NULL);

	xv_main_loop(frame); 
}

void
syntax_error()
{
	fprintf(stderr,"htool: Syntax error on line %d.\n", lineno);
	exit(1);
}

void
read_node_descr(node)
	Rectobj	node;
{
	Rectobj new;

	switch((Token) yylex()) {
		case T_STR:
			new = add_leaf(yytext, node);
			break;
		default:
			syntax_error();
			break;
	}

	while(1) 
	  switch((Token) yylex()) {
		case T_RPAR:
			return;
		case T_LPAR: 
			read_node_descr(new);
			break;
		default:
			syntax_error();
			break;
	}
}

int
read_node(node)
	Rectobj	node;
{

	switch((Token) yylex()) {
		case T_LPAR: 
			break;
		case T_EOF:
			return FALSE;
		default:
			syntax_error();
			break;
	}
	read_node_descr(node);
	return TRUE;
}

yywrap()
{
	return(1);
}


Rectobj
add_leaf(string, node)
	char		*string;
	Rectobj		node;
{
	Rectobj		new;

	new = (Rectobj) xv_create(tree, DRAWTEXT, 
		DRAWTEXT_STRING, string,
		NULL);

	xv_set(tree, 
		TREE_ADD_LINK, node, new, 
		NULL);
	return new;
}



void
set_horizontal(menu, menu_item)
	Menu	menu;
	Menu_item menu_item;
{

	/*
	 * Set the size of the tree before changing it. 
	 * This will have the effect of centering it if
	 * all the space isn't used.
	 */
	xv_set(tree,
		XV_WIDTH, xv_get(shell, XV_WIDTH),
		XV_HEIGHT, xv_get(shell, XV_HEIGHT),

		TREE_LAYOUT, TREE_LAYOUT_HORIZONTAL,
		NULL);

	/*
	 * In case the tree grew bigger than the paint window,
	 * set the paint window size.  This way, the scrollbars
	 * will be properly set.
	 */
	xv_set(shell,	
		CANVAS_MIN_PAINT_WIDTH, xv_get(tree, XV_WIDTH),
		CANVAS_MIN_PAINT_HEIGHT, xv_get(tree, XV_HEIGHT),
		NULL);
}


void
set_vertical(menu, menu_item)
	Menu	menu;
	Menu_item menu_item;
{
	xv_set(tree,
		XV_WIDTH, xv_get(shell, XV_WIDTH),
		XV_HEIGHT, xv_get(shell, XV_HEIGHT),

		TREE_LAYOUT, TREE_LAYOUT_VERTICAL,
		NULL);

	xv_set(shell,	
		CANVAS_MIN_PAINT_WIDTH, xv_get(tree, XV_WIDTH),
		CANVAS_MIN_PAINT_HEIGHT, xv_get(tree, XV_HEIGHT),
		NULL);
}


void
resize(shell, width, height)
	Canvas_shell	shell;
	int		width;	
	int		height;
{
	if(tree)
		xv_set(tree,	
			XV_WIDTH, width,
			XV_HEIGHT, height,
			NULL);
}

