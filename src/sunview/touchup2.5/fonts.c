
/**************************************************************************
   Touchup a bitmap graphics editor for the Sun Workstation running SunView
   Copyright (c) 1988 by Raymond Kreisel
   1/22/88 @ Suny Stony Brook

   This program may be redistributed without fee as long as this copyright
   notice is intact.

==> PLEASE send comments and bug reports to one of the following addresses:

	   Ray Kreisel
	   CS Dept., SUNY at Stony Brook, Stony Brook NY 11794

	   UUCP: {allegra, philabs, pyramid, research}!sbcs!rayk   
	   ARPA-Internet: rayk@sbcs.sunysb.edu			
	   CSnet: rayk@suny-sb
	   (If nobody is home at any of the above addresses try:
		S72QKRE@TOWSONVX.BITNET			        )

 "If I get home before daylight, I just might get some sleep tonight...."

**************************************************************************/
/**************************************************************************
	file: fonts
	purpose: The functions in this file build a list of the where
		are the available fonts are by reading from a ".touchup"
		that the user can supply.

	modifications:
		date:	Wed Jul 27 00:34:17 EDT 1988
		author:	sgoell@sbcs.sunyb.edu
		changes:total rewrite of fonts functions, so that the user
			give a list of fonts and where they are located

		date:	Wed Jul 27 00:34:17 EDT 1988
		author:	rayk
		changes:built the user menus for the fonts list and the
			point size list
**************************************************************************/

#include "header.h"

typedef struct {
  char *name;
  struct font *node;
} fontnamelist;

typedef struct font {
	int dir;
	char *filename;
	char *fontname;
	int pointsize[15];
	struct font *next;
} *fontlist;

fontnamelist fonts[60];
struct  pixfont  *real_font;

Menu font_menu, pt_menu;

int cur_pt_size=3;


#define skipws(file) { \
	c = getc(file);\
	while ((c == ' ') || (c == '\t') || (c == '\n'))\
	    c = getc(file);\
}

#define set_psize(font, index, psize) {\
	font->pointsize[index] = psize;\
	psize = 0;\
}

char fontdirs[6][MAX_FILE_NAME];
fontlist allfonts;

#define USER_FONTFILE "/.touchup"
#define SYS_FONTFILE "/touchup_fonts"
#define SYSFONTDIR 	0
#define SYSFONTPATH 	"/usr/lib/fonts/fixedwidthfonts"
#define DEFAULT_FONT	"Screen"
#define DEFAULT_FNAME	"screen.r."
#define MAXFONTS	60



/*
 * Give me the pointer to the node for this font and the point size
 * and this function will return the full path of where to find the font
 */
char *mk_path(font, ptsize)
fontlist font;
int ptsize;
{
static char path[MAX_FILE_NAME];
char  psize[3];
    
    strcpy(path, fontdirs[font->dir]);
    strcat(path, "/");
    strcat(path, font->filename);
    sprintf(psize, "%d", font->pointsize[ptsize]);
    strcat(path, psize);
    return(path);
}


/*
 * Add a new font to the existing list of fonts, remember which dir the font
 * is in, the name, and the filename
 */
fontlist mk_font(dir, name, file)
int	dir;
char	*name;
char	*file;
{
    char *malloc();
    fontlist tmp;

    tmp = (fontlist) malloc(sizeof(struct font));
    tmp->dir = dir;
    tmp->fontname = malloc(strlen(name)+1);
    strcpy(tmp->fontname, name);
    tmp->filename = malloc(strlen(file)+1);
    strcpy(tmp->filename, file);
    tmp->next = 0;
    return(tmp);
}

int change_pt_size();

/*
 * First build the list of the all of the fonts we know then
 * set up everything for the fonts and point size menus.
 */
init_font()
{
int i;
fontlist currfont;

    real_font = main_font;
    for(i=0;i<60;i++)
	fonts[i].name = 0;

    init_fontlist();
    font_menu = menu_create(MENU_NCOLS, 4,
			    MENU_INITIAL_SELECTION_SELECTED, TRUE,
			    MENU_INITIAL_SELECTION, MENU_SELECTED,
			    MENU_DEFAULT_SELECTION, MENU_SELECTED,
			    0);
    pt_menu = menu_create(MENU_NCOLS, 3,
			    MENU_INITIAL_SELECTION_SELECTED, TRUE,
			    MENU_INITIAL_SELECTION, MENU_SELECTED,
			    MENU_DEFAULT_SELECTION, MENU_SELECTED,
			    MENU_STRINGS,"7","11","12","14",0,0);

    currfont = allfonts;
    i=0;
    while (currfont) {
	fonts[i].node = currfont;
	fonts[i].name = currfont->fontname;
	menu_set(font_menu,MENU_STRING_ITEM,currfont->fontname,++i,0);
	currfont = currfont->next;
    } 
}


/***********************************************************************\
* 								        *
*  init_fontlist reads the fontnames from the fontfile and initializes  *
*  the font menu.						        *
* 								        *
\***********************************************************************/
init_fontlist()

{
    int c;
    FILE *fontfile, *fopen();
    int currfdir = 0, fontcnt = 0;
    char name[100], file[MAX_FILE_NAME];
    fontlist newfont, currfont;
    int ps0, ps1, ps2, ps3, ps4, ps5, ps6, ps7, ps8, ps9, ps10, ps11, ps12,
    	ps13, ps14;

    /*
     * First check if the user has a font list in his HOME dir with
     * the name ".touchup"
     */
    if (!getenv("HOME"))
	printf("Cannot find the environment variable HOME\n");
    else
        strcpy(file_name,getenv("HOME"));
    strcat(file_name,USER_FONTFILE);
    if (!(fontfile = fopen(file_name, "r")))
     {
	/*
	 * If the user does not have a font list in his HOME dir then
	 * check the dir where touchup was installed
	 */
	strcpy(file_name,INSTALL_DIR);
	strcat(file_name,SYS_FONTFILE);
        if (!(fontfile = fopen(file_name, "r")))
           {
	      /*
	       * We can not find anything so give them a couple founds
	       */
	      printf("Cannot find font list in:%s\n",file_name);
	      printf("Using default fonts.\n");
	      allfonts = mk_font(SYSFONTDIR, DEFAULT_FONT, DEFAULT_FNAME);
	      strcpy(fontdirs[0],SYSFONTPATH);
    	      allfonts->pointsize[0] = 7;
    	      allfonts->pointsize[1] = 11;
              allfonts->pointsize[2] = 12;
              allfonts->pointsize[3] = 13;
              allfonts->pointsize[4] = 14;
	      return;
	   }
     }

    allfonts = mk_font(SYSFONTDIR, DEFAULT_FONT, DEFAULT_FNAME);
    strcpy(fontdirs[0],SYSFONTPATH);
    allfonts->pointsize[0] = 7;
    allfonts->pointsize[1] = 11;
    allfonts->pointsize[2] = 12;
    allfonts->pointsize[3] = 13;
    allfonts->pointsize[3] = 14;
    cur_pt_size = 3;
    currfont = allfonts;
    skipws(fontfile);
    while (c == '#') {
	while (getc(fontfile) != '\n');
    	skipws(fontfile);
    }
new_dir:
    ungetc(c, fontfile);
    fscanf(fontfile, "%s", fontdirs[++currfdir]);
    skipws(fontfile);
    while (c == '#') {
	while (getc(fontfile) != '\n');
    	skipws(fontfile);
    }

new_font:
    fscanf(fontfile, "%[^\"]\" %s %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d", name, file, &ps0, &ps1, &ps2, &ps3, &ps4, &ps5, &ps6, &ps7, &ps8, &ps9, &ps10, &ps11, &ps12, &ps13, &ps14);
    newfont = mk_font(currfdir, name, file);
    currfont->next = newfont;
    currfont = currfont->next;
    set_psize(newfont, 0, ps0);
    set_psize(newfont, 1, ps1);
    set_psize(newfont, 2, ps2);
    set_psize(newfont, 3, ps3);
    set_psize(newfont, 4, ps4);
    set_psize(newfont, 5, ps5);
    set_psize(newfont, 6, ps6);
    set_psize(newfont, 7, ps7);
    set_psize(newfont, 8, ps8);
    set_psize(newfont, 9, ps9);
    set_psize(newfont, 10, ps10);
    set_psize(newfont, 11, ps11);
    set_psize(newfont, 12, ps12);
    set_psize(newfont, 13, ps13);
    set_psize(newfont, 14, ps14);
    skipws(fontfile);
    while (c == '#') {
	while (getc(fontfile) != '\n');
	skipws(fontfile);
    }
    switch (c) {
	case EOF: 
	   fclose(fontfile);
	   return;

	case '"':
	    if (fontcnt++ == MAXFONTS)
	      {
	        fclose(fontfile);
	        return;
	      }
	    goto new_font;

	default:
	    if (currfdir == 5)
	      {
	        fclose(fontfile);
	        return;
	      }
	    goto new_dir;
    }
}


char pt_size[15][3];
int cur_font_index=0;

/*
 * Get a new font from the disk and use the first point size
 * Also build a menu of all of the point sizes for this font.
 */
get_new_font(event)
Event   *event;
{
int i,j;
char temp_str[4];

  i = (int)menu_show(font_menu, text_panel, event, 0);
  if (i==0) return;
  cur_pt_size = 0;
  if (cur_font_index)
	pf_close(real_font);
  cur_font_index = i-1;
  real_font = pf_open(mk_path(fonts[cur_font_index].node,cur_pt_size));
  if (!real_font)
    {
     ERRORstr("ERROR loading the font file:",mk_path(fonts[cur_font_index].node,cur_pt_size));
     real_font = main_font;
     menu_destroy(pt_menu);
     pt_menu = menu_create(MENU_NCOLS, 3,
			    MENU_INITIAL_SELECTION_SELECTED, TRUE,
			    MENU_INITIAL_SELECTION, MENU_SELECTED,
			    MENU_DEFAULT_SELECTION, MENU_SELECTED,
			MENU_STRINGS,"7","11","12","14",0,0);
     cur_pt_size = 3;
     cur_font_index = 0;
    }
  else
    {
      menu_destroy(pt_menu);
      pt_menu = menu_create(MENU_NCOLS, 3,
			    MENU_INITIAL_SELECTION_SELECTED, TRUE,
			    MENU_INITIAL_SELECTION, MENU_SELECTED,
			    MENU_DEFAULT_SELECTION, MENU_SELECTED,
			    0);
      for(j=0;j<15;j++)
       {
	 if (fonts[cur_font_index].node->pointsize[j])
	   {
	    sprintf(pt_size[j],"%d",fonts[cur_font_index].node->pointsize[j]);
	    menu_set(pt_menu,MENU_STRING_ITEM,pt_size[j],j+1,0);
	   }
       }
    }
}



/*
 * Get a new copy of the current font but with a different point size
 */
change_pt_size(event)
Event *event;
{
  cur_pt_size = (int)menu_show(pt_menu, text_panel, event, 0);
  if (cur_pt_size)
    {
	cur_pt_size--;
        if (cur_font_index)
	     pf_close(real_font);
        real_font = pf_open(mk_path(fonts[cur_font_index].node,cur_pt_size));
       	if (!real_font)
    	{
     	   ERRORstr("ERROR loading the font file:",
		mk_path(fonts[cur_font_index].node,cur_pt_size));
     	   real_font = main_font;
     	   menu_destroy(pt_menu);
     	   pt_menu = menu_create(MENU_NCOLS, 3,
			    MENU_INITIAL_SELECTION_SELECTED, TRUE,
			    MENU_INITIAL_SELECTION, MENU_SELECTED,
			    MENU_DEFAULT_SELECTION, MENU_SELECTED,
			    MENU_STRINGS,"7","11","12","14",0,0);
	   cur_font_index = 0;
	   cur_pt_size = 3;
	}
    }
}



char cur_text_str[100];
int cur_text_x,cur_text_y,org_text_x;
#define DELETE_KEY 127


/*
 * Let's do interactive text right onto the bitmap, that is let the
 * user type the text and we XOR it on to screen so that they
 * can backspace. But there is only one current text string, every thing
 * else is already on the bitmap.
 */
new_draw_text(event)
Event *event;
{
int i,ROP;
struct pr_size text_wid;
int text_hgt;
 

      text_hgt = real_font->pf_defaultsize.y;

      if (!(ROP = get_current_ROP()))
	 ROP = TRANSPARENT;

      text_wid = pf_textwidth(strlen(cur_text_str),real_font,cur_text_str);

      pw_batch_on(pw);
      switch(event_id(event)) {
	case '\n' :
	case '\r' :
		/*
		 * We got a <return> so erase the old text and do the
		 * final printing of the text string with the right ROP
		 */
		pw_replrop(pw,cur_text_x+ text_wid.x ,cur_text_y- text_hgt,
			   1,text_hgt, PIX_XOR, pattern[0],0,0);
		draw_text(cur_text_x,cur_text_y,cur_text_str,PIX_XOR);

		draw_text(cur_text_x,cur_text_y,cur_text_str,ROP);
		cur_text_y += text_hgt;
	        cur_text_x = org_text_x;
		cur_text_str[0] = '\0';
		pw_replrop(pw,cur_text_x,cur_text_y- text_hgt,
			   1,text_hgt, PIX_XOR, pattern[0],0,0);
		break;
	case '\b' :
	case DELETE_KEY :
		/*
		 * We got a delete so erase the old string and then 
		 * delete the last char of the string and reprint
		 */
		pw_replrop(pw,cur_text_x+ text_wid.x ,cur_text_y- text_hgt,
			   1,text_hgt, PIX_XOR, pattern[0],0,0);
		draw_text(cur_text_x,cur_text_y,cur_text_str,PIX_XOR);
		i = strlen(cur_text_str);
		if (i>0)
		   cur_text_str[i-1] = '\0';

      		text_wid = pf_textwidth(strlen(cur_text_str),
		     real_font,cur_text_str);

		switch(panel_get_value(text_choice)) {
		  case 0: cur_text_x = org_text_x - text_wid.x/2;
			  break;
		  case 1: break;
		  case 2: cur_text_x = org_text_x - text_wid.x;
			  break;
		}
		draw_text(cur_text_x,cur_text_y,cur_text_str,PIX_XOR);
		pw_replrop(pw,cur_text_x+ text_wid.x ,cur_text_y- text_hgt,
			   1,text_hgt, PIX_XOR, pattern[0],0,0);
		break;

	default :
		/*
		 * We got a normal char so erase the old string, then
		 * add the new char and reprint the string
		 */
		pw_replrop(pw,cur_text_x+ text_wid.x ,cur_text_y- text_hgt,
			   1,text_hgt, PIX_XOR, pattern[0],0,0);
		draw_text(cur_text_x,cur_text_y,cur_text_str,PIX_XOR);
		i = strlen(cur_text_str);
		cur_text_str[i] = event_id(event);
		cur_text_str[i+1] = '\0';

      		text_wid = pf_textwidth(strlen(cur_text_str),
		     real_font,cur_text_str);

		/*
		 * Check if the text is centered, left, or right justified
		 */
		switch(panel_get_value(text_choice)) {
		  case 0: cur_text_x = org_text_x - text_wid.x/2;
			  break;
		  case 1: break;
		  case 2: cur_text_x = org_text_x - text_wid.x;
			  break;
		}

		draw_text(cur_text_x,cur_text_y,cur_text_str,PIX_XOR);
		pw_replrop(pw,cur_text_x+ text_wid.x ,cur_text_y- text_hgt,
			   1,text_hgt, PIX_XOR, pattern[0],0,0);
			   
      }

      /*
       * =========> NOTE!!!! HACK <================
       * There seems to be a problem will pw_batch and pw_text,
       * because on large fonts pw_batch does not update all of the
       * canvas when the text drawn has decending characters
       * So this HACK just fakes pw_batch into thinking that a
       * larger area of the screen was updated, so the when it gets
       * to pw_batch_off() the correct part of the screen is refreshed.
       */
      pw_rop(pw,cur_text_x,cur_text_y,
			   text_wid.x*2,text_hgt*2, PIX_XOR,0,0,0);
      pw_batch_off(pw);
}

int text_cursor=FALSE;


/*
 * We are done with the text mode, so lets do the final printing of
 * the text and clear the cursor
 */
finish_text()
{
struct pr_size text_wid;
int ROP;

  if (cur_text_str[0]) 
   {	 
    pw_batch_on(pw);
    text_wid = pf_textwidth(strlen(cur_text_str),
	real_font,cur_text_str);
    pw_replrop(pw,cur_text_x+ text_wid.x ,cur_text_y- TEXT_HGT,
	1,TEXT_HGT, PIX_XOR, pattern[0],0,0);
    draw_text(cur_text_x,cur_text_y,cur_text_str,PIX_XOR);
    if (!(ROP = get_current_ROP()))
	 ROP = TRANSPARENT;
    draw_text(cur_text_x,cur_text_y,cur_text_str,ROP);
    cur_text_str[0] = '\0';
    text_cursor = FALSE;
    cur_text_y += TEXT_HGT;
    pw_batch_off(pw);
   }
  else if (text_cursor) 
   {	 
    /*
     * there is no cuurent text so just erase the cursor
     */
    pw_replrop(pw,cur_text_x,cur_text_y- TEXT_HGT,
	1,TEXT_HGT, PIX_XOR, pattern[0],0,0);
    text_cursor = FALSE;
   }
}

extern int get_new_font();
extern int change_pt_size();
extern Panel_item font_button;


/*
 * Change the font of th e current TEXT string and change the
 * the size of the text cursor, but first clear off the old stuff
 */
change_font(item, event)
Panel_item item;
Event *event;
{
struct pr_size text_wid;

  if (cur_text_str[0])
    {
     text_wid = pf_textwidth(strlen(cur_text_str),
	real_font,cur_text_str);
     pw_replrop(pw,cur_text_x+ text_wid.x ,cur_text_y- 
	real_font->pf_defaultsize.y,
	1, real_font->pf_defaultsize.y,
	 PIX_XOR, pattern[0],0,0);
     pw_text(pw,cur_text_x,cur_text_y,PIX_COLOR(cur_color) | PIX_XOR,
		real_font,cur_text_str);

     if (item == font_button)
     	get_new_font(event);
     else
	change_pt_size(event);

     text_wid = pf_textwidth(strlen(cur_text_str),real_font,cur_text_str);

    /*
     * Check if the text is centered, left, or right justified
     */
     switch(panel_get_value(text_choice)) {
	case 0: cur_text_x = org_text_x - text_wid.x/2;
	  break;
	case 1: break;
	case 2: cur_text_x = org_text_x - text_wid.x;
	  break;
     }

     draw_text(cur_text_x,cur_text_y,cur_text_str,PIX_XOR);
     pw_replrop(pw,cur_text_x+ text_wid.x ,cur_text_y- TEXT_HGT,
	1,TEXT_HGT, PIX_XOR, pattern[0],0,0);
    }
  else if (text_cursor) 
   {	 
     /*
      * There is no current text string so just change the size
      * of the cursor to reflex the size of the font
      */
     pw_replrop(pw,cur_text_x,cur_text_y- 
	real_font->pf_defaultsize.y,
	1, real_font->pf_defaultsize.y,
	 PIX_XOR, pattern[0],0,0);
     if (item == font_button)
     	get_new_font(event);
     else
	change_pt_size(event);
     pw_replrop(pw,cur_text_x,cur_text_y- TEXT_HGT,
	1,TEXT_HGT, PIX_XOR, pattern[0],0,0);
   }
  else
     /*
      * make sure we always know the last font, so that we
      * can erase the current string
      */
     if (item == font_button)
     	get_new_font(event);
     else
	change_pt_size(event);
}


