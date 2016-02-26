
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
	file:color_palet.c
	purpose: This file contains that functions that handle the color
		map for usage with color suns

	modifications:
		date:	Tue Mar 22 22:04:58 EST 1988
		author:	rayk
		changes:add comments

		date:	Thu Jun 16 21:02:52 EDT 1988
		author:	tnosoes!tom@mcvax.cwi.nl
		changes:corrected problems with FRAME_INHERIT_COLOR
			and the problem of the color table flashing
			on and off when you move the mouse between
			window.

		date:	Sat Jun 25 00:04:28 EDT 1988
		author:	rayk
		changes:took out the code in my_put_colormap that would
			reset color 1 & 254

		date:	Sat Jun 25 00:04:28 EDT 1988
		author:	tnosoes!tom@uunet.UU.NET
		changes:added at colormap editor, so that the RGB values
			of colormap enties can dynamiclly be changed

**************************************************************************/

#include "header.h"
#include <sunwindow/cms_mono.h>

/*
 * Lets go into color mode, Scotty
 *
 */
set_color()
{
  panel_set(mono_cycle,PANEL_SHOW_ITEM, FALSE,0);
  panel_set(color_button,PANEL_SHOW_ITEM, TRUE,0);
}


/*
 * Back to mono mode
 *
 */
set_mono()
{
  /*
   * set the color map to mono
   */
  if (((pw->pw_pixrect->pr_depth) > 1) && (!BW_mode))
   {
     cms_monochromeload(red,green,blue);
     my_put_colormap();
   }
  panel_set(color_button,PANEL_SHOW_ITEM, FALSE,0);
  panel_set(mono_cycle,PANEL_SHOW_ITEM, TRUE,0);
}


/*
 * Set the colormap for all of the windows
 */
unsigned char temp_red[256],temp_green[256],temp_blue[256];
extern struct singlecolor fore_ground,back_ground;
extern int fore_back_flag;
my_put_colormap()
{
Pixwin *temp_pw;

  pw_setcmsname(pw, "ray kreisel");
  pw_putcolormap(pw, 0,256,red,green,blue);
  pw_setcmsname(fat_pw, "ray kreisel");
  pw_putcolormap(fat_pw, 0,256,red,green,blue);
  pw_setcmsname(color_pw, "ray kreisel");
  pw_putcolormap(color_pw, 0,256,red,green,blue);

  bcopy(red,temp_red,256);
  bcopy(green,temp_green,256);
  bcopy(blue,temp_blue,256);

  if (fore_back_flag)
  {
    temp_red[0] = fore_ground.red;
    temp_green[0] = fore_ground.green;
    temp_blue[0] = fore_ground.blue;
  
    temp_red[255] = back_ground.red;
    temp_green[255] = back_ground.green;
    temp_blue[255] = back_ground.blue;
  }  

  temp_pw = (Pixwin *)window_get(base_frame, WIN_PIXWIN);
  pw_setcmsname(temp_pw, "ray kreisel");
  pw_putcolormap(temp_pw, 0,256,temp_red,temp_green,temp_blue);

  temp_pw = (Pixwin *)window_get(panel, WIN_PIXWIN);
  pw_setcmsname(temp_pw, "ray kreisel");
  pw_putcolormap(temp_pw, 0,256,temp_red,temp_green,temp_blue);

  temp_pw = (Pixwin *)window_get(command_panel, WIN_PIXWIN);
  pw_setcmsname(temp_pw, "ray kreisel");
  pw_putcolormap(temp_pw, 0,256,temp_red,temp_green,temp_blue);

  temp_pw = (Pixwin *)window_get(region_panel, WIN_PIXWIN);
  pw_setcmsname(temp_pw, "ray kreisel");
  pw_putcolormap(temp_pw, 0,256,temp_red,temp_green,temp_blue);

  temp_pw = (Pixwin *)window_get(pattern_panel, WIN_PIXWIN);
  pw_setcmsname(temp_pw, "ray kreisel");
  pw_putcolormap(temp_pw, 0,256,temp_red,temp_green,temp_blue);

  temp_pw = (Pixwin *)window_get(brush_panel, WIN_PIXWIN);
  pw_setcmsname(temp_pw, "ray kreisel");
  pw_putcolormap(temp_pw, 0,256,temp_red,temp_green,temp_blue);

  temp_pw = (Pixwin *)window_get(text_panel, WIN_PIXWIN);
  pw_setcmsname(temp_pw, "ray kreisel");
  pw_putcolormap(temp_pw, 0,256,temp_red,temp_green,temp_blue);

  temp_pw = (Pixwin *)window_get(color_frame, WIN_PIXWIN);
  pw_setcmsname(temp_pw, "ray kreisel");
  pw_putcolormap(temp_pw, 0,256,temp_red,temp_green,temp_blue);

  temp_pw = (Pixwin *)window_get(color_panel, WIN_PIXWIN);
  pw_setcmsname(temp_pw, "ray kreisel");
  pw_putcolormap(temp_pw, 0,256,temp_red,temp_green,temp_blue);

  temp_pw = (Pixwin *)window_get(fat_frame, WIN_PIXWIN);
  pw_setcmsname(temp_pw, "ray kreisel");
  pw_putcolormap(temp_pw, 0,256,temp_red,temp_green,temp_blue);

  temp_pw = (Pixwin *)window_get(fat_panel, WIN_PIXWIN);
  pw_setcmsname(temp_pw, "ray kreisel");
  pw_putcolormap(temp_pw, 0,256,temp_red,temp_green,temp_blue);
}


color_mode()
{
    (void)window_set(color_frame, WIN_SHOW, TRUE, 0);
    draw_colormap();
}


/*
 * Draw the colormap up on a canvas in the color palet window
 */
draw_colormap()
{
int i;
  for (i=0;i<256;i++)
    pw_write(color_pw,(i%16)*PALET_BLOCK,i/16*PALET_BLOCK,PALET_BLOCK,PALET_BLOCK,PIX_COLOR(i) | PIX_SRC,NILPR,0,0);
  update_cur_color(0,0,cur_color);
}



/*
 * Get rid of the color palet
 */
color_done()
{
    (void)window_set(color_frame, WIN_SHOW, FALSE, 0);
}


/*
 * Make an event handle for the mouse envent of the color palet
 * Let the use pick a block on the color palet and set the current
 * color a cordingly
 */
color_handle_event(canvas_local, event)
Canvas  canvas_local;
Event   *event;
{
    if (event_id(event) == MS_LEFT)
	update_cur_color(event_x(event),event_y(event),0);
}


/*
 * Redraw the currrent color at the bottom of the color palet
 */
update_cur_color(x,y,value)
{
  if (value)
   {
    cur_color= value;
   }
  else
   {
     if ((x >= PALET_BLOCK*16) || (y >= PALET_BLOCK*16))
	   return;
     cur_color = x/PALET_BLOCK + y/PALET_BLOCK*16;
   }
  pw_write(color_pw,0,PALET_BLOCK*16+8,PALET_BLOCK*16,PALET_BLOCK*2,PIX_COLOR(cur_color) | PIX_SRC,NILPR,0,0);
  col_choose();
}



/*
 * Color editing for editing RGB value of single colormap entries
 * written by: tnosoes!tom@uunet.UU.NET
 */

#define	CMS_SIZE	256

int			changed_0, changed_255;

Panel_item	red_slide, green_slide, blue_slide;
Panel_item	col_file;
Panel_item	cur_col_msg;

extern int		errno, sys_nerr;
extern char		*sys_errlist[];


/*
 * update the panel message with the number of current color
 * and the proper R, G, B values for that color table entry
 */
col_choose()
{
  char	s[5];

  sprintf(s, "%d", cur_color);
  panel_set(cur_col_msg, PANEL_LABEL_STRING, s, 0);
  panel_set(red_slide, PANEL_VALUE, red[cur_color], 0);
  panel_set(green_slide, PANEL_VALUE, green[cur_color], 0);
  panel_set(blue_slide, PANEL_VALUE, blue[cur_color], 0);
}


/*
 * get the user's new R, G, B values from the sliders
 */
col_change(item, value)
Panel_item	item;
int		value;
{
  switch ((int) panel_get(item, PANEL_CLIENT_DATA)) {
	case 0:
		red[cur_color]= value;
		break;
	case 1:
		green[cur_color]= value;
		break;
	case 2:
		blue[cur_color]= value;
		break;
  }
  my_put_colormap();

  if (cur_color == 0)
	changed_0= 1;
  if (cur_color == 255)
	changed_255= 1;
}


/*
 * Let's do file completion on what we have in the color file name prompt
 */
make_new_col_name()
{
  char	file_name[MAX_FILE_NAME];

  strcpy(file_name,(char*)panel_get_value(col_file));
  if (complete(file_name))
    window_bell(panel);
  panel_set(col_file,PANEL_VALUE,file_name,0);
}


/*
 * get a colormap from a disk file, the filename is in the filename
 * text string from the colormap control panel
 */
col_load()
{
  char	*name;
  FILE	*f;
  char	fname[MAX_FILE_NAME];

  if (*(name= panel_get(col_file, PANEL_VALUE)) == '\0') {
	ERROR("You should enter a filename first");
	return;
  }
  get_full_path(name, fname);
  if (file_is_dir(fname)) {
	ERROR("That file is a directory");
	return;
  }
  if ((f= fopen(fname, "r")) == NULL) {
	if (errno >= sys_nerr)
		ERROR("Cannot open that file");
	else
		ERROR(sys_errlist[errno]);
	return;
  }
  if (!fread(red, CMS_SIZE, 1, f)
	|| !fread(green, CMS_SIZE, 1, f)
	|| !fread(blue, CMS_SIZE, 1, f))
		ERROR("Load failed !");
  fclose(f);
  my_put_colormap();
}


/*
 * Save a colormap out to a disk file
 */
col_save()
{
  char	*name;
  FILE	*f;
  char	fname[MAX_FILE_NAME];

  if (*(name= panel_get(col_file, PANEL_VALUE)) == '\0') {
	ERROR("You should enter a filename first");
	return;
  }
  get_full_path(name, fname);
  if (file_exist(fname)) {
     if (!confirm("Overwrite existing file ?"))
	 return;
  }
  if ((f= fopen(fname, "w")) == NULL) {
	if (errno >= sys_nerr)
		ERROR("Cannot open that file");
	else
		ERROR(sys_errlist[errno]);
	return;
  }
  if (!fwrite(red, CMS_SIZE, 1, f)
	|| !fwrite(green, CMS_SIZE, 1, f)
	|| !fwrite(blue, CMS_SIZE, 1, f))
		ERROR("Save failed !");
  fclose(f);
}



