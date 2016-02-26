/********************************************************************************/
/* icon.h --									*/
/*										*/
/* Author : A.J.Doherty								*/
/* Date   : 14/05/93								*/
/* Version: 0.6	(19/05/93)							*/
/********************************************************************************/

#ifndef _FSPtool_ICON_H_
#define _FSPtool_ICON_H_ 1

extern void load_icons();
extern void set_icon(int,int,int);
extern void set_frame_icon();

extern Server_image return_file_glyph(int);

#endif
/********************************************************************************/
