/*
 *
 * 	bn_set.c  
 * 	modification des boutons 
 *
 * 	Modification :  11/11/93
 *
 *	Copyright (c) 1993 Bruno RIVAS
 *	All Rights Reserved
 *
 *
 * Permission to use, copy, modify, distribute and sell this software and its
 * documentation for any purpose and without fee is hereby granted, provided
 * that the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the name of Bruno RIVAS not be used in advertising
 * or publicity pertaining to distribution of the software without specific,
 * written prior permission.  Bruno RIVAS makes no representations about the
 * suitability of this software for any purpose.  It is provided "as is"
 * without express or implied warranty.
 *
 * Bruno RIVAS disclaims all warranties with regard to this software,
 * including all implied warranties of merchantability and fitness,
 * in no event shall Bruno RIVAS be liable for any special,
 * indirect or consequential damages or any damages
 * whatsoever resulting from loss of use, data or profits,
 * whether in an action of contract, negligence or other tortious
 * action, arising out of or in connection with the use 
 * or performance of this software.
 *
 *
 *	Bruno RIVAS 
 *      IMAN Development Toolkit version 1.0.d
 *
 *	Internet:       rivas@email.teaser.com
 *	Surface Mail:   Bruno RIVAS,
 *			30 avenue Marie
 *			93250 Villemomble -- FRANCE
 *	Voice phone:    (33) (1) 49.35.97.27
 *	Fax: 		(33) (1) 48.94.27.91
 *
 */



#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <malloc.h>
#include <memory.h>

#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Xos.h>


#include <X11/iman/widgets.h>






			/*** Changer l'etat ***/


int BN_Block(tk_display,buttonid)
TkDisplay *tk_display;
ButtonID buttonid;
{
 ButtonStruct *button;


 if(buttonid>=0 && buttonid<tk_display->maxwidgets && tk_display->widgets[buttonid].class==WI_BUTTON&&tk_display->widgets[buttonid].button!=NULL)
   button=tk_display->widgets[buttonid].button;
 else return -1;

 if((button->state&Blocked)!=Blocked) 
   button->state=button->state+Blocked;
 return button->state;
}





int BN_Unblock(tk_display,buttonid)
TkDisplay *tk_display;
ButtonID buttonid;
{
  ButtonStruct *button;


 if(buttonid>=0 && buttonid<tk_display->maxwidgets && tk_display->widgets[buttonid].class==WI_BUTTON&&tk_display->widgets[buttonid].button!=NULL)
   button=tk_display->widgets[buttonid].button;
 else return -1;

 if((button->state&Blocked)==Blocked) 
  button->state=button->state-Blocked;
 return button->state;
}





int BN_Push(tk_display,buttonid)
TkDisplay *tk_display;
ButtonID buttonid;
{
 ButtonStruct *button;


 if(buttonid>=0 && buttonid<tk_display->maxwidgets && tk_display->widgets[buttonid].class==WI_BUTTON&&tk_display->widgets[buttonid].button!=NULL)
   button=tk_display->widgets[buttonid].button;
 else return -1;


 if((button->state&Pushed)!=Pushed){ 
   button->state=button->state+Pushed;
   if (button->type == BN_PUSHBUTTON) 	   bn_DrawPushButton(tk_display,buttonid);
   else if(button->type == BN_REPEATBUTTON)  bn_DrawRepeatButton(tk_display,buttonid);
   else if(button->type == BN_POPUPBUTTON)   bn_DrawPopupButton(tk_display,buttonid);
   else if(button->type == BN_CROSSBUTTON)   bn_DrawCrossButton(tk_display,buttonid);
   else if(button->type == BN_RADIOBUTTON)   bn_DrawRadioButton(tk_display,buttonid); 
   else if(button->type == BN_POPUPRADIOBUTTON) bn_DrawPopupRadioButton(tk_display,buttonid); 
   else if(button->type == BN_SCROLLBUTTON)  bn_DrawScrollButton(tk_display,buttonid);
   else if(button->type == BN_THUMBBUTTON)   bn_DrawThumbButton(tk_display,buttonid); 
   else if(button->type == BN_CHECKBUTTON)   bn_DrawCheckButton(tk_display,buttonid);
   else if(button->type == BN_COMBOBUTTON)   bn_DrawComboButton(tk_display,buttonid);
 }
 return button->state;
}





int BN_Unpush(tk_display,buttonid)
TkDisplay *tk_display;
ButtonID buttonid;
{
 ButtonStruct *button;


 if(buttonid>=0 && buttonid<tk_display->maxwidgets && tk_display->widgets[buttonid].class==WI_BUTTON&&tk_display->widgets[buttonid].button!=NULL)
   button=tk_display->widgets[buttonid].button;
 else return -1;


 if ((button->state&Pushed)==Pushed){ 
   button->state=button->state-Pushed;
   if (button->type ==BN_PUSHBUTTON) bn_DrawPushButton(tk_display,buttonid);
   else if(button->type == BN_REPEATBUTTON) bn_DrawRepeatButton(tk_display,buttonid);
   else if(button->type == BN_POPUPBUTTON)  bn_DrawPopupButton(tk_display,buttonid);
   else if(button->type == BN_CROSSBUTTON) bn_DrawCrossButton(tk_display,buttonid);
   else if(button->type == BN_RADIOBUTTON) bn_DrawRadioButton(tk_display,buttonid);
   else if(button->type == BN_POPUPRADIOBUTTON) bn_DrawPopupRadioButton(tk_display,buttonid); 
   else if(button->type == BN_REPEATBUTTON) bn_DrawRepeatButton(tk_display,buttonid); 
   else if(button->type == BN_SCROLLBUTTON) bn_DrawScrollButton(tk_display,buttonid);
   else if(button->type == BN_THUMBBUTTON) bn_DrawThumbButton(tk_display,buttonid); 
   else if(button->type == BN_CHECKBUTTON) bn_DrawCheckButton(tk_display,buttonid);
   else if(button->type == BN_COMBOBUTTON)   bn_DrawComboButton(tk_display,buttonid);
 }
 return button->state;
}





int BN_Gray(tk_display,buttonid)
TkDisplay *tk_display;
ButtonID buttonid;
{
 ButtonStruct *button;


 if(buttonid>=0 && buttonid<tk_display->maxwidgets && tk_display->widgets[buttonid].class==WI_BUTTON&&tk_display->widgets[buttonid].button!=NULL)
   button=tk_display->widgets[buttonid].button;
 else return -1;


 if ((button->state&Grayed)!=Grayed){
   button->state=button->state+Grayed;
   if (button->type ==BN_PUSHBUTTON) bn_DrawPushButton(tk_display,buttonid);
   else if(button->type == BN_REPEATBUTTON) bn_DrawRepeatButton(tk_display,buttonid);
   else if(button->type == BN_POPUPBUTTON)  bn_DrawPopupButton(tk_display,buttonid);
   else if(button->type == BN_CROSSBUTTON) bn_DrawCrossButton(tk_display,buttonid);
   else if(button->type == BN_RADIOBUTTON) bn_DrawRadioButton(tk_display,buttonid);
   else if(button->type == BN_POPUPRADIOBUTTON) bn_DrawPopupRadioButton(tk_display,buttonid); 
   else if(button->type == BN_SCROLLBUTTON) bn_DrawScrollButton(tk_display,buttonid);
   else if(button->type == BN_THUMBBUTTON) bn_DrawThumbButton(tk_display,buttonid); 
   else if(button->type == BN_CHECKBUTTON) bn_DrawCheckButton(tk_display,buttonid);
   else if(button->type == BN_COMBOBUTTON)   bn_DrawComboButton(tk_display,buttonid);
 }
 return button->state;
}



int BN_Ungray(tk_display,buttonid)
TkDisplay *tk_display;
ButtonID buttonid;
{
 ButtonStruct *button;


 if(buttonid>=0 && buttonid<tk_display->maxwidgets && tk_display->widgets[buttonid].class==WI_BUTTON&&tk_display->widgets[buttonid].button!=NULL)
   button=tk_display->widgets[buttonid].button;
 else return -1;

 if ((button->state&Grayed)==Grayed){
   button->state=button->state-Grayed;
   if (button->type ==BN_PUSHBUTTON) bn_DrawPushButton(tk_display,buttonid);
   else if(button->type == BN_REPEATBUTTON) bn_DrawRepeatButton(tk_display,buttonid);
   else if(button->type == BN_POPUPBUTTON)  bn_DrawPopupButton(tk_display,buttonid);
   else if(button->type == BN_CROSSBUTTON) bn_DrawCrossButton(tk_display,buttonid);
   else if(button->type == BN_RADIOBUTTON) bn_DrawRadioButton(tk_display,buttonid);
   else if(button->type == BN_POPUPRADIOBUTTON) bn_DrawPopupRadioButton(tk_display,buttonid); 
   else if(button->type == BN_SCROLLBUTTON) bn_DrawScrollButton(tk_display,buttonid);
   else if(button->type == BN_THUMBBUTTON) bn_DrawThumbButton(tk_display,buttonid); 
   else if(button->type == BN_CHECKBUTTON) bn_DrawCheckButton(tk_display,buttonid);
   else if(button->type == BN_COMBOBUTTON)   bn_DrawComboButton(tk_display,buttonid);
 }
 return button->state;
}



int BN_SetLighting(tk_display,buttonid,lighting,delay)
TkDisplay *tk_display;
ButtonID buttonid;
int lighting;
Bool delay;
{
 ButtonStruct *button;


 if(buttonid>=0 && buttonid<tk_display->maxwidgets && tk_display->widgets[buttonid].class==WI_BUTTON&&tk_display->widgets[buttonid].button!=NULL)
   button=tk_display->widgets[buttonid].button;
 else return -1;

 button->lighting=lighting;
 if(delay==NO)
 {
  if (button->type ==BN_PUSHBUTTON) bn_DrawPushButton(tk_display,buttonid);
  else if(button->type == BN_REPEATBUTTON) bn_DrawRepeatButton(tk_display,buttonid);
  else if(button->type == BN_POPUPBUTTON)  bn_DrawPopupButton(tk_display,buttonid);
  else if(button->type == BN_CROSSBUTTON) bn_DrawCrossButton(tk_display,buttonid);
  else if(button->type == BN_RADIOBUTTON) bn_DrawRadioButton(tk_display,buttonid);
  else if(button->type == BN_POPUPRADIOBUTTON) bn_DrawPopupRadioButton(tk_display,buttonid); 
  else if(button->type == BN_SCROLLBUTTON) bn_DrawScrollButton(tk_display,buttonid);
  else if(button->type == BN_THUMBBUTTON) bn_DrawThumbButton(tk_display,buttonid); 
  else if(button->type == BN_CHECKBUTTON) bn_DrawCheckButton(tk_display,buttonid); 
  else if(button->type == BN_COMBOBUTTON)   bn_DrawComboButton(tk_display,buttonid);
 }
 return button->lighting;
}





int BN_SetText(tk_display,buttonid,text,key,txt_gravity,txt_x,txt_y,delay)
TkDisplay *tk_display;
ButtonID buttonid;
char *text;
unsigned char  key;
int txt_gravity, txt_x, txt_y;
Bool delay;
{
 ButtonStruct *button;


 if(buttonid>=0 && buttonid<tk_display->maxwidgets && tk_display->widgets[buttonid].class==WI_BUTTON&&tk_display->widgets[buttonid].button!=NULL)
   button=tk_display->widgets[buttonid].button;
 else return -1;

 button->text=text;
 button->txt_key=key;
 button->txt_gravity=txt_gravity;
 button->txt_x=txt_x;
 button->txt_y=txt_y;
 if((button->flags&TextFlag)!=TextFlag) button->flags=button->flags+TextFlag;

 if(delay==NO) {
  if (button->type ==BN_PUSHBUTTON) bn_DrawPushButton(tk_display, buttonid);
  else if(button->type == BN_REPEATBUTTON) bn_DrawRepeatButton(tk_display, buttonid);
  else if(button->type == BN_POPUPBUTTON)  bn_DrawPopupButton(tk_display,buttonid);
  else if(button->type == BN_CROSSBUTTON) bn_DrawCrossButton(tk_display, buttonid);
  else if(button->type == BN_RADIOBUTTON) bn_DrawRadioButton(tk_display, buttonid);
  else if(button->type == BN_POPUPRADIOBUTTON) bn_DrawPopupRadioButton(tk_display,buttonid); 
  else if(button->type == BN_SCROLLBUTTON) bn_DrawScrollButton(tk_display, buttonid);
  else if(button->type == BN_THUMBBUTTON) bn_DrawThumbButton(tk_display, buttonid);
  else if(button->type == BN_CHECKBUTTON) bn_DrawCheckButton(tk_display, buttonid);
  else if(button->type == BN_COMBOBUTTON)   bn_DrawComboButton(tk_display,buttonid);
 }
 return 0;
}



int BN_SetPixmap(tk_display,buttonid,pixmap,pix_mask,pix_gravity,pix_x,pix_y,pix_width,pix_height,pix_depth,delay)
TkDisplay *tk_display;
ButtonID buttonid;
Pixmap pixmap, pix_mask;
int pix_gravity, pix_x, pix_y, pix_width, pix_height, pix_depth;
Bool delay;
{
 ButtonStruct *button;


 if(buttonid>=0 && buttonid<tk_display->maxwidgets && tk_display->widgets[buttonid].class==WI_BUTTON&&tk_display->widgets[buttonid].button!=NULL)
   button=tk_display->widgets[buttonid].button;
 else return -1;


 button->pixmap=pixmap;
 if(pix_mask>0) button->pix_mask=pix_mask;
 button->pix_gravity=pix_gravity;
 button->pix_x=pix_x;
 button->pix_y=pix_y;
 button->pix_width=pix_width;
 button->pix_height=pix_height;
 button->pix_depth=pix_depth;
 if((button->flags&PixmapFlag)!=PixmapFlag) button->flags=button->flags+PixmapFlag;
 if(pix_mask!=0 && (button->flags&PixmapMaskFlag)!=PixmapMaskFlag) button->flags=button->flags+PixmapMaskFlag;

 if(delay==NO) {
  if (button->type ==BN_PUSHBUTTON) bn_DrawPushButton(tk_display, buttonid);
  else if(button->type == BN_REPEATBUTTON) bn_DrawRepeatButton(tk_display,buttonid);
  else if(button->type == BN_POPUPBUTTON)  bn_DrawPopupButton(tk_display,buttonid);
  else if(button->type == BN_CROSSBUTTON) bn_DrawCrossButton(tk_display,buttonid);
  else if(button->type == BN_RADIOBUTTON) bn_DrawRadioButton(tk_display,buttonid);
  else if(button->type == BN_POPUPRADIOBUTTON) bn_DrawPopupRadioButton(tk_display,buttonid); 
  else if(button->type == BN_SCROLLBUTTON) bn_DrawScrollButton(tk_display,buttonid);
  else if(button->type == BN_THUMBBUTTON) bn_DrawThumbButton(tk_display,buttonid);
  else if(button->type == BN_CHECKBUTTON) bn_DrawCheckButton(tk_display,buttonid);
  else if(button->type == BN_COMBOBUTTON)   bn_DrawComboButton(tk_display,buttonid);
 }
 return 0;
}




int BN_SetPixmapGrayed(tk_display,buttonid,pixmap_grayed,pixgrayed_mask,pixgrayed_gravity,pixgrayed_x,pixgrayed_y,pixgrayed_width,pixgrayed_height,pixgrayed_depth,delay)
TkDisplay *tk_display;
ButtonID buttonid;
Pixmap pixmap_grayed, pixgrayed_mask;
int pixgrayed_gravity, pixgrayed_x, pixgrayed_y, pixgrayed_width, pixgrayed_height, pixgrayed_depth;
Bool delay;
{
 ButtonStruct *button;


 if(buttonid>=0 && buttonid<tk_display->maxwidgets && tk_display->widgets[buttonid].class==WI_BUTTON&&tk_display->widgets[buttonid].button!=NULL)
   button=tk_display->widgets[buttonid].button;
 else return -1;


 button->pixmap_grayed=pixmap_grayed;
 if(pixgrayed_mask!=0) button->pixgrayed_mask=pixgrayed_mask;

 if((button->flags&PixmapGrayedFlag)!=PixmapGrayedFlag) button->flags=button->flags+PixmapGrayedFlag;
 if(pixgrayed_mask!=0 && (button->flags&PixmapMaskGrayedFlag)!=PixmapMaskGrayedFlag) button->flags=button->flags+PixmapMaskGrayedFlag;

 button->pixgrayed_gravity=pixgrayed_gravity;
 button->pixgrayed_x=pixgrayed_x;
 button->pixgrayed_y=pixgrayed_y;
 button->pixgrayed_width=pixgrayed_width;
 button->pixgrayed_height=pixgrayed_height;
 button->pixgrayed_depth=pixgrayed_depth;


 if(delay==NO) {
  if (button->type ==BN_PUSHBUTTON) bn_DrawPushButton(tk_display,buttonid);
  else if(button->type == BN_REPEATBUTTON) bn_DrawRepeatButton(tk_display,buttonid);
  else if(button->type == BN_POPUPBUTTON)  bn_DrawPopupButton(tk_display,buttonid);
  else if(button->type == BN_CROSSBUTTON) bn_DrawCrossButton(tk_display,buttonid);
  else if(button->type == BN_RADIOBUTTON) bn_DrawRadioButton(tk_display,buttonid);
  else if(button->type == BN_POPUPRADIOBUTTON) bn_DrawPopupRadioButton(tk_display,buttonid); 
  else if(button->type == BN_SCROLLBUTTON) bn_DrawScrollButton(tk_display,buttonid);
  else if(button->type == BN_THUMBBUTTON) bn_DrawThumbButton(tk_display,buttonid);
  else if(button->type == BN_CHECKBUTTON) bn_DrawCheckButton(tk_display,buttonid);
  else if(button->type == BN_COMBOBUTTON)   bn_DrawComboButton(tk_display,buttonid);
 }
 return 0;
}




int BN_DeleteText(tk_display,buttonid,delay)
TkDisplay *tk_display;
ButtonID buttonid;
Bool delay;
{
 ButtonStruct *button;


 if(buttonid>=0 && buttonid<tk_display->maxwidgets && tk_display->widgets[buttonid].class==WI_BUTTON&&tk_display->widgets[buttonid].button!=NULL)
   button=tk_display->widgets[buttonid].button;
 else return -1;


 button->text="";
 button->txt_key=0;

 if((button->flags&TextFlag)==TextFlag) button->flags=button->flags-TextFlag;

 if(delay==NO) {
  if (button->type ==BN_PUSHBUTTON) bn_DrawPushButton(tk_display,buttonid);
  else if(button->type == BN_REPEATBUTTON) bn_DrawRepeatButton(tk_display,buttonid);
  else if(button->type == BN_POPUPBUTTON)  bn_DrawPopupButton(tk_display,buttonid);
  else if(button->type == BN_CROSSBUTTON) bn_DrawCrossButton(tk_display,buttonid);
  else if(button->type == BN_RADIOBUTTON) bn_DrawRadioButton(tk_display,buttonid);
  else if(button->type == BN_POPUPRADIOBUTTON) bn_DrawPopupRadioButton(tk_display,buttonid); 
  else if(button->type == BN_SCROLLBUTTON) bn_DrawScrollButton(tk_display,buttonid);
  else if(button->type == BN_THUMBBUTTON) bn_DrawThumbButton(tk_display,buttonid);
  else if(button->type == BN_CHECKBUTTON) bn_DrawCheckButton(tk_display,buttonid);
  else if(button->type == BN_COMBOBUTTON)   bn_DrawComboButton(tk_display,buttonid);
 }
 return 0;
}



int BN_DeletePixmap(tk_display,buttonid,delay)
TkDisplay *tk_display;
ButtonID buttonid;
Bool delay;
{
 ButtonStruct *button;


 if(buttonid>=0 && buttonid<tk_display->maxwidgets && tk_display->widgets[buttonid].class==WI_BUTTON&&tk_display->widgets[buttonid].button!=NULL)
   button=tk_display->widgets[buttonid].button;
 else return -1;


 button->pixmap=0;
 button->pix_mask=0;

 if((button->flags&PixmapFlag)==PixmapFlag) button->flags=button->flags-PixmapFlag;
 if((button->flags&PixmapMaskFlag)!=PixmapMaskFlag) button->flags=button->flags-PixmapMaskFlag;

 if(delay==NO) {
  if (button->type ==BN_PUSHBUTTON) bn_DrawPushButton(tk_display,buttonid);
  else if(button->type == BN_REPEATBUTTON) bn_DrawRepeatButton(tk_display,buttonid);
  else if(button->type == BN_POPUPBUTTON)  bn_DrawPopupButton(tk_display,buttonid);
  else if(button->type == BN_CROSSBUTTON) bn_DrawCrossButton(tk_display,buttonid);
  else if(button->type == BN_RADIOBUTTON) bn_DrawRadioButton(tk_display,buttonid);
  else if(button->type == BN_POPUPRADIOBUTTON) bn_DrawPopupRadioButton(tk_display,buttonid); 
  else if(button->type == BN_SCROLLBUTTON) bn_DrawScrollButton(tk_display,buttonid);
  else if(button->type == BN_THUMBBUTTON) bn_DrawThumbButton(tk_display,buttonid);
  else if(button->type == BN_CHECKBUTTON) bn_DrawCheckButton(tk_display,buttonid);
  else if(button->type == BN_COMBOBUTTON)   bn_DrawComboButton(tk_display,buttonid);
 }
 return 0;
}



int BN_DeletePixmapGrayed(tk_display,buttonid,delay)
TkDisplay *tk_display;
ButtonID buttonid;
Bool delay;
{
 ButtonStruct *button;


 if(buttonid>=0 && buttonid<tk_display->maxwidgets && tk_display->widgets[buttonid].class==WI_BUTTON&&tk_display->widgets[buttonid].button!=NULL)
   button=tk_display->widgets[buttonid].button;
 else return -1;


 button->pixmap_grayed=0;
 button->pixgrayed_mask=0;

 if((button->flags&PixmapGrayedFlag)==PixmapGrayedFlag) button->flags=button->flags-PixmapGrayedFlag;
 if((button->flags&PixmapMaskGrayedFlag)!=PixmapMaskGrayedFlag) button->flags=button->flags-PixmapMaskGrayedFlag;

 if(delay==NO) {
  if (button->type ==BN_PUSHBUTTON) bn_DrawPushButton(tk_display,buttonid);
  else if(button->type == BN_REPEATBUTTON) bn_DrawRepeatButton(tk_display,buttonid);
  else if(button->type == BN_POPUPBUTTON)  bn_DrawPopupButton(tk_display,buttonid);
  else if(button->type == BN_CROSSBUTTON) bn_DrawCrossButton(tk_display,buttonid);
  else if(button->type == BN_RADIOBUTTON) bn_DrawRadioButton(tk_display,buttonid);
  else if(button->type == BN_POPUPRADIOBUTTON) bn_DrawPopupRadioButton(tk_display,buttonid); 
  else if(button->type == BN_SCROLLBUTTON) bn_DrawScrollButton(tk_display,buttonid);
  else if(button->type == BN_THUMBBUTTON) bn_DrawThumbButton(tk_display,buttonid);
  else if(button->type == BN_CHECKBUTTON) bn_DrawCheckButton(tk_display,buttonid);
  else if(button->type == BN_COMBOBUTTON)   bn_DrawComboButton(tk_display,buttonid);
 }
 return 0;
}




int BN_SetFont(tk_display,buttonid,xfontstruct,delay)
TkDisplay *tk_display;
ButtonID buttonid;
XFontStruct *xfontstruct;
Bool delay;
{
 ButtonStruct *button;


 if(buttonid>=0 && buttonid<tk_display->maxwidgets && tk_display->widgets[buttonid].class==WI_BUTTON&&tk_display->widgets[buttonid].button!=NULL)
   button=tk_display->widgets[buttonid].button;
 else return -1;

 button->txt_font=xfontstruct;
 if(button->type==BN_CROSSBUTTON || button->type==BN_RADIOBUTTON) button->txt_y = button->txt_font->ascent+3;

 if(delay==NO) {
  if (button->type ==BN_PUSHBUTTON) bn_DrawPushButton(tk_display,buttonid);
  else if(button->type == BN_REPEATBUTTON) bn_DrawRepeatButton(tk_display,buttonid);
  else if(button->type == BN_POPUPBUTTON)  bn_DrawPopupButton(tk_display,buttonid);
  else if(button->type == BN_CROSSBUTTON) bn_DrawCrossButton(tk_display,buttonid);
  else if(button->type == BN_RADIOBUTTON) bn_DrawRadioButton(tk_display,buttonid);
  else if(button->type == BN_POPUPRADIOBUTTON) bn_DrawPopupRadioButton(tk_display,buttonid); 
  else if(button->type == BN_SCROLLBUTTON) bn_DrawScrollButton(tk_display,buttonid);
  else if(button->type == BN_THUMBBUTTON) bn_DrawThumbButton(tk_display,buttonid);
  else if(button->type == BN_CHECKBUTTON) bn_DrawCheckButton(tk_display,buttonid);
  else if(button->type == BN_COMBOBUTTON)   bn_DrawComboButton(tk_display,buttonid);
 }
 return 0;
}




int BN_Configure(tk_display,buttonid,x,y,width,height)
TkDisplay *tk_display;
ButtonID buttonid;
int x, y;
unsigned int width, height;
{
 int mask;
 XWindowChanges xwc;
 ButtonStruct *button;


 if(buttonid>=0 && buttonid<tk_display->maxwidgets && tk_display->widgets[buttonid].class==WI_BUTTON&&tk_display->widgets[buttonid].button!=NULL)
   button=tk_display->widgets[buttonid].button;
 else return -1;



 xwc.x=button->x=x;
 xwc.y=button->y=y;
 xwc.width=width;
 xwc.height=height;

 mask=CWX|CWY|CWWidth|CWHeight;
 XConfigureWindow(tk_display->display,button->window,mask,&xwc);

 if(width<button->width||height<button->height)
 {
   button->width=width;
   button->height=height;
   if (button->type ==BN_PUSHBUTTON) bn_DrawPushButton(tk_display,buttonid);
   else if(button->type == BN_REPEATBUTTON) bn_DrawRepeatButton(tk_display,buttonid);
   else if(button->type == BN_POPUPBUTTON)  bn_DrawPopupButton(tk_display,buttonid);
   else if(button->type == BN_CROSSBUTTON) bn_DrawCrossButton(tk_display,buttonid);
   else if(button->type == BN_RADIOBUTTON) bn_DrawRadioButton(tk_display,buttonid);
   else if(button->type == BN_POPUPRADIOBUTTON) bn_DrawPopupRadioButton(tk_display,buttonid); 
   else if(button->type == BN_SCROLLBUTTON) bn_DrawScrollButton(tk_display,buttonid);
   else if(button->type == BN_THUMBBUTTON) bn_DrawThumbButton(tk_display,buttonid);
   else if(button->type == BN_CHECKBUTTON) bn_DrawCheckButton(tk_display,buttonid); 
   else if(button->type == BN_COMBOBUTTON)   bn_DrawComboButton(tk_display,buttonid);
   return 0;
 }

 button->width=width;
 button->height=height;
 return 0;
}




int BN_Resize(tk_display,buttonid,width,height)
TkDisplay *tk_display;
ButtonID buttonid;
unsigned int width, height;
{
 ButtonStruct *button;


 if(buttonid>=0 && buttonid<tk_display->maxwidgets && tk_display->widgets[buttonid].class==WI_BUTTON&&tk_display->widgets[buttonid].button!=NULL)
   button=tk_display->widgets[buttonid].button;
 else return -1;


 XResizeWindow(tk_display->display,button->window,width,height);
 if(width>button->width||height>button->height)
 {
   button->width=width;
   button->height=height;
   if (button->type ==BN_PUSHBUTTON) bn_DrawPushButton(tk_display,buttonid);
   else if(button->type == BN_REPEATBUTTON) bn_DrawRepeatButton(tk_display,buttonid);
   else if(button->type == BN_POPUPBUTTON)  bn_DrawPopupButton(tk_display,buttonid);
   else if(button->type == BN_CROSSBUTTON) bn_DrawCrossButton(tk_display,buttonid);
   else if(button->type == BN_RADIOBUTTON) bn_DrawRadioButton(tk_display,buttonid);
   else if(button->type == BN_POPUPRADIOBUTTON) bn_DrawPopupRadioButton(tk_display,buttonid); 
   else if(button->type == BN_SCROLLBUTTON) bn_DrawScrollButton(tk_display,buttonid);
   else if(button->type == BN_THUMBBUTTON) bn_DrawThumbButton(tk_display,buttonid);
   else if(button->type == BN_CHECKBUTTON) bn_DrawCheckButton(tk_display,buttonid); 
   else if(button->type == BN_COMBOBUTTON)   bn_DrawComboButton(tk_display,buttonid);
 }
 return 0;
}





int BN_Move(tk_display,buttonid,x,y)
TkDisplay *tk_display;
ButtonID buttonid;
int x, y;
{
 ButtonStruct *button;


 if(buttonid>=0 && buttonid<tk_display->maxwidgets && tk_display->widgets[buttonid].class==WI_BUTTON&&tk_display->widgets[buttonid].button!=NULL)
   button=tk_display->widgets[buttonid].button;
 else return -1;

 button->x=x;
 button->y=y;

 XMoveWindow(tk_display->display,button->window,x,y);
 return 0;
}






int BN_GiveFocus(tk_display,buttonid)
TkDisplay *tk_display;
ButtonID buttonid;
{
 XWindowAttributes attrib;
 ButtonStruct *button;


 if(buttonid>=0 && buttonid<tk_display->maxwidgets && tk_display->widgets[buttonid].class==WI_BUTTON&&tk_display->widgets[buttonid].button!=NULL)
   button=tk_display->widgets[buttonid].button;
 else return -1;


 attrib.map_state=IsUnviewable;
 XGetWindowAttributes(tk_display->display,button->window,&attrib);			  

 if((button->state&Grayed)!=Grayed && button->neverFocus==False&&attrib.map_state==IsViewable)
 {
   XSetInputFocus(tk_display->display,button->window,RevertToPointerRoot,CurrentTime); 
   return 0;
 }
 else return -1;
}





int BN_SetNeverfocusFlag(tk_display,buttonid,flag)
TkDisplay *tk_display;
ButtonID buttonid;
Bool flag;
{
 ButtonStruct *button;


 if(buttonid>=0 && buttonid<tk_display->maxwidgets && tk_display->widgets[buttonid].class==WI_BUTTON&&tk_display->widgets[buttonid].button!=NULL)
   button=tk_display->widgets[buttonid].button;
 else return -1;


 button->neverFocus=flag;
 if(button->hasFocus==True) XSetInputFocus(tk_display->display,RootWindow(tk_display->display,tk_display->screen),RevertToPointerRoot,CurrentTime);
 return 0;
}





int BN_SetCrossType(tk_display,buttonid,type,delay)
TkDisplay *tk_display;
ButtonID buttonid;
int type;
Bool delay;
{
 ButtonStruct *button;


 if(buttonid>=0 && buttonid<tk_display->maxwidgets && tk_display->widgets[buttonid].class==WI_BUTTON&&tk_display->widgets[buttonid].button!=NULL)
   button=tk_display->widgets[buttonid].button;
 else return -1;


 if(button->type==BN_CROSSBUTTON)
 {
   if(type>=0 && type<=3)
     button->crosstype=type;
   if(delay==False) bn_DrawCrossButton(tk_display,buttonid);
   return 0;
 }
 else return -1;
}


