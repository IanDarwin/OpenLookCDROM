//  1994 by M. Roth
#include "view.h"
#include "main.h"
#include "szene.h"
#include <unistd.h>
#include <X11/Xlib.h>
#include <X11/Xos.h>
#include <X11/Xatom.h>
#include <X11/keysym.h>
#include <X11/Xutil.h>
#include <stdio.h>
#include <iostream.h>
#include <xview/xview.h>
#include <xview/frame.h>
#include <xview/panel.h>
#include <xview/file_chsr.h>
#include <xview/canvas.h>
#include <xview/xv_xrect.h>
int Load(char *);

Display             *display=NULL;
int                  screen;
XEvent               report;
Colormap             defaultColormap;
Frame                frame;
Canvas               canvas;
struct itimerval     timer;
Panel_item           selectItem; 
Window               window;
Pixmap               drawbuffer;
GC                   gc;
GC                   shadowgc;

char                *progname;
int                  lastColor=-1;
int                  animFlag=0;
int                  solidFlag=0;
int                  shadowFlag=0;
char                 pfad[255];

Matrix rotZ;
Matrix rotZinv;
Matrix rotX;
Vektor pos;
Matrix ml,mr;
Matrix mu,md;
Vektor vh;
Vektor vv;

// Farbtabelle zur Umsetzung in X-Colors
unsigned long plane_masks_return[1];
unsigned long pixels_return[256];
 
int height = 400;
int width = 640;

/* Tastatureingabe verarbeiten */
void tastaturEvent(Xv_Window window, Event *event, Notify_arg arg)
{
  char buffer[5];
  KeySym key=0;
  XComposeStatus compose;

  switch  (event->ie_xevent->type)
    {
    case KeyPress:
      XLookupString((XKeyEvent *)(event->ie_xevent),buffer,5,&key,&compose);
      break;
    default:
      return;
    } 
  if (XPending(display)) return;
  switch(key)
    {
    case XK_Left:
      rotZ=rotZ*mr;
      rotZinv=rotZinv*ml;
      break;
    case XK_Right:
      rotZ=rotZ*ml;
      rotZinv=rotZinv*mr;
      break;
    case XK_Prior:
      rotX=rotX*mu;
      break;
    case XK_Next:
      rotX=rotX*md;
      break;
    case XK_Up:
      pos=pos+rotZinv*(vh*-1);
      break;
    case XK_Down:
      pos=pos+rotZinv*vh;
      break;
    case '+':
      pos=pos+vv*-1;
      break;
    case '-':
      pos=pos+vv;
      break;
    }
  if(key)
  {
    SzeneSetPos(rotX*rotZ*pos);
    SzeneSetRot(rotX*rotZ);
    if (!(animFlag))
      SzeneErstellen();
  }
}

// Timerinterrupt 
Notify_value timerNotifyProc(Notify_client client,int wich)
{
  SzeneErstellen();
}

/* Alle Parameter setzen */
void setSelect()
{
  int mode=0;

  if (animFlag)
  {
    /* timer setzen */
    timer.it_value.tv_usec = 100000;
    timer.it_interval.tv_usec = 100000;
    notify_set_itimer_func(frame,
			   (Notify_func)timerNotifyProc,
			   ITIMER_REAL,
			   &timer,
			   NULL);
  }
  else
  {
    notify_set_itimer_func(frame,
			   NOTIFY_FUNC_NULL,
			   ITIMER_REAL,
			   NULL,
			   NULL);
  }
  if (solidFlag) 
    mode |= SOLID;
  else
    mode |= WIRE;
  if (animFlag)
    mode |= ANIMATE;
  if (shadowFlag)
    mode |= SHADOW;
  SzeneDrawMode( mode );
  xv_set(selectItem,PANEL_VALUE, animFlag+(solidFlag<<1)+(shadowFlag<<2),NULL);
}

/*---------------------------------------------------------------------------*
  Szene in Screen einpassen
 *---------------------------------------------------------------------------*/
void CenterSzene()
{
  float maxX,minX,maxY,minY,maxZ,minZ;
  float max;

  rotZ=Matrix(0,0,0);
  rotZinv=Matrix(0,0,0);
  rotX=Matrix(0,0,0);
  ml=Matrix(0,0,3);mr=Matrix(0,0,-3);
  mu=Matrix(3,0,0);md=Matrix(-3,0,0);
  vh=Vektor(0,10,0);
  vv=Vektor(0,0,10);
  pos=Vektor(0,0,0);
  SzeneGetMinMax(minX,maxX,minY,maxY,minZ,maxZ);
  max=maxX;
  if( (maxZ) > max) max=maxZ;
  if( (-minX) > max) max=-minX;
  if( (-minZ) > max) max=-minZ;
  max=max/300*800-minY;
  pos=pos+(Vektor(0,1,0)*max);
  pos=pos+Vektor(0,0,1)*(-maxZ/2);
  SzeneSetPos(rotX*rotZ*pos);
  SzeneSetRot(rotX*rotZ);
  SzeneResetTime();
  setSelect();
}

/*---------------------------------------------------------------------------*
 Grafik context erzeugen 
 *---------------------------------------------------------------------------*/
void createGC()
{
  unsigned long valuemask = 0; /* ignore XGCvalues and use defaults */
  XGCValues values;

  /* Create default Graphics Context */
  /* xlib 1/118 */
  gc = XCreateGC(display, window, valuemask, &values);
  shadowgc = XCreateGC(display, window, valuemask, &values);
  XSetPlaneMask(display, shadowgc,1+2+4+8);
  XSetFunction(display, shadowgc,GXclear);
}

// Anzeige- und Zeichen-Screen tauschen
void X_SwopScreen()
{
  XCopyArea(display, drawbuffer,window, gc, 0, 0, width,height,0,0);
}

// Loeschen des Aktuellen Bildspeichers
void X_ClearScreen()
{
  lastColor=0;
  XSetForeground(display, gc, BlackPixel(display,screen));
  XFillRectangle(display,drawbuffer,gc,0,0,width,height);
}

int loadFileCB(File_chooser fc,char *path,char *file,Xv_opaque client_data)
{
  int c;
  char oldpath[256] = "/";

  SzeneEnd();
  SzeneInit();
  strcpy(oldpath,path);
  c=0;  
  while (oldpath[c]) c++;
  while (oldpath[c] != '/') c--;
  oldpath[c+1]=0;
  chdir(oldpath);
  strcpy(pfad,path);
  Load(path);
  if (SzeneGetDrawMode() & ANIMATE) animFlag=1; else animFlag=0;
  if (SzeneGetDrawMode() & SOLID) solidFlag=1; else solidFlag=0;
  CenterSzene();
  SzeneErstellen();
  return XV_OK;
}

// Anzeigen des File-Requests
void loadNotifyProc()
{
  File_chooser open_chsr;

  open_chsr = (File_chooser)xv_create(frame,FILE_CHOOSER_OPEN_DIALOG,
				      XV_LABEL,"Load 3D-file",
				      FILE_CHOOSER_NOTIFY_FUNC,loadFileCB,
				      NULL);
  
  xv_set(open_chsr,XV_SHOW,TRUE,NULL);
}

// Aktuelles file neu laden
void reloadFile()
{
  SzeneEnd();
  SzeneInit();
  Load(pfad);
  if (SzeneGetDrawMode() & ANIMATE) animFlag=1; else animFlag=0;
  if (SzeneGetDrawMode() & SOLID) solidFlag=1; else solidFlag=0;
  CenterSzene();
  SzeneErstellen();
}

// Zeichenflaeche neu zeichnen
void canvasRePaint(Canvas canvas,
		 Xv_Window xv_window,
		 Display *disp, 
		 Window win,
		 Xv_xrectlist *xrects)
{
  static int x=0;
  if (x==0)
  {
    display = disp;
    window = win;
    /* Create pixmap of depth 8 (bitmap) for icon */
    drawbuffer = XCreatePixmap(display, 
			       window,
			       width, 
			       height,
			       8);
    /* Tastatur-Event einrichten */
    xv_set(xv_window,
	   WIN_EVENT_PROC,tastaturEvent,
	   WIN_CONSUME_X_EVENT_MASK,KeyPressMask,
	   NULL);
    createGC();
    x++;
  }  
  SzeneErstellen();
}

// Select interrupt 
void selectNotifyProc(Panel_item item,int value,Event *event)
{
  animFlag=0;
  solidFlag=0;
  shadowFlag=0;
  if (value & 1) animFlag=1;
  if (value & 2) solidFlag=1;
  if (value & 4) shadowFlag=1;
  setSelect();
  SzeneErstellen();
}

// oeffnen eines Screens mit 640*400 Punkten und 256 Farben
void X_OpenScreen()
{
  Panel panel;
  
  if (SzeneGetDrawMode() & ANIMATE) animFlag=1;
  if (SzeneGetDrawMode() & SOLID) solidFlag=1;
//  xv_init(XV_INIT_ARGC_PTR_ARGV, 0, 0, NULL);
  xv_init(0);
  /* Create base frame */
  frame = (Frame)xv_create(0, FRAME,
			   FRAME_LABEL, "Virtual 1.0  1993 by Marcus Roth",
			   XV_WIDTH,    640,
			   XV_HEIGHT,   430,
			   XV_Y,0,
			   XV_X,0,
			   NULL);
  canvas = (Canvas)xv_create(frame,CANVAS,
			     CANVAS_REPAINT_PROC,    canvasRePaint,
			     CANVAS_X_PAINT_WINDOW,  TRUE,
			     CANVAS_AUTO_SHRINK,FALSE,
			     CANVAS_AUTO_EXPAND,FALSE,
			     CANVAS_WIDTH,640,
			     CANVAS_HEIGHT,400,
			     XV_WIDTH,    640,
			     XV_HEIGHT,   400,
			     NULL);
  /* Install a panel and a panel button */
  panel = (Panel)xv_create(frame, PANEL,
			   XV_Y,400,
			   XV_X,0,
			   NULL);
  selectItem = (Panel_item) xv_create(panel, PANEL_CHECK_BOX,
		   PANEL_LAYOUT,         PANEL_HORIZONTAL,
		   PANEL_CHOICE_STRINGS, "animate","solid","shadow",NULL,
		   PANEL_VALUE, animFlag+(solidFlag<<1)+(shadowFlag<<2),
		   PANEL_NOTIFY_PROC,  selectNotifyProc,
		   NULL);
  (void) xv_create(panel, PANEL_BUTTON,
		   PANEL_LABEL_STRING, "Reload file",
		   PANEL_NOTIFY_PROC,  reloadFile,
		   NULL);
  (void) xv_create(panel, PANEL_BUTTON,
		   PANEL_LABEL_STRING, "Load file ...",
		   PANEL_NOTIFY_PROC,  loadNotifyProc,
		   NULL);
  /* Install a panel and a panel button */
  (void) xv_create(panel, PANEL_BUTTON,
		   PANEL_LABEL_STRING, "Quit",
		   PANEL_NOTIFY_PROC,  End,
		   NULL);
  /* connect to X server */
  if ( (display=XOpenDisplay(NULL)) == NULL )
    {
      fprintf( stderr, "%s cannot connect to X server %s\n",
	      progname,XDisplayName(NULL));
      Error("Programmabbruch");
    }
  /* get screen size from display structure macro */
  screen = DefaultScreen(display);
  // Default Farbtabelle
  defaultColormap = DefaultColormap(display,screen);
  XAllocColorCells(display,
		   defaultColormap,
		   True,
		   plane_masks_return,
		   0,
		   pixels_return,
		   112);
  XAllocColorCells(display,
		   defaultColormap,
		   True,
		   plane_masks_return,
		   0,
		   pixels_return,
		   112);
}

// Displaygeometry
float X_Geometry()
{
  float w,h,wm,hm;

  w=DisplayWidth(display,screen);
  h=DisplayHeight(display,screen);
  wm=DisplayWidthMM(display,screen);
  hm=DisplayHeightMM(display,screen);
  return -(w/wm) / (h/hm);
}

// Screen vor aufruf des Programms wieder herstellen
void X_CloseScreen()
{
  XFreeGC(display, gc);
  XFreeGC(display, shadowgc);
  XCloseDisplay(display);
}

// Punkt setzen
void X_Point(int x,int y, int f)
{
}

// Punkt schattieren
void X_ShadowPoint(int x,int y)
{
}

void X_Line(int x1,int y1,int x2, int y2, int farbe)
{
  farbe+=16;
  /* Zeichenfarbe setzen */
  if (lastColor!=farbe)
    {
      XSetForeground(display, gc,farbe);
      lastColor=farbe;
    }
  XDrawLine(display,drawbuffer,gc,x1,y1,x2,y2);
}

// Shattierte Zeile zeichnen
void X_ShadeLine(int y,int laenge,int xv,int hv,int hp,int farbe)
{
  farbe = (farbe << 4) + (hv>>8);
  farbe+=16;

  /* Zeichenfarbe setzen */
  if (lastColor!=farbe)
    {
      XSetForeground(display, gc,farbe);
      lastColor=farbe;
    }
  XDrawLine(display,drawbuffer,gc,xv,y,xv+laenge,y);
}

// Zeile beschatten
void X_ShadowLine(int y,int xv,int laenge)
{
  int nr;

  XDrawLine(display,drawbuffer,shadowgc,xv,y,xv+laenge,y);
  /* Zeichenfarbe setzen */
/*  do
  {
    if (lastColor!=nr)
    {
      XSetForeground(display, gc,colorTab[8*16]);
      lastColor=nr;
    }
    XDrawPoint(display,drawbuffer,gc,xv,y);
    xv++;
  } while(laenge--);*/
}

// Farbe anfordern
void X_AllocColor(int nr,int r,int g,int b)
{
  XColor color;
  static XColor cshadow;
  static int last=0;
  
  nr+=16;
  color.red = r<<10;
  color.green = g<<10;
  color.blue = b<<10;
  color.flags = DoRed | DoGreen | DoBlue;
  color.pixel = nr;
//  XAllocColor(display, defaultColormap, &color);
  XStoreColor(display, defaultColormap, &color);
  if((nr & 15) == 0)
    cshadow=color;
}

// X Funktionen setzen
void ViewModeX()
{
  ViewSet( &X_OpenScreen,
	   &X_CloseScreen,
	   &X_SwopScreen,
	   &X_ClearScreen,
	   &X_Line,
	   &X_Point,
	   &X_ShadeLine,
	   &X_ShadowLine,
	   639,
	   399);
}

// Taste von der Tastatur lesen
int GetKey(int wait)
{
  char buffer[5];
  KeySym key=0;
  XComposeStatus compose;

  /* X-Events auswerten */
  while (XPending(display) || wait)  {
    wait=0;
    XNextEvent(display, &report);
    switch  (report.type) {
    case Expose:
      /* Beim letzten Expose wird der Bildschirm neu gezeichnet */
      if (report.xexpose.count != 0)
	break;
      /* windowrefresh */
      SzeneErstellen();
      break;
    case ConfigureNotify:
      /* Fenstergroesse wurde geaendert */
      return 0;
      break;
    case ButtonPress:
      Error("");
      break;
    case KeyPress:
       XLookupString((XKeyEvent *)(&report),buffer,5,&key,&compose);
       break;
    default:
      /* all events selected by StructureNotifyMask
       * except ConfigureNotify are thrown away here,
       * since nothing is done with them */
      break;
    } 
  } 
  return key;
}

void Interakt()
{
  if (SzeneGetDrawMode() & ANIMATE) animFlag=1; else animFlag=0;
  if (SzeneGetDrawMode() & SOLID) solidFlag=1; else solidFlag=0;
  CenterSzene();
  xv_main_loop(frame);
}



