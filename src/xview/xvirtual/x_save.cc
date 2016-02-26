//  1994 by M. Roth
#include "view.h"
#include "main.h"
#include "szene.h"
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Xos.h>
#include <X11/Xatom.h>
#include <X11/keysym.h>

#include <stdio.h>

#include "windowIcon"

Display             *display=NULL;
int                  screen;
XEvent               report;
Colormap             defaultColormap;
 
Window               window;
Pixmap               windowIcon;
Pixmap               drawbuffer;
XFontStruct         *font_info;
GC                   gc;

char                *progname;
int                  lastColor=-1;

// Farbtabelle zur Umsetzung in X-Colors
int colorTab[256];

int height = 400;
int width = 640;

/*---------------------------------------------------------------------------*
  Verbindung mit dem X-Server aufnehmen
 *---------------------------------------------------------------------------*/
void openDisplay()
{
}

/*---------------------------------------------------------------------------*
 Grafik context erzeugen 
 *---------------------------------------------------------------------------*/
void creatGC()
{
  unsigned long valuemask = 0; /* ignore XGCvalues and use defaults */
  XGCValues values;

  /* Create default Graphics Context */
  /* xlib 1/118 */
  gc = XCreateGC(display, window, valuemask, &values);
}

/*---------------------------------------------------------------------------*
  Setzen der Properties fuer das Rootwindow
 *---------------------------------------------------------------------------*/
void setProperty(int argc,char **argv)
{
  static XTextProperty  windowName, iconName;
  char                 *window_name = "Virtual 1.0   1994 by M. Roth";
  char                 *icon_name = "Virtaul 1.0";
  XSizeHints           *size_hints;
  XIconSize            *size_list;
  XWMHints             *wm_hints;
  XClassHint           *class_hints;

  /* Speicherplatz fuer die Hints reservieren */
  if (!(size_hints = XAllocSizeHints())) {
    fprintf(stderr, "%s: failure allocating memory\n", progname);
    End();
  }
  if (!(wm_hints = XAllocWMHints())) {
    fprintf(stderr, "%s: failure allocating memory\n", progname);
    Error("Programmabbruch");
  }
  if (!(class_hints = XAllocClassHint())) {
    fprintf(stderr, "%s: failure allocating memory\n", progname);
    Error("Programmabbruch");
  }

  /* Size Hint xlib 1/409 */
  size_hints->flags = PPosition | PSize | PMinSize | PMaxSize;
  size_hints->min_width  = width;  /* Minimale Breite */
  size_hints->min_height = height;  /* Minimale Hoehe */
  size_hints->max_width  = width;    /* Maximale Breite */
  size_hints->max_height = width;    /* Maximale Hoehe */
 
  /* WM Hint xlib 1/412 */
  wm_hints->flags         = StateHint | IconPixmapHint | InputHint;
  wm_hints->initial_state = NormalState; /* Nicht als Icon starten */
  wm_hints->input         = True;    /* Erwarte Tastateneingabe */
  wm_hints->icon_pixmap   = windowIcon;   /* Verweis aus Windowicon */  

  /* Class Hint setzen (Zugriff auf Properties */
  class_hints->res_name  = progname;  /* Programmname */
  class_hints->res_class = "Virtual"; /* Programmgruppe */

  /* Setzen des Fensternamens */
  XStringListToTextProperty(&window_name, 1, &windowName);
  /* Setzen des Iconnamens */
  XStringListToTextProperty(&icon_name, 1, &iconName);

  /* Hints aktivieren */
  XSetWMProperties(display, window, &windowName, &iconName, 
		   argv, argc, size_hints, wm_hints, 
		   class_hints);

}

/*---------------------------------------------------------------------------*
  Eroeffnen eines Fensters
 *---------------------------------------------------------------------------*/
void openWindow()
{
  /* Fenster definieren */
  window = XCreateSimpleWindow(display,
				RootWindow(display,screen), 
				0, 0,     /* x,y */ 
				width, height, /* Breite, Hoehe */
				4,        /* Rahmenbreite */
				WhitePixel(display,screen),
				BlackPixel(display,screen));

  /* Create pixmap of depth 1 (bitmap) for icon */
  windowIcon = XCreateBitmapFromData(display, 
				     window,
				     windowIcon_bits, 
				     windowIcon_width,
				     windowIcon_height);

  /* Create pixmap of depth 1 (bitmap) for icon */
  drawbuffer = XCreatePixmap(display, 
			     window,
			     width, 
			     height,
			     8);

  // Default Farbtabelle
  defaultColormap = DefaultColormap(display,screen);
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

// oeffnen eines Screens mit 640*400 Punkten und 256 Farben
void X_OpenScreen()
{
  progname = "virtual";
  openDisplay();
  openWindow();
  setProperty(0,0);

   /* Select event types wanted */
  XSelectInput(display,
	       window,
	       ExposureMask | KeyPressMask |
	       ButtonPressMask | StructureNotifyMask);
  /* create GC for text and drawing */
  creatGC();
  /* Display window */
  XMapWindow(display, window);
  /* Zeichenfarbe setzen */
  XSetForeground(display, gc, BlackPixel(display,screen));
  
  /* Linienattribute  xlib 1/122 */
  XSetLineAttributes(display, gc,
		     0,             /* line width (0=Fastest) */
		     LineSolid    , /* line style xlib 1/124 */
		     CapNotLast,       /* cap style  xlib 1/125 */
		     JoinMiter      /* join_style xlib 1/125 */
		     );
  XNextEvent(display, &report);
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
  /* Zeichenfarbe setzen */
  if (lastColor!=farbe)
    {
      XSetForeground(display, gc,colorTab[farbe]);
      lastColor=farbe;
    }
  XDrawLine(display,drawbuffer,gc,x1,y1,x2,y2);
}

// Shattierte Zeile zeichnen
void X_ShadeLine(int y,int laenge,int xv,int hv,int hp,int farbe)
{
  farbe = (farbe << 4) + (hv>>8);

  /* Zeichenfarbe setzen */
  if (lastColor!=farbe)
    {
      XSetForeground(display, gc,colorTab[farbe]);
      lastColor=farbe;
    }
  XDrawLine(display,drawbuffer,gc,xv,y,xv+laenge,y);
}

// Zeile beschatten
void X_ShadowLine(int y,int xv,int laenge)
{
}

// Farbe anfordern
void X_AllocColor(int nr,int r,int g,int b)
{
  XColor color;
  
  color.red = r<<10;
  color.green = g<<10;
  color.blue = b<<10;
  XAllocColor(display, defaultColormap, &color);

  colorTab[nr] = color.pixel;
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
  int key;
  Matrix rotZ=Matrix(0,0,0);
  Matrix rotZinv=Matrix(0,0,0);
  Matrix rotX=Matrix(0,0,0);
  Vektor pos;
  Matrix ml=Matrix(0,0,3),mr=Matrix(0,0,-3);
  Matrix mu=Matrix(3,0,0),md=Matrix(-3,0,0);
  Vektor vh=Vektor(0,10,0);
  Vektor vv=Vektor(0,0,10);
  KoerperPtr k;
  int drawMode=WIRE;
  int animMode=ANIMATE;
  float maxX,minX,maxY,minY,maxZ,minZ;
  float max;

  SzeneGetMinMax(minX,maxX,minY,maxY,minZ,maxZ);
  max=maxX;
  if( (maxZ) > max) max=maxZ;
  if( (-minX) > max) max=-minX;
  if( (-minZ) > max) max=-minZ;
  max=max/300*800-minY;
  pos=pos+(Vektor(0,1,0)*max);
  pos=pos+Vektor(0,0,1)*(-maxZ/2);
//  SzeneDrawMode( drawMode|animMode );
  SzeneResetTime();
  k=SzeneGetKoerper("");
  do
  {
    if(animMode&=ANIMATE)
      key=GetKey(0);       /* Nicht auf Taste warten */
    else
      key=GetKey(1);       /* Auf Taste warten */

    SzeneSetPos(rotX*rotZ*pos);
    SzeneSetRot(rotX*rotZ);
    SzeneErstellen();

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
      case XK_F1:
	drawMode=WIRE;
	SzeneDrawMode( drawMode|animMode );
	break;
      case XK_F2:
	drawMode=SOLID;
	SzeneDrawMode( drawMode|animMode );
	break;
      case XK_F3:
	drawMode=SOLID | SHADOW;
	SzeneDrawMode( drawMode|animMode );
	break;
      case XK_F4:
	animMode^=ANIMATE;
	SzeneDrawMode( drawMode|animMode );
	break;
    }
  } while (key!='q');
}
