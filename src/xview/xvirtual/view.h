//  1993 by M. Roth
#ifndef _view_h
#define _view_h 1

void ViewSet( void (*OpenScreen)(),
	      void (*CloseScreen)(),
	      void (*SwopScreen)(),
	      void (*ClearScreen)(),
	      void (*Line)(int,int,int,int,int),
	      void (*Point)(int,int,int),
	      void (*ShadeLine)(int y,int l,int xv,int hv,int hp,int f),
	      void (*ShadowLine)(int y,int x,int laenge),
	      int MaxX,
	      int MaxY);
void ViewInit();
void ViewEnd();
void ViewSwop();
void ViewClear();
void ViewPoint(int x,int y,int farbe);
void ViewLine(int x1,int y1,int x2,int y2,int farbe);
void ViewShadeLine(int y,int laenge,int xv,int hv,int hp,int farbe);
void ViewShadowLine(int y,int x,int laenge);
void ViewClearColor();
void ViewDefColor(char *farbname,float rot,float gruen,float blau);
void ViewSetColor(char *farbname);
void ViewSetColor(int);
void ViewModeS3();
void ViewModeVGA();
void ViewProjektion(float &x,float &y,float &z);
int  ViewGetMaxX();
int  ViewGetMaxY();
int  ViewGetColor();

#endif