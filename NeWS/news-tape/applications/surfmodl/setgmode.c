#include "surfmodl.h"
#include "draw.h"
#include "math.h"
#include "string.h"

void setgmode()
{
	ps_newimage();
} /*procedure Setgmode*/
void window_main_loop()
{
  int firstime, menuindex;
  double D;
  firstime = TRUE;

/* loop forever checking for menu selections */

  while(1) {
	if (firstime && display) {	/* draw picture first time through */
		
	    graphics();
	    		/* set angle slider values */
	    ps_angleslider(trunc(180.0*Ztheta/M_PI),trunc(180.0*Zphi/M_PI)); 
	    ps_flush_PostScript();
	}
	firstime = FALSE;
	if(check_main_menu(&menuindex)) {	/* set drawing style */
	    if (menuindex != 0 && menuindex < 4) drawing_style = menuindex;
	    if (menuindex == QUIT) break;
	    display = 1;
	}else if (check_para_menu(&menuindex)) { /* select paramenu */
	    paramenu(menuindex);
	    display = 1;
	}else if (check_lite_menu(&menuindex)) { /* select litemenu */
	    litemenu(menuindex + 1);
	    display = 1;
	}else if (check_zoom_button(&menuindex)) { /* zoom buttons */
	    if (menuindex == 1) Magnify = Magnify/0.9; /* Zoom In */
	    if (menuindex == 2) Magnify = Magnify*0.9; /* Zoom Out */
	    display = 1;
	}else if (check_slider_value(&menuindex)) { /* select Zcut */
	    Zcutnear = menuindex;
	    display = 1;
	}else if (check_slider_theta(&menuindex)) { /* select theta */
	   D = sqrt(sqr(Xfocal-Xeye) + sqr(Yfocal-Yeye) +sqr(Zfocal-Zeye));
	   Ztheta = M_PI*menuindex/180.;
	   Zeye = Zfocal + D*cos(Zphi);
	   Yeye = Xfocal + D*sin(Zphi)*sin(Ztheta);
	   Xeye = Yfocal + D*sin(Zphi)*cos(Ztheta);
	   printf (" Xeye,Yeye,Zeye,index= %f,%f,%f, %d\n",Xeye,Yeye,Zeye,menuindex);
	   display = 1;
	}else if (check_slider_phi(&menuindex)) { /* select phi */
	   D = sqrt(sqr(Xfocal-Xeye) + sqr(Yfocal-Yeye) +sqr(Zfocal-Zeye));
	   Zphi  = M_PI*menuindex/180;
	   Zeye = Zfocal + D*cos(Zphi);
	   Yeye = Xfocal + D*sin(Zphi)*sin(Ztheta);
	   Xeye = Yfocal + D*sin(Zphi)*cos(Ztheta);
	   printf (" Xeye,Yeye,Zeye,index= %f,%f,%f, %d\n",Xeye,Yeye,Zeye,menuindex);
	   display = 1;
	}
	if (display) graphics();
  }
}
void leavegraphic()
{
	ps_display();
	ps_flush_PostScript();
} /* procedure Leavegraphic */
closegraph()
{
	ps_close_PostScript();
}
setprintfile(Filename)
char *Filename;
{
	char *getcwd(), filename[100];
	getcwd(filename,80);
	strcat(filename,"/");
	strcat(filename,Filename);
	strcat(filename,".ps");
	ps_printfile(filename);
}
