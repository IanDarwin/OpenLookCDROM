#include <stdio.h>
#include <suntool/tool_hs.h>
#include <suntool/gfxsw.h>
#include "fractool.h"


extern struct curve *init_curve;


extern struct curve *replicate_curve(), *iterate();
extern struct gfxsubwindow *gfxsw_init();


auto_run(listfilename)
char *listfilename;
{
	int n_iterations;
	char *cp, *cp1, buffer[80];
	FILE *listfile;
	struct curve *fractal, *new_fractal;
	struct gfxsubwindow *auto_gfxsw;
	struct pixwin *auto_pw;
	int i;
	struct rect rect;

	auto_gfxsw = gfxsw_init(0, NULL);
	if (auto_gfxsw == NULL)
		exit(1);
	win_getsize(auto_gfxsw->gfx_windowfd, &rect);
	auto_pw = auto_gfxsw->gfx_pixwin;
	pw_writebackground(auto_pw, 0, 0, rect.r_width,
		    	   rect.r_height, PIX_SRC);

	while (1) {
		listfile = fopen(listfilename, "r");
		if (listfile == NULL) {
			printf("Couldn't open list file.\n");
			exit(1);
		}
		while (fgets(buffer, 80, listfile) != NULL) {
			n_iterations = atoi(buffer);
			cp = buffer;
			while (*cp <= ' ' ||
				(*cp >= '0' && *cp <= '9'))
					cp++;
			cp1 = cp;
			while (*cp1 >= ' ')
				cp1++;
			*cp1 = 0;
			if (restore(cp) == -1)
				continue;
			fractal = replicate_curve(init_curve);
			display_autocurve(fractal, auto_gfxsw);
			sleep(1);
			while (n_iterations > 0) {
				if (auto_gfxsw->gfx_flags & GFX_DAMAGED)
					gfxsw_handlesigwinch(auto_gfxsw);
				new_fractal = iterate(fractal);
				discard_curve(fractal);
				fractal = new_fractal;
				display_autocurve(fractal, auto_gfxsw);
				sleep(1);
				n_iterations--;
			}
			sleep(3);
			win_getsize(auto_gfxsw->gfx_windowfd, &rect); 
                        pw_write(auto_pw, 0, 0, rect.r_width,
                          rect.r_height, PIX_SET, NULL, 0, 0);
			sleep(1);
		}
		fclose(listfile);
	}
}



display_autocurve(curve, subwin)
struct curve *curve;
struct gfxsubwindow *subwin;
{
        struct endpoint *e;
        int x0, x1, y0, y1;
        struct rect rect;
        struct curve *c;
	struct pixrect *mem_image;

	win_getsize(subwin->gfx_windowfd, &rect);
	mem_image = mem_create(rect.r_width, rect.r_height, 1);

        c = curve;
        while (c) {
                e = c->start;
                if (e != NULL) {
                        x0 = e->x;
                        y0 = e->y;
                        e = e->next;
                        if (e == NULL) {
                                c = c->next;
                                continue;
                        }
                        x1 = e->x;
                        y1 = e->y;
                        while(e) {
                                pr_vector(mem_image, x0, y0, x1, y1,
                                  PIX_SET, 1);
                                x0 = x1;
                                y0 = y1;
                                e = e->next;
                                if (e) {
                                        x1 = e->x;
                                        y1 = e->y;
                                }
                        }
                }
                c = c->next;
        }

	pw_rop(subwin->gfx_pixwin, 0, 0, rect.r_width, rect.r_height,
	  PIX_SRC, mem_image, 0, 0);
	pr_destroy(mem_image);
}       
        
