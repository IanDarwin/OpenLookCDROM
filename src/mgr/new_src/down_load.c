/*                        Copyright (c) 1987 Bellcore
 *                            All Rights Reserved
 *       Permission is granted to copy or use this program, EXCEPT that it
 *       may not be sold for profit, the copyright notice must be reproduced
 *       on copies, and credit should be given to Bellcore where it is due.
 *       BELLCORE MAKES NO WARRANTY AND ACCEPTS NO LIABILITY FOR THIS PROGRAM.
 */
/*	$Header: /home/sau/mgr/nsrc/RCS/down_load.c,v 1.4 91/08/27 09:50:56 sau Exp Locker: sau $
	$Source: /home/sau/mgr/nsrc/RCS/down_load.c,v $
*/
static char	RCSid_[] = "$Source: /home/sau/mgr/nsrc/RCS/down_load.c,v $$Revision: 1.4 $";

/* down load text -- called from put_window.c */

#include <sys/file.h>
#include "bitmap.h"
#include "font.h"
#include "defs.h"
#include "menu.h"
#include "event.h"
#include <stdio.h>
#include "window.h"

int
down_load(win,window,text)
register WINDOW *win;
BITMAP *window, *text;
   {
	WINDOW *win2;
   int cnt;
   struct font *Get_font();
   struct menu_state *do_menu();
   int id;

   cnt = W(esc_cnt);
      switch(W(code)) {
         case T_INVALID:		/* invalid text mode */
		      if (debug )
		         fprintf(stderr,"mgr: invalid download code\n");
		      break;
         case T_MENU:				/* down load menu */
            {
            struct font *f;
            int fn = W(esc[1]);

            if (*W(snarf) && cnt>1 )
               f = Get_font(fn);
            else
               f = W(font);
                  
            if (*W(snarf)) {
               W(menus)[*W(esc)] = do_menu(W(snarf),f,W(style));
					if (active == win ) {
						if (W(menu[0]) == *W(esc) && button_state==BUTTON_2)
							go_menu(0);
						else if (W(menu[1]) == *W(esc) && button_state==BUTTON_1)
							go_menu(1);
						}
               }
            else
               W(menus)[*W(esc)] = (struct menu_state *) 0;
            }
            break;
         case T_EVENT:				/* down load an event */
            cnt=W(esc)[0];
            if (!CHK_EVENT(cnt)) {
               break;
               }
            if (W(events)[GET_EVENT(cnt)]) {
               free(W(events)[GET_EVENT(cnt)]);
               W(events)[GET_EVENT(cnt)] = (char *) 0;
               }
            if (*W(snarf)) {
               W(events)[GET_EVENT(cnt)] = W(snarf);
               W(snarf) = NULL;
               EVENT_SET_MASK(win,cnt);
#ifdef DEBUG
               dprintf(e)(stderr,"%s: setting event %d (%d)[%s]\r\n",
                        W(tty),GET_EVENT(cnt),strlen(W(snarf)), W(snarf));
#endif
                   /* if button is down, then do the associated event */

               if (win == active  &&
			             (cnt==EVENT_B1_DOWN && button_state == BUTTON_1 ||
                       cnt==EVENT_B2_DOWN && button_state == BUTTON_2))
		            do_event(button_state,win,E_MAIN);
               }
            else {
               EVENT_CLEAR_MASK(win,cnt);
#ifdef DEBUG
               dprintf(e)(stderr,"%s: clearing event %d\r\n",
                             W(tty),GET_EVENT(cnt));
#endif
               }
            break;
         case T_STRING:				/* draw text into offscreen bitmap */
            {
            int x = cnt>1 ? Scalex(W(esc[1])) : 0;
            int y = cnt>2 ? Scaley(W(esc[2])) : 0;

            if (y<FSIZE(high))
               y=FSIZE(high);
            if (x<0)
                x=0;
#ifdef DEBUG
            dprintf(y)(stderr,"%s: drawing [%s] to %d\r\n",
                    W(tty),W(snarf),*W(esc));
#endif
            if (*W(esc)>0 && W(bitmaps)[*W(esc)-1] == (BITMAP *) 0) {
               W(bitmaps)[*W(esc)-1] = bit_alloc(
                          x+strlen(W(snarf))*FSIZE(wide),
                          y, NULL_DATA,1);	/* text is always 1 bit deep */
#ifdef DEBUG
               dprintf(y)(stderr,"%s: STRING creating %d (%dx%d)\n",
                          W(tty),*W(esc),
                          x+strlen(W(snarf))*FSIZE(wide),y);
#endif
               }
            if (*W(esc) > 0)
               put_str(W(bitmaps)[*W(esc)-1],x,y,W(font),W(op),W(snarf));
            else
               put_str(window,x,y,W(font),W(op),W(snarf));
            }
            break;
         case T_YANK:				/* fill yank buffer */
            if (snarf)
               free (snarf);
            snarf = W(snarf);
#ifdef DEBUG
            dprintf(y)(stderr,"%s: yanking [%s]\r\n",W(tty),snarf);
#endif
            id_message = W(pid);
            W(snarf) = (char *) 0;
            for(win2=active;win2 != (WINDOW *) 0;win2=win2->next)
               do_event(EVENT_SNARFED,win2,E_MAIN);
            break;

         case T_SEND:				/* send a message */
            id = *W(esc);
            if (message) {
               free(message);
               message = (char *) 0;
               }
            message = W(snarf);
            id_message = W(pid);
            W(snarf) = NULL;
#ifdef DEBUG
            dprintf(e)(stderr,"%s: sending [%s]\r\n",W(tty),W(snarf));
            dprintf(c)(stderr,"sending %d->%d: %s\r\n",
                   W(pid),cnt==0?0:id,message);
#endif
            for(win2=active;win2 != (WINDOW *) 0;win2 = win2->next)
               if (cnt==0 || win2->pid==id) {
                  do_event(EVENT_ACCEPT,win2,E_MAIN);
                  if (cnt)
                     break;
                  }
            break;

         case T_GMAP:				/* load a bitmap from a file */
            {
            BITMAP *b, *bitmapread();
		      FILE *fp = NULL;
            char filename[MAX_PATH];
            char buff[20];
            char c = *W(snarf);

            /* make relative to icon directory */

            if (c == '/' || (c == '.' && W(snarf)[1]=='/'))
               strcpy(filename,W(snarf));
            else
               sprintf(filename,"%s/%s",icon_dir,W(snarf));

	         if (W(flags)&W_DUPKEY)
		         sprintf(buff,"%c ",W(dup));
            else
               *buff = '\0';

            if (*W(esc) > 0 && *W(esc) < MAXBITMAPS &&
                       read_ok(filename) &&
			             (fp = fopen(filename,"r")) != NULL &&
                      (b = bitmapread(fp))) {
		         if (W(bitmaps[*W(esc)-1]) ) {
			         bit_destroy(W(bitmaps[*W(esc)-1]));
                  }
		         W(bitmaps[*W(esc)-1]) = b;
               sprintf(buff+strlen(buff),"%d %d %d\n",
                                  BIT_WIDE(b),BIT_HIGH(b),BIT_DEPTH(b));
               }
            else {
               strcat(buff,"\n");
               }
            write(W(to_fd),buff,strlen(buff));

		      if (fp != NULL )
			      fclose(fp);
            }
            break;

         case T_SMAP:				/* save a bitmap on a file */
            {
		      FILE *fp;
            BITMAP *b;
            int size;
            int exists;		/* file already exists */
            int free_b = 0;
            int num = *W(esc);
            BITMAP *get_map();

            switch(cnt) {
               case 1:			/* off screen bitmap */
                  if (num > 0)
                      b = W(bitmaps[num-1]);
                  else
                      b = screen;
                  break;
               case 0:			/* my window */
                  free_b++;
                  b = bit_alloc(BIT_WIDE(window),BIT_HIGH(window),
                                       NULL_DATA,BIT_DEPTH(window));
                  if (b)
                     bit_blit(b,0,0,BIT_WIDE(b),BIT_HIGH(b),BIT_SRC,window,0,0);
                  break;
               case 2:			/* other guy's window */
                  free_b++;
                  b = get_map(num,W(esc[1]));
                  break;
                  }
               
#ifdef DEBUG
               dprintf(*)(stderr,"saving...\n");
#endif
               if (b && W(snarf) && ((exists=access(W(snarf),0)),
                            write_ok(W(snarf))) &&
                            (fp = fopen(W(snarf),"w")) != NULL) {
#ifdef DEBUG
                  dprintf(y)(stderr,"saving bitmap %d x %d on %s (%d)\n",BIT_WIDE(b),BIT_HIGH(b),W(snarf),fileno(fp));
#endif
                  if (exists<0)	/* file just created */
                     fchown(fileno(fp),getuid(),getgid());
                  bitmapwrite( fp, b, get_bm_type() );
                  fclose(fp);
#ifdef DEBUG
                  dprintf(y)(stderr,"saved %d on %s\n",size,W(snarf));
#endif
                  }

              if (b && free_b)
                 bit_destroy(b);
              }
              break;

         case T_GIMME:				/* send to process */
            if (W(snarf) && *W(snarf)) 
               write_event(win,W(snarf),E_LIST_UP);
#ifdef DEBUG
            dprintf(y)(stderr,"%s: sending [%s]\r\n",W(snarf));
#endif
            break;

         case T_GRUNCH:				/* graphics scrunch mode (experimental) */
            if (W(snarf)) 
               grunch(win,window);
#ifdef DEBUG
            dprintf(y)(stderr,"%s: grunching [%d]\r\n",W(tty),W(esc)[cnt]);
#endif
            break;

         case T_FONT:				/* change a font name */
            if (W(esc)[0] <= MAXFONT && W(esc)[0] > 0) {
               if (fontlist[W(esc[0])-1])
                  free(fontlist[W(esc[0])-1]);
               fontlist[W(esc[0])-1] = W(snarf);
               W(snarf) = NULL;
               }
              break;
         case T_BITMAP:				/* down-load a bitmap */
              win_map(win,window);
              W(snarf) = NULL;
              break;
         }
	if (W(snarf))
		free(W(snarf));
   W(snarf) = NULL;		/* better be free by now!! sau 6/91) */
   }

/* find bitmap associated with window id */

BITMAP *
get_map(id,sub)
int id;				/* pid of process controlling window */
int sub;			/* window number of this window */
   {
   register WINDOW *win;
   register BITMAP *map;

   for(win=active;win != (WINDOW *) 0;win=W(next))
      if (W(pid)==id && W(num)==sub) {
         map = bit_alloc(BIT_WIDE(W(window)),BIT_HIGH(W(window)),
                         NULL_DATA,BIT_DEPTH(W(window)));
         if (map && W(flags)&W_ACTIVE)
            bit_blit(map,0,0,BIT_WIDE(map),BIT_HIGH(map),
                     BIT_SRC,W(window),0,0);
         else if (map)
            bit_blit(map,0,0,BIT_WIDE(map),BIT_HIGH(map),
                     BIT_SRC,W(save),SUM_BDR,SUM_BDR);
         return(map);
         }
   return(BIT_NULL);
   }
