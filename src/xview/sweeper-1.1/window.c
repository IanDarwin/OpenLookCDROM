static char sccsid[]="@(#)window.c	1.5 10/12/92";

#include "window.h"

void init_windows()
{
      Xv_Window pw;
      XID xid;
      Server_image    image;
      Icon            icon;
      Display *dpy;
      char bombmsg[40];
      GC gc;

      static unsigned char bits[]={0x88,0x88,0x00,0x00,0x22,0x22,0x00,0x00};


      image=(Server_image)xv_create(NULL               ,SERVER_IMAGE,
				    XV_HEIGHT          ,64,
				    XV_WIDTH           ,64,
				    SERVER_IMAGE_BITS  ,icon_bits,
				    0);

      icon=(Icon)xv_create(NULL,ICON,
			   ICON_IMAGE, image,
			   0);

      frame = xv_create( XV_NULL, FRAME_BASE,
			FRAME_ICON, icon, 
			XV_LABEL, VERSION,
			FRAME_SHOW_RESIZE_CORNER,FALSE,
			FRAME_SHOW_FOOTER,TRUE,
			0 );

      sprintf(bombmsg,"No bombs left: %d",no_bombs-no_marked_bombs);
      xv_set(frame,FRAME_LEFT_FOOTER,bombmsg,0);

      pan=xv_create(frame,PANEL,
		      WIN_BORDER, TRUE,
		      XV_WIDTH, area_width*BOX_SIZE,
		      0);

      ngamebutt=xv_create(pan,PANEL_BUTTON,
		      PANEL_LABEL_STRING,"New game",
		      PANEL_NOTIFY_PROC,new_game,
		      XV_Y, xv_row(pan,0),
		      0);

      (void)xv_create(pan,PANEL_CHOICE_STACK,
		      PANEL_LABEL_STRING,"Size",
		      PANEL_CHOICE_STRINGS, "10x10","15x15","18x25",0,
		      PANEL_VALUE,0,
		      PANEL_NOTIFY_PROC,set_size,
		      XV_Y,xv_row(pan,0),
		      0);

      (void)xv_create(pan,PANEL_BUTTON,
		      PANEL_LABEL_STRING,"Score...",
		      PANEL_NOTIFY_PROC,highscore_butt,
		      XV_Y, xv_row(pan,0),
		      0);

      (void)xv_create(pan,PANEL_CHOICE_STACK,
		      PANEL_LABEL_STRING,"Level",
		      PANEL_CHOICE_STRINGS,"Rookie","Easy","Medium","Hard","Impossible",0,
		      PANEL_VALUE,level,
		      PANEL_NOTIFY_PROC,set_diff,
		      XV_Y,xv_row(pan,1),
		      XV_X,xv_col(pan,1),
		      0);

      time_msg=xv_create(pan,PANEL_MESSAGE,
			 PANEL_LABEL_STRING,"Time: 0.00",
			 XV_Y,xv_row(pan,1)+4,
			 0);

      canvas=xv_create(frame,CANVAS,
		       XV_X               ,0,
		       XV_Y               ,xv_row(frame,6),
		       XV_WIDTH           ,area_width*BOX_SIZE,
		       XV_HEIGHT          ,area_height*BOX_SIZE,
		       CANVAS_REPAINT_PROC, repaint_proc,
		       CANVAS_X_PAINT_WINDOW, TRUE,
		       CANVAS_AUTO_CLEAR, TRUE,
		       0);

      xv_set(canvas_paint_window(canvas),
	     WIN_EVENT_PROC, cv_event,
	     WIN_CONSUME_X_EVENT_MASK,ButtonPressMask,
	     NULL);

      font=(Xv_Font)xv_find(NULL,FONT,
			     FONT_FAMILY,FONT_FAMILY_DEFAULT,
			     FONT_SIZES_FOR_SCALE,6,12,16,
			     NULL);

      largefont=(Xv_Font)xv_find(NULL,FONT,
				 FONT_RESCALE_OF, font, WIN_SCALE_EXTRALARGE,
				 NULL);

      numberfont=(Xv_Font)xv_find(NULL,FONT,
				  FONT_FAMILY,FONT_FAMILY_DEFAULT,
				  FONT_STYLE,FONT_STYLE_BOLD,
				  FONT_SIZE,18,
				  NULL);

      pw =xv_get(canvas,CANVAS_NTH_PAINT_WINDOW,0);
      xid=(XID)xv_get(pw,XV_XID);
      dpy=(Display *)xv_get(pw,XV_DISPLAY);
      gc=DefaultGC(dpy,DefaultScreen(dpy));
      XSetStipple(dpy,gc,XCreateBitmapFromData(dpy,xid,bits,16,4));
      repaint_proc((Canvas)NULL,pw,
		   (Display *)xv_get(pw,XV_DISPLAY),
		   xv_get(pw,XV_XID),(Xv_xrectlist *)NULL);
      window_fit_height(pan);
      window_fit(frame);
      xv_main_loop(frame);
}

void repaint_proc(canv,pw,dpy,xwin,xrects)
    Canvas canv;
    Xv_Window pw;
    Display *dpy;
    Window xwin;
    Xv_xrectlist *xrects;
{
      GC gc;
      int i,row,col;

      XClearArea(dpy,xwin,0,0,area_width*BOX_SIZE,area_height*BOX_SIZE,FALSE);
      gc=DefaultGC(dpy,DefaultScreen(dpy));
      /* draw grid */
      for(i=BOX_SIZE; i<BOX_SIZE*area_width;i+=BOX_SIZE)
	    XDrawLine(dpy,xwin,gc,i,0,i,area_height*BOX_SIZE);
      for(i=BOX_SIZE; i<BOX_SIZE*area_height;i+=BOX_SIZE)
	    XDrawLine(dpy,xwin,gc,0,i,area_width*BOX_SIZE,i);
      /* show marked/shown boxes */
      if(game_started==1||game_started==3)
	    for(row=0;row<area_height;row++)
		  for(col=0;col<area_width;col++)
			if(bomb_grid[row][col].selected==SHOWN){
			      fill_square(row,col,dpy,xwin);
			      if(bomb_grid[row][col].value>0)
				    draw_number(row,col,bomb_grid[row][col].value,
						dpy,xwin);
			      else if(bomb_grid[row][col].value==BOMB)
				    draw_bomb(row,col,dpy,xwin);
			}
}

void cv_event(pw,event)
    Xv_Window pw;
    Event *event;
{
      int row,col,xcord,ycord;


      if((game_started==1 || game_started==2) && bang_shown==0){
	    xcord=event_x(event);
	    ycord=event_y(event);
	    col=xcord/BOX_SIZE;
	    row=ycord/BOX_SIZE;
	    if(event_action(event)==ACTION_SELECT
	       && ! event_left_is_down(event)){
		  if(game_started==2) {
			start();
		  }
		  show_square(row,col,pw,
			      (Display *)xv_get(pw,XV_DISPLAY),
			      xv_get(pw,XV_XID));
	    }
	    else if(event_action(event)==ACTION_ADJUST 
		    && ! event_middle_is_down(event)){
		  mark_square(row,col,
			      (Display *)xv_get(pw,XV_DISPLAY),
			      xv_get(pw,XV_XID));     
	    }
      }
}

void show_square(row,col,pw,dpy,xwin)
    int row,col;
    Xv_Window pw;
    Display *dpy;
    Window xwin;    
{
      int val;

      if(bomb_grid[row][col].selected==FLAG) return;
      val=bomb_grid[row][col].value;
      if(val != 0 && val != -1){
	    no_shown_squares++;
	    fill_square(row,col,dpy,xwin);
	    draw_number(row,col,bomb_grid[row][col].value,dpy,xwin);
	    bomb_grid[row][col].selected=SHOWN;
	    done();
      }
      else if(val==0){
	    expose(row,col,pw,dpy,xwin);
	    done();
      }
      else {
	    bang(dpy,xwin);
      }
}

void draw_bomb(row,col,dpy,xwin)
    int row;
    int col;
    Display *dpy;
    Window xwin;
{
      GC gc;

      gc=DefaultGC(dpy,DefaultScreen(dpy));
      XFillArc(dpy,xwin,gc,
	       col*BOX_SIZE+BOX_SIZE/3,
	       row*BOX_SIZE+BOX_SIZE/3,
	       BOX_SIZE/2,BOX_SIZE/2,0,360*64);
}

void fill_square(row,col,dpy,xwin)
    int row;
    int col;
    Display *dpy;
    Window xwin;
{
      GC gc;

      gc=DefaultGC(dpy,DefaultScreen(dpy));
      XClearArea(dpy,xwin,col*BOX_SIZE+1,row*BOX_SIZE+1,
		 BOX_SIZE-2,BOX_SIZE-2,FALSE);
      XSetFillStyle(dpy,gc,FillStippled);
      XFillRectangle(dpy,xwin,gc,col*BOX_SIZE
		     ,row*BOX_SIZE,BOX_SIZE,BOX_SIZE);
      XSetFillStyle(dpy,gc,FillSolid);
}

void draw_number(row,col,number,dpy,xwin)
    int row;
    int col;
    int number;
    Display *dpy;
    Window xwin;
{
      GC gc;
      char str[5];

      gc=DefaultGC(dpy,DefaultScreen(dpy));
      XSetFont(dpy,gc,(Font)xv_get(numberfont,XV_XID));
      sprintf(str,"%d",number);
      XDrawString(dpy,xwin,gc,
		  col*BOX_SIZE+BOX_SIZE/3,
		  (int)(row*BOX_SIZE+BOX_SIZE*0.66),
		  str,strlen(str));
      XSetFont(dpy,gc,(Font)xv_get(font,XV_XID));
}

void bang(dpy,xwin)
    Display *dpy;
    Window xwin;
{
      GC gc;
      int xcord,ycord;

      gc=DefaultGC(dpy,DefaultScreen(dpy));
      
      xcord=(area_height*BOX_SIZE)/2-30;
      ycord=(area_width*BOX_SIZE)/2-110;
      XDrawRectangle(dpy,xwin,gc,ycord,xcord,220,60);
      XClearArea(dpy,xwin,ycord+1,xcord+1,218,58,FALSE);
      XSetFont(dpy,gc,(Font)xv_get(largefont,XV_XID));
      XDrawString(dpy,xwin,gc,ycord+10,xcord+40,
		  "B A N G !",9);
      XSetFont(dpy,gc,(Font)xv_get(font,XV_XID));
      bang_shown=1;
      xv_set(ngamebutt,PANEL_INACTIVE,TRUE,0);
      notify_set_itimer_func(frame,NOTIFY_FUNC_NULL,ITIMER_REAL,NULL,NULL);
      timer.it_value.tv_usec=92767;
      timer.it_interval.tv_usec=92767;    
      notify_set_itimer_func(frame,bang_timer,ITIMER_REAL,&timer,NULL);
}

Notify_value bang_timer()
{
      static counter=0;

      counter++;
      counter%=15;
      if(counter==0){
	    show_all();
	    stop();
	    xv_set(ngamebutt,PANEL_INACTIVE,FALSE,0);
	    bang_shown=0;
      }
      return 0;
}

void expose(x,y,pw,dpy,xwin)
    int x,y;
    Xv_Window pw;
    Display *dpy;
    Window xwin;    
{
      int row,col;
      int rs,re,cs,ce;
      int no_found,i;

      if(bomb_grid[x][y].value==0){
	    bomb_grid[x][y].selected=SHOWN;
	    rs=x-1;
	    if(rs<0)rs=0;
	    re=x+1;
	    if(re>=area_height)re=area_height-1;
	    cs=y-1;
	    if(cs<0)cs=0;
	    ce=y+1;
	    if(ce>=area_width)ce=area_width-1;
	    no_found=0;
	    for(row=rs;row<=re;row++){
		  for(col=cs;col<=ce;col++){
			if(bomb_grid[row][col].selected==UNMARKED){
			      no_found++;
			      push(row,col);
			      bomb_grid[row][col].selected=CHECK;
			}
		  }
	    }
	    for(i=0;i<no_found;i++){
		  expose(top->x,top->y,pw,dpy,xwin);
		  pop();
	    }
      }
      bomb_grid[x][y].selected=SHOWN;
      no_shown_squares++;
      fill_square(x,y,dpy,xwin);
      if(bomb_grid[x][y].value>0)
	    draw_number(x,y,bomb_grid[x][y].value,dpy,xwin);
}

void mark_square(row,col,dpy,xwin)
    int row,col;
    Display *dpy;
    Window xwin;    
{
      if(bomb_grid[row][col].selected==SHOWN)
	    return;
      if(bomb_grid[row][col].selected==UNMARKED){
	    no_shown_squares++;
	    no_marked_bombs++;
	    if(bomb_grid[row][col].value==BOMB)
		  no_cor_marked_bombs++;
	    bomb_grid[row][col].selected=FLAG;
	    if(done() == 1) return;
	    fill_square(row,col,dpy,xwin);
	    draw_bomb(row,col,dpy,xwin);
      }
      else{
	    no_shown_squares--;
	    no_marked_bombs--;	
	    XClearArea(dpy,xwin,col*BOX_SIZE+1,row*BOX_SIZE+1,
		       BOX_SIZE-1,BOX_SIZE-1,FALSE);
	    if(bomb_grid[row][col].value==BOMB)
		  no_cor_marked_bombs--;
	    bomb_grid[row][col].selected=UNMARKED;
      }
}

int done()
{
      int i;
      char bombmsg[40];

      if(no_cor_marked_bombs==no_bombs &&
	 no_marked_bombs==no_bombs){/* &&
	 no_shown_squares==area_width*area_height){*/
	    show_all();
	    i=update_highscore();
	    show_highscore(i);
	    xv_set(frame,FRAME_LEFT_FOOTER,"You made it",0);
	    stop();
	    return 1;
      }
      else{
	    sprintf(bombmsg,"No bombs left: %d",no_bombs-no_marked_bombs);
	    xv_set(frame,FRAME_LEFT_FOOTER,bombmsg,0);
	    return 0;
      }
}

void show_all()
{
      int row,col;
      Xv_Window pw;
      Display *dpy;
      Window xwin;
      GC gc;
      int i;
      
      pw =xv_get(canvas,CANVAS_NTH_PAINT_WINDOW,0);
      dpy=(Display *)xv_get(pw,XV_DISPLAY);
      xwin=xv_get(pw,XV_XID);
      gc=DefaultGC(dpy,DefaultScreen(dpy));
      XClearArea(dpy,xwin,0,0,area_width*BOX_SIZE,area_height*BOX_SIZE,FALSE);
      gc=DefaultGC(dpy,DefaultScreen(dpy));
      /* draw grid */
      for(i=BOX_SIZE; i<BOX_SIZE*area_width;i+=BOX_SIZE)
	    XDrawLine(dpy,xwin,gc,i,0,i,area_height*BOX_SIZE);
      for(i=BOX_SIZE; i<BOX_SIZE*area_height;i+=BOX_SIZE)
	    XDrawLine(dpy,xwin,gc,0,i,area_width*BOX_SIZE,i);
      /* show marked/shown boxes */
      for(row=0;row<area_height;row++)
	    for(col=0;col<area_width;col++){
		  if(bomb_grid[row][col].selected==SHOWN ||
		     bomb_grid[row][col].selected==FLAG )
			fill_square(row,col,dpy,xwin);
		  if(bomb_grid[row][col].value>=0 &&
			  bomb_grid[row][col].selected==FLAG){
			draw_bomb(row,col,dpy,xwin);
			XDrawLine(dpy,xwin,gc,col*BOX_SIZE+5,row*BOX_SIZE+5,
				  (col+1)*BOX_SIZE-5,(row+1)*BOX_SIZE-5);
			XDrawLine(dpy,xwin,gc,col*BOX_SIZE+5,(row+1)*BOX_SIZE-5,
				  (col+1)*BOX_SIZE-5,row*BOX_SIZE+5);
		  }
		  else if(bomb_grid[row][col].value>0)
			draw_number(row,col,bomb_grid[row][col].value,dpy,xwin);
		  else if(bomb_grid[row][col].value==BOMB)
			draw_bomb(row,col,dpy,xwin);
	    }
}

void set_size(item,val,event)
    Panel_item item;
    int val;
    Event *event;
{
      Xv_Window pw;

      stop();
      size=val;
      switch(val){
	 case S10X10:
	    area_width=10;
	    area_height=10;
	    break;
	 case S15X15:
	    area_width=15;
	    area_height=15;
	    break;
	 case S18X25:
	    area_width=25;
	    area_height=18;
	    break;
      }
      xv_set(pan,XV_WIDTH,area_width*BOX_SIZE,NULL);
      xv_set(canvas,XV_WIDTH,area_width*BOX_SIZE,NULL);
      xv_set(canvas,XV_HEIGHT,area_height*BOX_SIZE,NULL);
      window_fit(frame);
      pw =xv_get(canvas,CANVAS_NTH_PAINT_WINDOW,0);
      new_game();
      repaint_proc((Canvas)NULL,pw,
		   (Display *)xv_get(pw,XV_DISPLAY),
		   xv_get(pw,XV_XID),(Xv_xrectlist *)NULL);
}

void set_diff(item,val,event)
    Panel_item item;
    int val;
    Event *event;
{
      Xv_Window pw;

      stop();
      pw =xv_get(canvas,CANVAS_NTH_PAINT_WINDOW,0);
      level=val;
      new_game();
      repaint_proc((Canvas)NULL,pw,
		   (Display *)xv_get(pw,XV_DISPLAY),
		   xv_get(pw,XV_XID),(Xv_xrectlist *)NULL);
}

void start()
{
      char str[30];

      game_started=1;
      timer.it_value.tv_usec=92767;
      timer.it_interval.tv_usec=92767;    
      notify_set_itimer_func(frame,tick_clock,ITIMER_REAL,&timer,NULL);
      sprintf(str,"No bombs left: %d",no_bombs);
      xv_set(frame,FRAME_LEFT_FOOTER,str,0);
      xv_set(time_msg,PANEL_LABEL_STRING,"Time: 0.00  ",0);
}

void new_game()
{
      Xv_Window pw;
      char bombmsg[40];
      
      pw =xv_get(canvas,CANVAS_NTH_PAINT_WINDOW,0);
      game_started=0;
      sec=0;
      no_shown_squares=0;
      init_board();
      stop();
      repaint_proc((Canvas)NULL,pw,
		   (Display *)xv_get(pw,XV_DISPLAY),
		   xv_get(pw,XV_XID),(Xv_xrectlist *)NULL);
      sprintf(bombmsg,"No bombs left: %d",no_bombs-no_marked_bombs);
      xv_set(frame,FRAME_LEFT_FOOTER,bombmsg,0);
      xv_set(time_msg,PANEL_LABEL_STRING,"Time: 0.00  ",0);
      game_started=2;
}

void stop()
{
      game_started=0;
      notify_set_itimer_func(frame,NOTIFY_FUNC_NULL,ITIMER_REAL,NULL,NULL);
}

Notify_value tick_clock()
{
      static int cntr=0;
      char str[20];

      if(cntr==0){
	    sec++;
	    sprintf(str,"Time: %d.%2.2d",sec/60,sec%60);
	    xv_set(time_msg,PANEL_LABEL_STRING,str,0);
      }
      cntr++;
      cntr%=10;
}

void highscore_butt()
{
      show_highscore(-1);
}

void show_highscore(pos)
    int pos;
{
      Panel cmd_pan;
      static Frame cframe=NULL;
      int i,scr,row;
      char str[18];
      static char levelnames[5][40]={"Rookie","Easy","Medium","Hard","Impossible"};
      static char sizes[3][40]={"10x10","15x15","18x25"};

      if(cframe!=NULL)
	    xv_destroy_safe(cframe);
      if(pos==-1)
	    read_highscore();
      cframe=xv_create(frame,FRAME_CMD,
		       FRAME_LABEL,"Highscores",
		       0);
      cmd_pan=xv_get(cframe,FRAME_CMD_PANEL);
      xv_set(cmd_pan,WIN_BORDER,TRUE,0);

      row=0;
      (void)xv_create(cmd_pan,PANEL_MESSAGE,
		      XV_Y,xv_row(cmd_pan,row),
		      XV_X,xv_col(cmd_pan,1),
		      PANEL_LABEL_STRING,sizes[size],
			    0);
      (void)xv_create(cmd_pan,PANEL_MESSAGE,
		      XV_Y,xv_row(cmd_pan,row),
		      XV_X,xv_col(cmd_pan,10),
		      PANEL_LABEL_STRING,levelnames[level],
			    0);
      row++;
      (void)xv_create(cmd_pan,PANEL_MESSAGE,
		      XV_Y,xv_row(cmd_pan,row),
		      XV_X,xv_col(cmd_pan,3),
		      PANEL_LABEL_STRING,"Player",
			    0);
      (void)xv_create(cmd_pan,PANEL_MESSAGE,
		      XV_Y,xv_row(cmd_pan,row),
		      XV_X,xv_col(cmd_pan,12),
		      PANEL_LABEL_STRING,"Time",
			    0);
      row++;
      for(i=0;i<NOHIGHSCORES;i++) {
	    if(highscore[level][size][i].score>=0){
		  if(pos==i)
			(void)xv_create(cmd_pan,PANEL_MESSAGE,
					XV_Y,xv_row(cmd_pan,row),
					XV_X,xv_col(cmd_pan,1),
					PANEL_LABEL_STRING,"->",
					0);
		  (void)xv_create(cmd_pan,PANEL_MESSAGE,
				  XV_Y,xv_row(cmd_pan,row),
				  XV_X,xv_col(cmd_pan,3),
				  PANEL_LABEL_STRING,highscore[level][size][i].name,
				  0);
		  scr=highscore[level][size][i].score;
		  sprintf(str,"%d.%2.2d",scr/60,scr%60);
		  (void)xv_create(cmd_pan,PANEL_MESSAGE,
				  XV_Y,xv_row(cmd_pan,row),
				  XV_X,xv_col(cmd_pan,12),
				  PANEL_LABEL_STRING,str,
				  0);
	    }
	    row++;
      }
      window_fit(cmd_pan);
      window_fit(cframe);
      xv_set(cframe,WIN_SHOW,TRUE,0);
}

int update_highscore()
{
      int i,j;
      char name[30];
      struct passwd *passwd_entry;

      stop();

      if( getlogin() )
	    strcpy(name,getlogin());
      else{
	    passwd_entry = getpwuid( geteuid() );
	    strcpy( name, passwd_entry->pw_name );
      }
      read_highscore();
      i=0;
      while( highscore[level][size][i].score <= sec &&
	    highscore[level][size][i].score != -1) i++;
      if(i<NOHIGHSCORES){
	    for(j=NOHIGHSCORES-1;j>i;j--){
		  highscore[level][size][j].score=highscore[level][size][j-1].score;
		  strcpy(highscore[level][size][j].name,highscore[level][size][j-1].name);
	    }
	    highscore[level][size][i].score=sec;
	    strcpy(highscore[level][size][i].name,name);
	    write_highscore();
	    return i;
      }
      else
	    return -1;
}

void read_highscore()
{
      FILE *fp;
      int i=0,j,k;
      int fs=2;
      int lvl;
      int sze;
      highscoretype hgsc;
	    

      for(k=0;k<5;k++)
	    for(j=0;j<3;j++)
		  for(i=0;i<NOHIGHSCORES;i++)
			highscore[k][j][i].score=-1;
      if((fp=fopen(HIGHSCOREFILE,"r"))==NULL)
	    return;
      fs=fscanf(fp,"%s %d %d %d %f",hgsc.name,
		&hgsc.score,&lvl,&sze);
      if(fs!=EOF){
	    do{
		  i=0;
		  while(highscore[lvl][sze][i].score>=0	
			&& i<NOHIGHSCORES) i++;
		  highscore[lvl][sze][i].score=hgsc.score;
		  strcpy(highscore[lvl][sze][i].name,hgsc.name);	
		  fs=fscanf(fp,"%s %d %d %d",hgsc.name,
			    &hgsc.score,&lvl,&sze);
	    } while(fs!=EOF);
      }
      fclose(fp);
}

void write_highscore()
{
      int i,j,k;
      FILE *fp;

      i=0;
      while((fp=fopen(HIGHSCOREFILE,"w"))==NULL && i<3 ){
	    sleep(1);
	    i++;
      }
      if(fp==NULL){
	    xv_set(frame,FRAME_LEFT_FOOTER,"Cannot open Highscore file",0);
	    return;
      }
      for(i=0;i<5;i++)
	    for(j=0;j<3;j++){
		  k=0;
		  while(highscore[i][j][k].score>=0 && k<NOHIGHSCORES){
			fprintf(fp,"%s %d %d %d\n",highscore[i][j][k].name,
				highscore[i][j][k].score,i,j);
			k++;
		  }
	    }
      fclose(fp);
}

main(argc,argv)
    int argc;
    char **argv;
{
      int i,j,k;

      xv_init(XV_INIT_ARGC_PTR_ARGV, &argc, argv, NULL);
      for(k=0;k<5;k++)
	    for(j=0;j<3;j++)
		  for(i=0;i<NOHIGHSCORES;i++)
			highscore[k][j][i].score=-1;
      seed=(long)getpid();
      srand48(seed);
      area_width=10;
      area_height=10;
      size=0;
      level=2;
      no_shown_squares=0;
      sec=0;
      bang_shown=0;
      init_board();
      game_started=2;
      init_windows();
      exit(0);
}

