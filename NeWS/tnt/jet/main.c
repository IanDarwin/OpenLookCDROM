#ident "@(#)main.c 1.30 91/09/14"

/*
 *
 * Copyright (c) 1991 by Sun Microsystems, Inc.
 *
 *
 * This file is a product of Sun Microsystems, Inc. and is provided for
 * unrestricted use provided that this legend is included on all tape
 * media and as a part of the software program in whole or part.  Users
 * may copy or modify this file without charge, but are not authorized to
 * license or distribute it to anyone else except as part of a product
 * or program developed by the user.
 * 
 * THIS FILE IS PROVIDED AS IS WITH NO WARRANTIES OF ANY KIND INCLUDING THE
 * WARRANTIES OF DESIGN, MERCHANTIBILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE, OR ARISING FROM A COURSE OF DEALING, USAGE OR TRADE PRACTICE.
 * 
 * This file is provided with no support and without any obligation on the
 * part of Sun Microsystems, Inc. to assist in its use, correction,
 * modification or enhancement.
 * 
 * SUN MICROSYSTEMS, INC. SHALL HAVE NO LIABILITY WITH RESPECT TO THE
 * INFRINGEMENT OF COPYRIGHTS, TRADE SECRETS OR ANY PATENTS BY THIS FILE
 * OR ANY PART THEREOF.
 * 
 * In no event will Sun Microsystems, Inc. be liable for any lost revenue
 * or profits or other special, indirect and consequential damages, even
 * if Sun has been advised of the possibility of such damages.
 * 
 * Sun Microsystems, Inc.
 * 2550 Garcia Avenue
 * Mountain View, California  94043
 *
 * Josh Siegel (siegel@sun.com)
 */

#include <sys/time.h>
#include <sys/fcntl.h>
#include <sys/file.h>
#include <string.h>
#include <memory.h>
#include <signal.h>

#ifdef OW_V2
#include <NeWS/wire.h>
#else
#include <wire/wire.h>
#endif

#include "term.h"
#include "patchlevel.h"

#if defined(__STDC__) || defined(__cplusplus)
#define allocate_tag(psname, cproc, data) {	\
    int tag = wire_AllocateTags(1);		\
    ps_set_tag_c(psname, tag);			\
    wire_RegisterTag(tag, cproc, data);		\
}
#else
#define allocate_tag(psname, cproc, data) {	\
    int tag = wire_AllocateTags(1);		\
    ps_set_tag_c(psname, tag);			\
    wire_RegisterTag(tag, cproc, data);		\
}
#endif

extern void damage_handler();
extern int  damagestart_tag;

struct jetargs {
  char *login_present;
  char *background_present;
  char *console_present;
  char *utmp_present;
  char *per_present;

  char *font;
  
  int  rows, cols;
  char commands[1024];
} jargs;

extern void send_args();
extern char dev_path[];

FILE *master_fp;
int master_fd;
char *progname;

static wire_Wire ps_land;

static int persistent_x1, persistent_x2, persistent_y1, persistent_y2;

static int display = -1;
static int server = -1;
static char host[64];
static char disp[64];
extern char *getenv(), *strchr();

void
change_head_handler(tag, data)
int tag;
caddr_t data;
{
  display = wire_ReadInt();
  ps_close_connection_c();
  sprintf(disp, "DISPLAY=%s:%d.%d", host, server, display);
  putenv(disp);
}

static
void moved_handler(tag, data)
int tag;
caddr_t data;
{
  persistent_x1 = wire_ReadInt();
  persistent_y1 = wire_ReadInt();
}


static
void persistent_handler(tag, data)
int tag;
caddr_t data;
{
  persistent_x1 = wire_ReadInt();
  persistent_y1 = wire_ReadInt();
  persistent_x2 = wire_ReadInt();
  persistent_y2 = wire_ReadInt();
}

void
quit_handler(foo)
int foo;
{
  wire_Close(ps_land);

  if (jargs.utmp_present == FALSE)
    clear_utmp();

  exit(0);
}


struct  {
  int scrollbar_side;
  char font[128];
  int font_size;
  int scrollmode;
  char term_id[128];
  int retained;
  int autoscale;
  int columns;
  int saved_lines;
} props = { 0, "Courier", 12, 3, "NeWS JET Term", 0, 0, 80, 256};

static void
load_props() {
  FILE *xd, *fopen();
  char buff[256], *home;
  int l;

  home = (char *) getenv("HOME");

  if (home == NULL)
    strcpy(buff,".Xdefaults");
  else
    sprintf(buff,"%s/.Xdefaults", home);

  xd = fopen(buff,"r");

  if (xd == NULL) 
    return;

  while(fgets(buff, sizeof(buff), xd) != NULL) {
    if (!strncmp(buff,"OpenWindows.",  12)) {
      if (!strncmp(&buff[12],"ScrollbarPlacement:", 19)) {
	if (!strncmp(&buff[32], "left", 4))
	  props.scrollbar_side = 0;
	else
	  props.scrollbar_side = 1;
      } else
      if (!strncmp(&buff[12],"Ansi.Font:", 10)) {
	l = strlen(&buff[23]) - 1;
	strncpy(props.font, &buff[23], l);
	props.font[l] = '\0';
      } else
      if (!strncmp(&buff[12],"Ansi.Font_Size:", 15)) {
	props.font_size = atoi(&buff[28]);
      } else
      if (!strncmp(&buff[12],"Ansi.Scroll_Mode:", 17)) {
	if (!strncmp(&buff[30], "supersmooth", 11))
	  props.scrollmode = 0;
	if (!strncmp(&buff[30], "smooth", 6))
	  props.scrollmode = 1;
	if (!strncmp(&buff[30], "jump", 4))
	  props.scrollmode = 2;
	if (!strncmp(&buff[30], "superjump", 9))
	  props.scrollmode = 3;
	scrolling_method = props.scrollmode;
      } else
      if (!strncmp(&buff[12],"Ansi.Terminal_Id:", 17)) {
	l = strlen(&buff[30]) - 1;
	strncpy(props.term_id, &buff[30], l);
	props.term_id[l] = '\0';
      } else
      if (!strncmp(&buff[12],"Ansi.Retained:", 14)) {
	if (!strncmp(&buff[27], "True", 4))
	  props.retained = 1;
	else
	  props.retained = 0;
      } else
      if (!strncmp(&buff[12],"Ansi.AutoScale:", 15)) {
	if (!strncmp(&buff[28], "True", 4))
	  props.autoscale = 1;
	else
	  props.autoscale = 0;
      } else
      if (!strncmp(&buff[12],"Ansi.Columns:", 13)) {
	props.columns = atoi(&buff[26]);
      } else
      if (!strncmp(&buff[12],"Ansi.Saved_Lines:", 17)) {
	props.saved_lines = atoi(&buff[30]);
      }
    }
  }
  fclose(xd);
}

static int
argument_handler(argc, argv, values)
int argc;
char *argv[]; 
char *values;
{
  int i;

  for (i = 0 ; i < argc; i++) {
    strcat(jargs.commands, " ");
    strcat(jargs.commands, argv[i]);
  }
    
  return(argc);
}

static int
define_handler(argc, argv, values)
int argc;
char *argv[]; 
char *values;
{
  putenv(argv[1]);

  return(0);
}


main(argc, argv)
int argc;
char *argv[];
{
  extern char *optarg;
  extern int optind, opterr;

  jargs.rows = 24;
  jargs.cols = 80;

  progname = argv[0];

  wire_AddOption("ARGUMENT", 
		 NULL, argument_handler, 
		 "[optional arguments...]");

  wire_AddOption("-D %s",
		 (caddr_t) NULL, define_handler,
		 "-D foo=bar   (define a environmental var)");

  wire_AddOption("-C %-",
		 (caddr_t) &jargs.console_present, NULL,
		 "-C - Run terminal as a console");
  
  wire_AddOption("-b %-",
		 (caddr_t) &jargs.background_present, NULL,
		 "-b - Run in background");
  
  wire_AddOption("-l %-",
		 (caddr_t) &jargs.login_present, NULL,
		 "-l - Run login shell");
  
  wire_AddOption("-u %-",
		 (caddr_t) &jargs.utmp_present, NULL,
		 "-u - Do not set utmp entry for terminal");
  
  wire_AddOption("-p %-",
		 (caddr_t) &jargs.per_present, NULL,
		 "-p - Run persistent terminal emulator");

  wire_AddOption("-c %d",
		 (caddr_t) &jargs.cols, NULL,
		 "-c columns");
  wire_AddOption("-width %d",
		 (caddr_t) &jargs.cols, NULL,
		 "-width columns");
  wire_AddOption("-Ww %d",
		 (caddr_t) &jargs.cols, NULL,
		 "-Wh columns");

  wire_AddOption("-r %d",
		 (caddr_t) &jargs.rows, NULL,
		 "-r Rows");
  wire_AddOption("-height %d",
		 (caddr_t) &jargs.rows, NULL,
		 "-height Rows");
  wire_AddOption("-Wh %d",
		 (caddr_t) &jargs.rows, NULL,
		 "-Wh Rows");

  wire_AddOption("-Wt %s", (caddr_t) &jargs.font, NULL, NULL);
  wire_AddOption("-fn %s", (caddr_t) &jargs.font, NULL, NULL);
  wire_AddOption("-font %s",
		 (caddr_t) &jargs.font, NULL,
		 "-font name - specify font name for window text");

  if (!wire_ParseArguments(argc, argv)) {
    exit(1);
  }

  if (jargs.background_present)
    if(fork())
      exit(0);

  master_fd=get_ptys();

  close(0);
  dup2(open(dev_path, O_RDWR), 0);

  if (jargs.utmp_present == FALSE)
    fill_out_utmp();

  if(master_fd < 0) {
    printf("Can't get a pty!\n");
    exit(0);
  }

  (void) load_pty();

  init_parse();
  (void) init_word_syntax();

  if (jargs.commands[0] != '\0')
    spawn_shell(jargs.commands,  (int) jargs.console_present, (int) jargs.login_present);
  else
    spawn_shell(NULL, (int ) jargs.console_present, (int )jargs.login_present);

  init_term(jargs.cols, jargs.rows);
  pty_set_size(master_fd, jargs.cols, jargs.rows);

  master_fp = fdopen(master_fd, "r+");
  if (master_fp == NULL) {
    perror("fdopen");
    exit(0);
  }

/* I want it to close down nice! */

  signal(SIGCHLD, quit_handler);
  signal(SIGINT, quit_handler);
  signal(SIGQUIT, quit_handler);
  signal(SIGHUP, quit_handler);
  signal(SIGTERM, quit_handler);

  if (build_wire() < 0) {
    perror("build_wire");
    exit(0);
  }

  while(1) {
    wire_EnterNotifier();

    if (jargs.per_present == FALSE)
      exit(0);

    while (build_wire() < 0)
      sleep(2);
    tnt_reshape_win_c(persistent_x1, persistent_y1, persistent_x2, persistent_y2);
  }
}

static int prev_con = -1;

static void
setup_display()
{
  char *d, *p;
  char buff[255];

  d = getenv("DISPLAY");
  if (d != NULL) {
    strcpy(buff, d);
    d = buff;

    p = strchr(d, ':');
    *p++ = '\0';
    server = atoi(p);
    strcpy(host, d);
    p = strchr(p, '.');
    if (p != NULL) {
      *p++ = '\0';
      display = atoi(p);
    } else
      display = 0;
  } else {
    server = 0;
    display = 0;
    strcpy(host, "unix");
  }
}

int
build_wire()
{
  int ret;

  if (prev_con >= 0) {
    dup2(0, prev_con);
  }

  wire_ReserveTags(2);

  setup_display();

  ps_land = wire_Open(wire_Display());

  if (ps_land == wire_INVALID_WIRE) {
    return(-1);
  }

  if (prev_con >= 0) 
    close(prev_con);

  prev_con = psio_fileno(PostScript);

  wire_Problems(ps_land, 
	 ps_death, 
		wire_DiseaseDefault, 
                     wire_UnknownTagDefault);

  allocate_tag("reshape_tag",reshape_handler,0);
  allocate_tag("set_mode_tag",set_mode_handler,0);
  allocate_tag("quit_tag",quit_handler,0);
  allocate_tag("text_size_tag",text_size_handler,0);
  allocate_tag("selection_start_tag",selection_start_handler,0);
  allocate_tag("selection_motion_tag",selection_motion_handler,0);
  allocate_tag("selection_stop_tag",selection_stop_handler,0);
  allocate_tag("deselect_tag",invert_off,0);
  allocate_tag("string_tag",string_handler,0);
  allocate_tag("update_tag",update_handler,0);
  allocate_tag("save_defaults_tag",save_defaults_handler,0);
  allocate_tag("reset_tag",reset_handler,0);
  allocate_tag("apply_tag",apply_handler,0);
  allocate_tag("ready_tag",ready_handler,0);
  allocate_tag("persistent_tag",persistent_handler,0);
  allocate_tag("find_tag",find_handler,0);
  allocate_tag("keyboard_tag",keyboard_handler,0);
  allocate_tag("change_head_tag",change_head_handler,0);
  allocate_tag("moved_tag",moved_handler,0);
  allocate_tag("damage_tag", damage_handler, 0);

  damagestart_tag = wire_AllocateTags(1);
  ps_set_tag_c("damagestart_tag", damagestart_tag);

  enter_package_c(&ret);

  if (ret == 0) {
    define_package_c();

    enter_package_c(&ret);

    if (ret == 0) {
      printf("Unable to access PostScript code!\n");
      exit(0);
    }
  }

  send_args();

  load_props();

  if (jargs.font != 0) 
    strcpy(props.font, jargs.font);

  set_saved_lines(props.saved_lines);

  ps_init_ps_c(props.font, props.font_size, 
	       props.retained, props.scrollbar_side,
	       props.autoscale, props.columns, 
	       jargs.cols, jargs.rows);

  ps_flush_PostScript();
  return(0);
}

void ready_handler(tag,data)
int tag;
caddr_t data;
{
  wire_AddFileHandler(master_fp, input_handler, 0);
}

void ps_death(tag, data) 
int tag;
caddr_t data;
{
  wire_ExitNotifier();
}

void
keyboard_handler(tag, caddr)
int tag;
caddr_t caddr;
{
  unsigned char c;

  c = wire_ReadInt();

  if(write(master_fd, &c, 1) != 1) {
    perror("write");
    ps_death(0, 0);
  }

  if (c == '\r' && flags.lnm) {
    c = '\n';
    write(master_fd, &c, 1);
  }

  if (scrolling) {
    ps_scroll_bottom_c();
    finish_scrolling();
  }
}

void
string_handler(tag, data)
int tag;
caddr_t data;
{
  char buff[65536], *c;
  int len;

  c = wire_ReadString(buff);
  len = strlen(c);

  if(write(master_fd, buff, len) != len) {
    perror("write");
    ps_death(0, 0);
  }

  if (scrolling) {
    ps_scroll_bottom_c();
    finish_scrolling();
  }
}

void
reset_handler(tag, data)
int tag;
caddr_t data;
{
  load_props();

  ps_set_defaults_c(props.font, props.font_size, 
		    props.retained, props.scrollmode, 
		    props.scrollbar_side, props.term_id,
		    props.autoscale, props.columns, props.saved_lines);
}

void
apply_handler(tag, data)
int tag;
caddr_t data;
{
  ps_apply_c();
}

void
save_defaults_handler(tag, data)
int tag;
caddr_t data;
{
  FILE *xd_in, *xd_out, *fopen();
  char main_file[256], backup_file[256], *home;
  char buff[256];

  wire_ReadString(props.font);
  props.font_size = wire_ReadInt();
  props.retained = wire_ReadInt();
  props.scrollmode = wire_ReadInt();
  props.scrollbar_side = wire_ReadInt();
  wire_ReadString(props.term_id);
  props.autoscale = wire_ReadInt();
  props.columns = wire_ReadInt();
  props.saved_lines = wire_ReadInt();

  home = (char *) getenv("HOME");

  if (home == NULL) {
    strcpy(main_file,".Xdefaults");
    strcpy(backup_file,".Xdefaults.BAK");
  } else {
    sprintf(main_file,"%s/.Xdefaults", home);
    sprintf(backup_file,"%s/.Xdefaults.BAK", home);
  }

  unlink(backup_file);

  if (rename(main_file, backup_file) != 0) {
    perror(main_file);
    return;
  }
  xd_in = fopen(backup_file,"r");
  xd_out = fopen(main_file,"w");

  while(fgets(buff, sizeof(buff), xd_in) != NULL) {
    if (!strncmp(buff,"OpenWindows.",  12)) {
      if (!strncmp(&buff[12],"Ansi", 4))
	continue;
    }
    fputs(buff, xd_out);
  }
  fprintf(xd_out, "OpenWindows.ScrollbarPlacement:\t%s\n", props.scrollbar_side == 0 ?
	  "left" : "right");
  fprintf(xd_out, "OpenWindows.Ansi.Font:\t%s\n", props.font);
  fprintf(xd_out, "OpenWindows.Ansi.Font_Size:\t%d\n", props.font_size);
  fprintf(xd_out, "OpenWindows.Ansi.Scroll_Mode:\t");
  switch(props.scrollmode) {
  case 0:
    fprintf(xd_out, "supersmooth\n");
    break;
  case 1:
    fprintf(xd_out, "smooth\n");
    break;
  case 2:
    fprintf(xd_out, "jump\n");
    break;
  case 3:
    fprintf(xd_out, "superjump\n");
    break;
  }
  fprintf(xd_out, "OpenWindows.Ansi.Terminal_Id:\t%s\n", props.term_id);
  fprintf(xd_out, "OpenWindows.Ansi.Retained:\t%s\n", props.retained == 0 ?
	  "False" : "True");
  fprintf(xd_out, "OpenWindows.Ansi.AutoScale:\t%s\n", props.autoscale == 0 ?
	  "False" : "True");
  fprintf(xd_out, "OpenWindows.Ansi.Columns:\t%d\n", props.columns);
  fprintf(xd_out, "OpenWindows.Ansi.Saved_Lines:\t%d\n", props.saved_lines);
  fclose(xd_out);
  fclose(xd_in);
}

void
send_args()
{
    wire_ExecuteArguments();

    ps_namedictbegin_c("jetargs");

    if (jargs.console_present) {
      ps_namebooleandef_c("console", jargs.console_present);
    }

    if (jargs.login_present) {
      ps_namebooleandef_c("login", jargs.login_present);
    }

    if (jargs.background_present) {
      ps_namebooleandef_c("background", jargs.background_present);
    }

    if (jargs.utmp_present) {
      ps_namebooleandef_c("utmp", jargs.utmp_present);
    }

    if (jargs.commands[0]) {
      ps_namestringdef_c("commands", jargs.commands);
    }

    ps_dictenddef_c();
    ps_flush_PostScript_c();
}
