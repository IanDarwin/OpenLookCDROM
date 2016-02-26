#ifndef	HEADER
#define	HEADER

/*
 * @(#)workman_ui.h	1.12 03 Jun 1995
 *
 * User interface structure declarations.
 */

extern Attr_attribute	INSTANCE;

extern Xv_opaque	track_menu_create();

typedef struct {
	Xv_opaque	window1;
	Xv_opaque	controls1;
	Xv_opaque	tracks;
	Xv_opaque	tracktimer;
	Xv_opaque	songpos;
	Xv_opaque	tracklen;
	Xv_opaque	speaker;
	Xv_opaque	repeat;
	Xv_opaque	mode;
	Xv_opaque	volume;
	Xv_opaque	shuffle;
	Xv_opaque	cdtimer;
	Xv_opaque	cdlen;
	Xv_opaque	cdgauge;
	Xv_opaque	button3;
	Xv_opaque	button2;
	Xv_opaque	button4;
	Xv_opaque	dbmenu;
} window1_objects;

extern window1_objects	*window1_objects_init();

typedef struct {
	Xv_opaque	popup1;
	Xv_opaque	controls2;
	Xv_opaque	artist;
	Xv_opaque	cdname;
	Xv_opaque	tracklist;
	Xv_opaque	trackname;
	Xv_opaque	trackoptions;
	Xv_opaque	whichvolume;
	Xv_opaque	defaultvolume;
	Xv_opaque	defaultspeaker;
	Xv_opaque	nonemsg;
	Xv_opaque	playmode;
	Xv_opaque	autoplay;
	Xv_opaque	button1;
	Xv_opaque	button8;
	Xv_opaque	buttonpl;
} popup1_objects;

extern popup1_objects	*popup1_objects_init();

typedef struct {
	Xv_opaque	about;
	Xv_opaque	controls3;
	Xv_opaque	message1;
	Xv_opaque	message2;
	Xv_opaque	message7;
	Xv_opaque	sink;
	Xv_opaque	message3;
	Xv_opaque	message4;
	Xv_opaque	message5;
	Xv_opaque	message6;
	Xv_opaque	drive;
} about_objects;

extern about_objects	*about_objects_init();

typedef struct {
	Xv_opaque	goodies;
	Xv_opaque	controls4;
	Xv_opaque	balance;
	Xv_opaque	phones;
	Xv_opaque	timemode_track;
	Xv_opaque	playnewcds;
	Xv_opaque	abrepeat;
	Xv_opaque	a;
	Xv_opaque	alabel;
	Xv_opaque	b;
	Xv_opaque	blabel;
	Xv_opaque	indexscan;
	Xv_opaque	split;
	Xv_opaque	delete;
	Xv_opaque	timemode_cd;
} goodies_objects;

extern goodies_objects	*goodies_objects_init();

typedef struct {
	Xv_opaque	plpopup;
	Xv_opaque	controls5;
	Xv_opaque	playlists;
	Xv_opaque	listname;
	Xv_opaque	button7;
	Xv_opaque	button6;
	Xv_opaque	button5;
	Xv_opaque	playlist;
	Xv_opaque	delete;
} plpopup_objects;

extern plpopup_objects	*plpopup_objects_init();

typedef struct {
	Xv_opaque	window;
	Xv_opaque	panel;

	Xv_opaque	filelist;
	Xv_opaque	personal;
	Xv_opaque	server;
	Xv_opaque	serversave;
	Xv_opaque	fuzz;

	Xv_opaque	apply;
	Xv_opaque	reset;
	Xv_opaque	dismiss;
} db_prefs_objects;

extern db_prefs_objects	*db_prefs_init();

#endif
