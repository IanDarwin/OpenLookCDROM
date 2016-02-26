/*
 * cn --C to NeWS example program
 *
 * This program shows input to a C program through a network window
 * under NeWS. Three types of input are illustrated: mouse (scaled to
 * window co-ordinates), menu (returns int), and keyboard (keyboard
 * input is edited and buffered and echoed in the NeWS server. The
 * input string is "flushed" to the C client in a single packet
 * on a <CR>).
 *
 * David A. LaVallee Sun Microsystems, Inc.
 * cps cn.cps;cc cn.c -o cn -I$NEWSHOME/include -L$NEWSHOME/lib -lcps
 * cps cn.cps;cc cn.c -o cn.4 -I$NEWSHOME/include -L$NEWSHOME/lib-sparc -lcps
 */

#include	"cn.h"

int     mouse_x, mouse_y, menu_index;
char	kbstring[300];

main() {
	start_window();
	window_input_loop();
	exit(0);
}

start_window() {
	ps_open_PostScript();
	ps_initialize();
	ps_flush_PostScript();
}

window_input_loop() {
	printf("starting input loop\n");
	while (!psio_eof(PostScript)) {
		if (ps_kbinput(kbstring)) {
			printf("%s\n", kbstring);
		}
		else if (ps_mouse_click(&mouse_x, &mouse_y)) {
                        printf("Mouse at %d, %d\n", mouse_x, mouse_y);
		}
                else if (ps_menu_choice(&menu_index)) {
                        printf("Menu index %d\n", menu_index);
		}
		else
			exit(-1);
	};
}

