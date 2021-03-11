#ifndef EXTERNS_H
#define EXTERNS_H

/*
 * Copyright (c) 1994    Daniel Williams
 * 
 * The X Consortium, and any party obtaining a copy of these files from
 * the X Consortium, directly or indirectly, is granted, free of charge,
 * a full and unrestricted irrevocable, world-wide, paid up,
 * royalty-free, nonexclusive right and license to deal in this software
 * and documentation files (the "Software"), including without limitation
 * the rights to use, copy, modify, merge, publish, distribute,
 * sublicense, and/or sell copies of the Software, and to permit persons
 * who receive copies from any such party to do so.  This license
 * includes without limitation a license to do the foregoing actions
 * under any patents of the party supplying this software to the X
 * Consortium.  The following conditions apply:
 * 
 * The above copyright notice and this permission notice shall be
 * included in all copies or substantial portions of the Software.
 * 
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
 * IN NO EVENT SHALL DANIEL WILLIAMS OR SYSTEMS & SCIENTIFIC SOFTWARE BE
 * LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
 * OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
 * WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

#ifndef lint
static char rcsid_externs_h[] = "externs.h,v 2.0 1994/05/19 02:01:05 dan Exp";
#endif

extern int main (int argc, char *argv[]);
extern void process_both_files (char *file1, char *name1, char *file2, char *name2);
extern void process_left_file (char *file1, char *name1);
extern void process_right_file (char *file2, char *name2);
extern void toggle_open_sensitive (Boolean sensitive);
extern void toggle_openlr_sensitive (Boolean sensitive);
extern void free_diff_info (DiffInfo *di);
extern DiffInfo *blank_diff_info (void);
extern DiffInfo *build_diff_info (char *prog, char *args, char *path1, char *path2);
extern int max (int i, int j);
extern int min (int i, int j);
extern int copy_to_file (FILE *fin, char *name);
extern void set_cursor (Widget w);
extern void reset_cursor (Widget w);
extern Widget get_top_shell (Widget w);
extern int file_tests (Widget w, char *filename);
extern void werror (Widget parent, char *title, char *msg1, char *msg2);
extern void werror_long (Widget parent, char *title, char **lines, int numlines);
extern void open_both_files (Widget parent, char *namel, char *namer);
extern void open_left_file (Widget parent, char *name);
extern void open_right_file (Widget parent, char *name);
extern void save_file (Widget parent, Block *b, char *name);
extern FILE *spawn_diff (char *prog, char *args, char *path1, char *path2);
extern void show_manual_page (Widget parent);
extern int modal_question (Widget parent, char *title, char *question);
extern void add_editres (Widget shell);
extern void turn_off_sash_traversal (Widget pane);
extern void show_legend (Widget parent);
extern void show_context (Widget parent);

#endif
