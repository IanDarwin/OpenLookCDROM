/* ********************************************************************** *\
 *         Copyright IBM Corporation 1988,1991 - All Rights Reserved      *
 *        For full copyright information see:'andrew/config/COPYRITE'     *
\* ********************************************************************** */

/*
	$Disclaimer: 
 * Permission to use, copy, modify, and distribute this software and its 
 * documentation for any purpose is hereby granted without fee, 
 * provided that the above copyright notice appear in all copies and that 
 * both that copyright notice, this permission notice, and the following 
 * disclaimer appear in supporting documentation, and that the names of 
 * IBM, Carnegie Mellon University, and other copyright holders, not be 
 * used in advertising or publicity pertaining to distribution of the software 
 * without specific, written prior permission.
 * 
 * IBM, CARNEGIE MELLON UNIVERSITY, AND THE OTHER COPYRIGHT HOLDERS 
 * DISCLAIM ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING 
 * ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS.  IN NO EVENT 
 * SHALL IBM, CARNEGIE MELLON UNIVERSITY, OR ANY OTHER COPYRIGHT HOLDER 
 * BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY 
 * DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, 
 * WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS 
 * ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE 
 * OF THIS SOFTWARE.
 *  $
*/


 

#ifndef AIX
/* type checking */
extern int SetIndent(/*struct rofftext *self,int u*/);
extern int SetTempIndent(/*struct rofftext *self,int u*/);
extern int Is_BOL(/*struct rofftext *self*/);
extern int SetBOL(/*struct rofftext *self*/);
extern int DestroyContext(/*IC c*/);
extern Trickle topen(/*struct rofftext *self, FILE *f, char *s*/);
extern int tclose(/*struct rofftext *self, Trickle t*/);
extern int g(/*struct rofftext *self, Trickle t*/);
extern int ung(/*struct rofftext *self,char c,Trickle t*/);
extern int tpush(/*struct rofftext *self, Trickle t, FILE *f, char *s, boolean push, int argc, char *argv[]*/);
extern int munch(/*struct rofftext *self, Trickle t*/);
extern int special(/*struct rofftext *self,Trickle t*/);
extern int setfont(/*struct rofftext *self, Trickle t*/);
extern int getwidth(/*struct rofftext *self, Trickle t*/);
extern int munchmove(/*struct rofftext *self, Trickle t*/);
extern int getname(/*struct rofftext *self, Trickle t, char *name*/);
extern char *getregister(/*struct rofftext *self, Trickle t*/);
extern int putregister(/*struct rofftext *self, char *name, double value, enum RegFmt fmt, double inc, boolean relative*/);
extern char *getstring(/*struct rofftext *self, char *name*/);
extern int putstring(/*struct rofftext *self, char *name, char *value*/);
extern int getarg(/*struct rofftext *self, Trickle t, char *buf, int n, boolean copymode*/);
extern int put(/*struct rofftext *self, char c*/);
extern int DoBreak(/*struct rofftext *self*/);
extern int get(/*struct rofftext *self, Trickle t*/);
extern int DoCommand(/*struct rofftext *self, Trickle t, char *name, boolean br*/);
extern int Scan(/*struct rofftext *self, Trickle t, char *cmd*/);
#endif /* AIX */
