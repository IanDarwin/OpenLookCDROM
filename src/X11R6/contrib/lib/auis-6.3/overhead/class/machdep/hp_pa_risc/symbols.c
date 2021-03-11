/*
 *      Copyright BellSouth 1991 - All Rights Reserved
 *      For full copyright information see:'andrew/config/COPYRITE.bls'.
 */

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

#ifndef NORCSID
#define NORCSID
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/overhead/class/machdep/hp_pa_risc/RCS/symbols.c,v 1.4 1992/12/15 20:59:27 rr2b R6tape $";
#endif


/* Symbols that are to be shared between static and dynamically
 * loaded code (for dynld) */

/* These are the globals from the ATK class runtime system */
extern class_Error;
extern class_ErrorReturnValue;

extern class_EnterInfo();
extern class_GetEText();
extern class_IsLoaded();
extern class_IsType();
extern class_IsTypeByName();
extern class_Load();
extern class_Lookup();
extern class_NewObject();
extern class_PrependClassPath();
extern class_SetClassPath();

extern strncpy();
extern scandir();
extern alphasort();
extern etext;
extern _sigpause();
extern sigpause();
extern __sigpause();

#include <stdio.h>

extern FILE __iob[];
extern char *_stdbuf, *_lastbuf, *_sibuf, *_smbuf, *_sobuf;
extern char **environ, **_environ;
extern int errno, errnet, _errnet;
extern char *brk(), *__brk(), *sbrk(), *_sbrk(); 

int define_symbols()
{
    int status;

    status = _define_symbol("__iob", (char *)__iob);
    status += _define_symbol("_bufendtab", (char *)&_bufendtab);
    status += _define_symbol("_stdbuf", (char *)&_stdbuf);
    status += _define_symbol("_lastbuf", (char *)&_lastbuf);
    status += _define_symbol("_sibuf", (char *)&_sibuf);
    status += _define_symbol("_smbuf", (char *)&_smbuf);
    status += _define_symbol("_sobuf", (char *)&_sobuf);
    status += _define_symbol("environ", (char *)&environ);
    status += _define_symbol("_environ", (char *)&_environ);
    status += _define_symbol("errno", (char *)&errno);
    status += _define_symbol("errnet", (char *)&errnet);
    status += _define_symbol("_errnet", (char *)&_errnet);
    status += _define_symbol("brk", (char *)brk);
    status += _define_symbol("__brk", (char *)__brk);
    status += _define_symbol("sbrk", (char *)sbrk);
    status += _define_symbol("_sbrk", (char *)_sbrk);

    status += _define_symbol("class_Error", (char *) &class_Error);
    status += _define_symbol("class_ErrorReturnValue", (char *) &class_ErrorReturnValue);
    status += _define_symbol("class_EnterInfo", (char *) class_EnterInfo);
    status += _define_symbol("class_GetEText", (char *) class_GetEText);
    status += _define_symbol("class_IsLoaded", (char *) class_IsLoaded);
    status += _define_symbol("class_IsType", (char *) class_IsType);
    status += _define_symbol("class_IsTypeByName", (char *) class_IsTypeByName);
    status += _define_symbol("class_Load", (char *) class_Load);
    status += _define_symbol("class_Lookup", (char *) class_Lookup);
    status += _define_symbol("class_NewObject", (char *) class_NewObject);
    status += _define_symbol("class_PrependClassPath", (char *) class_PrependClassPath);
    status += _define_symbol("class_SetClassPath", (char *) class_SetClassPath);

    status += _define_symbol("strncpy", (char *) strncpy);
    status += _define_symbol("scandir", (char *) scandir);
    status += _define_symbol("alphasort", (char *) alphasort);

    status += _define_symbol("etext", (char *) &etext);

/*    status += _define_symbol("_sigpause", (char *) _sigpause);
*/
    status += _define_symbol("sigpause", (char *) sigpause);
    status += _define_symbol("__sigpause", (char *) __sigpause);

    return status;
}
