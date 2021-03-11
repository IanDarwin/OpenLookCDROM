/* Copyright 1993 Carnegie Mellon University All rights reserved.
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
#ifndef ATKOS_H
#define ATKOS_H 1

#include <stdio.h> /* needed so that we can declare Andrew_tmpfile. */
 
#ifdef NEED_ANSI_TMPFILES
extern char *Andrew_tmpnam();
extern FILE *Andrew_tmpfile();

#define tmpnam(buf) Andrew_tmpnam(buf)
#define tmpfile() Andrew_tmpfile()

/* the following #defines may be overridden in the system.h file */
#ifndef P_tmpdir
#define P_tmpdir "/tmp/"
#endif
#ifndef L_tmpnam
#define L_tmpnam (sizeof(P_tmpdir)+15)
#endif
#ifndef TMP_MAX
#define TMP_MAX 52 /* maybe this should be larger, this is a conservative guess. */
#endif
#endif /* NEED_ANSI_TMPFILES */

#endif /* ATKOS_H */
