/* Copyright 1993,1994 Carnegie Mellon University All rights reserved.
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
 /* This provides the hp_lockf and hp_unlockf functions that
   translate lockf requests into the appropriate fcntl calls. )Rob */

#include <fcntl.h>

int hp_lockf(fd)
int fd; {
    struct flock f_rec;

    f_rec.l_type = F_WRLCK;
    f_rec.l_whence = 0;
    f_rec.l_start = 0;
    f_rec.l_len = 0;
    return((fcntl(fd, F_SETLK, &f_rec) !=-1)?0:-1);
}

int hp_unlockf(fd)
int fd; {
    struct flock f_rec;

    f_rec.l_type = F_UNLCK;
    f_rec.l_whence = 0;
    f_rec.l_start = 0;
    f_rec.l_len = 0;
    return((fcntl(fd, F_SETLK, &f_rec) != -1)?0:-1);
}
