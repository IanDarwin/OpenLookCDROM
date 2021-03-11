/* Copyright 1992 by the Andrew Consortium and Carnegie Mellon University. All rights reserved. */

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


/* this should match the atomname table in atoms.c */
#define	atoms_ATK		    0
#define	atoms_INCOMING		    1
#define	atoms_TARGETS		    2
#define	atoms_TIMESTAMP		    3
#define	atoms_MULTIPLE		    4
#define	atoms_TEXT		    5
#define	atoms_INCR		    6
#define	atoms_CLIPBOARD		    7
#define	atoms_LENGTH		    8
#define	atoms_WM_CHANGE_STATE	    9
#define	atoms_DROP_PROTOCOL	    10
#define	atoms_HOST_FILE_NAME	    11
#define	atoms_ATK_SHADES	    12
#define atoms_WM_PROTOCOLS	    13
#define atoms_WM_DELETE_WINDOW	    14

/* Support for IXI's X.Desktop drag and drop protocol. */
#define DROP_PROTOCOL "IXI_DROP_PROTOCOL"
#define HOST_FILE_NAME "HOST_AND_FILE_NAME"
/* static Atom drop_protocol = None;
  static Atom host_filelist = None; */

extern Atom *xim_SetupAtoms();
