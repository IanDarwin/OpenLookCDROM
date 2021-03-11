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

#ifndef NORCSID
#define NORCSID
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/ams/msclients/vui/RCS/pcpnlm.c,v 2.13 1992/12/15 21:23:32 rr2b R6tape $";
#endif

/*
 *      C Panel Handling Package
 *	Hardware-dependent routines
 *	IBM-PC version
 *      Supports monochrome, color, and B/W displays via the
 *      standard PC BIOS Int 0x10 Video Interface.
 *
 */


#include <vui.h>
#ifdef IBMPC
#include <dos.h>
#endif /* IBMPC */
#include <panel.h>
#include <vuidebug.h>
#pragma check_stack -

extern unsigned char msg_row, error_row, last_row, past_col, opt_row, bar_row, cursor_row, cursor_col, *AttrTablePtr, isrtmode;

extern unsigned char Color_AttrTable[], BW_AttrTable[], EGAco_AttrTable[], Mono_AttrTable[];

PRIVATE unsigned char color_display = FALSE;
PRIVATE unsigned char cursor_emulation_on = FALSE;

KeyHit()
    {
    return(kbhit());
    }

KeyIn()
    {
    union REGS inregs, outregs;

    inregs.h.ah = 0;                        /* Get keystroke      */
    int86 (0x16, &inregs, &outregs);        /* Keyboard interrupt */
    return(outregs.x.ax);
    }

SetCursorPosn()
    {
    union REGS inregs, outregs;

    inregs.h.dh = cursor_row;
    inregs.h.dl = cursor_col;
    inregs.h.bh = 0;                        /* Page number      */
    inregs.h.ah = 2;                        /* Set cursor posn  */
    int86 (0x10, &inregs, &outregs);        /* Video interrupt  */
    }

CursorOff ()
    {
    union REGS inregs, outregs;

    inregs.h.ah = 0x01;                 /* Cursor shape     */
    inregs.x.cx = 0x2506;               /* invisible cursor */
    int86 (0x10, &inregs, &outregs);    /* Video interrupt  */
    }

CursorOn ()
    {
    union REGS inregs, outregs;

    inregs.h.ah = 0x0F;                 /* Get mode info    */
    int86 (0x10, &inregs, &outregs);    /* Video interrupt  */
    if (outregs.h.al == 7) {
        if (isrtmode == INSERT)
            inregs.x.cx = 0x070D;
        else
            inregs.x.cx = 0x0C0D;
        }
    else {
        if (isrtmode == INSERT)
            inregs.x.cx = 0x0407;
        else
            inregs.x.cx = 0x0607;
        }
    inregs.h.ah = 0x01;                 /* Cursor shape     */
    int86 (0x10, &inregs, &outregs);    /* Video interrupt  */
    }

ClearLine (line_no)
int line_no;
    {
    union REGS inregs, outregs;

    inregs.h.bh = *(AttrTablePtr+NORMAL);
    inregs.h.ch = line_no;
    inregs.h.cl = 0;
    inregs.h.dh = line_no;
    inregs.h.dl = 79;
    inregs.x.ax = 0x0600;                   /* Clear region     */
    int86 (0x10, &inregs, &outregs);        /* Video interrupt  */
    }

PRIVATE SetupDefaultVideo (change_size)
int change_size;
    {
    union REGS inregs, outregs;
    int mode;

    inregs.x.dx = 24;
    inregs.h.bh = 0;
    inregs.x.ax = 0x1130;                       /* Get screen size   */
    int86 (0x10, &inregs, &outregs);            /* Video interrupt   */
    last_row = outregs.h.dl;

    inregs.h.ah = 0x0F;                         /* Get state info    */
    int86 (0x10, &inregs, &outregs);            /* Video interrupt   */

    inregs.h.al = 0;
    switch (outregs.h.al & 0x7F) {
        default:
        case 0:                                 /* B/W 40            */
        case 5:                                 /* CGA Med Res B/W   */
            inregs.h.al = 2;
            inregs.h.ah = 0;                    /* Set mode          */
            int86 (0x10, &inregs, &outregs);    /* Video interrupt   */
	    AttrTablePtr = BW_AttrTable;
	    color_display = FALSE;
	    break;

            /*
             * Note that Set Mode is only done when absolutely
             * necessary to establish an 80 column mode
             */

        case 1:                                 /* Color 40          */
        case 4:                                 /* CGA Med Res Color */
        case 0x0D:                              /* EGA Med Res       */
	    inregs.h.al = 3;
            inregs.h.ah = 0;                    /* Set mode          */
            int86 (0x10, &inregs, &outregs);    /* Video interrupt   */

        case 3:                                 /* Color 80          */
        case 0x0E:                              /* EGA Color        */
	case 0x10:					/* EGA Hi Res	*/
	    mode = outregs.h.al & 0x7F;
            AttrTablePtr = Color_AttrTable;
            color_display = TRUE;

            inregs.h.cl = 0x0F;
            inregs.x.bx = 0xFF10;
            inregs.x.ax = 0x1200;               /* EGA detect        */
            int86 (0x10, &inregs, &outregs);    /* Video interrupt   */
            if ((outregs.h.cl < 0x0F) && (outregs.h.bh == 0) && (outregs.h.bl <= 3)) {
		AttrTablePtr = EGAco_AttrTable;
		if (!change_size) {
		    char far *emul_bit;		/* turn on cursor emulation bit to get a cursor */ 
		    FP_SEG (emul_bit) = 0x00;
		    FP_OFF (emul_bit) = 0x487;
		    *emul_bit |= 0x01;
		    cursor_emulation_on = TRUE;
		    inregs.h.ah = 0;
		    inregs.h.al = mode;
		    int86 (0x10, &inregs, &outregs);
		    inregs.x.ax = (mode==3)?0x1112:0x1123;	/* Use 8 x 8 font for 43 rows */
		    inregs.h.bl = 0;
		    inregs.h.dl = 0x2B;
		    int86 (0x10, &inregs, &outregs);
/*		    last_row = error_row = 42;  ask no. of rows instead of this */
		    inregs.h.bh = 0;
		    inregs.x.ax = 0x1130;                       /* Get screen size   */
		    int86 (0x10, &inregs, &outregs);            /* Video interrupt   */
		    last_row = outregs.h.dl;
		    CursorOn();
		}
            } else {
		inregs.h.bh = 0;
		inregs.h.bl = Color_AttrTable[NORMAL] >> 4;
		inregs.h.ah = 0x0B;                 /* Set border color  */
		int86 (0x10, &inregs, &outregs);    /* Video interrupt   */
	    }
            break;

        case 7:                                 /* Monochrome        */
            AttrTablePtr = Mono_AttrTable;
            color_display = TRUE;
            break;
        case 0x0F:                              /* EGA Mono Hi Res   */
            AttrTablePtr = Mono_AttrTable;
            color_display = TRUE;
	    inregs.x.ax = 0x1112;			/* Use 8 x 8 font for 43 rows */
	    inregs.h.bl = 0;
	    int86 (0x10, &inregs, &outregs);
	    last_row = 42;
            break;

        case 2:                                 /* B/W 80            */
        case 6:                                 /* CGA Hi Res        */
            AttrTablePtr = BW_AttrTable;
            color_display = FALSE;
            break;

        }

    error_row =	last_row;			/* set up positions */
    msg_row = last_row - 1;
    opt_row = msg_row - 1;
    bar_row = opt_row - 1;

    inregs.h.al = 0;                            /* Select page 0     */
    inregs.h.ah = 0x05;                         /* Set active page   */
    int86 (0x10, &inregs, &outregs);            /* Video interrupt   */
    }

PRIVATE DosCursor (cursor_row)
int cursor_row;
    {
    union REGS inregs, outregs;

    inregs.h.dh = cursor_row;
    inregs.h.dl = 0;
    inregs.h.bh = 0;                    /* Always use Pg 0  */
    inregs.h.ah = 2;                    /* Set cursor posn  */
    int86 (0x10, &inregs, &outregs);    /* Video interrupt  */
    isrtmode = REPLACE;
    CursorOn ();
    }

SaveVideoEnv (vp, change_size)
VIDEOPARMS *vp;
int change_size;
    {
    char far *palette_ptr;
    union REGS inregs, outregs;

    FP_SEG (palette_ptr) = 0x40;
    FP_OFF (palette_ptr) = 0x66;
    vp->palette = *palette_ptr;

    inregs.h.ah = 0x0F;                     /* State info       */
    int86 (0x10, &inregs, &outregs);        /* Video interrupt  */
    vp->page = outregs.h.bh;
    vp->mode = outregs.h.al & 0x7F;
    vp->columns = outregs.h.ah;

    inregs.h.bh = vp->page;
    inregs.h.ah = 0x08;                     /* Get char/attr    */
    int86 (0x10, &inregs, &outregs);        /* Video interrupt  */
    vp->attr = (outregs.h.ah == 0) ? 7 : outregs.h.ah;

    SetupDefaultVideo (change_size);
    }

RestoreVideoEnv (vp, clear, cursor_row)
VIDEOPARMS *vp;
int clear;
int cursor_row;
    {
    int oldmode;
    union REGS inregs, outregs;

    inregs.h.ah = 0x0F;                     /* Get state info    */
    int86 (0x10, &inregs, &outregs);        /* Video interrupt   */

    oldmode = outregs.h.al & 0x7F;

    if ((cursor_emulation_on) || (vp->mode != oldmode)) {
        inregs.h.al = vp->mode;
        inregs.h.ah = 0;                    /* Set mode         */
        int86 (0x10, &inregs, &outregs);    /* Video interrupt  */
        }

    if ((vp->mode == 3) || (vp->mode == 4) ||
        (vp->mode == 0x0D) || (vp->mode == 0x0E)) {
        inregs.h.bl = vp->palette;
        inregs.h.bh = 0;
        inregs.h.ah = 0x0B;                 /* Set palette      */
        int86 (0x10, &inregs, &outregs);    /* Video interrupt  */
        }

    if ((vp->mode < 4) || (vp->mode == 7) ||
        ((vp->mode >= 0x0D) && (vp->mode <= 0x0F))) {
        inregs.h.al = vp->page;
        inregs.h.ah = 0x05;                 /* Set active page  */
        int86 (0x10, &inregs, &outregs);    /* Video interrupt  */
        }

    if ((oldmode == 0x10) || (cursor_emulation_on)) {
	char far *emul_bit;	/* turn cursor emulation bit back off */ 
	FP_SEG (emul_bit) = 0x00;
	FP_OFF (emul_bit) = 0x487;
	*emul_bit &= 0xFE;
    }
    inregs.h.ch = (cursor_row ? cursor_row-1 : cursor_row);
    inregs.h.cl = 0;
    inregs.h.dh = last_row;
    inregs.h.dl = (clear ? vp->columns - 1 : 79);
    inregs.h.bh = vp->attr;
    inregs.x.ax = 0x0600;                   /* Clear region     */
    int86 (0x10, &inregs, &outregs);        /* Video interrupt  */

    DosCursor (cursor_row);
    }

ShowString (data, row, col, len, attr)
char *data;
unsigned char row, col, len, attr;
    {
    union REGS inregs, outregs;
    static char blanks[] = {
        ' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',
        ' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',
        ' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',
        ' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',
        ' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' '};

    if (!len)
        return;

    if ((attr == RINVIS) || (attr == INVISI) || (data == (unsigned char *)NULL))
        data = blanks;

    inregs.h.bl = *(AttrTablePtr + attr);   /* Resolve attribute*/
    inregs.h.bh = 0;                        /* Always use Pg 0  */
    inregs.x.cx = 1;                        /* Char count       */

    while (len--) {
        if (col == past_col) {
            debug((2,"Going around the horn. Row=%d, col=%d, len=%d\\n",row,col,len));
            col=0; row++;
        }
        inregs.h.dh = row;
        inregs.h.dl = col++;
        inregs.h.ah = 2;                    /* Set cursor posn  */
        int86 (0x10, &inregs, &outregs);    /* Video interrupt  */
        if (*data) {
	    if ((*data)>0 && (*data)<' ') (*data) = '.';
            inregs.h.al = *(data++);        /* Get char         */
        } else inregs.h.al = ' ';           /* If null use blank*/
        inregs.h.ah = 9;                    /* Write char       */
        int86 (0x10, &inregs, &outregs);    /* Video interrupt  */
        }

    inregs.h.dh = cursor_row;               /* Restore cursor   */
    inregs.h.dl = cursor_col;
    inregs.h.ah = 2;                        /* Set cursor posn  */
    int86 (0x10, &inregs, &outregs);        /* Video interrupt  */
    }

ErrorBeep ()
    {
    register int i;
    unsigned char save61;

    outp (0x43, 0xB6);
    outp (0x42, 0x00);
    outp (0x42, 0x17);
    save61 = inp (0x61);
    outp (0x61, save61 | 0x03);
    for (i=0; i<2500; i++);
    outp (0x61, save61);
    }

ClearScreen()
    {
    union REGS inregs, outregs;
    inregs.h.bh = *(AttrTablePtr+NORMAL);
    inregs.x.cx = 0;
    inregs.h.dh = last_row;
    inregs.h.dl = 79;
    inregs.x.ax = 0x0600;                   /* Clear region     */
    int86 (0x10, &inregs, &outregs);        /* Video interrupt  */
    }

FIELD *RedrawScreen(curfield)	/* mas V1.3 */
FIELD *curfield;
    {
    return (curfield);
    }

ShowError(msg)
PRMPT *msg;
    {
    register int i;

    ShowMsg(msg);
    ErrorBeep();
    for (i=0; i<32700; i++) ;
    ErrorBeep ();
    }

UpdateScreen()
{
}
