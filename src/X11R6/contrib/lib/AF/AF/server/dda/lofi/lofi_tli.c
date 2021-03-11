/*
 * Copyright 1993 by Digital Equipment Corporation, Maynard, Massachusetts.
 * 
 * Permission to use, copy, modify, distribute, and sell this software and its 
 * documentation for any purpose is hereby granted without fee, provided that 
 * the above copyright notice appear in all copies and that both that 
 * copyright notice and this permission notice appear in supporting 
 * documentation, and that the name of Digital not be used in advertising or 
 * publicity pertaining to distribution of the software without specific, 
 * written prior permission.  Digital makes no representations about the 
 * suitability of this software for any purpose.  It is provided "as is" 
 * without express or implied warranty.
 * 
 * DIGITAL DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING 
 * ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL 
 * DIGITAL BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY 
 * DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN 
 * AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF 
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 */

#include	<io/tc/lofi_reg.h>
#include	<io/tc/lofi.h>
#include	"codec.h"
#include	"lofi_io.h"
#include	"lofi_tli.h"

#define	LIMIT(low, val, high) \
	  if (val < (low)) val = (low); \
	  else \
	  if ((val) > (high)) val = (high);

void 
lofi_tli_offhook(lofi) 
struct lofi_info *lofi;
{
  LoFiSetCSR(lofi->us_reg, FHS, 1);
  msleep(lofi->us_reg, 400);
}

void 
lofi_tli_onhook(lofi) 
struct lofi_info *lofi;
{
  LoFiSetCSR(lofi->us_reg, FHS, 0);
}

/*
 * Perform a switch hook flash.  The duration of the flash
 * hook is specified in milliseconds.
 */
void 
lofi_tli_flash(lofi, flashtime)
struct lofi_info *lofi;
int	flashtime;	/* ms */
{

	LoFiSetCSR(lofi->us_reg, FHS, 0);
	msleep(lofi->us_reg, flashtime);
	LoFiSetCSR(lofi->us_reg, FHS, 1);
}

/*
 * Pulse dial a digit.
 * This fails if the
 * phone is ONHOOK!
 */
lofi_tli_pulse(lofi, c)
struct lofi_info *lofi;
char	    c;
{
    struct lofi_status_reg *csrp;
    CARD32   csr;
    register int	npulses;
    

    /* fail if in on-hook condition */
    csr = LoFiReadCSR(lofi->us_reg);
    csrp = (struct lofi_status_reg *) &csr;
    if(! csrp->s_hs){
/*	printf("currently onhook\n"); */
	return(-1);
    }

    if ((npulses = c-'0') == 0)
	    npulses = 10;
    while (npulses--) {
	    lofi_tli_flash(lofi, PULSE_BREAK);
	    if (npulses != 0)
		    msleep(lofi->us_reg, PULSE_MAKE);
	    else
		    msleep(lofi->us_reg, PULSE_ID);
    }
    return (1);
}
	

int lofi_dial(struct lofi_info *lofi, char *number)
{
    int		    status;
    char	    *digit;
    char	    c;
    int		    dialmode;
    int		    flashtime;
    struct lofi_status_reg *csrp;
    CARD32   csr;
    int		    pickup=0;

    digit = number;
    dialmode  = DIAL_TONE;
    
    /* pickup if not already off-hook */
    csr = LoFiReadCSR(lofi->us_reg);
    csrp = (struct lofi_status_reg *) &csr;
    if(! csrp->s_hs){	/* Currently onhook. */
	LoFiSetCSR(lofi->us_reg, FHS, 1);
	msleep(lofi->us_reg, 900);
	pickup = 1;
    }

    /* dial the number */
    status = -1;
    while(*digit != '\0'){
	switch((int)(c= *digit++)){
	/* SWITCH TO TONEDIAL	*/
	case 'T':
		dialmode = DIAL_TONE;
		break;

	/* SWITCH TO PULSEDIAL	*/
	case 'P':
		dialmode = DIAL_PULSE;
		break;

	/* WAIT FOR A DIALTONE 	*/
	case 'W':
		/*NYI*/
		break;

	/* WAIT FOR COMPLETION 	*/
	case 'X':
		/*NYI*/
		break;

	/* DIALING DELAY 	*/
	case '!':
		msleep(lofi->us_reg, 950);
		break;

	/* SWITCHHOOK FLASH 	*/
	case '^':
		flashtime = 0;	/* Each "^" is 250 ms.	*/
		while (*digit == '^') {
			++digit;
			flashtime += 250;
		}
		lofi_tli_flash(lofi, flashtime);
		break;

	/* DIAL A DIGIT		*/
	case '0':
	case '1':
	case '2':
	case '3':
	case '4':
	case '5':
	case '6':
	case '7':
	case '8':
	case '9':
	case '*':
	case '#':
	case 'A':
	case 'B':
	case 'C':
	case 'D':
		if (dialmode == DIAL_TONE)
			status = codec_dtmf(lofi, CODEC_PRIMARY, CODEC_EAR, c, DTMF_ON);
		else 
		    status = lofi_tli_pulse(lofi, c);
		if (status == -1)
			goto out;
		break;

	default:
		status = -1;
		goto out;
	}
    }
out:
    if ((status == -1) && (pickup == 1)) {
	LoFiSetCSR(lofi->us_reg, FHS, 0);
    }
    return(status);
}
