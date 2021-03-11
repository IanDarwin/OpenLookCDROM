

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
#define SPRAYMAX 8845

struct spraytimeval {
	u_int sec;
	u_int usec;
};
typedef struct spraytimeval spraytimeval;
bool_t xdr_spraytimeval();


struct spraycumul {
	u_int counter;
	spraytimeval clock;
};
typedef struct spraycumul spraycumul;
bool_t xdr_spraycumul();


typedef struct {
	u_int sprayarr_len;
	char *sprayarr_val;
} sprayarr;
bool_t xdr_sprayarr();


#define SPRAYPROG ((u_long)100012)
#define SPRAYVERS ((u_long)1)
#define SPRAYPROC_SPRAY ((u_long)1)
extern void *sprayproc_spray_1();
#define SPRAYPROC_GET ((u_long)2)
extern spraycumul *sprayproc_get_1();
#define SPRAYPROC_CLEAR ((u_long)3)
extern void *sprayproc_clear_1();

