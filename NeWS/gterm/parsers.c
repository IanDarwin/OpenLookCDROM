/*  parsers.c  --  parsers for custom termcap entrys
**		by Hugh Daniel 91/2/11 <hugh@toad.com>
*/

#ifndef lint
static	char sccsid[] =
	"@(#)";
static	char RCSid[] =
	"@(#)$Header: /it/grass/gterm/RCS/parsers.c,v 1.4 1991/04/23 06:52:53 hugh Grass2 $";
#endif

#include	<stdio.h>
#include	<ctype.h>
#include	<sys/types.h>
#include	<ref/config.h>
#include	"termcap.h"

/*#define DEBUG*/
#ifdef DEBUG
#define err0(A) fprintf(stderr,A)
#define err1(A,B) fprintf(stderr,A,B)
#define err2(A,B,C) fprintf(stderr,A,B,C)
#define err3(A,B,C,D) fprintf(stderr,A,B,C,D)
#else
#define err0(A)
#define err1(A,B)
#define err2(A,B,C)
#define err3(A,B,C,D)
#endif

#define ARGLENGTH 5	/* The number of characters in an argument */

/* WARNING:  These defines are possion dependent on an array declared at
**	     the bottom of tcap_ops.c.
*/

#define T_AL T[0]
#define T_DL T[1]
#define T_DC T[2]
#define T_DO T[3]
#define T_IC T[4]
#define T_LE T[5]
#define T_RI T[6]
#define T_UP T[7]
#define T_al T[8]
#define T_am T[9]
#define T_bc T[10]
#define T_bl T[11]
#define T_cd T[12]
#define T_ce T[13]
#define T_cl T[14]
#define T_cm T[15]
#define T_co T[16]
#define T_cr T[17]
#define T_cs T[18]
#define T_dc T[19]
#define T_dl T[20]
#define T_do T[21]
#define T_ei T[22]
#define T_el T[23]
#define T_ke T[24]
#define T_ks T[25]
#define T_ho T[26]
#define T_ic T[27]
#define T_im T[28]
#define T_le T[29]
#define T_li T[30]
#define T_ll T[31]
#define T_lm T[32]
#define T_mb T[33]
#define T_md T[34]
#define T_me T[35]
#define T_mr T[36]
#define T_nd T[37]
#define T_nl T[38]
#define T_pc T[39]
#define T_rc T[40]
#define T_sc T[41]
#define T_se T[42]
#define T_sf T[43]
#define T_sl T[44]
#define T_so T[45]
#define T_sr T[46]
#define T_ta T[47]
#define T_te T[48]
#define T_ti T[49]
#define T_ue T[50]
#define T_up T[51]
#define T_us T[52]
#define T_vb T[53]
#define T_xn T[54]
#define T_show T[55]

extern int PageFull;
extern struct tcap T[];



 /*  Process the incoming count of characters, returning the unprocessed
 **    number of characters.
 **  The idea behind simple is that there is no state, each character
 **    speaks for its self!
 */
 int
tc_display_dumb( cp, n)
  register u_char *cp;
  int n;
{
  register int count = 0;
  struct tcap *tp;

    /* */
    cp[n] = '\0';	/* Warning: this might mess somthing up??? */
    while (n > 0 && !PageFull) {
	    if( (cp[count] >= ' ') && (cp[count] <= '~')) {
		    register int subcount = count;
		    while(((cp[subcount]) >= ' ') &&
			  ((cp[subcount]) <= '~')) ++subcount;
		    T_show.t_text = (char *)&cp[count];
		    T_show.t_size = subcount - count;
		    (T_show.t_op)(&T_show);
		    n -= subcount - count;
		    count += subcount - count;
	    } else {
		    switch ( cp[count]) {
			  case '\000':	/* nul */
			  case '\001':	/* nul */
			  case '\002':	/* nul */
			  case '\003':	/* nul */
			  case '\004':	/* nul */
			  case '\005':	/* nul */
			  case '\006':	/* nul */
			  case '\007':	/* bel */
			    break;
			  case '\010':	/* bs  */
			    (T_bc.t_op)(&T_bc); break;
			  case '\011':	/* ht  */
			    (T_ta.t_op)(&T_ta); break;
			  case '\012':	/* nl  */
			    (T_nl.t_op)(&T_nl); break;
			  case '\013':	/* vt  */
			  case '\014':	/* nul */
			    (T_cl.t_op)(&T_cl); break;
			  case '\015':	/* cr  */
			    (T_cr.t_op)(&T_cr); break;
			  case '\016':	/* nul */
			  case '\017':	/* nul */
			  case '\020':	/* nul */
			  case '\021':	/* nul */
			  case '\022':	/* nul */
			  case '\023':	/* nul */
			  case '\024':	/* nul */
			  case '\025':	/* nul */
			  case '\026':	/* nul */
			  case '\027':	/* nul */
			  case '\030':	/* nul */
			  case '\031':	/* nul */
			  case '\032':	/* nul */
			  case '\033':	/* nul */
			  case '\034':	/* nul */
			  case '\035':	/* nul */
			  case '\036':	/* nul */
			  case '\037':	/* nul */
			  case '\177':	/* del */
			    break;
			  default:	/* The whole of printable ASCII */
			    fprintf(stderr,
				    "gterm: unknowen control character\n");
		    }
		    --n;
		    ++count;
	    }
    }
  return (n);
}

 /*
 */
 int
tc_display_psterm( cp, n)
  register u_char *cp;
  int n;
{
  register int count = 0;
  struct tcap *tp;
  /* Limets us to about 9999 lines... */
  static char *argindex, arg1[ARGLENGTH], arg2[ARGLENGTH];
  static enum { BASE_STATE, ESC_STATE,
		  N_STATE, O_STATE,
		  E1_STATE, E2_STATE,
		  CM1_STATE, CM2_STATE
	       } state;

  /*   Heres a small state machine!  We allways process one character
  ** per pass except for long blocks of ASCII printable characters.
  ** I am trusting the compiler to build nice jump tables for all the
  ** switch statements.
  */
  cp[n] = '\0';	/* Warning: this might mess somthing up??? */
  while (n > 0 && !PageFull) {
	  switch (state) {
		case CM1_STATE:
		  switch( cp[count] ) {
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
			  if( argindex - arg1 > ARGLENGTH)
			    state = BASE_STATE;
			  else 
			    *argindex++ = cp[count];
			  break;
			case ';':
			  argindex = arg2;
			  state = CM2_STATE;
			  break;
			default:
			  state = BASE_STATE;
		  };
		  --n; ++count;
		  break;
		case CM2_STATE:
		  switch( cp[count]) {
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
			  if( argindex - arg2 > ARGLENGTH)
			    state = BASE_STATE;
			  else
			    *argindex++ = cp[count];
			  break;
			case ';':
			  err2("cm %d;%d; ", atoi(arg1), atoi(arg2));
			  T_cm.t_x = atoi(arg2); T_cm.t_y = atoi(arg1);
			  (T_cm.t_op)(&T_cm);
			  state = BASE_STATE;
			  break;
			default:
			  state = BASE_STATE;
		  };
		  --n; ++count;
		  break;
		case E1_STATE:
		  switch( cp[count] ) {
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
			  if( argindex - arg1 > ARGLENGTH)
			    state = BASE_STATE;
			  else 
			    *argindex++ = cp[count];
			  break;
			case ';':
			  argindex = arg2;
			  state = E2_STATE;
			  break;
			default:
			  state = BASE_STATE;
		  };
		  --n; ++count;
		  break;
		case E2_STATE:
		  switch( cp[count] ) {
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
			  if( argindex - arg2 > ARGLENGTH)
			    state = BASE_STATE;
			  else
			    *argindex++ = cp[count];
			  break;
			case ';':
			  /* FIXUP: Do the work! */
			  err2("cs %d;%d; ", atoi(arg1), atoi(arg2));
			  T_cs.t_x = atoi(arg2); T_cs.t_y = atoi(arg1);
			  (T_cs.t_op)(&T_cs);
			  state = BASE_STATE;
			  break;
			default:
			  state = BASE_STATE;
		  };
		  --n; ++count;
		  break;
		case O_STATE:
		  switch( cp[count] ) {
			case 'i':
			  (T_im.t_op)(&T_im); state = BASE_STATE; break;
			case 'b':
			  (T_mb.t_op)(&T_mb); state = BASE_STATE; break;
			case 'd':
			  (T_md.t_op)(&T_md); state = BASE_STATE; break;
			case 'r':
			  (T_mr.t_op)(&T_mr); state = BASE_STATE; break;
			case 'l': /* both sl & ts termcap entrys... */
			  (T_sl.t_op)(&T_sl); state = BASE_STATE; break;
			case 'o':
			  (T_so.t_op)(&T_so); state = BASE_STATE; break;
			case 't':
			  (T_ti.t_op)(&T_ti); state = BASE_STATE; break;
			case 'u':
			  (T_us.t_op)(&T_us); state = BASE_STATE; break;
			default:
			  state = BASE_STATE;
		  };
		  --n; ++count;
		  break;
		case N_STATE:
		  switch( cp[count] ) {
			case 'i':
			  (T_ei.t_op)(&T_ei); state = BASE_STATE; break;
			case 'l': /* both el & fs entrys */
			  (T_el.t_op)(&T_el); state = BASE_STATE; break;
			case '*': /* _rs, _is & _me entrys */
			  (T_me.t_op)(&T_me); state = BASE_STATE; break;
			case 'o':
			  (T_se.t_op)(&T_se); state = BASE_STATE; break;
			case 't':
			  (T_te.t_op)(&T_te); state = BASE_STATE; break;
			case 'u':
			  (T_ue.t_op)(&T_ue); state = BASE_STATE; break;
			default:
			  state = BASE_STATE;
		  };
		  --n; ++count;
		  break;
		case ESC_STATE:
		  switch( cp[count] ) {
			case 'A':
			  (T_al.t_op)(&T_al); state = BASE_STATE; break;
			case 'B':
			  (T_cd.t_op)(&T_cd); state = BASE_STATE; break;
			case 'C':
			  (T_ce.t_op)(&T_ce); state = BASE_STATE; break;
			case 'E': /* Deeper we go... */
			  state = E1_STATE;
			  strncpy(arg1, "", ARGLENGTH);
			  strncpy(arg2, "", ARGLENGTH);
			  argindex = arg1;
			  break;
			case 'F':
			  (T_dc.t_op)(&T_dc); state = BASE_STATE; break;
			case 'K':
			  (T_dl.t_op)(&T_dl); state = BASE_STATE; break;
			case 'N': /* Deeper we go... */
			  state = N_STATE; break;
			case 'O': /* Deeper we go... */
			  state = O_STATE; break;
			case 'P':
			  (T_do.t_op)(&T_do); state = BASE_STATE; break;
			case 'R':
			  (T_ho.t_op)(&T_ho); state = BASE_STATE; break;
			case 'T':
			  (T_le.t_op)(&T_le); state = BASE_STATE; break;
			case 'U':
			  (T_ll.t_op)(&T_ll); state = BASE_STATE; break;
			case 'V':
			  (T_nd.t_op)(&T_nd); state = BASE_STATE; break;
			case 'W':
			  (T_sf.t_op)(&T_sf); state = BASE_STATE; break;
			case 'X':
			  (T_sr.t_op)(&T_sr); state = BASE_STATE; break;
			case 'Y':
			  (T_up.t_op)(&T_up); state = BASE_STATE; break;
			case 'Z':
			  (T_vb.t_op)(&T_vb); state = BASE_STATE; break;
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
			  /* Its part of a cm expantion */
			  state = CM1_STATE;
			  argindex = arg1;
			  strncpy(arg1, "", ARGLENGTH);
			  strncpy(arg2, "", ARGLENGTH);
			  *argindex++ = cp[count];
			  break;
			case ';':
			  argindex = arg2;
			  strncpy(arg1, "", ARGLENGTH);
			  strncpy(arg2, "", ARGLENGTH);
			  state = CM2_STATE;
			  break;
			default:	/* Unknown sequence */
			  state = BASE_STATE;
		  };
		  --n; ++count;
		  break;
		case BASE_STATE:
		  {
			  register int subcount = count;
			  while((cp[subcount] >= ' ') &&
				(cp[subcount] <= '~')) ++subcount;
			  if ( subcount > count ) {
				  T_show.t_text = (char *)&cp[count];
				  T_show.t_size = subcount - count;
				  (T_show.t_op)(&T_show);
				  n -= subcount - count;
				  count += subcount - count;
			  } else {
				  switch ( cp[count] ) {
#ifdef why_bother
					case '\000':	/* nul */
					case '\001':	/* nul */
					case '\002':	/* nul */
					case '\003':	/* nul */
					case '\004':	/* nul */
					case '\005':	/* nul */
					case '\006':	/* nul */
#endif
					case '\007':	/* bel */
					  (T_bl.t_op)(&T_bl); break;
					case '\010':	/* bs  */
					  (T_bc.t_op)(&T_bc); break;
					case '\011':	/* ht  */
					  (T_ta.t_op)(&T_ta); break;
					case '\012':	/* nl  */
					  (T_nl.t_op)(&T_nl); break;
					case '\013':	/* vt  */
					case '\014':	/* np  */
					  (T_cl.t_op)(&T_cl); break;
					case '\015':	/* cr  */
					  (T_cr.t_op)(&T_cr); break;
#ifdef why_bother
					case '\016':	/* nul */
					case '\017':	/* nul */
					case '\020':	/* nul */
					case '\021':	/* nul */
					case '\022':	/* nul */
					case '\023':	/* nul */
					case '\024':	/* nul */
					case '\025':	/* nul */
					case '\026':	/* nul */
					case '\027':	/* nul */
					case '\030':	/* nul */
					case '\031':	/* nul */
					case '\032':	/* nul */
#endif
					case '\033':	/* esc */
					  state = ESC_STATE;
					  break;
					case '\034':	/* fs  */
					  (T_rc.t_op)(&T_rc); break;
					case '\035':	/* fs  */
					  (T_sc.t_op)(&T_sc); break;
#ifdef why_bother
					case '\036':	/* nul */
					case '\037':	/* nul */
					case '\177':	/* del */
					  break;
					default:	/* Whole of printable ASCII */
					  fprintf(stderr,
					       "gterm: unknowen control character\n");
#endif
				  };
				  --n; ++count;
			  };
		  }
	  };
  };
  return (n);
}
