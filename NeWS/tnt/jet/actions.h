
/*
 *
 * Copyright (c) 1991 by Sun Microsystems, Inc.
 *
 *
 * This file is a product of Sun Microsystems, Inc. and is provided for
 * unrestricted use provided that this legend is included on all tape
 * media and as a part of the software program in whole or part.  Users
 * may copy or modify this file without charge, but are not authorized to
 * license or distribute it to anyone else except as part of a product
 * or program developed by the user.
 * 
 * THIS FILE IS PROVIDED AS IS WITH NO WARRANTIES OF ANY KIND INCLUDING THE
 * WARRANTIES OF DESIGN, MERCHANTIBILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE, OR ARISING FROM A COURSE OF DEALING, USAGE OR TRADE PRACTICE.
 * 
 * This file is provided with no support and without any obligation on the
 * part of Sun Microsystems, Inc. to assist in its use, correction,
 * modification or enhancement.
 * 
 * SUN MICROSYSTEMS, INC. SHALL HAVE NO LIABILITY WITH RESPECT TO THE
 * INFRINGEMENT OF COPYRIGHTS, TRADE SECRETS OR ANY PATENTS BY THIS FILE
 * OR ANY PART THEREOF.
 * 
 * In no event will Sun Microsystems, Inc. be liable for any lost revenue
 * or profits or other special, indirect and consequential damages, even
 * if Sun has been advised of the possibility of such damages.
 * 
 * Sun Microsystems, Inc.
 * 2550 Garcia Avenue
 * Mountain View, California  94043
 */


#define VT52 2
#define DECCOLM 3
#define INSERT 4
#define DECSCNM 5
#define DECOM 6
#define DECAWM 7
#define DECARM 8
#define LNM 20

#define CHAR_NULL   '\000'
#define CHAR_ENQ    '\005'
#define CHAR_BELL   '\007'
#define CHAR_BS     '\010'
#define CHAR_TAB    '\011'
#define CHAR_LF     '\012'
#define CHAR_VT     '\013'
#define CHAR_FF     '\014'
#define CHAR_CR     '\015'
#define CHAR_SO     '\016'
#define CHAR_SI     '\017'
#define CHAR_CAN    '\030'
#define CHAR_SUB    '\032'
#define CHAR_ESC    '\033'

enum {
  action_unknown_escape = 0,
  action_nop,
  action_state_trans,
  action_enq,
  action_put_char,
  action_flush_display,
  action_newline, /* Do a newline depending on current newline mode */
  action_return,  /* Beginning of line! */
  action_set_arg, /* set argument */
  action_new_arg, /* New argument */
  action_bell, /* Ring the bell */
  action_tab, /* Move to next tab stop */
  action_error, /* ERROR */
  action_il,  /* insert line */
  action_dl,  /* delete line */
  action_ic,  /* insert char */
  action_dc,  /* delete char */
  action_bs, /* cursor backward */
  action_cub, /* cursor backward */
  action_cud, /* cursor down */
  action_cuf, /* cursor forward */
  action_cup, /* cursor position */
  action_cuu, /* cursor up */
  action_da,  /* device attributes */
  action_decaln, /* screen alignment display */
  action_decdhl_top, /* Double height line */
  action_decdhl_bottom, /* Double height line */
  action_decdwl, /* double width line */
  action_decid, /* identify terminal */
  action_deckpam, /* keypad application mode */
  action_deckpnm, /* keypad numeric mode */
  action_decll, /* Load LEDS */
  action_decrc, /* Restore Cursor */
  action_decreqtparm, /* Request terminal paramters */
  action_decsc, /* Save cursor */
  action_decstbm, /* Set top and bottom margins */
  action_decswl, /* single width line */
  action_dsr,    /* Device status report */
  action_ed,     /* Erase Display */
  action_el,     /* Erase line */
  action_hts,    /* Horizontal tab set */
  action_ind,    /* index (move down one line with scroll) */
  action_nel,    /* new line */
  action_ri,     /* Reverse index */
  action_ris,    /* Reset terminal */
  action_rm,     /* Reset Mode */
  action_scs,    /* Select Character Set */
  action_sgr,    /* Select Graphics Redition */
  action_sm,     /* Set Mode */
  action_tbc,     /* Clear tabulation */
  action_home,   /* Go home */
  action_dca_arg, /* Direct cursor access argument */
  action_dca, /* Direct cursor access setup */
  action_ansi,

  action_set_label,
  action_build_label,
  action_setup_label,
  action_setup_icon_label
  } action_types;

