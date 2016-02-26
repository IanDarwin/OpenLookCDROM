#ident "@(#)gen_parse.c	1.5 91/09/14"

#include "actions.h"
#include "term.h"

char states[8][256];
char actions[8][256];

init_parse()
{
  int i,j;

  for(i=1;i<256;i++) {
    states[0][i] = 0;
    actions[0][i] = action_put_char;
    states[5][i] = 5;
    actions[5][i] = action_dca_arg;
    states[7][i] = 7;
    actions[7][i] = action_build_label;
  }

  bzero(states[2], sizeof(states[2]));
  bzero(actions[2], sizeof(actions[2]));
  bzero(states[3], sizeof(states[3]));
  bzero(actions[3], sizeof(actions[3]));
  bzero(states[4], sizeof(states[4]));
  bzero(actions[4], sizeof(actions[4]));
  bzero(states[6], sizeof(states[2]));
  bzero(actions[6], sizeof(actions[2]));

  for (i=0; i< sizeof(states) / sizeof(states[0]); i++) {
    states[i][CHAR_NULL] = i;
    actions[i][CHAR_NULL] = action_nop;

    actions[i][CHAR_CAN] = action_state_trans;
    states[i][CHAR_CAN] = 0;

    actions[i][CHAR_SUB] = action_nop;
    states[i][CHAR_SUB] = 0;

    actions[i][CHAR_BS] = action_bs;
    states[i][CHAR_BS] = i;
  }

  actions[0][CHAR_ENQ] = action_enq;
  actions[0][CHAR_BELL] = action_bell;
  actions[0][CHAR_TAB] = action_tab;
  actions[0][CHAR_BS] = action_bs;
  actions[0][CHAR_LF] = action_newline;
  actions[0][CHAR_VT] = action_newline;
  actions[0][CHAR_FF] = action_newline;
  actions[0][CHAR_CR] = action_return;
  actions[0][CHAR_SO] = action_state_trans;
  actions[0][CHAR_SI] = action_state_trans;

  actions[0][CHAR_ESC] = action_flush_display;
  states [0][CHAR_ESC] = 1;

  for (j = 2; j<=3; j++) {
    for(i='0'; i <= '9';i++) {
      actions[j][i] = action_set_arg;
      states[j][i] = j;
    }
    
    actions[j][';'] = action_new_arg;
    states[j][';'] = j;
    actions[j][':'] = action_new_arg;
    states[j][':'] = j;
  }

  ansi_mode();

  actions[4]['A'] = action_state_trans;
  actions[4]['B'] = action_state_trans;
  actions[4]['0'] = action_state_trans;
  actions[4]['1'] = action_state_trans;
  actions[4]['2'] = action_state_trans;
  actions[4]['8'] = action_decaln;
  actions[4]['3'] = action_decdhl_top;
  actions[4]['4'] = action_decdhl_bottom;
  actions[4]['5'] = action_decswl;
  actions[4]['6'] = action_decdwl;

  actions[3]['h'] = action_sm;
  actions[3]['l'] = action_rm;

  actions[2]['?'] = action_state_trans;
  states [2]['?'] = 3;
  actions[2]['@'] = action_ic;
  actions[2]['A'] = action_cuu;
  actions[2]['B'] = action_cud;
  actions[2]['C'] = action_cuf;
  actions[2]['D'] = action_cub;
  actions[2]['H'] = action_cup;
  actions[2]['J'] = action_ed;
  actions[2]['K'] = action_el;
  actions[2]['L'] = action_il;
  actions[2]['M'] = action_dl;
  actions[2]['P'] = action_dc;
  actions[2]['c'] = action_da;
  actions[2]['f'] = action_cup;
  actions[2]['g'] = action_tbc;
  actions[2]['h'] = action_sm;
  actions[2]['l'] = action_rm;
  actions[2]['m'] = action_sgr;
  actions[2]['n'] = action_dsr;
  actions[2]['q'] = action_decll;
  actions[2]['r'] = action_decstbm;
  actions[2]['x'] = action_decreqtparm;

  actions[6]['\\'] = action_set_label;
  states[6]['\\'] = 0;

  actions[6]['l'] = action_setup_label;
  states[6]['l'] =  7;

  actions[6]['L'] = action_setup_icon_label;
  states[6]['L'] =  7;

  actions[7][CHAR_ESC] = action_state_trans;
  states[7][CHAR_ESC] = 6;
}

ansi_mode()
{
  bzero(states[1], sizeof(states[1]));
  bzero(actions[1], sizeof(actions[1]));


  states[1][CHAR_NULL] = 1;
  actions[1][CHAR_NULL] = action_nop;
  
  actions[1][CHAR_CAN] = action_nop;
  states[1][CHAR_CAN] = 0;

  actions[1][CHAR_SUB] = action_nop;
  states[1][CHAR_SUB] = 0;

  actions[1][CHAR_BS] = action_bs;
  states[1][CHAR_BS] = 1;

  actions[1]['#'] = action_state_trans;
  states [1]['#'] = 4;
  actions[1]['('] = action_state_trans;
  states [1]['('] = 4;

  actions[1][')'] = action_state_trans;

  actions[1]['='] = action_deckpam;
  actions[1]['>'] = action_deckpnm;
  actions[1]['<'] = action_state_trans;


  actions[1]['D'] = action_ind;
  actions[1]['E'] = action_nel;
  actions[1]['M'] = action_ri;
  actions[1]['8'] = action_decrc;
  actions[1]['7'] = action_decsc;

  actions[1]['c'] = action_ris; /* This is very questionable.. */
  actions[1]['H'] = action_hts;

  actions[1]['['] = action_state_trans;
  states[1]['['] = 2;

  actions[1][']'] = action_state_trans;
  states[1][']'] = 6;
}

vt52_mode()
{
  ansi_mode();

  actions[1]['A'] = action_cuu;  /* up */
  actions[1]['B'] = action_cud;  /* down */
  actions[1]['C'] = action_cuf;  /* right */
  actions[1]['D'] = action_cub;  /* left */
  actions[1]['H'] = action_home; /* Go home */
  actions[1]['I'] = action_ri;   /* Reverse line feed */
  actions[1]['J'] = action_ed;   /* Erase to end of screen */
  actions[1]['K'] = action_el;   /* Erase to end of screen */
  actions[1]['Z'] = action_decid;
  actions[1]['='] = action_deckpam;
  actions[1]['>'] = action_deckpnm;
  actions[1]['<'] = action_ansi;
  actions[1]['Y'] = action_dca;  /* Direct Cursor access */
  states[1]['Y'] = 5;

  actions[1]['F'] = action_state_trans;  /* graphics mode */
  actions[1]['G'] = action_state_trans;  /* normal mode */
}
