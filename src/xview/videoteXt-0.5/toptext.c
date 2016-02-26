/*
 * toptext.c: Routines to handle TOP-Text extensions
 *
 * Copyright (c) 1994-95 Martin Buck  <martin.buck@student.uni-ulm.de>
 * Read COPYING for more information
 *
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <linux/vtx.h>
#include "vtx_assert.h"
#include "safe_malloc.h"
#include "misc.h"
#include "vtxtools.h"
#include "vtxdecode.h"
#include "toptext.h"


typedef struct {
  int pgnum, hour, minute, type, timeout;
} pending_t;
typedef struct adip_s {
  int pgnum, dcode;
  char text[13];
  struct adip_s *next;
} adip_t;


int toptext_ok;

static byte_t basictop[800], multipage[800];
static signed char toptable[800];
static adip_t *adip[800];
static int toptable_ok = FALSE, adip_found = FALSE, multipage_found = FALSE;
static int basictop_timeout, pending_count;



static int
get_basictop(pending_t *pending, const byte_t *buffer, const vtx_pageinfo_t *pginf) {
  int pos, val, digit;

  for (pos = 0; pos <= 799; pos++) {
    if ((val = vtx_chkhamming(buffer[pos])) < 0) {
      return FALSE;
    }
    basictop[pos] = val;
  }
  toptable_ok = FALSE;
  memset(pending, 0, sizeof(pending_t) * 10);
  for (pos = 0; pos <= 9; pos++) {
    for (digit = 0; digit <= 7; digit++) {
      if (vtx_chkhamming(buffer[20 * 40 + pos * 8 + digit]) < 0) {
        return FALSE;
      }
    }
    if ((val = vtx_chkhamming(buffer[20 * 40 + pos * 8])) == 0xe)
      continue;
    if (val == 0xf)
      break;
    pending[pos].pgnum = val * 0x100 + 0x10 * vtx_chkhamming(buffer[20 * 40 + pos * 8 + 1]) +
        vtx_chkhamming(buffer[20 * 40 + pos * 8 + 2]);
    pending[pos].hour = 0x10 * vtx_chkhamming(buffer[20 * 40 + pos * 8 + 3]) +
        vtx_chkhamming(buffer[20 * 40 + pos * 8 + 4]);
    pending[pos].minute = 0x10 * vtx_chkhamming(buffer[20 * 40 + pos * 8 + 5]) +
        vtx_chkhamming(buffer[20 * 40 + pos * 8 + 6]);
    val = vtx_chkhamming(buffer[20 * 40 + pos * 8 + 7]);
    if (val >= 1 && val <= 2) {
      pending[pos].type = val;
    } else {
      printf("WARNING: Unimplemented TOP-Page type %d for page %X\n", val, pending[pos].pgnum);
    }
  }
  return TRUE;
}


static int
get_multipage(const byte_t *buffer, const vtx_pageinfo_t *pginf) {
  int pos, val;
  
  for (pos = 0; pos <= 799; pos++) {
    if ((val = vtx_chkhamming(buffer[pos])) < 0) {
      return FALSE;
    }
    multipage[pos] = val;
  }
  multipage_found = TRUE;
  return TRUE;
}


static int
get_adip(const byte_t *buffer, const vtx_pageinfo_t *pginf) {
  byte_t chr;
  int entry, digit, pgnum, alnum_found;

  for (entry = 0; entry <= 43; entry++) {
    if (vtx_chkhamming(buffer[entry * 20]) == 0xe)
      continue;
    if (vtx_chkhamming(buffer[entry * 20]) == 0xf)
      break;
    for (digit = 0; digit <=7; digit++) {
      if (vtx_chkhamming(buffer[entry * 20 + digit]) < 0) {
        return FALSE;
      }
    }
  }
  for (entry = 0; entry <= 43; entry++) {
    if (vtx_chkhamming(buffer[entry * 20]) == 0xe)
      continue;
    if (vtx_chkhamming(buffer[entry * 20]) == 0xf)
      break;
    pgnum = 0x100 * vtx_chkhamming(buffer[entry * 20]) +
        0x10 * vtx_chkhamming(buffer[entry * 20 + 1]) + vtx_chkhamming(buffer[entry * 20 + 2]);
    if (vtx_chkpgnum(pgnum, FALSE)) {
      pgnum = vtx_hex2dec(pgnum) - 100;
      adip[pgnum] = scalloc(1, sizeof(adip_t));
      adip[pgnum]->pgnum = pgnum;
      adip[pgnum]->dcode = vtx_chkhamming(buffer[entry * 20 + 7]);
      alnum_found = FALSE;
      for (digit = 11; digit >= 0; digit--) {
        chr = buffer[entry * 20 + 8 + digit];
        if (!vtx_chkparity(&chr) || chr < ' ') {
          chr = ' ';
        }
        chr = vtx2iso_table[cct2vtx_table[pginf->hamming ? 0 : pginf->charset][chr - 32]];
        if (chr == ' ' && !alnum_found) {
          adip[pgnum]->text[digit] = '\0';
        } else {
          adip[pgnum]->text[digit] = chr;
          alnum_found = TRUE;
        }
      }
    }
  }
  adip_found = TRUE;
  return TRUE;
}


int
toptext_newpage(const byte_t *buffer, const vtx_pageinfo_t *pginf, topreq_cb getpage) {
  static pending_t pending[10];
  int pos;

  if (pginf->pagenum == 0x1f0) {
    if (!pginf->serial) {
      printf("WARNING: Parallel mode not implemented, can't use TOP-Text.\n");
      return FALSE;
    }
    pending_count = 0;
    if (!get_basictop(pending, buffer, pginf)) {
      if (basictop_timeout < 4) {
        basictop_timeout++;
        (*getpage)(0x1f0, -1, -1);
      }
      return TRUE;
    }
    for (pos = 0; pos <= 9; pos++) {
      if (pending[pos].type) {
        pending_count++;
        (*getpage)(pending[pos].pgnum, (pending[pos].hour ? pending[pos].hour : -1),
            (pending[pos].minute ? pending[pos].minute : -1));
      }
    }
    return TRUE;
  } else if (pending_count) {
    int pgfound = 0, status;
    
    for (pos = 0; pos <= 9; pos++) {
      if (pending[pos].type && pending[pos].pgnum == pginf->pagenum &&
          pending[pos].hour == pginf->hour && pending[pos].minute == pginf->minute) {
        status = TRUE;
        switch (pending[pos].type) {
          case 1:
            status = get_multipage(buffer, pginf);
          break;
          case 2:
            status = get_adip(buffer, pginf);
          break;
          default:
            assert(0);
          break;
        }
        if (!status) {
          if (pending[pos].timeout < 4) {
            pending[pos].timeout++;
            (*getpage)(pending[pos].pgnum, (pending[pos].hour ? pending[pos].hour : -1),
                (pending[pos].minute ? pending[pos].minute : -1));
          }
          return TRUE;
        }
        pending[pos].type = 0;
        pending_count--;
        pgfound = TRUE;
      }
    }
    if (!pending_count) {
      toptext_ok = TRUE;
    }
    return pgfound;
  }
  return FALSE;
}


int
toptext_getnext(int current, int inc, tt_pageclass_t initlevel, tt_pageclass_t maxlevel) {
  static tt_pageclass_t curr_level;
  int pgnum, pginc, found = FALSE;

  if (!toptext_ok)
    return -1;
  if (!toptable_ok) {
    for (pgnum = 0; pgnum <= 799; pgnum++) {
      switch (basictop[pgnum]) {
        case 2:
        case 4:
          toptable[pgnum] = TT_BLOCK;
        break;
        case 6:
          toptable[pgnum] = TT_GROUP;
        break;
        case 8:
        case 9:
          toptable[pgnum] = TT_NORM;
        break;
        case 3:
        case 5:
        case 7:
        case 0xa:
        case 0xb:
          toptable[pgnum] = TT_SUBPG;
        break;
        default:
          toptable[pgnum] = 0;
        break;
      }
    }
    toptable_ok = TRUE;
    curr_level = initlevel;
  }
  if (!inc)
    inc = 1;
  pginc = SIGN(inc);
  inc = abs(inc);
  pgnum = current = (vtx_hex2dec(current) - 100) % 800;
  while (1) {
    if (curr_level > maxlevel)
      return -1;
    do {
      if (toptable[pgnum] == curr_level) {
        if (!inc) {
          toptable[pgnum] = -toptable[pgnum];
          return vtx_dec2hex(pgnum) + 0x100;
        }
        found = TRUE;
      }
      if (inc && toptable[pgnum] &&
          ((abs(toptable[pgnum]) < TT_SUBPG && curr_level < TT_SUBPG) ||
          (abs(toptable[pgnum]) == TT_SUBPG && curr_level == TT_SUBPG))) {
        inc--;
      }
      pgnum += pginc;
      while (pgnum < 0)
        pgnum += 800;
      while (pgnum >= 800)
        pgnum -= 800;
    } while (pgnum != current || found);
    curr_level++;
  }
}


void
toptext_reset(void) {
  int entry;
  
  for (entry = 0; entry <= 799; entry++) {
    if (adip[entry]) {
      free(adip[entry]);
      adip[entry] = NULL;
    }
  }
  toptext_ok = FALSE;
  adip_found = FALSE;
  multipage_found = FALSE;
  basictop_timeout = 0;
  pending_count = 0;
}


int
toptext_numsubpg(int page) {
  int btop_entry;

  if (!toptext_ok)
    return -1;
  page = vtx_hex2dec(page) - 100;
  btop_entry = basictop[page];
  switch (btop_entry) {
    case 0:
    case 0xc:
    case 0xd:
    case 0xe:
    case 0xf:
      return 0;
    case 3:
    case 5:
    case 7:
    case 0xa:
    case 0xb:
      if (!multipage_found || (multipage[page] < 2 || multipage[page] > 9))
        return -1;
      return multipage[page];
    default:
      return 1;
  }
}


static int
findblockpg(int pgnum) {
  pgnum = vtx_hex2dec(pgnum);
  do {
    if (basictop[pgnum - 100] >= 2 && basictop[pgnum - 100] <= 5) {
      return vtx_dec2hex(pgnum);
    }
    pgnum--;
  } while (pgnum >= 100);
  return 0;
}


static int
findgrppg(int pgnum) {
  pgnum = vtx_hex2dec(pgnum);
  do {
    if (basictop[pgnum - 100] == 6 || basictop[pgnum - 100] == 7) {
      return vtx_dec2hex(pgnum);
    }
    if (basictop[pgnum - 100] >= 2 && basictop[pgnum - 100] <= 5) {
      return 0;
    }
    pgnum--;
  } while (pgnum >= 100);
  return 0;
}


int
toptext_nextblkpg(int pgnum) {
  int current;
  
  if (!toptext_ok)
    return 0;
  current = pgnum = (vtx_hex2dec(pgnum) - 99) % 800;
  do {
    if (basictop[current] >= 2 && basictop[current] <= 5) {
      return vtx_dec2hex(current) + 0x100;
    }
    current = (current + 1) % 800;
  } while (current != pgnum);
  return 0;
}


int
toptext_nextgrppg(int pgnum) {
  int loop, current;
  
  if (!toptext_ok)
    return 0;
  pgnum = vtx_hex2dec(pgnum) - 100;
  current = pgnum + 1;
  for (loop = 0; loop <= 1; loop++) {
    for (; current <= 799; current++) {
      if (basictop[current] == 6 || basictop[current] == 7)
        return vtx_dec2hex(current) + 0x100;
      if (basictop[current] >= 2 && basictop[current] <= 5) {
        if (!loop)
          break;
        return vtx_dec2hex(current) + 0x100;
      }
    }
    current = vtx_hex2dec(findblockpg(vtx_dec2hex(pgnum + 100)));
    if (!current)
      return 0;
    current -= 100;
  }
  return 0;
}


int
toptext_pginc(int page) {
  int current;
  
  if (!toptext_ok)
    return 0;
  current = page = (vtx_hex2dec(page) - 99) % 800;
  do {
    if (basictop[current] >= 1 && basictop[current] <= 0xb) {
      return vtx_dec2hex(current) + 0x100;
    }
    current = (current + 1) % 800;
  } while (current != page);
  return 0;
}


int
toptext_pgdec(int page) {
  int current;
  
  if (!toptext_ok)
    return 0;
  current = page = (vtx_hex2dec(page) - 101) % 800;
  while (current < 0) {
    current = page += 800;
  }
  do {
    if (basictop[current] >= 1 && basictop[current] <= 0xb) {
      return vtx_dec2hex(current) + 0x100;
    }
    current--;
    while (current < 0) {
      current += 800;
    }
  } while (current != page);
  return 0;
}


static char*
getpgtext(int pgnum) {
  int btop_entry;

  if (!adip_found)
    return NULL;
  pgnum = vtx_hex2dec(pgnum) - 100;
  btop_entry = basictop[pgnum];
  if (btop_entry == 0 || btop_entry == 8 || btop_entry == 0xa || btop_entry >= 0xc) {
    return NULL;
  }
  if (!adip[pgnum]) {
    return NULL;
  }
  return adip[pgnum]->text;
}


char*
toptext_pgdesc(int page) {
  static char desc[32];
  int grppg, blockpg;
  char *pgtxt, *blocktxt = NULL;
  
  if (!toptext_ok || !adip_found)
    return NULL;
  if (!basictop[vtx_hex2dec(page) - 100] || basictop[vtx_hex2dec(page) - 100] >= 0xc) {
    strcpy(desc, "Nonexistent");
  } else {
    pgtxt = getpgtext(page);
    if (!pgtxt) {
      grppg = findgrppg(page);
      if (grppg) {
        pgtxt = getpgtext(grppg);
      }
    }
    blockpg = findblockpg(page);
    if (blockpg && blockpg != page) {
      blocktxt = getpgtext(blockpg);
    }
    strcpy(desc, "");
    if (blocktxt) {
      strcat(desc, blocktxt);
      if (pgtxt) {
        strcat(desc, "/");
      }
    }
    if (pgtxt) {
      strcat(desc, pgtxt);
    }
  }
  sprintf(desc + strlen(desc), " (%X)", page);
  return desc;
}


ttdesc_t*
toptext_mkdesctable(int *count) {
  int pgnum;
  ttdesc_t *first, **current;
  
  if (!toptext_ok)
    return 0;
  *count = 0;
  first = NULL;
  current = &first;
  for (pgnum = 100; pgnum < 899; pgnum++) {
    if (getpgtext(vtx_dec2hex(pgnum))) {
      *current = smalloc(sizeof(ttdesc_t));
      (*current)->is_block = (basictop[pgnum - 100] >= 2 && basictop[pgnum - 100] <= 5);
      strcpy((*current)->fullname, toptext_pgdesc(vtx_dec2hex(pgnum)));
      strcpy((*current)->pagename, getpgtext(vtx_dec2hex(pgnum)));
      if (!(*current)->is_block) {
        sprintf((*current)->pagename + strlen((*current)->pagename), " (%d)", pgnum);
      }
      (*current)->page = vtx_dec2hex(pgnum);
      (*current)->next = NULL;
      current = &(*current)->next;
      (*count)++;
    }
  }
  return first;
}
