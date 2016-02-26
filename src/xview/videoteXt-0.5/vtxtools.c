/*
 * vtxtools.c: Misceallaneous routines for VideoteXt (parity checking, handling of hexadecimal
 *             page-numbers)
 *
 * Copyright (c) 1994-95 Martin Buck  <martin.buck@student.uni-ulm.de>
 * Read COPYING for more information
 *
 */

#include <linux/vtx.h>
#include "misc.h"
#include "vtxtools.h"


static byte_t parity_table[256];
static int init_done;



static void
tools_init(void) {
  int pos, val, bit;

  for (pos = 0; pos <= 255; pos++) {	/* Set up parity_table: If (parity_table[foo] & 0x80), */
    bit = 0;				/* foo has odd number of bits set */
    val = pos;
    while (val) {		/* Count number of set bits in val; see K&R, Exercise 2-9 */
      bit ^= 0x80;
      val &= val - 1;
    }
    parity_table[pos] = bit | 0x7f;
  }
  /* parity_table is also used for hamming decoding: If (parity_table[foo] & 0x40), foo has
   * more than one bit-error and can't be corrected; otherwise the correct(ed) value is
   * parity_table[foo] & 0xf
   */
  for (pos = 0; pos <= 15; pos++) {
    val = ( !(pos & 1) ^ !!(pos & 4) ^ !!(pos & 8)) << 0 | !!(pos & 1) << 1 |
          ( !(pos & 1) ^ !!(pos & 2) ^ !!(pos & 8)) << 2 | !!(pos & 2) << 3 |
          ( !(pos & 1) ^ !!(pos & 2) ^ !!(pos & 4)) << 4 | !!(pos & 4) << 5 |
          (!!(pos & 2) ^ !!(pos & 4) ^ !!(pos & 8)) << 6 | !!(pos & 8) << 7;
    for (bit = 0; bit <= 8; bit++) {
      parity_table[val ^ ((1 << bit) & 0xff)] &= (0x80 | pos);
    }
  }
  init_done = TRUE;
}


/* Check parity of *val (parity bit is bit 7, odd parity)
 * Clear bit 7 of *val if parity OK & return TRUE, FALSE otherwise
 */
int
vtx_chkparity(byte_t *val) {
  if (!init_done)
    tools_init();
  if (parity_table[*val] & 0x80) {
    *val &= 0x7f;
    return TRUE;
  } else return FALSE;
}


/* Add odd parity bit to val
 */
byte_t
vtx_mkparity(byte_t val) {
  if (!init_done)
    tools_init();
  val &= 0x7f;
  return val | ((parity_table[val] & 0x80) ? 0 : 128);
}


/* Check hamming-encoding of *val
 * Replace with decoded value & return TRUE if encoding is OK, return FALSE otherwise
 */
int
vtx_chkhamming(byte_t val) {
  if (!init_done)
    tools_init();
  if (parity_table[val] & 0x40) {
    return -1;
  } else {
    return parity_table[val] & 0xf;
  }
}


/* Increase page-number. Skip over hexadecimal pages
 */
int
inc_vtxpage(int page) {
  page++;
  if ((page & 0xf) >= 0xa)
    page = (page & ~0xf) + 0x10;
  if ((page & 0xf0) >= 0xa0)
    page = (page & ~0xff) + 0x100;
  if (page >= 0x899)
    page = 0x100;
  return page;
}


/* Decrease page-number. Skip over hexadecimal pages
 */
int
dec_vtxpage(int page) {
  page--;
  if ((page & 0xf) >= 0xa)
    page = (page & ~0xf) + 9;
  if ((page & 0xf0) >= 0xa0)
    page = (page & ~0xff) + 0x99;
  if (page < 0x100)
    page = 0x899;
  return page;
}


int
vtx_hex2dec(int pgnum) {
  return (pgnum / 0x100) * 100 + ((pgnum / 0x10) % 0x10) * 10 + (pgnum % 0x10);
}


int
vtx_dec2hex(int pgnum) {
  return (pgnum / 100) * 0x100 + ((pgnum / 10) % 10) * 0x10 + (pgnum % 10);
}


int
vtx_chkpgnum(int pgnum, int hex_ok) {
  if (hex_ok) {
    return (pgnum >= 0x100 && pgnum <= 0x8ff);
  } else {
    return (pgnum >= 0x100 && pgnum <= 0x899 && (pgnum & 0xff) <= 0x99 && (pgnum & 0xf) <= 9);
  }
}
