/*
 * postscript.c: Convert a videotext-page to an Encapsulated PostScript-file
 *
 * Copyright (c) 1995 Martin Buck  <martin.buck@student.uni-ulm.de>
 * Read COPYING for more information
 *
 */

#include <stdio.h>
#include <time.h>
#include <linux/vtx.h>
#include "vtxdecode.h"
#include "misc.h"
#include "postscript.h"

#include "psheader.h"


typedef enum { SB_TEXT, SB_SYMBOL, SB_GRAPH } sb_type;
typedef struct {
  FILE *file;
  int len;
  unsigned char str[41];
  sb_type type;
} stringbuf_t;



static void
flush_string_buffer(stringbuf_t *stringbuf) {
  int pos;
  unsigned char chr;
  
  if (stringbuf->len) {
    putc('(', stringbuf->file);
    for (pos = 0; pos < stringbuf->len; pos++) {
      chr = stringbuf->str[pos];
      if (chr < ' ' || chr >= 127) {
        fprintf(stringbuf->file, "\\%03o", chr);
      } else if (chr == '(' || chr == ')' || chr == '\\') {
        fprintf(stringbuf->file, "\\%c", chr);
      } else {
        putc(chr, stringbuf->file);
      }
    }
    fprintf(stringbuf->file, ") %c ",
        (stringbuf->type == SB_TEXT) ? 'T' : ((stringbuf->type == SB_SYMBOL) ? 'S' : 'G'));
    stringbuf->len = 0;
  }
}


static void
add_string_buffer(stringbuf_t *stringbuf, unsigned char chr, sb_type type) {
  if (type != stringbuf->type) {
    flush_string_buffer(stringbuf);
    stringbuf->type = type;
  }
  stringbuf->str[stringbuf->len++] = chr;
}


void
write_postscript(FILE *file, const vtxpage_t *page, const char *station, int color, int invert,
    int show_hidden) {
  unsigned char **line, chr;
  time_t curr_time;
  int pos;
  attrib_t lastattr = 7;
  stringbuf_t stringbuf;

  curr_time = time(NULL);
  fprintf(file, "%%!PS-Adobe-2.0 EPSF-2.0\n"
      "%%%%Title: %s, Page %X\n"
      "%%%%Creator: VideoteXt V" VTXVERSION ", Copyright (C) 1994-95 by Martin Buck\n"
      "%%%%CreationDate: %s"
      "%%%%BoundingBox: 35 194 567 609\n"
      "%%%%DocumentFonts: Courier Symbol\n"
      "%%%%EndComments\n\n", station, page->info.pagenum, asctime(localtime(&curr_time)));
  for (line = ps_header; *line; line++) {
    fprintf(file, "%s\n", *line);
  }
  fprintf(file, "%%EndProlog\n\n"
      "$VTXDict begin\n\n"
      "/UseColor %s def\n"
      "/InvertPage %s def\n\n"
      "SH\n"
      "XOfs YOfs moveto\n"
      "NL\n\n"
      "(                                         ) T NL\n", color ? "true" : "false",
      invert ? "true" : "false");
  stringbuf.file = file;
  stringbuf.len = 0;
  stringbuf.type = SB_TEXT;
  for (pos = 0; pos < VTX_PAGESIZE; pos++) {
    if ((lastattr & VTX_COLMASK) != (page->attrib[pos] & VTX_COLMASK)) {
      flush_string_buffer(&stringbuf);
      fprintf(file, "%d FG ", page->attrib[pos] & VTX_COLMASK);
    }
    if ((lastattr & VTX_BGMASK) != (page->attrib[pos] & VTX_BGMASK)) {
      flush_string_buffer(&stringbuf);
      fprintf(file, "%d BG ", (page->attrib[pos] >> 3) & VTX_COLMASK);
    }
    if ((lastattr & VTX_DOUBLE2) != (page->attrib[pos] & VTX_DOUBLE2)) {
      flush_string_buffer(&stringbuf);
      fprintf(file, "%cH ", ((page->attrib[pos] & VTX_DOUBLE2) ? 'D' : 'S'));
    }
    chr = page->chr[pos];
    if (show_hidden || !(page->attrib[pos] & VTX_HIDDEN)) {
      if (chr >= 128 && chr <= 191) {
        add_string_buffer(&stringbuf, chr - 128 + (page->attrib[pos] & VTX_GRSEP ? 64 : 0),
            SB_GRAPH);
      } else if (chr == 91 || chr == 93 || chr == 94 || chr == 126) {
        add_string_buffer(&stringbuf, chr, SB_SYMBOL);
      } else {
        add_string_buffer(&stringbuf, chr, SB_TEXT);
      }
    } else {
      add_string_buffer(&stringbuf, ' ', SB_TEXT);
    }
    lastattr = page->attrib[pos];
    if (pos % 40 == 39) {
      if (lastattr & VTX_BGMASK) {
        flush_string_buffer(&stringbuf);
        fputs("0 BG ", file);
      }
      add_string_buffer(&stringbuf, ' ', SB_TEXT);
      flush_string_buffer(&stringbuf);
      lastattr = 7;
      fprintf(file, "NL\n");
    }
  }
  fputs("(                                         ) T NL\n\n"
      "showpage\n\n"
      "end\n", file);
}
