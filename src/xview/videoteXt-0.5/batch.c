/*
 * batch.c: This is the commandline-version of VideoteXt (minus option & argument parsing, which
 *          is done in vtxget.c)
 *
 * Copyright (c) 1994-95 Martin Buck  <martin.buck@student.uni-ulm.de>
 * Read COPYING for more information
 *
 */

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <time.h>
#include <string.h>
#include <errno.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <linux/vtx.h>
#include "safe_malloc.h"
#include "misc.h"
#include "cct.h"
#include "vtxtools.h"
#include "vtxget.h"
#include "vtxdecode.h"
#include "toptext.h"
#include "postscript.h"
#include "batch.h"

#define CCTCHK(cmd) \
        do { \
          int cctretval; \
          if ((cctretval = cmd)) { \
            report_cct_error(cctretval); \
          } \
        } while (0)

typedef struct {
  int page, subpage, firstsubpg, lastsubpg, seq;
  time_t time;
} pagelist_t;


int interleave = 20;

static pagelist_t *pagelist = NULL;
static int lastpage = 0x100;
tt_pageclass_t init_pglevel, max_pglevel;

static const char *ctrl_chars[] = {
  "{AL_BLACK}", "{AL_RED}", "{AL_GREEN}", "{AL_YELLOW}", "{AL_BLUE}", "{AL_MAGENTA}",
  "{AL_CYAN}", "{AL_WHITE}", "{FLASH_ON}", "{FLASH_OFF}", "{BOX_END}", "{BOX_START}",
  "{HEIGHT_NORM}", "{HEIGHT_DBL}", "{SO}", "{SI}", "{GR_BLACK}", "{GR_RED}", "{GR_GREEN}",
  "{GR_YELLOW}", "{GR_BLUE}", "{GR_MAGENTA}", "{GR_CYAN}", "{GR_WHITE}", "{CONCEAL}",
  "{GR_CONT}", "{GR_SEP}", "{ESC}", "{BG_BLACK}", "{BG_NEW}", "{GR_HOLD}", "{GR_RELEASE}"
};


static void
my_usleep(unsigned long usec) {
  struct timeval timeout;

  timeout.tv_sec = usec / 1000000;
  timeout.tv_usec = usec - 1000000 * timeout.tv_sec;
  select(1, NULL, NULL, NULL, &timeout);
}


static void
report_cct_error(int err) {
  fprintf(stderr, "%s: Fatal: videotext-driver returned %serror\n", smalloc_progname,
    (err == CCTERR ? "I/O-" : (err == CCTEINVAL ? "invalid argument " : "unknown ")));
  exit(1);
}


static void
print_status(const char *msg, int subpg_request, int page, int subpg) {
  switch (subpg_request) {
    case -2:
      fprintf(stderr, "%s: %X.loop\n", msg, page);
    break;
    case -1:
      fprintf(stderr, "%s: %X.next\n", msg, page);
    break;
    case 0:
      fprintf(stderr, "%s: %X.all\n", msg, page);
    break;
    default:
      fprintf(stderr, "%s: %X.%X\n", msg, page, subpg);
    break;
  }
}


static void
write_page(const byte_t *buf, const vtx_pageinfo_t *info, int seq) {
  vtxpage_t page;
  int pos;
  char *fname = NULL;
  FILE *file = stdout;

  if (outdir) {
    if (seq) {
      fname = smalloc(strlen(outdir) + 1 + strlen(fname_prefix) + 19);
      sprintf(fname, "%s/%s%X_%d%s", outdir, fname_prefix, info->pagenum, seq,
          (ofmt == FMT_VTX ? ".vtx" :
          ((ofmt == FMT_PS || ofmt == FMT_IPS || ofmt == FMT_CPS) ? ".ps" : "")));
    } else {
      fname = smalloc(strlen(outdir) + 1 + strlen(fname_prefix) + 11);
      sprintf(fname, "%s/%s%X_%X%s", outdir, fname_prefix, info->pagenum, info->minute,
          (ofmt == FMT_VTX ? ".vtx" :
          ((ofmt == FMT_PS || ofmt == FMT_IPS || ofmt == FMT_CPS) ? ".ps" : "")));
    }
    if (!(file = fopen(fname, "w"))) {
      fprintf(stderr, "%s: %s: %s\n", smalloc_progname, fname, strerror(errno));
      exit(1);
    }
  }
  page.info = *info;
  decode_page(buf, &page, 0, 23);
  if (show_header && ofmt != FMT_VTX && ofmt != FMT_PS) {
    fprintf(file, "{HEADER}\npage: %X\nhour: %X\nminute: %X\ncharset: %d\n"
        "delete_page: %d\ninsert_headline: %d\ninsert_subtitle: %d\n"
        "suppress_header: %d\nupdate_page: %d\ninterrupted_seq: %d\n"
        "suppress_display: %d\nserial_mode: %d\n",
        info->pagenum, info->hour, info->minute, info->charset, info->delete, info->headline,
        info->subtitle, info->supp_header, info->update, info->inter_seq, info->dis_disp,
        info->serial);
  }
  if (!outdir) {
    fprintf(file, "{PAGE %X/%X}\n", info->pagenum, info->minute);
  }
  switch (ofmt) {
    case FMT_ISO:
      for (pos = 0; pos < VTX_PAGESIZE; pos++) {
        if (show_hidden || !(page.attrib[pos] & VTX_HIDDEN)) {
          fputc(vtx2iso_table[page.chr[pos]], file);
        } else {
          fputc(' ', file);
        }
        if (pos % 40 == 39)
          fputc('\n', file);
      }
    break;
    case FMT_ANSI: {
      attrib_t lastattr = 7;
      
      for (pos = 0; pos < VTX_PAGESIZE; pos++) {
        if (pos % 40 == 0) {
          fputs("\33[37m\33[40m", file);
          lastattr = 7;
        }
        if ((lastattr & VTX_COLMASK) != (page.attrib[pos] & VTX_COLMASK)) {
          fprintf(file, "\33[%dm", (page.attrib[pos] & VTX_COLMASK) + 30);
        }
        if ((lastattr & VTX_BGMASK) != (page.attrib[pos] & VTX_BGMASK)) {
          fprintf(file, "\33[%dm", ((page.attrib[pos] >> 3) & VTX_COLMASK) + 40);
        }
        if ((lastattr & VTX_FLASH) != (page.attrib[pos] & VTX_FLASH)) {
          fprintf(file, "\33[%dm", ((page.attrib[pos] & VTX_FLASH) ? 5 : 25));
        }
        lastattr = page.attrib[pos];
        if (show_hidden || !(page.attrib[pos] & VTX_HIDDEN)) {
          fputc(vtx2iso_table[page.chr[pos]], file);
        } else {
          fputc(' ', file);
        }
        if (pos % 40 == 39) {
          fputs("\33[0m\n", file);
        }
      }
      fputs("\33[0m\n", file);
    }
    break;
    case FMT_TEXT: {
      byte_t chr;
      
      for (pos = 0; pos < VTX_PAGESIZE; pos++) {
        chr = buf[pos];
        if (!vtx_chkparity(&chr)) {
          fputs("{PARITY}", file);
        } else if (chr >= ' ') {
          if (page.chr[pos] >= 128 && page.chr[pos] <= 191) {
            fprintf(file, "{GCHR_%d}", page.chr[pos] - 128);
          } else {
            fputc(vtx2iso_table[page.chr[pos]], file);
          }
        } else {
          fputs(ctrl_chars[chr], file);
        }
        if (pos % 40 == 39)
          fputc('\n', file);
      }
    }
    break;
    case FMT_VTX: {
      fputs("VTXV2", file);
      fputc(info->pagenum & 0xff, file);
      fputc(info->pagenum / 0x100, file);
      fputc(info->hour & 0xff, file);
      fputc(info->minute & 0xff, file);
      fputc(info->charset & 0xff, file);
      fputc(info->delete << 7 | info->headline << 6 | info->subtitle << 5 | info->supp_header << 4 |
          info->update << 3 | info->inter_seq << 2 | info->dis_disp << 1 | info->serial << 0, file);
      fputc(info->notfound << 7 | info->pblf << 6 | info->hamming << 5, file);
      for (pos = 0; pos < VTX_PAGESIZE; pos++) {
        fputc(buf[pos], file);
      }
    }
    break;
    case FMT_PS:
    case FMT_IPS:
    case FMT_CPS:
      write_postscript(file, &page, "Videotext", ofmt == FMT_CPS, ofmt == FMT_IPS,
          show_hidden);
    break;
  }
  if (outdir) {
    if (ferror(file)) {
      fprintf(stderr, "%s: %s: %s\n", smalloc_progname, fname, strerror(errno));
      exit(1);
    }
    fclose(file);
    free(fname);
  } else {
    fflush(file);
  }
}


static int
pagelist_get_next(int getall, pagelist_t *page) {
  int pgnum;
  static int entry = 0;
  
  if (!pagelist)
    return FALSE;
  if (!getall || !toptext_ok) {						/* Normal mode */
    if (!pagelist[entry].page)
      return FALSE;
    if (page) {
      *page = pagelist[entry];
      page->firstsubpg = page->lastsubpg = 0;
      page->time = time(NULL);
      if (page->subpage == -2) {
        page->seq = 1;
      } else {
        page->seq = 0;
      }
      entry++;
    }
    return TRUE;
  } else if (toptext_ok && (pgnum = toptext_getnext(lastpage, interleave, init_pglevel,
      max_pglevel)) > 0) {
    page->page = pgnum;							/* TOP-Text mode */
    page->subpage = page->firstsubpg = page->lastsubpg = page->seq = 0;
    page->time = time(NULL);
    return TRUE;
  }
  return FALSE;
}


static void
top_pagereq(int page, int hour, int minute) {
  ascii_insert_pagelist(page, minute);
}


void
ascii_insert_pagelist(int page, int subpage) {
  static int lastentry;
  
  pagelist = srealloc(pagelist, (lastentry + 2) * sizeof(pagelist_t));
  pagelist[lastentry].page = page;
  pagelist[lastentry].subpage = subpage;
  lastentry++;
  pagelist[lastentry].page = 0;
}


int
ascii_get_pages(int getall) {
  int buf, bufbusy, newpage, error_occured = FALSE;
  pagelist_t *dau_status;
  byte_t vtxraw[VTX_PAGESIZE];
  vtx_pageinfo_t pageinfo;
  
  init_pglevel = (getall == GET_ALLSUB) ? TT_SUBPG : TT_BLOCK;
  max_pglevel = (getall == GET_ALLNORM) ? TT_NORM : TT_SUBPG;
  dau_status = scalloc(vtx_info.numpages, sizeof(pagelist_t));
  do {
    for (buf = 0; buf < vtx_info.numpages; buf++) {
      if (dau_status[buf].page) {
        if ((newpage = cct_checkpage(buf)) < 0) {
          report_cct_error(newpage);
        }
        if (!newpage) {
          my_usleep(200000);
          CCTCHK(cct_getpage(buf, 0, 0, 39, 23, vtxraw, &pageinfo));
          CCTCHK(cct_reset_pgfound(buf));
          lastpage = pageinfo.pagenum;
          if (!toptext_newpage(vtxraw + 40, &pageinfo, &top_pagereq)) {
            if (dau_status[buf].subpage == -1 || dau_status[buf].subpage == -2 ||
                !pageinfo.minute) {					/* Get next page */
              write_page(vtxraw, &pageinfo, dau_status[buf].seq);
              print_status("found", dau_status[buf].subpage, pageinfo.pagenum, pageinfo.minute);
              if (dau_status[buf].subpage == -2) {
                dau_status[buf].seq++;
                dau_status[buf].time = time(NULL);
              } else {
                dau_status[buf].page = 0;
                CCTCHK(cct_stop_dau(buf));
              }
            } else if (dau_status[buf].subpage) {			/* Get a certain subpage */
              if (dau_status[buf].subpage == pageinfo.minute) {		/* Right subpage arrived */
                write_page(vtxraw, &pageinfo, 0);
                print_status("found", dau_status[buf].subpage, pageinfo.pagenum, pageinfo.minute);
                dau_status[buf].page = 0;
                CCTCHK(cct_stop_dau(buf));
              } else {						/* Wrong subpage arrived */
                if (dau_status[buf].lastsubpg != pageinfo.minute) {
                  dau_status[buf].time = time(NULL);
                  dau_status[buf].lastsubpg = pageinfo.minute;
                  if (!dau_status[buf].firstsubpg) {
                    dau_status[buf].firstsubpg = pageinfo.minute;
                  } else if (pageinfo.minute == dau_status[buf].firstsubpg ||	/* Subpage */
                      pageinfo.minute == dau_status[buf].firstsubpg - 1) {	/* doesn't exist */
                    print_status("nonexistent", dau_status[buf].subpage, dau_status[buf].page,
                        dau_status[buf].subpage);
                    error_occured = TRUE;
                    dau_status[buf].page = 0;
                    CCTCHK(cct_stop_dau(buf));
                  }
                }
              }
            } else if (dau_status[buf].lastsubpg != pageinfo.minute) {	/* Get all subpages */
              dau_status[buf].time = time(NULL);
              if (pageinfo.minute != dau_status[buf].firstsubpg) {
                write_page(vtxraw, &pageinfo, 0);
                print_status("found", 1, pageinfo.pagenum, pageinfo.minute);
              }
              dau_status[buf].lastsubpg = pageinfo.minute;
              if (!dau_status[buf].firstsubpg) {
                dau_status[buf].firstsubpg = pageinfo.minute;
              } else if (pageinfo.minute == dau_status[buf].firstsubpg ||
                  pageinfo.minute == dau_status[buf].firstsubpg - 1) {
                fprintf(stderr, "found: %X (subpages complete)\n", pageinfo.pagenum);
                dau_status[buf].page = 0;
                CCTCHK(cct_stop_dau(buf));
              }
            }
          } else {
            dau_status[buf].page = 0;
          }
        } else if (time(NULL) - dau_status[buf].time > page_timeout) {
          print_status("timeout", dau_status[buf].subpage, dau_status[buf].page,
              dau_status[buf].subpage);
          error_occured = TRUE;
          dau_status[buf].page = 0;
          CCTCHK(cct_stop_dau(buf));
        }
      }
      if (!dau_status[buf].page && pagelist_get_next(getall, &dau_status[buf])) {
        print_status("searching", dau_status[buf].subpage, dau_status[buf].page,
            dau_status[buf].subpage);
        CCTCHK(cct_searchpage(dau_status[buf].page, 0, 0, PGMASK_PAGE, buf));
        CCTCHK(cct_reset_pgfound(buf));
      }
    }
    my_usleep(100000);
    bufbusy = FALSE;
    for (buf = 0; buf < vtx_info.numpages; buf++) {
      if (dau_status[buf].page)
        bufbusy = TRUE;
    }
  } while (bufbusy || pagelist_get_next(getall, NULL));
  return error_occured;
}


int
display_file(const char *fname) {
  byte_t tmp_buffer[VTX_PAGESIZE];
  vtx_pageinfo_t tmp_inf;
  int pos, tmpbits, tmp_errno;
  unsigned char tmpstr[256];
  struct stat st;
  FILE *file;

  if (!(file = fopen(fname, "r"))) {
    return errno;
  }
  if (fscanf(file, "VTXV%c", tmpstr) != 1) {
    if (fstat(fileno(file), &st) < 0) {
      tmp_errno = errno;
      fclose(file);
      return tmp_errno;
    /* The stupid INtv format doesn't use a header, so we have to use the file-length instead */
    } else if (st.st_size != 1008) {
      fprintf(stderr, "%s: %s: Magic number missing. This is no VideoteXt- or INtv-file.\n",
          smalloc_progname, fname);
      fclose(file);
      return -1;
    }
    memset(&tmp_inf, 0, sizeof(tmp_inf));			/* Read ITV-file */
    rewind(file);
    for (pos = 0; pos <= 23; pos++) {
      fseek(file, 2, SEEK_CUR);
      fread(tmp_buffer + pos * 40, 40, 1, file);
    }
    /* The first 8 bytes in the INtv-format usually contain garbage (or data I don't understand) */
    memset(tmp_buffer, vtx_mkparity(' '), 8 * sizeof(byte_t));
    for (pos = 0; pos <= 2; pos++) {
      tmpstr[pos] = tmp_buffer[8 + pos];
      vtx_chkparity(&tmpstr[pos]);
    }
    tmpstr[3] = '\0';
    sscanf(tmpstr, "%3x", &tmp_inf.pagenum);
    if (!vtx_chkpgnum(tmp_inf.pagenum, TRUE)) {
      tmp_inf.pagenum = 0;
    }
  } else {
    if (tmpstr[0] != '2') {
      fprintf(stderr, "%s: %s: Magic number missing. This is no VideoteXt- or INtv-file.\n",
          smalloc_progname, fname);
      fclose(file);
      return -1;
    }
    tmp_inf.pagenum = fgetc(file) + 0x100 * fgetc(file);	/* Read VTX-file */
    tmp_inf.hour = fgetc(file);
    tmp_inf.minute = fgetc(file);
    tmp_inf.charset = fgetc(file);
    tmpbits = fgetc(file);
    tmp_inf.delete = !!(tmpbits & 0x80);
    tmp_inf.headline = !!(tmpbits & 0x40);
    tmp_inf.subtitle = !!(tmpbits & 0x20);
    tmp_inf.supp_header = !!(tmpbits & 0x10);
    tmp_inf.update = !!(tmpbits & 8);
    tmp_inf.inter_seq = !!(tmpbits & 4);
    tmp_inf.dis_disp = !!(tmpbits & 2);
    tmp_inf.serial = (tmpbits & 1);
    tmpbits = fgetc(file);
    tmp_inf.notfound = !!(tmpbits & 0x80);
    tmp_inf.pblf = !!(tmpbits & 0x40);
    tmp_inf.hamming = !!(tmpbits & 0x20);
    fread(tmp_buffer, VTX_PAGESIZE, 1, file);
  }
  if (feof(file)) {
    fprintf(stderr, "%s: %s: Error while reading; file is corrupt\n", smalloc_progname, fname);
    fclose(file);
    return -1;
  }
  if (ferror(file) || fclose(file) < 0) {
    tmp_errno = errno;
    fclose(file);
    return tmp_errno;
  }
  write_page(tmp_buffer, &tmp_inf, 0);
  return 0;
}
