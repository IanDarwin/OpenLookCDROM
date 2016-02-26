/*
 * vtxget.c: Parse command-line options & args, build queue with requested pages
 *
 * Copyright (c) 1994-95 Martin Buck  <martin.buck@student.uni-ulm.de>
 * Read COPYING for more information
 *
 */

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <ctype.h>
#include <errno.h>
#include <sys/stat.h>
#include <linux/vtx.h>
#include "vtx_assert.h"
#include "safe_malloc.h"
#include "misc.h"
#include "batch.h"
#include "cct.h"
#include "vtxdecode.h"
#include "vtxget.h"


int page_timeout = 60, show_hidden, show_header;
ofmt_t ofmt;
vtx_info_t vtx_info;
vtxpage_t page;
char *outdir, *fname_prefix = "";



int
main(int argc, char *argv[]) {
  char tmpchr, tmpstr[3];
  int optchr, index, status, page, subpage, getall = FALSE, err, view_files = FALSE;

  if ((smalloc_progname = strrchr(argv[0], '/'))) {
    smalloc_progname++;
  } else {
    smalloc_progname = argv[0];
  }

  while ((optchr = getopt(argc, argv, "t:i:hrf:o:p:d:v?")) != -1) {
    switch (optchr) {
      case 't':
        page_timeout = strtol(optarg, NULL, 10);
        if (page_timeout < 1 || page_timeout > 600) {
          fprintf(stderr, "%s: invalid timeout-value: %s (valid range is 1-600)\n", smalloc_progname, optarg);
          exit(1);
        }
      break;
      case 'i':
        interleave = strtol(optarg, NULL, 10);
        if (interleave < -100 || interleave > 100) {
          fprintf(stderr, "%s: invalid page-interleave: %s (valid range is -100-100)\n", smalloc_progname, optarg);
          exit(1);
        }
      break;
      case 'h':
        show_header = TRUE;
      break;
      case 'r':
        show_hidden = TRUE;
      break;
      case 'f':
        if (!strcmp(optarg, "iso")) {
          ofmt = FMT_ISO;
        } else if (!strcmp(optarg, "ansi")) {
          ofmt = FMT_ANSI;
        } else if (!strcmp(optarg, "text")) {
          ofmt = FMT_TEXT;
        } else if (!strcmp(optarg, "vtx")) {
          ofmt = FMT_VTX;
        } else if (!strcmp(optarg, "ps")) {
          ofmt = FMT_PS;
        } else if (!strcmp(optarg, "ips")) {
          ofmt = FMT_IPS;
        } else if (!strcmp(optarg, "cps")) {
          ofmt = FMT_CPS;
        } else {
          fprintf(stderr, "%s: invalid output format: %s\n", smalloc_progname, optarg);
          exit(1);
        }
      break;
      case 'o': {
        struct stat st;

        outdir = optarg;
        if (stat(outdir, &st) || access(outdir, W_OK)) {
          if (errno == ENOENT) {
            if (mkdir(outdir, S_IRWXU | S_IRWXG | S_IRWXO)) {
              fprintf(stderr, "%s: %s: %s\n", smalloc_progname, outdir, strerror(errno));
              exit(1);
            }
          } else {
            fprintf(stderr, "%s: %s: %s\n", smalloc_progname, outdir, strerror(errno));
            exit(1);
          }
        } else if (!S_ISDIR(st.st_mode)) {
          fprintf(stderr, "%s: %s: not a directory\n", smalloc_progname, outdir);
          exit(1);
        }
      }
      break;
      case 'p':
        fname_prefix = optarg;
      break;
      case 'd':
        cct_device = optarg;
      break;
      case 'v':
        view_files = TRUE;
      break;
      case '?':
        printf("Usage: %s [options] <pages...>\n"
               "Page-syntax:\n"
               "  x        Get next subpage of page x (x:100-8FF)\n"
               "  x.y      Get subpage y of page x (y:1-7F)\n"
               "  x.0      Get all subpages of page x\n"
               "  x.l      Keeep seraching for page x (until timeout)\n"
               "  allnorm  Get all normal pages (TOP-Text required)\n"
               "  allsub   Get all pages with subpages\n"
               "  all      Get all normal and subpages\n"
               "Options:\n"
               "  -t <timeout>                       Set page-timeout (default: 60s)\n"
               "  -i <interleave>                    Set page-interleave (default: 20)\n"
               "  -h                                 Print page-header & status-bits\n"
               "  -r                                 Reveal hidden parts of page\n"
               "  -f {iso|ansi|text|vtx|ps|ips|cps}  Set output format (default: iso)\n"
               "  -o <dir>                           Set output directory (default: stdout)\n"
               "  -p <prefix>                        Set prefix for output-files\n"
               "  -d <device>                        Set device to use (default: /dev/vtx)\n"
               "  -v                                 Display files, don't search for pages\n"
               "  -\\?                                Show this help\n", smalloc_progname);
        exit(1);
      break;
      default:
        assert(0);
      break;
    }
  }
  if (ofmt == FMT_VTX && !outdir) {
    fprintf(stderr, "%s: can't write vtx-file to stdout\n", smalloc_progname);
    exit(1);
  }

  if (!view_files) {
    if (optind >= argc) {
      fprintf(stderr, "%s: no pages to search for (type `%s -\\?' to get help)\n",
          smalloc_progname, smalloc_progname);
      exit(1);
    }
    if (!strcasecmp(argv[optind], "allnorm")) {
      ascii_insert_pagelist(0x1f0, -1);
      getall = GET_ALLNORM;
    } else if (!strcasecmp(argv[optind], "allsub")) {
      ascii_insert_pagelist(0x1f0, -1);
      getall = GET_ALLSUB;
    } else if (!strcasecmp(argv[optind], "all")) {
      ascii_insert_pagelist(0x1f0, -1);
      getall = GET_ALL;
    } else {
      for (index = optind; index < argc; index++) {
        if (sscanf(argv[index], "%x.%2[0-9a-fA-FlL]", &page, tmpstr) != 2) {
          if (sscanf(argv[index], "%x%c", &page, &tmpchr) != 1) {
            page = -1;				/* This will produce an error */
          }
          subpage = -1;
        } else {
          if (!strcasecmp("l", tmpstr)) {
            subpage = -2;
          } else {
            if (sscanf(tmpstr, "%x%c", &subpage, &tmpchr) != 1) {
              page = -1;			/* This will produce an error */
            } else if (subpage < 0 || subpage > 0x7f) {
              page = -1;			/* This will produce an error */
            }
          }
        }
        if (page < 0x100 || page > 0x8ff) {
          fprintf(stderr, "%s: invalid page-number: %s\n", smalloc_progname, argv[index]);
          exit(1);
        }
        ascii_insert_pagelist(page, subpage);
      }
    }
    if ((status = cct_open(REQ_MAJOR, REQ_MINOR, &vtx_info)) < 0) {
      if (status == CCTEVERSION) {
        fprintf(stderr, "%s: can't open videotext-device: incompatible driver version (need %d.%d)\n",
            smalloc_progname, REQ_MAJOR, REQ_MINOR);
      } else {
        fprintf(stderr, "%s: can't open videotext-device %s: %s\n", smalloc_progname, cct_device,
            strerror(errno));
      }
      exit(1);
    }
    exit(ascii_get_pages(getall) ? 2 : 0);
  } else {
    if (optind >= argc) {
      fprintf(stderr, "%s: no files to display (type `%s -\\?' to get help)\n",
          smalloc_progname, smalloc_progname);
      exit(1);
    }
    for (index = optind; index < argc; index++) {
      if ((err = display_file(argv[index]))) {
        if (err > 0) {
          fprintf(stderr, "%s: %s: %s\n", smalloc_progname, argv[index], strerror(err));
        }
        exit(1);
      }
    }
  }
  exit(0);
}
