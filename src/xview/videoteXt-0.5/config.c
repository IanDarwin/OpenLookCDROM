/*
 * config.c: Routines to read/write the vtxrc-file
 *
 * Copyright (c) 1994-95 Martin Buck  <martin.buck@student.uni-ulm.de>
 * Read COPYING for more information
 *
 */

#include <stdio.h>
#include <ctype.h>
#include <stdlib.h>
#include <time.h>
#include <errno.h>
#include <string.h>
#include "safe_malloc.h"
#include "vtxqueue.h"
#include "xinit.h"
#include "xevents.h"
#include "dialog.h"


static const char *placement_str[] = { "centered", "left", "right", "top", "bottom" };



int
config_load(const char *fname) {
  int err, linecount = 0, popup_p, popup_s, tmpnum;
  char line[256], token[100], tmpstr[100];
  FILE *file;

  if (!(file = fopen(fname, "r"))) {
    return -1;
  }
  while (1) {
    if (!fgets(line, 256, file)) {
      if (feof(file))
        break;
      err = errno;
      fclose(file);
      errno = err;
      return -1;
    }
    linecount++;
    tmpstr[0] = '\0';
    if (sscanf(line, " %99[^\1- =] = %99[^\1-\37] ", token, tmpstr) != 2) {
      if (strspn(line, " \f\n\r\t\v") == strlen(line))
        continue;
      fprintf(stderr, "%s:%d: Warning: Syntax error (ignoring line).\n", fname, linecount);
      continue;
    }
    popup_p = popup_s = -1;
    if (!strcasecmp(token, "font")) {
      for (vtxwin.font = 0; vtxwin.font <= vtxmaxfont; vtxwin.font++) {
        if (!strcasecmp(tmpstr, vtxfonts[vtxwin.font].fontname))
          break;
      }
      if (vtxwin.font > vtxmaxfont) {
        fprintf(stderr, "%s:%d: Warning: Can't find font '%s', using '%s'.\n", fname, linecount,
            tmpstr, vtxfonts[0].fontname);
        vtxwin.font = 0;
      }
    } else if (!strcasecmp(token, "screen_updates")) {
      for (update_entry = 0; update_entry <= 2; update_entry++) {
        if (!strcasecmp(tmpstr, update_table[update_entry].menu_string))
          break;
      }
      if (update_entry > 2) {
        fprintf(stderr, "%s:%d: Warning: Invalid update-value, using 'slow'.\n", fname,
            linecount);
        vtxwin.font = 0;
      }
    } else if (!strcasecmp(token, "auto_reset")) {
      if (!strcasecmp(tmpstr, "true")) {
        auto_reset = TRUE;
      } else if (!strcasecmp(tmpstr, "false")) {
        auto_reset = FALSE;
      } else {
        fprintf(stderr, "%s:%d: Warning: Invalid value, using 'true'.\n", fname, linecount);
        auto_reset = TRUE;
      }
    } else if (!strcasecmp(token, "history_placement")) {
      popup_p = POPUP_HISTORY;
    } else if (!strcasecmp(token, "toptext_placement")) {
      popup_p = POPUP_TOPTEXT;
    } else if (!strcasecmp(token, "hotlist_placement")) {
      popup_p = POPUP_HOTLIST;
    } else if (!strcasecmp(token, "stationlist_placement")) {
      popup_p = POPUP_SELST;
    } else if (!strcasecmp(token, "history_startup")) {
      popup_s = POPUP_HISTORY;
    } else if (!strcasecmp(token, "toptext_startup")) {
      popup_s = POPUP_TOPTEXT;
    } else if (!strcasecmp(token, "hotlist_startup")) {
      popup_s = POPUP_HOTLIST;
    } else if (!strcasecmp(token, "stationlist_startup")) {
      popup_s = POPUP_SELST;
    } else if (!strcasecmp(token, "toptext_when_active")) {
      if (!strcasecmp(tmpstr, "true")) {
        toptext_when_active = TRUE;
      } else if (!strcasecmp(tmpstr, "false")) {
        toptext_when_active = FALSE;
      } else {
        fprintf(stderr, "%s:%d: Warning: Invalid value, using 'false'.\n", fname, linecount);
        toptext_when_active = FALSE;
      }
    } else if (!strcasecmp(token, "stationlist_lines")) {
      tmpnum = strtol(tmpstr, NULL, 0);
      if (tmpnum < 3 || tmpnum > 50) {
        fprintf(stderr, "%s:%d: Warning: Invalid value, using 5.\n", fname, linecount);
        tmpnum = 5;
      }
      popups[POPUP_SELST].default_rows = tmpnum;
    } else if (!strcasecmp(token, "hotlist_lines")) {
      tmpnum = strtol(tmpstr, NULL, 0);
      if (tmpnum < 3 || tmpnum > 50) {
        fprintf(stderr, "%s:%d: Warning: Invalid value, using 9.\n", fname, linecount);
        tmpnum = 9;
      }
      popups[POPUP_HOTLIST].default_rows = tmpnum;
    } else if (!strcasecmp(token, "history_lines")) {
      tmpnum = strtol(tmpstr, NULL, 0);
      if (tmpnum < 3 || tmpnum > 50) {
        fprintf(stderr, "%s:%d: Warning: Invalid value, using 9.\n", fname, linecount);
        tmpnum = 9;
      }
      popups[POPUP_HISTORY].default_rows = tmpnum;
    } else if (!strcasecmp(token, "display_on_tv")) {
      if (!strcasecmp(tmpstr, "true")) {
        display_on_tv = TRUE;
      } else if (!strcasecmp(tmpstr, "false")) {
        display_on_tv = FALSE;
      } else {
        fprintf(stderr, "%s:%d: Warning: Invalid value, using 'false'.\n", fname, linecount);
        display_on_tv = FALSE;
      }
    } else if (!strcasecmp(token, "use_interlace")) {
      if (!strcasecmp(tmpstr, "true")) {
        use_interlace = TRUE;
      } else if (!strcasecmp(tmpstr, "false")) {
        use_interlace = FALSE;
      } else {
        fprintf(stderr, "%s:%d: Warning: Invalid value, using 'true'.\n", fname, linecount);
        use_interlace = TRUE;
      }
    } else if (!strcasecmp(token, "display_mode")) {
      if (!strcasecmp(tmpstr, "normal")) {
        display_mode = 1;
      } else if (!strcasecmp(tmpstr, "transparent")) {
        display_mode = 2;
      } else if (!strcasecmp(tmpstr, "insert")) {
        display_mode = 3;
      } else {
        fprintf(stderr, "%s:%d: Warning: Invalid value, using 'insert'.\n", fname, linecount);
        display_mode = 3;
      }
    } else if (!strcasecmp(token, "default_cps_printer")) {
      strcpy(default_cps_prn, tmpstr);
    } else if (!strcasecmp(token, "default_ps_printer")) {
      strcpy(default_ps_prn, tmpstr);
    } else if (!strcasecmp(token, "default_ascii_printer")) {
      strcpy(default_ascii_prn, tmpstr);
    } else if (!strcasecmp(token, "default_bm_printer")) {
      strcpy(default_bm_prn, tmpstr);
    } else if (!strcasecmp(token, "wm_offset")) {
      if (strcasecmp(tmpstr, "auto")) {
        if (sscanf(tmpstr, "%d , %d", &win_adj_x, &win_adj_y) != 2 ||
            win_adj_x < -50 || win_adj_x > 50 || win_adj_y < -50 || win_adj_y > 50) {
          fprintf(stderr, "%s:%d: Warning: Invalid value, using '0,0'.\n", fname, linecount);
          win_adj_x = win_adj_y = 0;
        }
        win_adjusted = -1;
      }
    } else if (!strcasecmp(token, "wm_decoration")) {
      if (strcasecmp(tmpstr, "auto")) {
        if (sscanf(tmpstr, "%d , %d , %d , %d", &win_lb, &win_rb, &win_tb, &win_bb) != 4 ||
            win_lb < 0 || win_lb > 50 || win_rb < 0 || win_rb > 50 || 
            win_tb < 0 || win_tb > 50 || win_bb < 0 || win_bb > 50) {
          fprintf(stderr, "%s:%d: Warning: Invalid value, using '10,10,30,10'.\n", fname,
              linecount);
          win_lb = win_rb = win_bb = 10;
          win_tb = 30;
        }
        win_adjusted = -1;
      }
    } else {
      fprintf(stderr, "%s:%d: Warning: Syntax error (ignoring line).\n", fname, linecount);
    }
    if (popup_p != -1) {
      for (popups[popup_p].placement = 0; popups[popup_p].placement <= 4;
          popups[popup_p].placement++) {
        if (!strcasecmp(placement_str[popups[popup_p].placement], tmpstr))
          break;
      }
      if (popups[popup_p].placement > 4) {
        fprintf(stderr, "%s:%d: Warning: Invalid value, using 'centered'.\n", fname, linecount);
        popups[popup_p].placement = 0;
      }
    } else if (popup_s != -1) {
      if (!strcasecmp(tmpstr, "true")) {
        popups[popup_s].startup = TRUE;
      } else if (!strcasecmp(tmpstr, "false")) {
        popups[popup_s].startup = FALSE;
      } else {
        fprintf(stderr, "%s:%d: Warning: Invalid value, using 'false'.\n", fname, linecount);
        popups[popup_s].placement = FALSE;
      }
    }
  }
  return fclose(file);
}


int
config_save(const char *fname) {
  int err;
  FILE *file;
  
  file = fopen(fname, "w");
  
  fprintf(file, "font=%s\n", vtxfonts[vtxwin.font].fontname);
  fprintf(file, "screen_updates=%c%s\n\n", tolower(*update_table[update_entry].menu_string),
      update_table[update_entry].menu_string + 1);
  
  fprintf(file, "history_placement=%s\n", placement_str[popups[POPUP_HISTORY].placement]);
  fprintf(file, "toptext_placement=%s\n", placement_str[popups[POPUP_TOPTEXT].placement]);
  fprintf(file, "hotlist_placement=%s\n", placement_str[popups[POPUP_HOTLIST].placement]);
  fprintf(file, "stationlist_placement=%s\n", placement_str[popups[POPUP_SELST].placement]);
  fprintf(file, "history_startup=%s\n", (popups[POPUP_HISTORY].startup ? "true" : "false"));
  fprintf(file, "toptext_startup=%s\n", (popups[POPUP_TOPTEXT].startup ? "true" : "false"));
  fprintf(file, "hotlist_startup=%s\n", (popups[POPUP_HOTLIST].startup ? "true" : "false"));
  fprintf(file, "stationlist_startup=%s\n", (popups[POPUP_SELST].startup ? "true" : "false"));
  fprintf(file, "toptext_when_active=%s\n\n", (toptext_when_active ? "true" : "false"));
  
  fprintf(file, "stationlist_lines=%d\nhotlist_lines=%d\nhistory_lines=%d\n\n",
      popups[POPUP_SELST].default_rows, popups[POPUP_HOTLIST].default_rows,
      popups[POPUP_HISTORY].default_rows);

  fprintf(file, "display_on_tv=%s\n", (display_on_tv ? "true" : "false"));
  fprintf(file, "use_interlace=%s\n", (use_interlace ? "true" : "false"));
  fprintf(file, "display_mode=%s\n\n", (display_mode == 1 ? "normal" :
      (display_mode == 2 ? "transparent" : "insert")));

  fprintf(file, "auto_reset=%s\n", (auto_reset ? "true" : "false"));
  fprintf(file, "default_cps_printer=%s\n", default_cps_prn);
  fprintf(file, "default_ps_printer=%s\n", default_ps_prn);
  fprintf(file, "default_ascii_printer=%s\n", default_ascii_prn);
  fprintf(file, "default_bm_printer=%s\n\n", default_bm_prn);

  if (win_adjusted == -1) {
    fprintf(file, "wm_offset=%d,%d\n", win_adj_x, win_adj_y);
    fprintf(file, "wm_decoration=%d,%d,%d,%d\n", win_lb, win_rb, win_tb, win_bb);
  } else {
    fprintf(file, "wm_offset=auto\nwm_decoration=auto\n");
  }
  
  if (ferror(file) || fclose(file)) {
    err = errno;
    fclose(file);
    errno = err;
    return -1;
  }
  return 0;
}
