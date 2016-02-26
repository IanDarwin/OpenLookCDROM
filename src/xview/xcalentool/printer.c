/*
 * $Id: printer.c,v 2.3 1994/08/29 17:34:39 billr Exp $
 */
/*
 * calentool - day/week/month/year-at-a-glance calendar for XView/Open Look
 * 
 * Copyright 1988, 1989, 1991, 1994 by Tektronix, Inc. - All Rights Reserved.
 * 
 * Permission to use, copy, modify, and distribute this software and its
 * documentation for any purpose is hereby granted without fee, provided that
 * the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the name of Tektronix, Inc. not be used in
 * advertising or publicity pertaining to distribution of the software
 * without specific, written prior permission.
 * 
 * TEKTRONIX INCORPORATED MAKES NO REPRESENTATIONS ABOUT THE
 * SUITABILITY OF THIS SOFTWARE FOR ANY PURPOSE.  IT IS PROVIDED "AS IS"
 * WITHOUT EXPRESS OR IMPLIED WARRANTY.  TEKTRONIX INCORPORATED
 * DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING ALL IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.  IN NO
 * EVENT SHALL TEKTRONIX INCORPORATED BE LIABLE FOR ANY SPECIAL,
 * INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM
 * LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE
 * OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
 * PERFORMANCE OF THIS SOFTWARE.
 * 
 * Author: Bill Randle, Tektronix, Inc. <billr@saab.cna.tek.com>
 */ 
/*
 * Modified parse_date to allow +nnn and -nnn syntax for dates relative to the
 * current date.  Peter Marshall <peter.marshall@uwo.ca>. 1989-09-19.
 */
/********************************************
 *					    *
 *              Printing routines.	    *
 *					    *
 ********************************************/

#include <stdio.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/XWDFile.h>
#include <X11/Intrinsic.h>
#include <xview/xview.h>
#include <xview/canvas.h>
#include <xview/font.h>
#include <sys/time.h>
#include "xv_ct.h"
#include "ct.h"

extern struct tm current, today;
extern int n_slots;
extern struct dayslot *slots;
extern char apts_pathname[];
extern int nr_weekdays, n_tslots;
extern char *mailto;
extern int findex;
extern struct appt_entry future[];
extern char printer[];
extern Canvas canvas;
extern Xv_window cpwindow;
extern int mainsw_state;
extern Xv_Font font, sfont;
extern int monday_first, hour24, day_first;
extern int print_to_file;
extern int print_dev, week_ofs;
extern char *daynames[], *monthnames[];
extern char clockstr[], strbuf[];
extern void xv_pf_text();
extern int print_trailer;
int format = ZPixmap;
char trailer[256];

char rasfile[] = "/usr/tmp/calentool.xwd";
char psfile[256] = "/usr/tmp/calentool.ps";

#ifdef __STDC__
void print_one_day (int which, FILE *output, int gdrtn);
int dorasfile (void);
int Image_Size (XImage *image);
int Get_XColors (XWindowAttributes *win_info, XColor **colors);
void _swaplong (register char *bp, register unsigned int n);
void _swapshort (register char *bp, register unsigned int n);
void fmt_note (char *s, char *dname, int d, int m, int y, char *str);
void fmt_date (char *s, char *dname, int d, int m, int y, int h, int mn, int eh, int em, int h24, char *str);
#else
void print_one_day ();
int dorasfile ();
int Image_Size ();
int Get_XColors ();
void _swaplong ();
void _swapshort ();
void fmt_note ();
void fmt_date ();
#endif

/*
 * convert appt entry to ASCII string for display with date, time and msg
 */
char *
format_appt(appt)
struct appt_entry *appt;
{
	int e_hour, e_minutes, duration;
	struct tm Save;

	if (appt->arrows > 0) {
		duration = appt->arrows + 1;
		e_hour = appt->hour + duration/2;
		e_minutes = appt->minute + ((duration%2) * 30);
	} else {
		e_hour = appt->hour;
		e_minutes = appt->minute + 30;
	}
	if (e_minutes == 60) {
		e_minutes = 0;
		++e_hour;
	}
	/* get day of week */
	Save = current;
	current.tm_year = appt->year;
	current.tm_mon = appt->month;
	current.tm_mday = appt->day;
	fix_current_day();

	if (appt->flags & A_NOTE) {
		/* note */
		if (appt->flags & ALL_YEARS)
			fmt_note(strbuf, daynames[current.tm_wday], appt->day,
				appt->month+1, 99, appt->str);
		else if (appt->year > 99)
			fmt_note(strbuf, daynames[current.tm_wday], appt->day,
				appt->month+1, appt->year-100, appt->str);
		else
			fmt_note(strbuf, daynames[current.tm_wday], appt->day,
				appt->month+1, appt->year, appt->str);
	} else {
		/* standard appointment */
		fmt_date(strbuf, daynames[current.tm_wday], appt->day,
			appt->month+1, appt->year, appt->hour,
			appt->minute, e_hour, e_minutes, hour24, appt->str);
	}
	current = Save;
	return(strbuf);
}

/*
 * format a note based on the type of date being displayed
 */
void fmt_note(s, dname, d, m, y, str)
char *s;
char *dname;
int d, m, y;
char *str;
{
	if (y == 99)
		/* don't show the year */
		switch (day_first) {
		    case 1:
			sprintf(s, "%3.3s %2d/%02d    --  %s",
				dname, d, m, str);
			break;
		    case 2:
			sprintf(s, "%3.3s %02d-%02d    --  %s",
				dname, m, d, str);
			break;
		    case 0:
		    default:
			sprintf(s, "%3.3s %2d/%02d    --  %s",
				dname, m, d, str);
			break;
		}
	else
		switch (day_first) {
		    case 1:
			sprintf(s, "%3.3s %2d/%02d/%02d    --  %s",
				dname, d, m, y, str);
			break;
		    case 2:
			sprintf(s, "%3.3s %02d-%02d-%02d    --  %s",
				dname, y, m, d, str);
			break;
		    case 0:
		    default:
			sprintf(s, "%3.3s %2d/%02d/%02d    --  %s",
				dname, m, d, y, str);
			break;
		}
}

/*
 * format an appointment based on the type of date being displayed
 */
void fmt_date(s, dname, d, m, y, h, mn, eh, em, h24, str)
char *s;
char *dname;
int d, m, y;
int h, mn, eh, em, h24;
char *str;
{
	if (h24)
		/* format for 24 hour clock */
		switch (day_first) {
		    case 1:
			sprintf(s,"%3.3s %2d/%02d/%02d -- %2d:%02d to %2d:%02d   %s",
				dname, d, m, y, h, mn, eh, em, str);
			break;
		    case 2:
			sprintf(s,"%3.3s %2d/%02d/%02d -- %2d:%02d to %2d:%02d   %s",
				dname, y, m, d, h, mn, eh, em, str);
			break;
		    case 0:
		    default:
			sprintf(s,"%3.3s %2d/%02d/%02d -- %2d:%02d to %2d:%02d   %s",
				dname, m, d, y, h, mn, eh, em, str);
			break;
		}
	else
		/* format for am/pm */
		switch (day_first) {
		    case 1:
			sprintf(s,"%3.3s %2d/%02d/%02d -- %2d:%02d%s to %2d:%02d%s   %s",
				dname, d, m, y, (h < 13 ? h : h-12), mn,
				(h < 12 ? "am" : "pm"), (eh < 13 ? eh : eh-12),
				em, (eh < 12 ? "am" : "pm"), str);
			break;
		    case 2:
			sprintf(s,"%3.3s %02d-%02d-%02d -- %2d:%02d%s to %2d:%02d%s   %s",
				dname, y, m, d, (h < 13 ? h : h-12), mn,
				(h < 12 ? "am" : "pm"), (eh < 13 ? eh : eh-12),
				em, (eh < 12 ? "am" : "pm"), str);
			break;
		    case 0:
		    default:
			sprintf(s,"%3.3s %2d/%02d/%02d -- %2d:%02d%s to %2d:%02d%s   %s",
				dname, m, d, y, (h < 13 ? h : h-12), mn,
				(h < 12 ? "am" : "pm"), (eh < 13 ? eh : eh-12),
				em, (eh < 12 ? "am" : "pm"), str);
			break;
		}
}

/*
 * convert appt entry to ASCII string for display with time and msg
 *	if <esc_parens> is true then put '\' in front of parens (for Ps)
 */
char *
format_appt_nd(appt, esc_parens)
struct appt_entry *appt;
int	esc_parens;
{
	int e_hour, e_minutes, duration;
	char *p, *q;

	if (appt->arrows > 0) {
		duration = appt->arrows + 1;
		e_hour = appt->hour + duration/2;
		e_minutes = appt->minute + ((duration%2) * 30);
	} else {
		e_hour = appt->hour;
		e_minutes = appt->minute + 30;
	}
	if (e_minutes == 60) {
		e_minutes = 0;
		++e_hour;
	}

	strbuf[0] = '\0';
	if (!(appt->flags & A_NOTE)) {
		/* standard appointment */
		if (hour24)
			sprintf(strbuf,"%2d:%02d to %2d:%02d  ",
				appt->hour, appt->minute, e_hour, e_minutes);
		else
			sprintf(strbuf,"%2d:%02d%s to %2d:%02d%s  ",
				(appt->hour < 13 ? appt->hour : appt->hour-12), appt->minute,
				(appt->hour < 12 ? "am" : "pm"),
				(e_hour < 13 ? e_hour : e_hour-12), e_minutes,
				(e_hour < 12 ? "am" : "pm"));
	}
	p = appt->str;
	q = strbuf + strlen(strbuf);
	while (*p) {
		if (esc_parens)
			switch (*p) {
				/* ignore these */
				case '\b':
				case '\f':
				case '\n':
				case '\r':
				case '\t':
					break;
				/* escape these for PostScript */
				case '\\':
				case '(':
				case ')':
					*q++ = '\\';
					break;
			}
		*q++ = *p++;
	}
	*q = '\0';

	return(strbuf);
}

/*
 * Print today's appointments to stdout or mail (useful if we only have an ASCII
 * terminal connected to our Sun). Invoked by the "-p", "-P", "-m" or
 * "-M" options. The -T option is used to select print device
 * (PostScript or ASCII).
 * Month information is only printed as PostScript output.
 */
void
print_apts(which, dest)
int which, dest;
{
	int i, gd_rtn, target, ndays;
	FILE *output, *popen();
	int pclose();
	char cmd[80], *name, *cuserid(), *mail_subj();
	struct tm Save;

	fix_current_day();
	sprintf(trailer, "File: '%s' printed for %s on %s",
		apts_pathname, cuserid(NULL), clockstr);
	gd_rtn = get_day_appts();
	if (which == PRI_DAY && !gd_rtn)
		return;	/* nothing to show */
	if ((which == PRI_DAY_XNOTES) && (!gd_rtn || gd_rtn == SOME_MKNOTES))
		/* all we have is marked notes */
		return;	/* nothing to show */
	if (dest == DST_MAIL) {
		if (mailto != NULL) {
			name = mailto;
		} else if ((name = cuserid(NULL)) == NULL) {
			err_rpt("nobody to mail to", FATAL);
		}
		sprintf(cmd, "%s -s \"Appointments for %s\" %s", MAILPROG, mail_subj(which), name);
		if ((output = popen(cmd, "w")) == NULL)
			err_rpt("Couldn't pipe to 'mail'", FATAL);
	} else {
		output = stdout;
	}
	
	if (which == PRI_DAY || which == PRI_DAY_XNOTES) {
		if (print_dev == PR_POSTSCRIPT)
#ifdef RASTER_ONLY
			fprintf(output, "PostScript output option not compiled in\n");
#else
			print_psday(output, (which == PRI_DAY_XNOTES ? TRUE : FALSE));
#endif
		else
			print_one_day(which, output, gd_rtn);
	} else if (which == PRI_WEEK || which == PRI_WEEK_XNOTES) {
		Save = current;
		if (!week_ofs) {
			/* start printing on first dow */
			if (nr_weekdays == 7 && !monday_first)
				target = SUN;
			else
				target = MON;
			if (current.tm_wday >= target)
				while (current.tm_wday-- > target)
					current.tm_mday--;
			else
				/* looking at Sun */
				current.tm_mday++;
			fix_current_day();
			ndays = nr_weekdays;
		} else {
			ndays = 7;
		}
		if (print_dev == PR_POSTSCRIPT)
#ifdef RASTER_ONLY
			fprintf(output, "PostScript output option not compiled in\n");
#else
			print_psweek(output, (which == PRI_WEEK_XNOTES ? TRUE : FALSE));
#endif
		else
			for (i=0;i<ndays;i++) {
				gd_rtn = get_day_appts();
				if ((gd_rtn && which == PRI_WEEK) ||
				    (gd_rtn & ~SOME_MKNOTES && which == PRI_WEEK_XNOTES))
					print_one_day(which, output, gd_rtn);
				current.tm_mday++;
				fix_current_day();
			}
		current = Save;
	} else if (which == PRI_MONTH || which == PRI_MONTH_XNOTES) {
		if (print_dev == PR_POSTSCRIPT)
#ifdef RASTER_ONLY
			fprintf(output, "PostScript output option not compiled in\n");
#else
			print_psmonth(output, (which == PRI_MONTH_XNOTES ? TRUE : FALSE));
#endif
		else
			fprintf(output, "\nNo ASCII month printout is available\n");
	}
	fflush(output);
	if (dest == DST_MAIL)
		pclose(output);
}

void
print_one_day(which, output, gdrtn)
int which;
FILE *output;
int gdrtn;
{
	int i;
	struct appt_entry tmp_apt, *aptr;
	char *format_appt();

	if (day_first)
		fprintf(output,"\n\t*** Appointments for %s %d %s %d ***\n\n", 
			daynames[current.tm_wday], current.tm_mday,
			monthnames[current.tm_mon], current.tm_year+1900);
	else
		fprintf(output,"\n\t*** Appointments for %s %s %d, %d ***\n\n", 
			daynames[current.tm_wday], monthnames[current.tm_mon],
			current.tm_mday, current.tm_year+1900);

	for (i=0; i<n_slots; i++) {
		if (i == n_tslots)
			/* start of notes section */
			if ((gdrtn & SOME_NOTES) ||
			   (!(which & PRI_XNOTES) && (gdrtn & SOME_MKNOTES)))
				fprintf(output,"\n\t\t     ===== Notes =====\n");
		if (slots[i].first != NULL && slots[i].active > 0) {
			/* at least one appt here */
			aptr = slots[i].first;
			do {
				if ((which & PRI_XNOTES) && ((aptr->flags & MARKED_NOTE) == MARKED_NOTE))
					continue;
				if (chk_deleted(&slots[i], aptr))
					continue;
				tmp_apt = *aptr;
				tmp_apt.year = current.tm_year;
				tmp_apt.month = current.tm_mon;
				tmp_apt.day = current.tm_mday;
				fprintf(output, "%s\n", format_appt(&tmp_apt));
			} while ((aptr = aptr->next) != NULL);
		}
	}
	if (findex) {
		/* print out future appointments */
		fprintf(output, "\n\t\t===== Future Reminders =====\n");
		for (i=0; i<findex; i++)
			fprintf(output, "%s\n", format_appt(&future[i]));
	}
	fprintf(output,"\n------------------------------------------------------------------\n");
}

/*
 * mail_subj() - make subject line for appointment mail. Use "today"
 * or "tomorrow" if appropriate, otherwise use the actual date.
 */
char *
mail_subj(which)
int which;
{
	struct tm Save, day_before;

	if (!ymd_compare(current, today)) {
		if (which == PRI_WEEK)
			sprintf(strbuf, "this week");
		else
			sprintf(strbuf, "today");
	} else {
		Save = current;
		current.tm_mday--;
		fix_current_day();
		day_before = current;
		current = Save;
		if (!ymd_compare(day_before, today))
			sprintf(strbuf, "tomorrow");
		else if (day_first)
			sprintf(strbuf, "%s%s %d %s %d.",
				(which==PRI_WEEK ? "the week of " : ""),
				daynames[current.tm_wday], current.tm_mday,
				monthnames[current.tm_mon], current.tm_year+1900);
		else
			sprintf(strbuf, "%s%s %s %d, %d",
				(which==PRI_WEEK ? "the week of " : ""),
				daynames[current.tm_wday], monthnames[current.tm_mon],
				current.tm_mday, current.tm_year+1900);
	}
	return (strbuf);
}

#ifndef NO_PRINTER
/*
 * Print to Postscript compatable printer.  If we are displaying
 * the year page, then create a raster file of the canvas and
 * feed it to a raster->ps filter (sun2ps).  If on any other page,
 * then print a pretty PostScript calendar with appts written in
 * (as best can be).
 */
void
print_calendar(file_type)
int file_type;
{
	char prntcmd[256];
	FILE *pfp;
	struct tm Save;
	int target;

	lock_cursors();
	working(TRUE);
	sprintf(trailer, "File: '%s' printed for %s on %s",
		apts_pathname, cuserid(NULL), clockstr);
#ifndef RASTER_ONLY
	if (file_type == PR_POSTSCRIPT) {
		if ((pfp = fopen(psfile, "w")) == NULL) {
			err_rpt("can't open tmp ps file", NON_FATAL);
			return;
		}
		switch (mainsw_state) {
			case DISPLAYING_DAY:
				print_psday(pfp, FALSE);
				break;
			case DISPLAYING_WEEK:
				/* start printing on first dow */
				Save = current;
				if (nr_weekdays == 7 && !monday_first)
					target = SUN;
				else
					target = MON;
				if (current.tm_wday >= target)
					while (current.tm_wday-- > target)
						current.tm_mday--;
				else
					/* looking at Sun */
					current.tm_mday++;
				fix_current_day();
				print_psweek(pfp, FALSE);
				current = Save;
				break;
			case DISPLAYING_MONTH:
				print_psmonth(pfp, TRUE);
				break;
			case DISPLAYING_YEAR:
				if (dorasfile()) {
				    if (print_trailer)
					    sprintf(prntcmd, "xpr -device ps -trailer \"%s\" -compact %s > %s",
						trailer, rasfile, psfile);
				    else
					    sprintf(prntcmd, "xpr -device ps -compact %s > %s",
						rasfile, psfile);
				    system(prntcmd);
				    unlink(rasfile);
				}
				break;
		}
		fclose(pfp);
		if (!print_to_file) {
			sprintf(prntcmd, "%s %s", printer, psfile);
			system(prntcmd);
			unlink(psfile);
		}
	} else {
#else
	{
#endif /* RASTER_ONLY */
		if (dorasfile() && !print_to_file) {
			sprintf(prntcmd, "%s -v %s", printer, rasfile);
			system(prntcmd);
			unlink(rasfile);
		}
	}
	working(FALSE);
	unlock_cursors();
}

/*
 * convert screen display to X Window Dump format
 */
/*
 *	excerpted from xwd.c:
 * Copyright 1987 Massachusetts Institute of Technology
 *
 */
int
dorasfile()
{
    FILE *fp;
    unsigned long swaptest = 1;
    XColor *colors;
    unsigned buffer_size;
    int win_name_size;
    int header_size;
    int ncolors, i;
    char *win_name;
    XWindowAttributes win_info;
    XImage *image;
    XWDFileHeader header;

    if ((fp = fopen(rasfile, "w")) == NULL) {
	err_rpt("can't open tmp raster file", NON_FATAL);
	return (0);
    }

    /*
     * Get the parameters of the window being dumped.
     */
    drawable = (Drawable)xv_get(cpwindow, XV_XID);
    if(!XGetWindowAttributes(mydisplay, drawable, &win_info))
	err_rpt("Can't get target window attributes.", FATAL);
    (void)XGetGeometry(mydisplay, drawable, &m_root, &m_x, &m_y,
	    &g_width, &g_height, &m_border, &m_depth);

    win_name = "calentool";
    /* sizeof(char) is included for the null string terminator. */
    win_name_size = strlen(win_name) + sizeof(char);

    /*
     * Snarf the pixmap with XGetImage.
     */
    image = XGetImage (mydisplay, drawable, 0, 0, g_width, g_height, AllPlanes, format);
    if (!image)
	err_rpt("Unable to get image.", FATAL);

    /*
     * Determine the pixmap size.
     */
    buffer_size = Image_Size(image);

    ncolors = Get_XColors(&win_info, &colors);

    /*
     * Calculate header size.
     */
    header_size = sizeof(header) + win_name_size;

    /*
     * Write out header information.
     */
    header.header_size = (CARD32) header_size;
    header.file_version = (CARD32) XWD_FILE_VERSION;
    header.pixmap_format = (CARD32) format;
    header.pixmap_depth = (CARD32) image->depth;
    header.pixmap_width = (CARD32) image->width;
    header.pixmap_height = (CARD32) image->height;
    header.xoffset = (CARD32) image->xoffset;
    header.byte_order = (CARD32) image->byte_order;
    header.bitmap_unit = (CARD32) image->bitmap_unit;
    header.bitmap_bit_order = (CARD32) image->bitmap_bit_order;
    header.bitmap_pad = (CARD32) image->bitmap_pad;
    header.bits_per_pixel = (CARD32) image->bits_per_pixel;
    header.bytes_per_line = (CARD32) image->bytes_per_line;
    header.visual_class = (CARD32) win_info.visual->class;
    header.red_mask = (CARD32) win_info.visual->red_mask;
    header.green_mask = (CARD32) win_info.visual->green_mask;
    header.blue_mask = (CARD32) win_info.visual->blue_mask;
    header.bits_per_rgb = (CARD32) win_info.visual->bits_per_rgb;
    header.colormap_entries = (CARD32) win_info.visual->map_entries;
    header.ncolors = ncolors;
    header.window_width = (CARD32) win_info.width;
    header.window_height = (CARD32) win_info.height;
    header.window_x = 0;
    header.window_y = 0;
    header.window_bdrwidth = (CARD32) win_info.border_width;

    if (*(char *) &swaptest) {
	_swaplong((char *) &header, sizeof(header));
	for (i = 0; i < ncolors; i++) {
	    _swaplong((char *) &colors[i].pixel, sizeof(long));
	    _swapshort((char *) &colors[i].red, 3 * sizeof(short));
	}
    }

    (void) fwrite((char *)&header, sizeof(header), 1, fp);
    (void) fwrite(win_name, win_name_size, 1, fp);

    /*
     * Write out the color maps, if any
     */
    (void) fwrite((char *) colors, sizeof(XColor), ncolors, fp);

    /*
     * Write out the buffer.
     */
    /*
     *    This copying of the bit stream (data) to a file is to be replaced
     *  by an Xlib call which hasn't been written yet.  It is not clear
     *  what other functions of xwd will be taken over by this (as yet)
     *  non-existant X function.
     */
    (void) fwrite(image->data, (int) buffer_size, 1, fp);
    fclose(fp);

    /*
     * free the color buffer.
     */
    if(ncolors > 0) free(colors);

    /*
     * Free image
     */
    XDestroyImage(image);
    return (1);
}

/*
 * Determine the pixmap size.
 */

int Image_Size(image)
     XImage *image;
{
    if (format != ZPixmap)
      return(image->bytes_per_line * image->height * image->depth);

    return(image->bytes_per_line * image->height);
}

#define lowbit(x) ((x) & (~(x) + 1))

/*
 * Get the XColors of all pixels in image - returns # of colors
 */
int Get_XColors(win_info, colors)
     XWindowAttributes *win_info;
     XColor **colors;
{
    int i, ncolors;

    if (!win_info->colormap)
	return(0);

    if (win_info->visual->class == TrueColor)
	return(0);    /* colormap is not needed */

    ncolors = win_info->visual->map_entries;
    if (!(*colors = (XColor *) malloc (sizeof(XColor) * ncolors)))
      err_rpt("Out of memory!", FATAL);

    if (win_info->visual->class == DirectColor) {
	Pixel red, green, blue, red1, green1, blue1;

	red = green = blue = 0;
	red1 = lowbit(win_info->visual->red_mask);
	green1 = lowbit(win_info->visual->green_mask);
	blue1 = lowbit(win_info->visual->blue_mask);
	for (i=0; i<ncolors; i++) {
	  (*colors)[i].pixel = red|green|blue;
	  (*colors)[i].pad = 0;
	  red += red1;
	  if (red > win_info->visual->red_mask)
	    red = 0;
	  green += green1;
	  if (green > win_info->visual->green_mask)
	    green = 0;
	  blue += blue1;
	  if (blue > win_info->visual->blue_mask)
	    blue = 0;
	}
    } else {
	for (i=0; i<ncolors; i++) {
	  (*colors)[i].pixel = i;
	  (*colors)[i].pad = 0;
	}
    }

    XQueryColors(mydisplay, win_info->colormap, *colors, ncolors);
    
    return(ncolors);
}

void
_swapshort (bp, n)
    register char *bp;
    register unsigned n;
{
    register char c;
    register char *ep = bp + n;

    while (bp < ep) {
	c = *bp;
	*bp = *(bp + 1);
	bp++;
	*bp++ = c;
    }
}

void
_swaplong (bp, n)
    register char *bp;
    register unsigned n;
{
    register char c;
    register char *ep = bp + n;
    register char *sp;

    while (bp < ep) {
	sp = bp + 3;
	c = *sp;
	*sp = *bp;
	*bp++ = c;
	sp = bp + 1;
	c = *sp;
	*sp = *bp;
	*bp++ = c;
	bp += 2;
    }
}
#endif	/* NO_PRINTER */
