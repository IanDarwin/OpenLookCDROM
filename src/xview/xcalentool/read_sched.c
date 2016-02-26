/*
 * $Id: read_sched.c,v 2.3 1994/08/19 19:53:08 billr Exp $
 */
/*
 * This routine has been extracted from the month(1L) program.
 */
/***********************************************************
 *  Month - Visual Monthly Calendar and Time/Event Browser
 *
 *  Original Author: Tim Stoehn (zeus!tims)
 *  "Book code" originally written by Scott Turner (srt@ucla-cs.ARPA)
 *  Subsequent Modifications: Jim Hull (hull@hpda) and
 *                            Michael Morrell (morrell@hpda)
 *
 *
 ***********************************************************/
#ifndef lint
/*
static char rcsid[] = "read_sched.c,v 8.4 88/04/04 11:00:49 hull Exp";
*/
#endif

#include <stdio.h>
#include "month.h"
#include "ct.h"
#include <sys/file.h>
#include <sys/stat.h>

extern struct event_rec events;

int
read_schedule(m_dir, read_mode)
char *m_dir;
short read_mode;
{
	char *schedule_file_name;
	unsigned rec_size;
	short file_version;
	int fd;
	struct event_rec event_buf, *event_ptr, *chain_ptr;
	extern float get_version();
	extern char *strcat(), *strcpy();

	schedule_file_name = malloc((unsigned) strlen(m_dir)+8);
	if (schedule_file_name == NULL) {
		fprintf(stderr, "mt2ct: out of memory\n");
		exit(1);
	}
	strcpy(schedule_file_name, m_dir);
	strcat(schedule_file_name, "/.month");

	rec_size = sizeof(struct event_rec);
	umask(0);

	chain_ptr = events.next_event;	/* free old events */
	while (chain_ptr) {
		event_ptr = chain_ptr;
		chain_ptr = chain_ptr->next_event;
		free((char *)event_ptr);
	}
	events.next_event = 0;

        fd = open(schedule_file_name, O_RDONLY);
	free(schedule_file_name);

	if (fd != -1) {
		if (read(fd, (char *) &file_version, sizeof(file_version))
		    == sizeof(file_version)) {
		    if (file_version != (int) get_version()) {
			close(fd);
			fd = -1;
			return(file_version);
		    }
		} else {	/* no version field so assume empty file */
		    if (read_mode == READ_ONLY) {
			close(fd);
			fd = -1;
		    }
		    return(1);
		}

		chain_ptr = &events;

		while (read(fd, &event_buf, rec_size) == rec_size) {
			if ((event_ptr = (struct event_rec *)malloc(rec_size))) {
				chain_ptr->next_event = event_ptr;
				chain_ptr = event_ptr;
				*chain_ptr = event_buf;
				chain_ptr->next_event = (struct event_rec *)0;
			} else
				break;
		}
		if (read_mode == READ_ONLY) {
		    close(fd);
		    fd = -1;
		}
		return(0);
	}
	/* if here, open failed */
	return(-1);
}

float
get_version()
{
	return (8.7);
}
