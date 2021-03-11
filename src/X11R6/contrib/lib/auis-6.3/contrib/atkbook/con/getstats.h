/* ********************************************************************** *\
 *         Copyright IBM Corporation 1988,1991 - All Rights Reserved      *
 *        For full copyright information see:'andrew/config/COPYRITE'     *
\* ********************************************************************** */

/* Additions to this list should be supplemented by additions to the corresponding lists in getstats.c and  getstatsob.c */

#define LOADCPU 1 /* Percentage of time CPU is not idle */
#define LOADIO 2 /* Percentage of time in use for I/O */
#define LOADUSER 3 /* Percentage of time CPU devoted to user processes */
#define	LOADSYS	4 /* Percentage of time CPU devoted to system processes */
#define LOADIDLE 5 /* Percentage of time CPU is idle */
#define VM 6 /* Percentage of available virtual memory in use */
#define PAGEIN 7 /* Pages paged in to real memory */
#define PAGEOUT 8 /* Pages paged out */
#define PAGEREPLACABLE 9 /* Pages deemed to be available to page out */
#define PAGEDEFICIT 10 /* Pages by which system is short of working set */
#define MEMACTIVE 11 /* Percentage of available memory in use */
#define MEMFREE 12 /* Percentage of available memory free */
#define QUEUERUN 13 /* Number of processes in the run queue */
#define QUEUEBLOCK 14 /* Number of processes blocked for some reason */
#define QUEUEMEM 15 /* Number of processes blocked awaiting memory */
#define INTSIO 16 /* I/O interrupts */
#define INTSSYS 17 /* System interrupts */
#define INTSSWAP 18 /* Swap interrupts */
#define NDSTATIN 19 /* For network disk systems, recent blocks (?) in */
#define NDSTATOUT 20 /* Network disk blocks (?) out */
#define NDSTATERR 21 /* Network disk error count */
#define PROCSUSER 22 /* Percentage of current user's available processes in use */
#define PROCSTOTAL 23 /* Percentage of total available processes in use */
#define PROCSOTHER 24 /* Number of processes used not by root or current user */
#define DISK1 25 /* Percentage of first disk in use */
#define DISK2 26 /* Percentage of second disk in use */
#define DISK3 27 /* Percentage of third disk in use */
#define DISK4 28 /* Percentage of fourth disk in use */
#define DISK5 29 /* Percentage of fifth disk in use */
#define DISK6 30 /* Percentage of sixth disk in use */
#define LASTVAL 30
