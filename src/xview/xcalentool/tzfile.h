/* Small subset of tzfile.h, for systems that lack it */

/* Funcdamental constants of the implementation :-)) */

#define	SECS_PER_MIN	60
#define	MINS_PER_HOUR	60
#define	HOURS_PER_DAY	24
#define	DAYS_PER_WEEK	7
#define	DAYS_PER_NYEAR	365
#define	DAYS_PER_LYEAR	366
#define	SECS_PER_HOUR	(SECS_PER_MIN * MINS_PER_HOUR)
#define	SECS_PER_DAY	((long) SECS_PER_HOUR * HOURS_PER_DAY)
#define	MONS_PER_YEAR	12

/* Convenience macros */

/* isleap(yr) -- true if yr is a leap year, only valid if yr>=1752 */
#define	isleap(yr) (((yr) % 4) == 0 && ((yr) % 100 ) != 0) || ((yr % 400) == 0)
