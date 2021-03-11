/*
 * Dclock.c -- a digital clock widget.
 * Copyright (c) 1988 Dan Heller <argv@sun.com>
 *     Modified  1991 Herve Soulard <soulard@sor.inria.fr>
 *          - support for English and French language
 *          - support for audio on Sparc station
 */
#ifndef _XtDclock_h
#define _XtDclock_h

/* Parameters:

 Name                Class              RepType         Default Value
 ----                -----              -------         -------------
 alarm		     Boolean		Boolean		False
 alarmTime	     Time		long		00:00:00
 alarm_snd           String             String          Bell
 displayTime	     Boolean		Boolean		True
 background          Background         pixel           White
 bell		     Boolean		Boolean		False
 border              BorderColor        pixel           Black      
 borderWidth         BorderWidth        int             1
 date		     String		String		NULL
 destroyCallback     Callback           Pointer         NULL
 foreground          Foreground         Pixel           Black 
 half_hours_snd      String             String          Bell
 height              Height             int             80
 hours_snd           String             String          Bell
 language	     String		String		English
 mappedWhenManaged   MappedWhenManaged  Boolean         True
 reverseVideo        ReverseVideo       Boolean         False
 seconds	     Boolean		Boolean		False
 scroll		     Boolean		Boolean		True
 fade		     Boolean		Boolean		False
 fadeRate	     Time		int		50
 tails		     Boolean		Boolean		False
 time		     Time		long		current time
 x                   Position           int             0 
 y                   Position           int             0

*/

#ifndef XtRDimension
#define XtRDimension	"dimension"
#endif /* XtRDimension */

#define XtRTime		"Time"
#define XtCTime		"Time"

#define XtNseconds	"seconds"
#define XtNalarm	"alarm"
#define XtNalarmTime	"alarmTime"
#define XtNdisplayTime	"displayTime"
#define XtNtime		"time"
#define XtNbell		"bell"
#define XtNscroll	"scroll"
#define XtNfade		"fade"
#define XtNfadeRate	"fadeRate"
#define XtNtails	"tails"
#define XtNdate		"date"
#define XtNmilitaryTime	"militaryTime"
#define XtNlanguage     "language"
#define XtNhours_snd    "hours_snd"
#define XtNhalf_hours_snd    "half_hours_snd"
#define XtNalarm_snd    "alarm_snd"

typedef struct _DclockRec *DclockWidget;
typedef struct _DclockClassRec *DclockWidgetClass;

extern WidgetClass dclockWidgetClass;

#endif /* _XtDclock_h */
