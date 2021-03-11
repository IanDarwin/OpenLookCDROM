/*
 * Copyright 1993 Network Computing Devices, Inc.
 *
 * Permission to use, copy, modify, distribute, and sell this software and
 * its documentation for any purpose is hereby granted without fee, provided
 * that the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the name Network Computing Devices, Inc. not be
 * used in advertising or publicity pertaining to distribution of this 
 * software without specific, written prior permission.
 * 
 * THIS SOFTWARE IS PROVIDED 'AS-IS'.  NETWORK COMPUTING DEVICES, INC.,
 * DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING WITHOUT
 * LIMITATION ALL IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
 * PARTICULAR PURPOSE, OR NONINFRINGEMENT.  IN NO EVENT SHALL NETWORK
 * COMPUTING DEVICES, INC., BE LIABLE FOR ANY DAMAGES WHATSOEVER, INCLUDING
 * SPECIAL, INCIDENTAL OR CONSEQUENTIAL DAMAGES, INCLUDING LOSS OF USE, DATA,
 * OR PROFITS, EVEN IF ADVISED OF THE POSSIBILITY THEREOF, AND REGARDLESS OF
 * WHETHER IN AN ACTION IN CONTRACT, TORT OR NEGLIGENCE, ARISING OUT OF OR IN
 * CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 * 
 * Author:	Jim Fulton <jim@ncd.com>
 * 		Network Computing Devices
 * 
 * $NCDId: @(#)auinfo.c,v 1.14 1994/04/07 18:20:41 greg Exp $
 */

#include <stdio.h>
#include <audio/audiolib.h>

char *ProgramName;

#if NeedFunctionPrototypes
#define PROTO(list) list
#else
#define PROTO(list) ()
#endif

#ifdef __STDC__                         /* string concat macros */
#define STRCONCAT(a,b) a##b
#define STRCONCAT3(a,b,c) a##b##c
#define STRSTRINGIFY(a) #a
#else
#define STRCONCAT(a,b) a/**/b
#define STRCONCAT3(a,b,c) a/**/b/**/c
#define STRSTRINGIFY(a) "a"
#endif

static void print_formats PROTO((AuServer *));
static void print_devices PROTO((AuServer *));
static void print_server_buckets PROTO((AuServer *));
static void print_client_buckets PROTO((AuServer *));
static void print_elem_types PROTO((AuServer *));
static void print_wave_forms PROTO((AuServer *));
static void print_actions PROTO((AuServer *));

static int maxwidth = 0;		/* reset later */
#define TITLELEN 24
static _AuConst char *TITLEFMT = "%-24s";
#define INDENTPADSTR "    "
#define INDENTPADLEN (sizeof(INDENTPADSTR) - 1)
static _AuConst char *INDENTPAD = INDENTPADSTR;

int
main (argc, argv)
    int argc;
    char **argv;
{
    int i;
    char *audioname = NULL;
    AuServer *aud;
    char *msg = NULL;

    ProgramName = argv[0];

    for (i = 1; i < argc; i++) {
	char *arg = argv[i];

	if (arg[0] == '-') {
	    switch (arg[1]) {
	      case 'a':			/* -audio server */
		if (++i >= argc) goto usage;
		audioname = argv[i];
		continue;
	      case 'w':			/* -width num */
		if (++i >= argc) goto usage;
		maxwidth = atoi (argv[i]);
		continue;
	    }
	}
      usage:
	fprintf (stderr, "usage:  %s [-audio server] [-width num]\n",
		 ProgramName);
	exit (1);
    }

    if (maxwidth < INDENTPADLEN)
	maxwidth = 79;

    aud = AuOpenServer (audioname, 0, NULL, 0, NULL, &msg);
    if (!aud) {
	fprintf (stderr, "%s:  unable to connect to audio server\n",
		 ProgramName);
	exit (1);
    }

    printf (TITLEFMT, "Audio Server:");
    printf ("%s\n", AuServerString(aud));
    printf (TITLEFMT, "Version Number:");
    printf ("%d.%d\n", AuServerProtocolMajorVersion(aud),
		       AuServerProtocolMinorVersion(aud));
    printf (TITLEFMT, "Vendor:");
    printf ("%s\n", AuServerVendor(aud));
    printf (TITLEFMT, "Vendor Release:");
    printf ("%d\n", AuServerVendorRelease(aud));
    printf (TITLEFMT, "Min Sample Rate:");
    printf ("%d\n", AuServerMinSampleRate(aud));
    printf (TITLEFMT, "Max Sample Rate:");
    printf ("%d\n", AuServerMaxSampleRate(aud));
    printf (TITLEFMT, "Max Tracks:");
    printf ("%d\n", AuServerMaxTracks(aud));
    printf (TITLEFMT, "Number of Formats:");
    printf ("%d\n", AuServerNumFormats(aud));
    print_formats (aud);
    printf (TITLEFMT, "Number of Elem Types:");
    printf ("%d\n", AuServerNumElementTypes(aud));
    print_elem_types (aud);
    printf (TITLEFMT, "Number of Wave Forms:");
    printf ("%d\n", AuServerNumWaveForms(aud));
    print_wave_forms (aud);
    printf (TITLEFMT, "Number of Actions:");
    printf ("%d\n", AuServerNumActions(aud));
    print_actions (aud);
    printf (TITLEFMT, "Number of Devices:");
    printf ("%d\n", AuServerNumDevices(aud));
    print_devices (aud);
    printf (TITLEFMT, "Number of Buckets:");
    printf ("%d\n", AuServerNumBuckets(aud));
    print_server_buckets (aud);
    print_client_buckets (aud);
    AuCloseServer (aud);
    exit (0);
}

typedef struct _nametable {
    int value;
    _AuConst char *name;
    int len;
} NameTable;

/*
 * Warning: you must not put any spaces between the , and the "what" value
 * when invoking the following macro.  Otherwise you will screw up the
 * string concatenation.
 */
#define MAKEENTRY(prefix,what) { STRCONCAT(prefix,what), STRSTRINGIFY(what), \
				     sizeof(STRSTRINGIFY(what)) - 1 }
#define MBITENTRY(prefix,what) { STRCONCAT3(prefix,what,Mask), \
				     STRSTRINGIFY(what), \
				     sizeof(STRSTRINGIFY(what)) - 1 }
#define NENTRIES(tab) (sizeof((tab))/sizeof((tab)[0]))

static NameTable _nt_formats[] = {
    MAKEENTRY (AuFormat,ULAW8),		/* no spaces allowed in macro */
    MAKEENTRY (AuFormat,LinearUnsigned8),
    MAKEENTRY (AuFormat,LinearSigned8),
    MAKEENTRY (AuFormat,LinearSigned16MSB),
    MAKEENTRY (AuFormat,LinearUnsigned16MSB),
    MAKEENTRY (AuFormat,LinearSigned16LSB),
    MAKEENTRY (AuFormat,LinearUnsigned16LSB),
};

static NameTable _nt_elemtypes[] = {
    MAKEENTRY (AuElementType,ImportClient),
    MAKEENTRY (AuElementType,ImportDevice),
    MAKEENTRY (AuElementType,ImportBucket),
    MAKEENTRY (AuElementType,ImportWaveForm),
    MAKEENTRY (AuElementType,ImportRadio),
    MAKEENTRY (AuElementType,Bundle),
    MAKEENTRY (AuElementType,MultiplyConstant),
    MAKEENTRY (AuElementType,AddConstant),
    MAKEENTRY (AuElementType,Sum),
    MAKEENTRY (AuElementType,ExportClient),
    MAKEENTRY (AuElementType,ExportDevice),
    MAKEENTRY (AuElementType,ExportBucket),
    MAKEENTRY (AuElementType,ExportRadio),
    MAKEENTRY (AuElementType,ExportMonitor),
};

static NameTable _nt_waveforms[] = {
    MAKEENTRY (AuWaveForm,Square),
    MAKEENTRY (AuWaveForm,Sine),
    MAKEENTRY (AuWaveForm,Saw),
    MAKEENTRY (AuWaveForm,Constant),
};

static NameTable _nt_actions[] = {
    MAKEENTRY (AuElementAction,ChangeState),
    MAKEENTRY (AuElementAction,SendNotify),
    MAKEENTRY (AuElementAction,Noop),
};

static NameTable _nt_devicechangables[] = {
    MBITENTRY (AuCompCommon,ID),
    MBITENTRY (AuCompCommon,Kind),
    MBITENTRY (AuCompCommon,Use),
    MBITENTRY (AuCompCommon,Format),
    MBITENTRY (AuCompCommon,NumTracks),
    MBITENTRY (AuCompCommon,Access),
    MBITENTRY (AuCompCommon,Description),

    MBITENTRY (AuCompDevice,MinSampleRate),
    MBITENTRY (AuCompDevice,MaxSampleRate),
    MBITENTRY (AuCompDevice,Location),
    MBITENTRY (AuCompDevice,Gain),
    MBITENTRY (AuCompDevice,LineMode),
    MBITENTRY (AuCompDevice,Children),
};


static NameTable _nt_compkinds[] = {
    MAKEENTRY (AuComponentKind,PhysicalInput),
    MAKEENTRY (AuComponentKind,PhysicalOutput),
    MAKEENTRY (AuComponentKind,Bucket),
    MAKEENTRY (AuComponentKind,Radio),
};


static NameTable _nt_uses[] = {
    MBITENTRY (AuComponentUse,Import),
    MBITENTRY (AuComponentUse,Export),
};

static NameTable _nt_accesses[] = {
    MBITENTRY (AuAccess,Import),
    MBITENTRY (AuAccess,Export),
    MBITENTRY (AuAccess,Destroy),
    MBITENTRY (AuAccess,List),
};

static NameTable _nt_locations[] = {
    MBITENTRY (AuDeviceLocation,Left),
    MBITENTRY (AuDeviceLocation,Center),
    MBITENTRY (AuDeviceLocation,Right),
    MBITENTRY (AuDeviceLocation,Top),
    MBITENTRY (AuDeviceLocation,Middle),
    MBITENTRY (AuDeviceLocation,Bottom),
    MBITENTRY (AuDeviceLocation,Back),
    MBITENTRY (AuDeviceLocation,Front),
    MBITENTRY (AuDeviceLocation,Internal),
    MBITENTRY (AuDeviceLocation,External),
};

static NameTable _nt_linemodes[] = {
    MAKEENTRY (AuDeviceLineMode,None),
    MAKEENTRY (AuDeviceLineMode,Low),
    MAKEENTRY (AuDeviceLineMode,High),
};

#define TA_NONE		0
#define TA_INDIVIDUAL	1
#define TA_ALL		2

#define TEXT_ALIGNMENT	TA_NONE

#if TEXT_ALIGNMENT == TA_INDIVIDUAL
#define ROUNDUP(n) ((((n) + 7) >> 3) << 3)
static int _table_maxlen (tab, nents)
    register NameTable *tab;
    register int nents;
{
    register int m = 0;

    while (nents > 0) {
	if (tab->len > m)
	    m = tab->len;
	nents--;
	tab++;
    }
    return m;
}
#endif

#if TEXT_ALIGNMENT == TA_ALL
#define ALL_COLUMN_WIDTH 24
#endif

static NameTable *_lookup_name (tab, nents, val, isbit)
    register NameTable *tab;
    register int nents;
    register int val;
    register AuBool isbit;		/* use bit compare instead of equal? */
{
    static NameTable nttmp;
    static char tmpbuf[20];

    if (isbit && !val)
	return (NameTable *) 0;

    while (nents > 0) {
	if (isbit ? (tab->value & val) : (tab->value == val))
	    return tab;
	nents--;
	tab++;
    }
    if (isbit)
	return (NameTable *) 0;

    sprintf (tmpbuf, (isbit ? "0x%lx" : "%d"), val);
    nttmp.value = val;
    nttmp.name = tmpbuf;
    nttmp.len = strlen (tmpbuf);
    return &nttmp;
}


static void _print_names (aud, tab, nents, title, total, getvaliter, p, isbit)
    AuServer *aud;
    NameTable *tab;
    int nents;
    _AuConst char *title;
    int total;
    int (*getvaliter) PROTO((AuServer *, int, AuPointer));
    AuPointer p;
    AuBool isbit;
{
    int i;
    char fmt[20];
    int col;
#if TEXT_ALIGNMENT == TA_NONE
    int move;
#endif
#if TEXT_ALIGNMENT == TA_INDIVIDUAL
    int maxlen = _table_maxlen (tab, nents);
    int move = ROUNDUP(maxlen);

    if (move == maxlen)
	move += 8;
#endif
#if TEXT_ALIGNMENT == TA_ALL
    int move = ALL_COLUMN_WIDTH;
#endif

    if (total < 1)
	return;

    printf (TITLEFMT, title);
    col = TITLELEN;
#if TEXT_ALIGNMENT == TA_NONE
    strcpy (fmt, "%s  ");
#define TAPAD 2
#else
    sprintf (fmt, "%%-%ds", move);	/* reset for moving */
#endif

    for (i = 0; i < total; i++) {
	int val = (*getvaliter) (aud, i, p);
	NameTable *nt = _lookup_name (tab, nents, val, isbit);
	_AuConst char *f = fmt;

	if (!nt)
	    continue;

#if TEXT_ALIGNMENT == TA_NONE
	move = nt->len + TAPAD;
#endif

	if (col != TITLELEN) {
	    if (col + nt->len > maxwidth) {
		printf ("\n");
		printf (TITLEFMT, "");
		col = TITLELEN;
	    } else if (col + move > maxwidth) {
		f = "%s";
	    }
	}
	printf (f, nt->name);
	col += move;
	if (col > maxwidth) {
	    printf ("\n");
	    printf (TITLEFMT, "");
	    col = TITLELEN;
	}
    }
    printf ("\n");
}


/* ARGSUSED2 */
static int _get_format (aud, i, p)
    AuServer *aud;
    int i;
    AuPointer p;
{
    return AuServerFormat (aud, i);
}

static void print_formats (aud)
    AuServer *aud;
{
    _print_names (aud, _nt_formats, NENTRIES(_nt_formats), 
		  "Formats:", AuServerNumFormats(aud), _get_format, 
		  NULL, AuFalse);
}

/* ARGSUSED */
static int _get_use (aud, i, p)
    AuServer *aud;
    int i;
    AuPointer p;
{
    return (1 << i) & AuCommonUse ((AuCommonPart *) p);
}

/* ARGSUSED */
static int _get_access (aud, i, p)
    AuServer *aud;
    int i;
    AuPointer p;
{
    return (1 << i) & AuCommonAccess ((AuCommonPart *) p);
}

static void print_comp (aud, c)
    AuServer *aud;
    register AuCommonPart *c;
{
    register AuMask vmask = AuCommonValueMask (c);

    if (vmask & AuCompCommonIDMask)
	printf ("%s    ID:             0x%lx\n", INDENTPAD,
		AuCommonIdentifier(c));
    if (vmask & AuCompCommonKindMask) {
	NameTable *nt = _lookup_name (_nt_compkinds, NENTRIES(_nt_compkinds),
				      AuCommonKind(c), AuFalse);
	printf ("%s    Kind:           %s\n", INDENTPAD, nt->name);
    }
    if (vmask & AuCompCommonUseMask) 
	_print_names (aud, _nt_uses, NENTRIES(_nt_uses),
		      "        Use:", NENTRIES(_nt_uses), _get_use,
		      (AuPointer) c, AuTrue);
    if (vmask & AuCompCommonFormatMask) {
	NameTable *nt = _lookup_name (_nt_formats, NENTRIES(_nt_formats),
				      AuCommonFormat(c), AuFalse);
	printf ("%s    Format:         %s\n", INDENTPAD, nt->name);
    }
    if (vmask & AuCompCommonNumTracksMask)
	printf ("%s    Num Tracks:     %d\n", INDENTPAD, AuCommonNumTracks(c));
    if (vmask & AuCompCommonAccessMask)
	_print_names (aud, _nt_accesses, NENTRIES(_nt_accesses),
		      "        Access:", NENTRIES(_nt_accesses), _get_access,
		      (AuPointer) c, AuTrue);
    if (vmask & AuCompCommonDescriptionMask)
	printf ("%s    Description:    \"%s\"\n", INDENTPAD, 
		(AuCommonDescription(c)->data ? AuCommonDescription(c)->data :
		 ""));
}


/* ARGSUSED */
static int _get_devicechangables (aud, i, p)
    AuServer *aud;
    int i;
    AuPointer p;
{
    return (1 << i) & AuDeviceChangableMask((AuDeviceAttributes *) p);
}

/* ARGSUSED */
static int _get_locations (aud, i, p)
    AuServer *aud;
    int i;
    AuPointer p;
{
    return (1 << i) & AuDeviceLocation ((AuDeviceAttributes *) p);
}


static void print_device (aud, d)
    AuServer *aud;
    AuDeviceAttributes *d;
{
    AuMask vmask = AuDeviceValueMask (d);

    if (AuDeviceChangableMask(d))
	_print_names (aud, _nt_devicechangables,
		      NENTRIES(_nt_devicechangables),
		      "        Changable:", 32,
		      _get_devicechangables, (AuPointer) d, AuTrue);
    print_comp (aud, &d->common);

    if (vmask & AuCompDeviceMinSampleRateMask)
	printf ("%s    Min Rate:       %d\n", INDENTPAD,
		AuDeviceMinSampleRate(d));
    if (vmask & AuCompDeviceMaxSampleRateMask)
	printf ("%s    Max Rate:       %d\n", INDENTPAD,
		AuDeviceMaxSampleRate(d));
    if (vmask & AuCompDeviceLocationMask)
	_print_names (aud, _nt_locations, NENTRIES(_nt_locations),
		      "        Location:", NENTRIES(_nt_locations),
		      _get_locations, (AuPointer) d, AuTrue);
    if (vmask & AuCompDeviceGainMask)
	printf ("%s    Gain Percent:   %d\n", INDENTPAD,
		AuFixedPointRoundDown(AuDeviceGain(d)));
    if (vmask & AuCompDeviceLineModeMask) {
	NameTable *nt = _lookup_name (_nt_linemodes, NENTRIES(_nt_linemodes),
				      AuDeviceLineMode(d), AuFalse);
	printf ("%s    Line Mode:      %s\n", INDENTPAD, nt->name);
    }
    if (vmask & AuCompDeviceChildrenMask) {
	int i;
	AuDeviceID *kids = AuDeviceChildren(d);

	printf ("%s    Num Children:   %d\n", INDENTPAD,
		AuDeviceNumChildren(d));
	if (AuDeviceNumChildren(d) > 0) {
	    printf ("%s    Children:       ", INDENTPAD);
	    for (i = 0; i < AuDeviceNumChildren(d); i++)
		printf ("0x%lx ", kids[i]);
	    printf ("\n");
	}
    }
}

static void print_devices (aud)
    AuServer *aud;
{

    int i;

    for (i = 0; i < AuServerNumDevices(aud); i++) {
	printf ("%sDevice %d:\n", INDENTPAD, i);
	print_device (aud, AuServerDevice(aud,i));
    }
}

static void
print_bucket(aud, ba)
    AuServer	*aud;
    AuBucketAttributes	*ba;
{
    AuMask vmask = AuBucketValueMask (ba);

    print_comp(aud, &ba->common);
    if (vmask & AuCompBucketSampleRateMask)
	printf ("%s    Sample Rate:    %d\n", INDENTPAD,
		AuBucketSampleRate(ba));
    if (vmask & AuCompBucketNumSamplesMask)
	printf ("%s    Num Samples:    %d\n", INDENTPAD,
		AuBucketNumSamples(ba));
}


static void print_server_buckets (aud)
    AuServer *aud;
{
    int i;

    for (i = 0; i < AuServerNumBuckets(aud); i++) {
	printf("%sBucket %d:\n", INDENTPAD, i);
	print_bucket (aud, AuServerBucket(aud, i));
    }
}


static void print_client_buckets (aud)
    AuServer *aud;
{
    int	num, i;
    AuBucketAttributes	*bas;
    AuBool first = AuTrue;

    bas = AuListBuckets(aud, AuNone, NULL, &num, NULL);

    for (i = 0; i < num; i++) {
	if (AuClientOfID(aud, AuBucketIdentifier(&bas[i])))
	{
	    if (first)
	    {
		printf("Client Buckets:\n");
		first = AuFalse;
	    }
	    else
		printf("\n");
	    print_bucket (aud, &bas[i]);
	}
    }
    AuFreeBucketAttributes(aud, num, bas);
}


/* ARGSUSED2 */
static int _get_elem_type (aud, i, p)
    AuServer *aud;
    int i;
    AuPointer p;
{
    return AuServerElementType (aud, i);
}

static void print_elem_types (aud)
    AuServer *aud;
{
    _print_names (aud, _nt_elemtypes, NENTRIES(_nt_elemtypes),
		  "Element Types:", AuServerNumElementTypes(aud),
		  _get_elem_type, NULL, AuFalse);
}


/* ARGSUSED2 */
static int _get_wave_form (aud, i, p)
    AuServer *aud;
    int i;
    AuPointer p;
{
    return AuServerWaveForm (aud, i);
}

static void print_wave_forms (aud)
    AuServer *aud;
{
    _print_names (aud, _nt_waveforms, NENTRIES(_nt_waveforms), 
		  "Wave Forms:", AuServerNumWaveForms(aud), _get_wave_form,
		  NULL, AuFalse);
}


/* ARGSUSED2 */
static int _get_action (aud, i, p)
    AuServer *aud;
    int i;
    AuPointer p;
{
    return AuServerAction (aud, i);
}

static void print_actions (aud)
    AuServer *aud;
{
    _print_names (aud, _nt_actions, NENTRIES(_nt_actions), 
		  "Actions:", AuServerNumActions(aud), _get_action, 
		  NULL, AuFalse);
}



