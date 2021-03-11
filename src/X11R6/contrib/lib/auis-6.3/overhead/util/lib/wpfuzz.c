/* ********************************************************************** *\
 *         Copyright IBM Corporation 1988,1991 - All Rights Reserved      *
 *        For full copyright information see:'andrew/config/COPYRITE'     *
\* ********************************************************************** */

/*
	$Disclaimer: 
 * Permission to use, copy, modify, and distribute this software and its 
 * documentation for any purpose is hereby granted without fee, 
 * provided that the above copyright notice appear in all copies and that 
 * both that copyright notice, this permission notice, and the following 
 * disclaimer appear in supporting documentation, and that the names of 
 * IBM, Carnegie Mellon University, and other copyright holders, not be 
 * used in advertising or publicity pertaining to distribution of the software 
 * without specific, written prior permission.
 * 
 * IBM, CARNEGIE MELLON UNIVERSITY, AND THE OTHER COPYRIGHT HOLDERS 
 * DISCLAIM ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING 
 * ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS.  IN NO EVENT 
 * SHALL IBM, CARNEGIE MELLON UNIVERSITY, OR ANY OTHER COPYRIGHT HOLDER 
 * BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY 
 * DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, 
 * WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS 
 * ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE 
 * OF THIS SOFTWARE.
 *  $
*/

#ifndef NORCSID
#define NORCSID
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/overhead/util/lib/RCS/wpfuzz.c,v 2.12 1993/05/04 00:53:32 susan Exp $";
#endif

/* ************************************************************ *\
	wpfuzz.c
	Common procedures and data for manipulating the fields and B-tree
	making up the WP database.
	For use only internally to the WP library.
\* ************************************************************ */

#include <andyenv.h>
#include <stdio.h>
#include <ctype.h>
#include <andrewos.h>		/* strings.h */
#include <util.h>
#include <pwd.h>
#ifdef WHITEPAGES_ENV  /* avoid makedepend "errors" */
#include <wp.h>
#include <btwp.h>
#endif /* WHITEPAGES_ENV   */

extern int bwDebugging;

#ifndef _IBMR2
extern char *malloc(), *realloc();
#endif /* _IBMR2 */

static int CmpIxVal(Loc1, Loc2)
char *Loc1, *Loc2;
{/* Helper (comparator) procedure for wpSortIxValue and qsort. */
    return strncmp(Loc1, Loc2, PKLEN);
}

int wpSortIxValue(IxLoc, IxSize)
char *IxLoc; int IxSize;
{/* Sort the array of index values at IxLoc (of length IxSize, in units of PKLEN), in place, and flush duplicates.  Return the size of the canonicalized array, in units of PKLEN. */
    char *Src, *Dst, *TopLoc;
    int DstCount;

    if (IxSize <= 1) return IxSize;	/* 0-len, 1-len lists easy to canonicalize */
    qsort(IxLoc, IxSize, PKLEN, CmpIxVal);
    Dst = IxLoc;
    Src = Dst + PKLEN;
    TopLoc = IxLoc + (IxSize * PKLEN);
    DstCount = 1;
    for (; Src < TopLoc; Src += PKLEN) {
	if (strncmp(Src, Dst, PKLEN) != 0) {
		Dst += PKLEN;
		strncpy(Dst, Src, PKLEN);
		++DstCount;
	}
    }
    return (DstCount);
}

/* Routines to canonicalize surnames, given names, and nicknames. */

struct Map {char *Left, *Right; int LeftLen;};

static struct Map InitRules[] = {
	{"ya", "ja", 2},
	{"ye", "je", 2},
	{"yi", "ji", 2},
	{"yo", "jo", 2},
	{"yu", "ju", 2},
	{"y", "i", 1},
	{"x", "z", 1},
	{"wr", "r", 2},
	{"hr", "r", 2},
	{"pf", "f", 2},
	{"ir", "er", 2},
	{"i", "e", 1},
	{"a", "e", 1}
};
#define NumInitRules  (sizeof(InitRules) / sizeof(InitRules[0]))

static struct Map InitCRules[] = {
	{"mcg", "mak", 3},
	{"macg", "mak", 4},
	{"mage", "make", 4},
	{"mcgh", "mak", 4},
	{"macgh", "mak", 5},
	{"mac", "mak", 3},
	{"mc", "mak", 2},
	{"kn", "n", 2},
	{"cz", "z", 2},
	{"ps", "s", 2},
	{"georg", "georj", 5},
	{"angel", "anjel", 5},
	{"engel", "engle", 5},
	{"inge", "inga", 4},
	{"gey", "gay", 3},
	{"gei", "gai", 3},
	{"geh", "gah", 3},
	{"gea", "gaa", 3},
	{"gig", "gaig", 3}
};
#define NumInitCRules  (sizeof(InitCRules) / sizeof(InitCRules[0]))

static struct Map TermRules[] = {
	{"witz", "vitz", 4},
	{"rnst", "rnest", 4},
	{"ng", "n", 2},
	{"nds", "ns", 3},
	{"nts", "ns", 3},
	{"tt", "t", 2},
	{"dt", "t", 2},
	{"lz", "ltz", 2},
	{"mz", "mtz", 2},
	{"nz", "ntz", 2},
	{"rz", "rtz", 2},
	{"ld", "l", 2},
	{"md", "m", 2},
	{"nd", "n", 2},
	{"rd", "r", 2},
	{"rt", "r", 2},
	{"nt", "n", 2},
	{"mt", "m", 2},
	{"lt", "l", 2}
};
#define NumTermRules  (sizeof(TermRules) / sizeof(TermRules[0]))

static struct Map TermGivRules[] = {
	{"witz", "vitz", 4},
	{"rnst", "rnest", 4},
	{"ng", "n", 2},
	{"nds", "ns", 3},
	{"nts", "ns", 3},
	{"tt", "t", 2},
	{"dt", "t", 2},
	{"lz", "ltz", 2},
	{"mz", "mtz", 2},
	{"nz", "ntz", 2},
	{"rz", "rtz", 2}
};
#define NumTermGivRules  (sizeof(TermGivRules) / sizeof(TermGivRules[0]))

static struct Map TermCRules[] = {
	{"lch", "lsh", 3},
	{"mch", "msh", 3},
	{"nch", "nsh", 3},
	{"rch", "rsh", 3},
	{"shch", "sch", 4},
	{"sch", "sh", 3},
	{"ch", "k", 2},
	{"cs", "ks", 2},
	{"agh", "of", 3},
	{"dge", "j", 3},
	{"rge", "rj", 3},
	{"ge", "g", 2},
	{"ll", "l", 2},
	{"nn", "n", 2},
	{"gel", "gle", 3},
	{"cun", "son", 3},
	{"nce", "ntz", 3},
	{"nz", "ntz", 2},
	{"isle", "ile", 4}
};
#define NumTermCRules  (sizeof(TermCRules) / sizeof(TermCRules[0]))

static struct Map Rules[] = {
	{"y", "i", 1},
	{"x", "ks", 1},
	{"ough", "of", 4},
	{"augh", "of", 4},
	{"gh", "h", 2},
	{"qu", "kw", 2},
	{"q", "k", 1},
	{"tk", "k", 2},
	{"ph", "f", 2},
	{"mps", "ms", 3},
	{"stm", "sm", 3},
	{"stn", "sn", 3},
	{"rtn", "rn", 3},
	{"ktl", "kl", 3},
	{"rdn", "rn", 3},
	{"ldw", "lw", 3},
	{"dt", "t", 2},
	{"dj", "j", 2},
	{"dz", "z", 2},
	{"nd", "n", 2},
	{"nst", "ns", 3},
	{"dd", "t", 2},
	{"tz", "ts", 2},	/* this one's suspect */
	{"st.", "saint", 3}
};
#define NumRules  (sizeof(Rules) / sizeof(Rules[0]))

static struct Map CRules[] = {
	{"tsch", "ch", 4},
	{"shch", "sch", 4},
	{"sch", "sh", 3},
	{"tch", "ch", 3},
	{"glie", "lie", 4},
	{"gae", "gaea", 3},
	{"ae", "ea", 2},
	{"eie", "uye", 3},
	{"eye", "uye", 3},
	{"cs", "s", 2},
	{"rdt", "rt", 3}
};
#define NumCRules  (sizeof(CRules) / sizeof(CRules[0]))

static int ApplyInit(Ruleset, NumRuleset, str, Title)
struct Map Ruleset[];
int NumRuleset;
char *str, *Title;
{/* Apply the given ruleset at the beginning of the string. */
	register char *Src, *Dst, *LastSrc;
	register int Which, Changed, RightLen;
	int EverChanged = 0;

	for (;;) {
	    Changed = 0;
	    for (Which = 0; Which < NumRuleset; ++Which) {
		if (strncmp(str, Ruleset[Which].Left, Ruleset[Which].LeftLen) == 0) {
		    if (bwDebugging) fprintf(stderr, "[(%s) %s -> ", Title, str);
		    RightLen = strlen(Ruleset[Which].Right);
		    if (RightLen <= Ruleset[Which].LeftLen) {
			strncpy(str, Ruleset[Which].Right, RightLen);
			strcpy(str + RightLen, str + Ruleset[Which].LeftLen);
		    } else {
			Src = &str[strlen(str)];
			LastSrc = str + Ruleset[Which].LeftLen;
			Dst = Src + RightLen - Ruleset[Which].LeftLen;
			while (Src >= LastSrc) *Dst-- = *Src--;
			strncpy(str, Ruleset[Which].Right, RightLen);
		    }
		    if (bwDebugging) fprintf(stderr, "%s]\n", str);
		    EverChanged = Changed = 1;
		}
	    }
	    if (Changed == 0) return EverChanged;
	}
}

static int ApplyTerm(Ruleset, NumRuleset, str, Title)
struct Map Ruleset[];
int NumRuleset;
char *str, *Title;
{/* Apply the given ruleset at the end of the string. */
	register char *Fing, *Src, *Dst, *LastSrc;
	register int Which, Changed, RightLen;
	int EverChanged = 0;

	for (;;) {
	    Changed = 0;
	    for (Which = 0; Which < NumRuleset; ++Which) {
		RightLen = strlen(str);
		if (RightLen >= Ruleset[Which].LeftLen) {
		    Fing = &str[RightLen - Ruleset[Which].LeftLen];
		    if (strncmp(Fing, Ruleset[Which].Left, Ruleset[Which].LeftLen) == 0) {
			if (bwDebugging) fprintf(stderr, "[(%s) %s -> ", Title, str);
			RightLen = strlen(Ruleset[Which].Right);
			if (RightLen <= Ruleset[Which].LeftLen) {
			    strncpy(Fing, Ruleset[Which].Right, RightLen);
			    strcpy(Fing + RightLen, Fing + Ruleset[Which].LeftLen);
			} else {
			    Src = &Fing[strlen(Fing)];
			    LastSrc = Fing + Ruleset[Which].LeftLen;
			    Dst = Src + RightLen - Ruleset[Which].LeftLen;
			    while (Src >= LastSrc) *Dst-- = *Src--;
			    strncpy(Fing, Ruleset[Which].Right, RightLen);
			}
			if (bwDebugging) fprintf(stderr, "%s]\n", str);
			EverChanged = Changed = 1;
		    }
		}
	    }
	    if (Changed == 0) return EverChanged;
	}
}

static int Apply(Ruleset, NumRuleset, str, Title)
struct Map Ruleset[];
int NumRuleset;
char *str, *Title;
{/* Apply the given ruleset anywhere in the string. */
	register char *Fing, *Src, *Dst, *LastSrc;
	register int Which, Changed, RightLen;
	int EverChanged = 0;

	for (;;) {
	    Changed = 0;
	    for (Which = 0; Which < NumRuleset; ++Which) {
		for (Fing = str; *Fing != '\0'; ++Fing) {
		    if (strncmp(Fing, Ruleset[Which].Left, Ruleset[Which].LeftLen) == 0) {
			if (bwDebugging) fprintf(stderr, "[(%s) %s -> ", Title, str);
			RightLen = strlen(Ruleset[Which].Right);
			if (RightLen <= Ruleset[Which].LeftLen) {
			    strncpy(Fing, Ruleset[Which].Right, RightLen);
			    strcpy(Fing + RightLen, Fing + Ruleset[Which].LeftLen);
			} else {
			    Src = &Fing[strlen(Fing)];
			    LastSrc = Fing + Ruleset[Which].LeftLen;
			    Dst = Src + RightLen - Ruleset[Which].LeftLen;
			    while (Src >= LastSrc) *Dst-- = *Src--;
			    strncpy(Fing, Ruleset[Which].Right, RightLen);
			}
			if (bwDebugging) fprintf(stderr, "%s]\n", str);
			EverChanged = Changed = 1;
		    }
		}
	    }
	    if (Changed == 0) return EverChanged;
	}
}

static char *BasicCanon(str)
char *str;
{/* Allocate and return a minimally-processed representation of the given name, or NULL. */
	char *ostr;
	register char *Src, *Dst;

	ostr = malloc(strlen(str) + 15);
	if (ostr == NULL) return NULL;
	strcpy(ostr, str);
	for (Src = ostr; *Src != '\0'; ++Src) if (isupper(*Src)) *Src = tolower(*Src);
	Src = Dst = ostr;
	do {
		if (index(" \t\n\r", *Src) == NULL || *Src == '\0') *Dst++ = *Src;
	} while (*Src++ != '\0');
	return ostr;
}

static void GivenHack(str)
char *str;
{/* Check for a particular pattern of given names and treat specially, overwriting the string if found. */
	if (strcmp(str, "st.") == 0) strcpy(str, "saint");
}

static void MapCG(str)
char *str;
{/* Do the C-{S,K} and G-{G,J} mapping for the given string, in place. */
	register char *Src;

	for (Src = str; *Src != '\0'; ++Src) {
		if (*Src == 'c') {
			if (bwDebugging) fprintf(stderr, "[(C) %s -> ", str);
			switch (Src[1]) {
			    case 'e': case 'i': case 'y':
				*Src = 's';
				break;
			    default:
				*Src = 'k';
				break;
			}
			if (bwDebugging) fprintf(stderr, "%s]\n", str);
		} else if (*Src == 'g') {
			switch (Src[1]) {
			    case 'e': case 'i': case 'y':
				if (bwDebugging) fprintf(stderr, "[(G) %s -> ", str);
				*Src = 'j';
				if (bwDebugging) fprintf(stderr, "%s]\n", str);
				break;
			    case 'g':
				if (Src[2] != '\0') Src += 2;
				break;
			    default:
				break;
			}
		}
	}
}

static void PhoneticMap(str, IsGiven)
char *str; int IsGiven;
{/* Do the phonetic mapping for str, in place. */
	register int Changed;

	for (;;) {
		Changed = 0;
		if (ApplyInit(InitCRules, NumInitCRules, str, "InitCRules")) Changed = 1;
		if (ApplyTerm(TermCRules, NumTermCRules, str, "TermCRules"))
						Changed = 1;
		if (Apply(CRules, NumCRules, str, "CRules")) Changed = 1;
		if (Changed == 0) break;
	}
	MapCG(str);
	for (;;) {
		Changed = 0;
		if (ApplyInit(InitRules, NumInitRules, str, "InitRules")) Changed = 1;
		if (IsGiven) {
			if (ApplyTerm(TermGivRules, NumTermGivRules, str,
				"TermGivRules")) Changed = 1;
		} else {
			if (ApplyTerm(TermRules, NumTermRules, str, "TermRules"))
				Changed = 1;
		}
		if (Apply(Rules, NumRules, str, "Rules")) Changed = 1;
		if (Changed == 0) break;
	}
}

static void Codify(str, Codes)
char *str; char Codes[];
{/* Use the alphabetic code table Codes to reduce str to canonical form, in place. */
	register char *Src, *Dst;
	register char Code, LastCode;

	Src = str;
	while (*Src < 'a' || *Src > 'z') {if (*Src == '\0') break; else ++Src;}
	if (*Src == '\0') {*str = '\0'; return;}
	LastCode = Codes[*Src - 'a'];
	Dst = ++Src;
	do {
		if (*Src >= 'a' && *Src <= 'z') {
			Code = Codes[*Src - 'a'];
			if (Code != LastCode) {
				if (Code != 0) *Dst++ = Code;
			}
			LastCode = Code;
		}
	} while (*Src++ != '\0');
	*Dst = '\0';
}

/* The following table maps letters into eight classes of consonant phonemes:
	1. l -> /l/
	2. m, n -> /m/ /n/ /ng/
	3. r -> /r/
	4. d, t -> /d/ /t/ /th/ (hard /th/)
	5. b, p -> /b/ /p/
	6. f, v -> /f/ /v/
	7. k, g, q, x -> /k/ /ch/ /g/
	8. s, z, j -> /s/ /sh/ /z/ /zh/ /j/
*/
static char SurnCodes[26] = {
	0, 'b', 0, 'd', 0, 'f', 'k', 0, 0, 's',	/* a through j */
	'k', 'l', 'm', 'm', 0, 'b', 'k', 'r', 's', 'd',	/* k through t */
	0, 'f', 0, 'k', 0, 's'
};

char *CanonSurn(str)
char *str;
{/* Allocate and return a canonical representation of the given surname, or NULL. */
	char *ostr;

	ostr = BasicCanon(str);
	if (ostr == NULL) return NULL;
	PhoneticMap(ostr, 0);
	Codify(ostr, SurnCodes);

	return ostr;
}

char *CanonGiven(str)
char *str;
{/* Allocate and return a canonical representation of the given given name, or NULL. */
	char *ostr;

	ostr = BasicCanon(str);
	if (ostr == NULL) return NULL;
	GivenHack(ostr);
	PhoneticMap(ostr, 1);
	Codify(ostr, SurnCodes);

	return ostr;
}

char *CanonNick(str)
char *str;
{/* Allocate and return a canonical representation of the given nickname, or NULL. */
    char *ostr;
    int oLen;
    register int oLenMin, NLen, Ix;
    static char *NoiseSuffixes[] = {
	"alin", "lin", "line", "ana", "ann", "inka", "bella", "bel", "bell", "belle", "beau", "iss", "issa",
	"ique", "ietta", "etta", "elle", "ine", "inea", "alyn", "lyn", "ette", "ett", "ella",
	"ita", "enka", "inka",
	"ius", "us", "cia", "is", "ice", "iece", "isa", "een", "ene", "ena", "uska", "iska", "eska", 
	"vika", "ika", "lita", "ikka", "inda", "ye", "ett", "lynn", "lou", "illa", "ille",
	"i", "ie", "y", "ia", "ina", "a"
    };
#define NumNoiseSuffixes (sizeof(NoiseSuffixes) / sizeof(NoiseSuffixes[0]))

    ostr = BasicCanon(str);
    if (ostr == NULL) return NULL;
    GivenHack(ostr);
    oLen = strlen(ostr);
    oLenMin = oLen - 3;
    for (Ix = 0; Ix < NumNoiseSuffixes; ++Ix) {
	NLen = strlen(NoiseSuffixes[Ix]);
	if (NLen <= oLenMin) {
	    if (strcmp(NoiseSuffixes[Ix], &ostr[oLen - NLen]) == 0) {
		if (bwDebugging) fprintf(stderr, "[(suffix) %s -> ", ostr);
		if (ostr[oLen - NLen - 1] == 'c') {
		    switch(*NoiseSuffixes[Ix]) {
			case 'e': case 'i': case 'y':
			    ostr[oLen - NLen - 1] = 's';
			    break;
			default:
			    ostr[oLen - NLen - 1] = 'k';
			    break;
		    }
		}
		ostr[oLen - NLen] = '\0';
		if (bwDebugging) fprintf(stderr, "%s]\n", ostr);
		break;
	    }
	}
    }
    PhoneticMap(ostr, 1);
    Codify(ostr, SurnCodes);

    return ostr;
}
