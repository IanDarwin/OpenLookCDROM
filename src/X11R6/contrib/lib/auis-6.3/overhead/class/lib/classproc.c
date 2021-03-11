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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/overhead/class/lib/RCS/classproc.c,v 1.4 1992/12/15 21:00:13 rr2b R6tape $";
#endif

/* This is now separated off from class.c so that on the hp9000s800
 * class_RoutineStruct and the ClassEntry<i>'s can be loaded into
 * both the text and data segments. Otherwise class procedure calls
 * from dynamically loaded classes to other dynamically loaded classes
 * will fail.
 */

#include <class.h>

long ClassEntry0();	long ClassEntry1();	long ClassEntry2();	long ClassEntry3();	long ClassEntry4();
long ClassEntry5();	long ClassEntry6();	long ClassEntry7();	long ClassEntry8();	long ClassEntry9();
long ClassEntry10();	long ClassEntry11();	long ClassEntry12();	long ClassEntry13();	long ClassEntry14();
long ClassEntry15();	long ClassEntry16();	long ClassEntry17();	long ClassEntry18();	long ClassEntry19();
long ClassEntry20();	long ClassEntry21();	long ClassEntry22();	long ClassEntry23();	long ClassEntry24();
long ClassEntry25();	long ClassEntry26();	long ClassEntry27();	long ClassEntry28();	long ClassEntry29();
long ClassEntry30();	long ClassEntry31();	long ClassEntry32();	long ClassEntry33();	long ClassEntry34();
long ClassEntry35();	long ClassEntry36();	long ClassEntry37();	long ClassEntry38();	long ClassEntry39();
long ClassEntry40();	long ClassEntry41();	long ClassEntry42();	long ClassEntry43();	long ClassEntry44();
long ClassEntry45();	long ClassEntry46();	long ClassEntry47();	long ClassEntry48();	long ClassEntry49();
long ClassEntry50();	long ClassEntry51();	long ClassEntry52();	long ClassEntry53();	long ClassEntry54();
long ClassEntry55();	long ClassEntry56();	long ClassEntry57();	long ClassEntry58();	long ClassEntry59();
long ClassEntry60();	long ClassEntry61();	long ClassEntry62();	long ClassEntry63();	long ClassEntry64();
long ClassEntry65();	long ClassEntry66();	long ClassEntry67();	long ClassEntry68();	long ClassEntry69();
long ClassEntry70();	long ClassEntry71();	long ClassEntry72();	long ClassEntry73();	long ClassEntry74();
long ClassEntry75();	long ClassEntry76();	long ClassEntry77();	long ClassEntry78();	long ClassEntry79();
long ClassEntry80();	long ClassEntry81();	long ClassEntry82();	long ClassEntry83();	long ClassEntry84();
long ClassEntry85();	long ClassEntry86();	long ClassEntry87();	long ClassEntry88();	long ClassEntry89();
long ClassEntry90();	long ClassEntry91();	long ClassEntry92();	long ClassEntry93();	long ClassEntry94();
long ClassEntry95();	long ClassEntry96();	long ClassEntry97();	long ClassEntry98();	long ClassEntry99();
long ClassEntry100();	long ClassEntry101();	long ClassEntry102();	long ClassEntry103();	long ClassEntry104();
long ClassEntry105();	long ClassEntry106();	long ClassEntry107();	long ClassEntry108();	long ClassEntry109();
long ClassEntry110();	long ClassEntry111();	long ClassEntry112();	long ClassEntry113();	long ClassEntry114();
long ClassEntry115();	long ClassEntry116();	long ClassEntry117();	long ClassEntry118();	long ClassEntry119();
long ClassEntry120();	long ClassEntry121();	long ClassEntry122();	long ClassEntry123();	long ClassEntry124();
long ClassEntry125();	long ClassEntry126();	long ClassEntry127();	long ClassEntry128();	long ClassEntry129();
long ClassEntry130();	long ClassEntry131();	long ClassEntry132();	long ClassEntry133();	long ClassEntry134();
long ClassEntry135();	long ClassEntry136();	long ClassEntry137();	long ClassEntry138();	long ClassEntry139();
long ClassEntry140();	long ClassEntry141();	long ClassEntry142();	long ClassEntry143();	long ClassEntry144();
long ClassEntry145();	long ClassEntry146();	long ClassEntry147();	long ClassEntry148();	long ClassEntry149();

struct basicobject_methods class_RoutineStruct = {
NULL,
ClassEntry0,		ClassEntry1,		ClassEntry2,		ClassEntry3,		ClassEntry4,
ClassEntry5,		ClassEntry6,		ClassEntry7,		ClassEntry8,		ClassEntry9,
ClassEntry10,		ClassEntry11,		ClassEntry12,		ClassEntry13,		ClassEntry14,
ClassEntry15,		ClassEntry16,		ClassEntry17,		ClassEntry18,		ClassEntry19,
ClassEntry20,		ClassEntry21,		ClassEntry22,		ClassEntry23,		ClassEntry24,
ClassEntry25,		ClassEntry26,		ClassEntry27,		ClassEntry28,		ClassEntry29,
ClassEntry30,		ClassEntry31,		ClassEntry32,		ClassEntry33,		ClassEntry34,
ClassEntry35,		ClassEntry36,		ClassEntry37,		ClassEntry38,		ClassEntry39,
ClassEntry40,		ClassEntry41,		ClassEntry42,		ClassEntry43,		ClassEntry44,
ClassEntry45,		ClassEntry46,		ClassEntry47,		ClassEntry48,		ClassEntry49,
ClassEntry50,		ClassEntry51,		ClassEntry52,		ClassEntry53,		ClassEntry54,
ClassEntry55,		ClassEntry56,		ClassEntry57,		ClassEntry58,		ClassEntry59,
ClassEntry60,		ClassEntry61,		ClassEntry62,		ClassEntry63,		ClassEntry64,
ClassEntry65,		ClassEntry66,		ClassEntry67,		ClassEntry68,		ClassEntry69,
ClassEntry70,		ClassEntry71,		ClassEntry72,		ClassEntry73,		ClassEntry74,
ClassEntry75,		ClassEntry76,		ClassEntry77,		ClassEntry78,		ClassEntry79,
ClassEntry80,		ClassEntry81,		ClassEntry82,		ClassEntry83,		ClassEntry84,
ClassEntry85,		ClassEntry86,		ClassEntry87,		ClassEntry88,		ClassEntry89,
ClassEntry90,		ClassEntry91,		ClassEntry92,		ClassEntry93,		ClassEntry94,
ClassEntry95,		ClassEntry96,		ClassEntry97,		ClassEntry98,		ClassEntry99,
ClassEntry100,		ClassEntry101,		ClassEntry102,		ClassEntry103,		ClassEntry104,
ClassEntry105,		ClassEntry106,		ClassEntry107,		ClassEntry108,		ClassEntry109,
ClassEntry110,		ClassEntry111,		ClassEntry112,		ClassEntry113,		ClassEntry114,
ClassEntry115,		ClassEntry116,		ClassEntry117,		ClassEntry118,		ClassEntry119,
ClassEntry120,		ClassEntry121,		ClassEntry122,		ClassEntry123,		ClassEntry124,
ClassEntry125,		ClassEntry126,		ClassEntry127,		ClassEntry128,		ClassEntry129,
ClassEntry130,		ClassEntry131,		ClassEntry132,		ClassEntry133,		ClassEntry134,
ClassEntry135,		ClassEntry136,		ClassEntry137,		ClassEntry138,		ClassEntry139,
ClassEntry140,		ClassEntry141,		ClassEntry142,		ClassEntry143,		ClassEntry144,
ClassEntry145,		ClassEntry146,		ClassEntry147,		ClassEntry148,		ClassEntry149
};


