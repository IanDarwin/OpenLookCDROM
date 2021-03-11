/* THIS IS A GENERATED FILE
 *
 * Do not change!  Changing this file implies a protocol change!
 */

#include "audio.h"
#include "Aatom.h"
extern AAtom MakeAtom();
MakePredeclaredAtoms()
{
    if (MakeAtom("ATOM", 4, 1) != AA_ATOM) AtomError();
    if (MakeAtom("CARDINAL", 8, 1) != AA_CARDINAL) AtomError();
    if (MakeAtom("INTEGER", 7, 1) != AA_INTEGER) AtomError();
    if (MakeAtom("STRING", 6, 1) != AA_STRING) AtomError();
    if (MakeAtom("AC", 2, 1) != AA_AC) AtomError();
    if (MakeAtom("DEVICE", 6, 1) != AA_DEVICE) AtomError();
    if (MakeAtom("TIME", 4, 1) != AA_TIME) AtomError();
    if (MakeAtom("MASK", 4, 1) != AA_MASK) AtomError();
    if (MakeAtom("TELEPHONE", 9, 1) != AA_TELEPHONE) AtomError();
    if (MakeAtom("COPYRIGHT", 9, 1) != AA_COPYRIGHT) AtomError();
    if (MakeAtom("FILENAME", 8, 1) != AA_FILENAME) AtomError();
    if (MakeAtom("SAMPLE_MU255", 12, 1) != AA_SAMPLE_MU255) AtomError();
    if (MakeAtom("SAMPLE_ALAW", 11, 1) != AA_SAMPLE_ALAW) AtomError();
    if (MakeAtom("SAMPLE_LIN16", 12, 1) != AA_SAMPLE_LIN16) AtomError();
    if (MakeAtom("SAMPLE_LIN32", 12, 1) != AA_SAMPLE_LIN32) AtomError();
    if (MakeAtom("SAMPLE_ADPCM32", 14, 1) != AA_SAMPLE_ADPCM32) AtomError();
    if (MakeAtom("SAMPLE_ADPCM24", 14, 1) != AA_SAMPLE_ADPCM24) AtomError();
    if (MakeAtom("SAMPLE_CELP1016", 15, 1) != AA_SAMPLE_CELP1016) AtomError();
    if (MakeAtom("SAMPLE_CELP1015", 15, 1) != AA_SAMPLE_CELP1015) AtomError();
    if (MakeAtom("LAST_NUMBER_DIALED", 18, 1) != AA_LAST_NUMBER_DIALED) AtomError();
}
