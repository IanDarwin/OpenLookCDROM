/*
 * Copyright 1993 by Digital Equipment Corporation, Maynard, Massachusetts.
 * 
 * Permission to use, copy, modify, distribute, and sell this software and its 
 * documentation for any purpose is hereby granted without fee, provided that 
 * the above copyright notice appear in all copies and that both that 
 * copyright notice and this permission notice appear in supporting 
 * documentation, and that the name of Digital not be used in advertising or 
 * publicity pertaining to distribution of the software without specific, 
 * written prior permission.  Digital makes no representations about the 
 * suitability of this software for any purpose.  It is provided "as is" 
 * without express or implied warranty.
 * 
 * DIGITAL DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING 
 * ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL 
 * DIGITAL BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY 
 * DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN 
 * AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF 
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 */
#include <AF/AFUtils.h>
#include <string.h>
#include <stdio.h>

struct AFSampleTypes AF_sample_sizes[] = {
  { 1, 1, "mu-255"},
  { 1, 1, "a-law"},
  { 2, 1, "PCM-16"},
  { 4, 1, "PCM-32"},
  { 1, 2, "G.721"},
  { 3, 8, "G.723"},
  { 0, 0, "unknown"},
  { 1, 2, "IMA" },
  { 1, 4, "crladpcm2"},
  { 3, 8, "crladpcm3"},
  { 1, 2, "crladpcm4"},
  { 0, 0, "LPC10e"},
  { 18, 240, "CELP1016"},
  { 4, 1, "IEEES" },
  { 8, 1, "IEEED" },
  { 1, 1, "G.722" },
  { 0, 0, "MPEG" },
  { 33, 160, "GSM" },
  { 0, 0, "VSELP" },
  { 0, 0, 0L }
};

static struct { AEncodeType type; char *name; } altNames[] = {
  { MU255, "ulaw" },
  { MU255, "mulaw" },
  { MU255, "au" },
  { LIN16, "lin" },
  { LIN16, "lin16" },
  { LIN16, "pcm16" },
  { LIN16, "pcm" },
  { LIN16, "short" },
  { LIN32, "int" },
  { IEEES, "float" },
  { IEEED, "double" },
  { UNKNOWN_ENCODETYPE, ""}
};

AEncodeType AFFindEncodeType(char *type)
{
  AEncodeType test = 0;
  int i = 0;
  while (AF_sample_sizes[test].name != NULL) {
    if (strcasecmp(type, AF_sample_sizes[test].name) == 0) return(test);
    test += 1;
  }
  while (altNames[i].type != UNKNOWN_ENCODETYPE) {
    if (strcasecmp(type, altNames[i].name) == 0) return(altNames[i].type);
    i += 1;
  }
  return(UNKNOWN_ENCODETYPE);
}

void AFPrintKnownEncodeTypes()
{
  AEncodeType test = 0;
  int i = 0;
  fprintf(stderr, "Known encoding types are: ");
  while (AF_sample_sizes[test].name != NULL) {
    fprintf(stderr, " %s", AF_sample_sizes[test].name);
    test += 1;
  }
  while (altNames[i].type != UNKNOWN_ENCODETYPE) {
    fprintf(stderr, " %s", altNames[i].name);
    i += 1;
  }
  fprintf(stderr, "\n");
}



