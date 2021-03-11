/*  
 STFunctions.c of

 Scribetext: a facility to convert Scribe manuscript files to files 
 compatible with the ATK file format.

 Scribetext is copyright (c) 1989, 1990 by the Massachusetts Institute of
 Technology.

 Scribe is a registered trademark of Scribe Systems, Inc.

 Permission to use, copy, modify, and distribute this software and
 its documentation for any purpose and without fee is hereby granted,
 provided that the above copyright notice and the name of the author(s)
 appear in all copies; that both that copyright notice, the name of
 the author(s) and this permission notice appear in supporting
 documentation; and that the name of the Massachusetts Institute of
 Technology not be used in advertising or publicity pertaining to
 distribution of the software without specific, written prior
 permission.  The Massachusetts Institute of Technology makes no
 representations about the suitability of this software for any purpose.
 It is provided "as is" without express or implied warranty.

 Scribetext was written entirely by Jeremy Paul Kirby, jpkirby@ATHENA.MIT.EDU

 $Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/mit/scribetext/RCS/STFunctions.c,v 1.8 1993/12/07 02:13:10 rr2b Exp $
*/

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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/mit/scribetext/RCS/STFunctions.c,v 1.8 1993/12/07 02:13:10 rr2b Exp $";
#endif

#include <andrewos.h>
#include <stdio.h>
#include <sys/file.h>
#ifdef SOLARIS
#include <unistd.h>
#endif
#include "scribetext.h"

extern TABLE Table;
TABLE FindNode();
void CloseFiles();
long int ParseText();
extern int offset();
FP AssignFunc();
char *GetInstruction(), *STGreek();
int STUniqueID(), STDelete(), STCopy(), STSymbol(), STNOP(), STStrip(), 
  STDevice(), STFootnote(), STBegin(), STEnd(), STCenter(), STCaps(),
  STTilde(), STInclude(), STChapter(), STDefine(), STNewpage(),
  STStupidstrip(), STVerbatim(), STItemize(), STBlankspace(),
  STLabel(), STRef(), STTsymbol(), STValue();

FP AssignFunc(ezword)
     char *ezword;      
{
  if(!ULstrcmp(ezword, "delete"))
    return(STDelete);

  if(!ULstrcmp(ezword, "symbol"))
    return(STSymbol);

  if(!ULstrcmp(ezword, "strip"))
    return(STStrip);

  if(!ULstrcmp(ezword, "device"))
    return(STDevice);

  if(!ULstrcmp(ezword, "imbed"))
    return(STBegin);

  if(!ULstrcmp(ezword, "footnote"))
    return(STFootnote);
  
  if(!ULstrcmp(ezword, "end"))
    return(STEnd);

  if(!ULstrcmp(ezword, "caps"))
    return(STCaps);

  if(!ULstrcmp(ezword, "tilde"))
    return(STTilde);

  if(!ULstrcmp(ezword, "nop"))
    return(STNOP);

  if(!ULstrcmp(ezword, "include"))
    return(STInclude);

  if(!ULstrcmp(ezword, "chapter"))
    return(STChapter);

  if(!ULstrcmp(ezword, "define"))
    return(STDefine);

  if(!ULstrcmp(ezword, "newpage"))
    return(STNewpage);

  if(!ULstrcmp(ezword, "stupidstrip"))
    return(STStupidstrip);

  if(!ULstrcmp(ezword, "verbatim"))
    return(STVerbatim);

  if(!ULstrcmp(ezword, "itemize"))
    return(STItemize);

  if(!ULstrcmp(ezword, "blankspace"))
    return(STBlankspace);

  if(!ULstrcmp(ezword, "label"))
    return(STLabel);

  if(!ULstrcmp(ezword, "ref"))
    return(STRef);

  if(!ULstrcmp(ezword, "pageref"))
    return(STRef);

  if(!ULstrcmp(ezword, "tsymbol"))
    return(STTsymbol);

  if(!ULstrcmp(ezword, "define"))
    return(STValue);

  return(STNOP);
}

int STUniqueID()
{
  return(++Token);
}

int STDevice(command, tofind)
     char *command;
     int tofind;
{
  char ch, device[TMP_SIZE];
  int in;

  device[0] = '\0';

  while((in = fgetc(fin)) != EOF)
    {
      ch = (char) in;
      if(ch == tofind)
	{
	  ch = (char) fgetc(fin);
	  if(ch !='\n' && ch != '\r')
	    ungetc(ch, fin);
	  else
	    CurrLine++;
	  break;
	}
      else
	sprintf(device, "%s%c", device, ch);
    }

  if(ULstrcmp(device, "postscript"))
    {
      printf("* Output device %s was specified in input file.\n",
	      device);
      printf("  Please note that printer selections must occur %s\n",
	      "from within the specific");
      printf("  Andrew application.\n");
    }
}


int STDelete(command, tofind)
     char *command;
     int tofind;
{
  ParseText(tofind, "", "", NORMAL, NOP);
}

  
int STNOP(command, tofind)
     char *command;
     int tofind;
{
}


int STCopy(command, tofind)
     char *command;
     int tofind;
{
  fputs(command, fout);
}


int STStrip(command, tofind)
     char *command;
     int tofind;
{
  ParseText(tofind, "", "", NORMAL, PRINTTOFILE);
}



int STStupidstrip(command, tofind)
     char *command;
     int tofind;
{
  char ch;
  int in;

  while((in = fgetc(fin)) != EOF)
    {
      ch = (char) in;
      if(ch == tofind)
	{
	  ch = (char) fgetc(fin);
	  if(ch != '\n' && ch != '\r')
	    ungetc(ch, fin);
	  else
	    CurrLine++;
	  break;
	}
      else
	fputc(ch, fout);
    }
}


int STSymbol(command, tofind)
     char *command;
     int tofind;
{
  int token;

  token = STUniqueID();

  fprintf(fout, "\\begindata{acc_%s, %d}\n\\enddata{aac_%s, %d}\n",
	  command, token, command, token);
  fprintf(fout, "\\view{aac_%s, %d, 0, 0, 0}", command, token);
}


int STItemize(command, tofind)
     char *command;
     int tofind;
{
  fputs("\\itemize{", fout);

  ParseText(tofind, "\\symbol{7}\t", "", NORMAL, PRINTTOFILE);

  fputs("}\n", fout);
}

int STError(command, tofind)
     char *command;
     int tofind;
{
  fprintf(ferr, "* Unknown error!\n* %s: unknown error in input file.\n", me);
}


int STFootnote(command, tofind)
     char *command;
     int tofind;
{
  int token;

  token = STUniqueID();

  fprintf(fout, "\\footnote{\\\n");
  fprintf(fout, "\\begindata{fnote,%d}\n", token);
  fprintf(fout, "\\textdsversion{%d}\n", TextDSVersion);

  ParseText(tofind, "", "", NORMAL, PRINTTOFILE);

  fprintf(fout, "\\\n\\enddata{fnote,%d}\n\\view{fnotev,%d,3,0,0}}",
	  token, token);
}


int STBegin(command, tofind)
     char *command;
     int tofind;
{
  char ch, *instruction, *makelower();
  int in;
  TABLE tmp;

  instruction = (char *) calloc (TMP_SIZE, sizeof(char));

  while((in = fgetc(fin)) != EOF)
    {
      ch = (char) in;
      if(ch == tofind)
	{
	  ch = (char) fgetc(fin);
	  if(ch != '\n' && ch != '\r')
	    ungetc(ch, fin);
	  else
	    CurrLine++;
	  ch = (char) fgetc(fin);
	  if(ch != '\n' && ch != '\r')
	    ungetc(ch, fin);
	  else
	    CurrLine++;
	  break;
	}
      else
	sprintf(instruction, "%s%c", instruction, ch);
    }

  instruction = makelower(instruction);

  tmp = FindNode(SCRIBECOLUMN, instruction);
  if(tmp == NULL)
    {
      printf("* Scribe environment @%s not recognized\n", instruction);
      fprintf(fout, "@begin(%s)", instruction);
    }
  else
    {
      if(!(tmp->mode & COMMAND))
	{
	  fprintf(fout, "\\%s{", tmp->ez.word);
	  ParseText(POP_JOB, "", "", NORMAL, PRINTTOFILE);
	  fputc('}', fout);
	}
      else
	  return(tmp->ez.fun(instruction, POP_JOB));
    }
}


int STEnd(command, tofind)
     char *command;
     int tofind;
{
  char ch, *instruction, *makelower();
  int in;
  TABLE tmp;

  instruction = (char *) calloc (TMP_SIZE, sizeof(char));

  while((in = fgetc(fin)) != EOF)
    {
      ch = (char) in;
      if(ch == tofind)
	{
	  ch = (char) fgetc(fin);
 	  if(ch != '\n' && ch != '\r')
	    ungetc(ch, fin);
	  else
	    CurrLine++;
	  break;
	}
      sprintf(instruction, "%s%c", instruction, ch);
    }

  instruction = makelower(instruction);

  tmp = FindNode(SCRIBECOLUMN, instruction);

  if(tmp == NULL)
    {
      printf("* Scribe environment @%s not recognized...\n", instruction);
      fprintf(fout, "@end(%s)", instruction);
    }
  else
    return(POP_JOB);
}


int STCaps(command, tofind)
     char *command;
     int tofind;
{
  fputs("\\smaller{", fout);

  ParseText(tofind, "", "", CAPS, PRINTTOFILE);

  fputc('}', fout);
}


int STTilde(command, tofind)
     char *command;
     int tofind;
{
  /* ABSORB EVERYTHING UNTIL THE NEXT PRINTABLE CHARACTER */

  char ch;
  int in;
  
  while(in = fgetc(fin) != EOF)
    {
      ch = (char) in;
      if(ch != '\n' && ch != '\r')
	break;
      else
	CurrLine++;
    }
	
  if(in == EOF)
    {
      fprintf(ferr, "* Error!\n* %s: End of file reached after @~ command\n", me);
      CloseFiles();
    }
}


int STInclude(command, tofind)
     char *command;
     int tofind;
{
  char ch, instruction[TMP_SIZE];
  int in, accessible, readable;
  long int tempcurrline;

  sprintf(instruction, "");

  while((in = fgetc(fin)) != EOF)
    {
      ch = (char) in;
      if(ch == tofind)
	{
	  ch = (char) fgetc(fin);
 	  if(ch != '\n' && ch != '\r')
	    ungetc(ch, fin);
	  else
	    CurrLine++;
	  break;
	}
      sprintf(instruction, "%s%c", instruction, ch);
    }

  accessible = access(instruction, F_OK);
  readable = access(instruction, R_OK);

  if(accessible)
    {
      fprintf(ferr, "* Unable to process @INCLUDE(%s):\n", instruction);
      fprintf(ferr, "  File %s not found.\n", instruction);
    }
  else if(readable)
    {
      fprintf(ferr, "* Unable to process @INCLUDE(%s):\n", instruction);
      fprintf(ferr, "  Insufficient access to read %s.\n", instruction);
    }
  else
    {
      printf("* Incorporating %s\n", instruction);
      PushFile(instruction);
      tempcurrline = ParseText(EOF, "", "", NORMAL, PRINTTOFILE);
      printf("* Finished incorporating %ld lines of %s\n",  tempcurrline,
	     instruction);
    }
}


int STChapter(command, tofind)
     char *command;
     int tofind;
{
  int token;

  token = STUniqueID();

  fprintf(fout, 
	  "\\begindata{bp,%d}\n\\enddata{bp,%d}\n\\view{bpv,%d,1,0,0}\n",
	  token, token, token);
  ReplaceText("chapter", WORD, tofind);
}



int STDefine(command, tofind)
     char *command;
     int tofind;
{
  int in, nomoreflag=FALSE;
  char ch, *instruction, *makelower();
  TABLE tmp;

  instruction = (char *) calloc (TMP_SIZE, sizeof(char));
  tmp = (TABLE) malloc(sizeof(struct TableStruct));
  
  while((in = fgetc(fin)) != EOF)
    {
      ch = (char) in;
      if(ch == tofind)
	{
	  ch = (char) fgetc(fin);
	  if(ch != '\n' && ch != '\r')
	    ungetc(ch, fin);
	  else
	    CurrLine++;
	  break;
	}
      else if(ch == ',' || ch == '=' || ch == ' ')
	nomoreflag = TRUE;
      else if(nomoreflag == FALSE)
	sprintf(instruction, "%s%c", instruction, ch);
    }

  /* Upon finding a "@define(new, ...)" / "@define(new=old, ...)",
     make an entry in the internal translation table with 
     .scribeword equal to instruction, mode equal to
     (COMMAND | SCRIBECOMMAND | PARAMETERIZED | USEDELS)
     and the command function pointer for STDelete.    */

  instruction = makelower(instruction);

  tmp->scribeword = (char *) calloc((strlen(instruction) + 1), sizeof(char));

  strcpy(tmp->scribeword, instruction);
  tmp->mode = (COMMAND | SCRIBECOMMAND | PARAMETERIZED | USEDELS);
  tmp->ez.fun = STDelete;
  tmp->next = Table;
  Table = tmp;

  printf("* @%s command encountered.  All future occurences of\n", command);
  printf("  @%s will be deleted.\n", instruction);

}


int STNewpage(command, tofind)
     char *command;
     int tofind;
{
  char ch, *instruction, *makelower();
  int in, times, token;
  void AbsorbNewlines();

  instruction = (char *) calloc (TMP_SIZE, sizeof(char));

  while((in = fgetc(fin)) != EOF)
    {
      ch = (char) in;
      if(ch == tofind)
	{
	  ch = (char) fgetc(fin);
 	  if(ch != '\n' && ch != '\r')
	    ungetc(ch, fin);
	  else
	    CurrLine++;
	  break;
	}
      sprintf(instruction, "%s%c", instruction, ch);
    }

  if(strcmp(instruction, ""))
    times = atoi(instruction);
  else
    times = 1;

  for(in=0; in<times; in++)
    {
      token = STUniqueID();
      fprintf(fout, "\\begindata{bp,%d}\n\\enddata{bp,%d}\n", token, token);
      fprintf(fout, "\\view{bpv,%d,1,0,0}\n", token);
    }

  AbsorbNewlines();
}

int STLabel(command, tofind)
     char *command;
     int tofind;
{
  char ch, *codeword;
  int in, token;

  codeword = (char *) malloc(TMP_SIZE, sizeof(char));

  while((in = fgetc(fin)) != EOF)
    {
      ch = (char) in;
      if(ch == tofind)
	{
	  ch = (char) fgetc(fin);
	  if(ch != '\n' && ch != '\r')
	    ungetc(ch, fin);
	  else
	    CurrLine++;
	  break;
	}
      sprintf(codeword, "%s%c", codeword, ch);
    }
  
  if(!strcmp(codeword, ""))
    {
      printf("* No codeword specified with %s.  Ignoring.\n", command);
      return(0);
    }
  else
    {
      token = STUniqueID();
      fputs("\\footnote{\\\n", fout);
      fprintf(fout, "\\begindata{texttag,%d}\n\\textdsversion{%d}\n", token,
	      TextDSVersion);
      fprintf(fout, "%s\\\n\\enddata{texttag,%d}\n\\view{texttagv,%d,0,0,0}}",
	      codeword, token, token);
    }
}
  
int STRef(command, tofind)
     char *command;
     int tofind;
{
  char ch, *codeword;
  int in, token;

  codeword = (char *) malloc(TMP_SIZE, sizeof(char));

  while((in = fgetc(fin)) != EOF)
    {
      ch = (char) in;
      if(ch == tofind)
	{
	  ch = (char) fgetc(fin);
	  if(ch != '\n' && ch != '\r')
	    ungetc(ch, fin);
	  else
	    CurrLine++;
	  break;
	}
      sprintf(codeword, "%s%c", codeword, ch);
    }
  
  if(!strcmp(codeword, ""))
    {
      printf("* No codeword specified with %s.  Ignoring.\n", command);
      return(0);
    }
  else
    {
      token = STUniqueID();
      fputs("\\footnote{\\\n", fout);
      fprintf(fout, "\\begindata{textref,%d}\n\\textdsversion{%d}\n", token,
	      TextDSVersion);
      
      if(!ULstrcmp(command, "pageref"))
	fprintf(fout, "# %s", codeword);
      else
	fprintf(fout, "%s", codeword);

      fprintf(fout, "\\\n\\enddata{textref,%d}\n\\view{textrefv,%d,0,0,0}}",
	      token, token);
    }
}

int STTsymbol(command, tofind)
     char *command;
     int tofind;
{
  if(!strcmp(command, "zts"))
    fputs("\\formatnote{\\\\(ts}", fout);

  return(STStupidstrip(command, tofind));

}

  
int STVerbatim(command, tofind)
     char *command;
     int tofind;
{
  verbatim++;
  ParseText(tofind, "", "", NORMAL, PRINTTOFILE);
  verbatim--;
}


int STBlankspace(command, tofind)
     char *command;
     int tofind;
{
char ch, *quan, *qual, *combined, quality;
int in;
double quantity, atof();

quan = (char *) malloc (TMP_SIZE, sizeof(char));
qual = (char *) malloc (TMP_SIZE, sizeof(char));
combined = (char *) malloc (TMP_SIZE, sizeof(char));

while((in=fgetc(fin)) != EOF)
  {
    ch = (char) in;
    if(ch == tofind)
      {
	ch = (char) fgetc(fin);
	if(ch != '\n' && ch != '\r')
	  ungetc(ch, fin);
	else
	  CurrLine++;
	break;
      }
    sprintf(combined, "%s%c", combined, ch);
  }

sscanf(combined, "%s %s", quan, qual);

quantity = (double) atof(quan);

if(!ULstrncmp(qual, "in", 2) || !(ULstrcmp(qual, "\"")))
  quality = 'i';
else if(!ULstrcmp(qual, "cm") || !ULstrncmp(qual, "centimeter", 10))
  quality = 'c';
else if(!ULstrcmp(qual, "mm") || !ULstrncmp(qual, "millimeter", 10))
  {
    quality = 'c';
    quantity *= 0.1;
  }
else if(!ULstrncmp(qual, "pt", 2) || !ULstrncmp(qual, "point", 5))
  quality = 'p';
else if(!ULstrncmp(qual, "pica", 4))
  quality = 'P';
else if(!ULstrncmp(qual, "em", 2) || !ULstrncmp(qual, "quad", 4))
  quality = 'm';
else if(!ULstrncmp(qual, "en", 2))
  quality = 'n';
else if(!ULstrncmp(qual, "line", 4))
  quality = 'v';
else
  {
    printf("* Unit of measurement %s not recognized:\n", qual);
    return(0);
  }

fprintf(fout, "\\formatnote{.sp %lf%c}\n", quantity, quality);
}
  
int STValue(command, tofind)
     char *command;
     int tofind;
{
  char *instruction, ch;
  int in;

  instruction = (char *) calloc (TMP_SIZE, sizeof(char));

  instruction[0] = '\0';

  while((in = fgetc(fin)) != EOF)
    {
      ch = (char) in;
      if(ch == tofind)
	{
	  ch = (char) fgetc(fin);
 	  if(ch != '\n' && ch != '\r')
	    ungetc(ch, fin);
	  else
	    CurrLine++;
	  break;
	}
      sprintf(instruction, "%s%c", instruction, ch);
    }



}
    


/* 
   THIS CODE IS NOT IMPLEMENTED ANYWHERE.  IT EXISTS AS A FUTURE POSSIBLE
   SOURCE FOR GREEK TRANSLITERATION USING   ParseText()'s    transform
   FACILITY.

char *STGreek(character)
     char character;
{
  char toreturn[2];

  switch (character)
    {
    case 'c':
      return("ch");
    case 'f':
      return("ph");
    case 'j':
      return("");
    case 'q':
      return("th");
    case 'v':
      return("");
    case 'w':
      return("o");
    case 'y':
      return("ps");
    default:
      toreturn[0]=character;
      toreturn[1]='\0';
      return(toreturn);
    }
}
*/
