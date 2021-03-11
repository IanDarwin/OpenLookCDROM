/*  
 parse.c of

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

 $Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/mit/scribetext/RCS/parse.c,v 1.6 1993/12/07 02:13:10 rr2b Exp $
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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/mit/scribetext/RCS/parse.c,v 1.6 1993/12/07 02:13:10 rr2b Exp $";
#endif

#include <andrewos.h>
#include <stdio.h>
#include <ctype.h>
#include "scribetext.h"

extern TABLE Table;

extern int STUniqueID();

void CloseFiles(), AbsorbSpace();
long int ParseText();

void ParseMain(Filein, Fileout)
     char *Filein, *Fileout;
{
  int token;
  
  CurrLine=1;
  token = STUniqueID();
  MasterToken = token;

  fprintf(fout, "\\begindata{text, %d}\n", MasterToken);
  fprintf(fout, "\\textdsversion{%d}\n", TextDSVersion);
  fputs("\\template{scribe}\n", fout);

  fputs("\\define{itemize\n", fout);
  fputs("menu:[Region, Itemize]\n", fout);
  fputs("attr:[LeftMargin LeftMargin Cm 82074]\n", fout);
  fputs("attr:[Indent LeftMargin Cm -10500]\n", fout);
  fputs("attr:[RightMargin RightMargin Cm 82074]}\n", fout);

  fputs("\\define{greek\n", fout);
  fputs("menu:[Font, Greek]\n", fout);
  fputs("attr:[FontFamily AndySymbol Int 0]}\n", fout);

  fputs("\\define{symbol\n", fout);
  fputs("menu:[Font, Symbol]\n", fout);
  fputs("attr:[FontFamily AndySymbola Int 0]}\n", fout);

  printf("* Processing manuscript file %s\n", Filein);
  printf("* Translating to %s\n\n", Fileout);

  ParseText(EOF, "", "", NORMAL, PRINTTOFILE);
  printf("* Finished processing %ld lines of %s.\n", CurrLine, Filein);
}


long int ParseText(tofind, prepend, append, transform, action)
     int tofind, transform, action;
     char *prepend, *append;
{
  char ch, ch2, *instruction, *tmp_instruction, 
  *GetInstruction(), *makelower();
  TABLE tmp, FindNode();
  int Execute(), ReplaceText(), i, in, in2, counter;
  long int tempcurrline;

  instruction = (char *) calloc (TMP_SIZE, sizeof(char));
  tmp_instruction = (char *) calloc (TMP_SIZE, sizeof(char));

  if(!action)
    fputs(prepend, fout);

  while((in = fgetc(fin)) != tofind)
    {
      ch = (char) in;
      if(ch == '@')
	{
	  ch = (char) fgetc(fin);
	  if(ch == '@' && (!action))
	    fputc('@', fout);
	  else if(ch == ' ' && (!action))
	    fputc(' ', fout);
	  else if(index(Scribechars, ch))
	    ExecuteSpecial(ch);
	  else
	    {
	      ungetc(ch, fin);
	      strcpy(tmp_instruction, GetInstruction());
	      tmp_instruction = makelower(tmp_instruction);
	      tmp = FindNode(SCRIBECOLUMN, tmp_instruction);
	      if(tmp == NULL && (!action))
		{
		  sprintf(instruction, "@%s", tmp_instruction);
		  fprintf(ferr, "* Scribe command %s not recognized\n",
			  instruction);
		  fputs(instruction, fout);
		}
	      else if(tmp != NULL)
		if(Execute(tmp_instruction) == tofind)
		  break;
	    }
	}
      else
	{
	  if((ch == '\\' || ch == '{' || ch == '}') && (!action))
	    fprintf(fout, "\\%c", ch);
	  else if(ch == '\n' || ch == '\r')
	    {
	      CurrLine++;
	      if(verbatim)
		counter = 2;
	      else
		counter = 1;
	      while((in2 = fgetc(fin)) != '\0')
		{
		  ch2 = (char) in2;
		  if(ch2 == '\n' || ch2 == '\r')
		    {
		      counter++;
		      CurrLine++;
		    }
		  else
		    {
		      ungetc(ch2, fin);
		      break;
		    }
		}
	      if((counter > 1) && (!verbatim))
		counter++;
	      fputs(append, fout);
	      for(i=1; i<=counter; i++)
		fputc('\n', fout);
	      if(i>2)
		fputs(prepend, fout);
	      if(!verbatim)
		AbsorbSpace();
	    }
	  else if(!action)
	    {
	      if(!transform)
		  fputc(ch, fout);
	      else if(transform == LOWERCASE && isupper(ch))
		  fputc(tolower(ch), fout);
	      else if(transform == CAPS && islower(ch))
		  fputc(toupper(ch), fout);
	      else
		  fputc(ch, fout);
	    }
	}
    }
  
  if(tofind == EOF)
    {
      tempcurrline = CurrLine++;
      PopFile();
      return(tempcurrline);
    }
  else if(tofind < 256)
    {
      ch = (char) fgetc(fin);
      if(ch != '\n' && ch != '\r')
	ungetc(ch, fin);
      else
	CurrLine++;
    }
return(CurrLine++);
}


TABLE FindNode(field, string)
     int field;
     char *string;
{
  TABLE tmp=Table;

  if(field == SCRIBECOLUMN)
    {
      while(tmp != NULL)
	{
	  if(!strcmp(tmp->scribeword, string))
	    return(tmp);
	  else
	    tmp = tmp->next;
	}
      return(NULL);
    }
  
  if(field == EZCOLUMN)
    {
      while(tmp != NULL)
	{
	  if(!strcmp(tmp->ez.word, string))
	    return(tmp);
	  else
	    tmp = tmp->next;
	}
      return(NULL);    
    }

  printf("* Unknown error!\n* %s:debug  Invalid mode sent to FindNode().\n", me);
  exit(0);
}


int ExecuteSpecial(character)
     char character;
{
  TABLE tmp;
  char string[2];
  
  sprintf(string, "%c", character);
  
  tmp = FindNode(SCRIBECOLUMN, string);
  
  if(tmp == NULL)
    {
      fprintf(ferr, "* Fatal error:\n* %s: %s%s%c%s\n%s\n%s\n", me,
	      "  Defective translation table:  ~", 
	      "  Scribechars entry promises meaning for \"", character, "\",",
	      "  but there is no corresponding entry.",
	      "  Program terminating.");
      exit(0);
    }
  else
    Execute(string);
}


char *GetInstruction()
{
  char character, *instruction;
  int in;

  instruction = (char *) calloc (TMP_SIZE, sizeof(char));

  while((in = fgetc(fin)) != EOF)
    {
      character = (char) in;
      if(character == ' ' || character == '\t'
	 || (index(Scribeopendelimiters, character) != 0) 
	 || (index(Scribeclosedelimiters, character) != 0))
	{
	  ungetc(character, fin);
	  return(instruction);
	}
      else if(character == '\n' || character == '\r')
	{
	  CurrLine++;
	  return(instruction);
	}
      else
	sprintf(instruction, "%s%c", instruction, character);
    }
  
  fprintf(ferr, "* Error:\n* %s: %s\n%s  ", me,
	  "Failure to find instruction after '@' character detected...",
	  "End of file reached.");
  
  CloseFiles();
}



int Execute(instruction)
     char *instruction;
{
  TABLE tmp;
  char ch, close;

  tmp = FindNode(SCRIBECOLUMN, instruction);

  if(tmp->mode & QUOTEDCHAR)
    fputc(tmp->ez.quote, fout);
  else if(tmp->mode & NAKED)
    {
      if(!(tmp->mode & COMMAND))
	ReplaceText(tmp->ez.word, tmp->mode, ' ');
      else
	return(tmp->ez.fun(instruction, ' '));
    }
  else
    {
      ch = ' ';
      while(ch == ' ' || ch == '\t' || ch == '\n' || ch == '\r')
	{
	  ch = (char) fgetc(fin);
	  if(ch=='\n' || ch=='\r')
	    CurrLine++;
	}
      
      if(index(Scribeopendelimiters, ch) || 
	 index(Scribeclosedelimiters, ch))
	{
	  if(index(Scribeclosedelimiters, ch))
	    {
	      fprintf(ferr, "* Fatal error!\n* %s: Bad nesting.", me);
	      exit(0);
	    }
	  else
	    {
	      close = Scribeclosedelimiters[offset(Scribeopendelimiters, ch)];
	      if(!(tmp->mode & COMMAND))
		ReplaceText(tmp->ez.word, tmp->mode, close);
	      else
		return(tmp->ez.fun(instruction, close));
	    }
	}	  
      else
	{
	  fprintf(ferr, "* Fatal error:\n* %s: Lack of parameters for command %s\n%s", me,
		  tmp->scribeword, "Program terminating");
	  exit(0);
	}
    }
}


int ReplaceText(instruction, mode, tofind)
     char *instruction, tofind;
     int mode;
{
  if(mode & NAKED)
    {
      fprintf(fout, "\n\\%s", instruction);      
      if(!(mode & SUPPRESSDELS))
	fputs("{}\n", fout);
      else
	fputs("\n", fout);
    }
  else
    {
      fprintf(fout, "\\%s{", instruction);

      ParseText(tofind, "", "", NORMAL, PRINTTOFILE);
      
      fputs("}\n", fout);
    }
}
