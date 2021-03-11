/*  
 R2Functions.c of

 Rtf2: a facility to convert RTF manuscript files to files 
 compatible with the ATK file format.

 Rtf2 is copyright (c) 1991 by the Massachusetts Institute of
 Technology.

 RTF is a product of the Microsoft Corporation.

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

 Rtf2 was written by Jeremy Paul Kirby, jpkirby@ATHENA.MIT.EDU and Scott Rixner, rixner@ATHENA.MIT.EDU

 $Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/mit/rtf2/RCS/R2Functions.c,v 1.2 1994/02/03 20:01:58 susan Exp $
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
 *  $Disclaimer: This software is part of version 5.2.0 of the 
 * Andrew User Interface System and is the 
 * property of IBM, Carnegie Mellon University, 
 * and the other copyright holders.  The source 
 * code of this version is for the sole use of 
 * members of the Andrew Consortium with 
 * memberships extending into calendar year 
 * 1993.  This source code is not to be distributed 
 * to non-members of the consortium nor beyond 
 * a fifty-mile radius from the membership address.  
 * Binary object code compiled or derived from 
 * these sources is not to be distributed to non-
 * members.  Members may have additional 
 * distribution rights granted by prior written 
 * permission of Carnegie Mellon University.
 * 
 * IBM, CARNEGIE MELLON UNIVERSITY, 
 * AND THE OTHER COPYRIGHT HOLDERS
 *  DISCLAIM ALL WARRANTIES WITH 
 * REGARD TO THIS SOFTWARE, INCLUDING 
 * ALL IMPLIED WARRANTIES OF MERCHANT-
 * ABILITY AND FITNESS. IN 
 * NO EVENT SHALL  IBM, CARNEGIE 
 * MELLON UNIVERSITY, OR ANY OTHER 
 * COPYRIGHT HOLDER BE LIABLE FOR 
 * ANY SPECIAL, INDIRECT OR CONSE-
 * QUENTIAL DAMAGES OR ANY DAMAGES 
 * WHATSOEVER RESULTING FROM LOSS OF
 * USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR
 * OTHER TORTIOUS ACTION, ARISING OUT 
 * OF OR IN CONNECTION WITH THE USE OR
 * PERFORMANCE OF THIS SOFTWARE.
 *  $
 *  $
*/

#ifndef NORCSID
#define NORCSID
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/mit/rtf2/RCS/R2Functions.c,v 1.2 1994/02/03 20:01:58 susan Exp $";
#endif


#include <stdio.h>
#include <sys/file.h>
#include <ctype.h>
#include "rtf2.h"
#include "input.h"

extern itoa();
extern reverse();
extern char *makelower();
extern FONT Font;
extern TABLE Table;
TABLE FindNode();
void CloseFiles();
long int ParseText();
extern int offset();
FP AssignFunc();
char *GetInstruction();
int R2UniqueID(), R2Delete(), R2Symbol(), R2NOP(), R2Footnote(), R2Caps(), R2SCaps(), R2Newpage(), R2Par(), R2FontDefine(), R2Margl(), R2Margr(), R2Font(), R2StyleSheet(), R2Plain(), R2Size(), R2Index(), R2Hidden(), R2Header(), R2Field(), R2Tab(), R2Current(), R2Indent(), R2Pard(), R2TabChange();

FP AssignFunc(ezword)
     char *ezword;
/*
 *
 *  Function that returns a pointer to the function
 *  associated with EZWORD.
 *
 */
{
  static struct func_words fnlist[FUNCTION_SIZE] = {
      {"caps",                R2Caps},
      {"current",             R2Current},
      {"delete",              R2Delete},
      {"field",               R2Field},
      {"font",                R2Font},
      {"fontdefine",          R2FontDefine},
      {"footnote",            R2Footnote},
      {"header",              R2Header},
      {"hidden",              R2Hidden},
      {"index",               R2Index},
      {"indent",              R2Indent},
      {"margl",               R2Margl},
      {"margr",               R2Margr},
      {"newpage",             R2Newpage},
      {"nop",                 R2NOP},
      {"par",                 R2Par},
      {"pard",                R2Pard},
      {"plain",               R2Plain},
      {"scaps",               R2SCaps},
      {"size",                R2Size},
      {"stylesheet",          R2StyleSheet},
      {"symbol",              R2Symbol},
      {"tab",                 R2Tab},
      {"tabchange",           R2TabChange}
  };
  int i;

  for(i = 0; i < FUNCTION_SIZE; i++)
     if(!ULstrcmp(ezword, fnlist[i].word))
       return(fnlist[i].fname);

  return(R2NOP);
}

int R2UniqueID()
/*
 *
 *  Function that returns a unique id number.
 *
 */
{
  return(++Token);
}

int R2Delete(command, numdel, tofind)
     char *command;
     int numdel;
     int tofind;
/*
 *
 *  Function that parses all of the text associated with COMMAND
 *  without processing it or writing it to the output file.
 *
 */
{
  ParseText(tofind, NORMAL, NOP);
  return CONTINUE;
}

  
int R2NOP(command, numdel, tofind)
     char *command;
     int numdel;
     int tofind;
/*
 *
 *  NOP function.
 *
 */
{
    return CONTINUE;
}

static int hex_digit_value(c)
    int c;
{
    if (c >= 'a' && c <= 'f')
	return 10 + c - 'a';
    else if (c >= 'A' && c <= 'F')
	return 10 + c - 'A';
    else
	return c - '0';
}

int R2Symbol(command, numdel, tofind)
     char *command;
     int numdel;
     int tofind;
/*
 *
 *  Reads a 2 digit hex number from the input, and writes
 *  its character equivelant to the output file.
 *
 */
{
     int number;

     /* fscanf(fin, "%2x", &number); */
     number = hex_digit_value(getc(fin)) * 16;
     number += hex_digit_value(getc(fin));

     printf("As number >%d< as char >%c<\n", number, number);
     fputc((char) number, fout);
     return CONTINUE;
}

     

int R2Error(command, numdel, tofind)
     char *command;
     int numdel;
     int tofind;
/*
 *
 *  Notification of errors.
 *
 */
{
  fprintf(ferr, "* Unknown error!\n* %s: unknown error in input file.\n", me);
  return CONTINUE;
}


int R2Footnote(command, numdel, tofind)
     char *command;
     int numdel;
     int tofind;
/*
 *
 *  Processes footnotes.  Writes required info to output file,
 *  then parses the rest of the input for the footnote.
 *
 */
{
  int token, i;

  token = R2UniqueID();
  CloseBraces();

  fprintf(fout, "\\footnote{\\\n");
  fprintf(fout, "\\begindata{fnote,%d}\n", token);
  fprintf(fout, "\\textdsversion{%d}\n", TextDSVersion);

  fnote = 1;
  ParseText(tofind, NORMAL, PRINTTOFILE);
  fnote = 0;

  fprintf(fout, "\\\n\\enddata{fnote,%d}\n", token);
  fprintf(fout, "\\view{fnotev,%d,3,0,0}}", token);

  return CONTINUE;
}


int R2Begin()
/*
 *
 *  Handles the occurence of a { during parsing of text.  If an
 *  instruction follows it is executed, otherwise the text is
 *  parsed with TOFIND as '}'.
 *
 */
{
  char ch, *makelower();
  char tmp_instruction[TMP_SIZE], instruction[TMP_SIZE]; 
  int in;
  TABLE tmp;

  AbsorbWhiteSpace();

  in = fgetc(fin);
  if(in==EOF)
    {
      fprintf(ferr, "* End of file reached after new group.\n");
      CloseFiles();
      exit(0);
    }
  
  ch = (char) in;
  if(ch == '\\')
    {
      strcpy(tmp_instruction, GetInstruction());
      sscanf(tmp_instruction, "%[A-z]", instruction);
      strcpy(instruction, makelower(instruction));

      tmp = FindNode(RTFCOLUMN, instruction);
      if(tmp == NULL)
	{
	  printf("* RTF command \\%s not recognized\n", instruction);
	  fprintf(fout, "\\%s", tmp_instruction);
	}
      else 
	{
	  Execute(tmp_instruction) ;
	  return;
	}
    }
  else
    ungetc(ch, fin);

  ParseText('}', NORMAL, PRINTTOFILE);
}

int R2FontDefine(command, numdel, tofind)
     char *command;
     int numdel;
     int tofind;
/*
 *
 *  Reads in the font table from the input file, and stores
 *  it in a table for use when fonts are referred to by
 *  number in the input file.
 *
 */
{
  int in, numtmp;
  char ch, facetmp[TMP_SIZE], qualitytmp[TMP_SIZE];
  FONT tmp;

  printf("Found font table.\n");

  while ((in = fgetc(fin)) != '}')
    {
      ch = (char) in;
     
      /* fscanf(fin, "\\f%d\\f%s %[^\;}];}", &numtmp, qualitytmp, facetmp); */
      input_match("\\f", fin);
      numtmp = input_number(fin);
      input_match("\\f", fin);
      input_read_up_to_whitespace(qualitytmp, TMP_SIZE, fin);
      input_skip_whitespace(fin);
      input_read_up_to_one_of(facetmp, TMP_SIZE, ";}", fin);
      input_match(";}", fin);

      tmp = (FONT) malloc(sizeof(struct FontStruct));
      tmp->number = numtmp;
      
      if(!strcmp(qualitytmp, "swiss"))
	tmp->ind = ANDYSANS;
      else if(!strcmp(qualitytmp, "roman"))
	tmp->ind = ANDY;
      else if(!strcmp(qualitytmp, "nil"))
	   tmp->ind = DEFAULT;
      else if(!strcmp(qualitytmp, "modern"))
	   tmp->ind = ANDYTYPE;
      else if(!strcmp(qualitytmp, "script"))
	   tmp->ind = DEFAULT;
      else if(!strcmp(qualitytmp, "tech"))
	   tmp->ind = ANDYSYMBOL;
      else if(!strcmp(qualitytmp, "decor"))
	tmp->ind = DEFAULT;
     
      if(Font == NULL)
	{
	  Font = tmp;
	  Font->next = NULL;
	}
      else
	{
	  tmp->next = Font;
	  Font = tmp;
	}
    }

  return CONTINUE;
}

int R2Caps(command, numdel, tofind)
     char *command;
     int numdel;
     int tofind;
/*
 *
 *  Converts input text to all capitals in the output file.
 *
 */
{
  ParseText(tofind, CAPS, PRINTTOFILE);
  return CONTINUE;
}

int R2SCaps(command, numdel, tofind)
     char *command;
     int numdel;
     int tofind;
/*
 *
 *  Converts input text to all small capitals in the output file.
 *
 */
{
  fputs("\\smaller{", fout);

  ParseText(tofind, CAPS, PRINTTOFILE);

  fputc('}', fout);
  return CONTINUE;
}

int R2Newpage(command, numdel, tofind)
     char *command;
     int numdel;
     int tofind;
/*
 *
 *  Handles page breaks.
 *
 */
{
  int token;

  token = R2UniqueID();
  fprintf(fout, "\\begindata{bp,%d}\n", token);
  fprintf(fout, "\\enddata{bp,%d}\n", token);
  fprintf(fout, "\\view{bpv,%d,1,0,0}\n", token);
  return CONTINUE;
}

int R2Margl(command, numdel, tofind)
   char *command;
   int numdel;
   int tofind;
/*
 *
 *  Change left margin.
 *
 */
{
     extern double LeftMargin;

     LeftMargin = (double) numdel/1440.0 - .5;

     fprintf(fout, "\n\\formatnote{.po %fi}\n", LeftMargin);
     return CONTINUE;
}


int R2Margr(command, numdel, tofind)
   char *command;
   int numdel;
   int tofind;
/*
 *
 *  Change right margin.
 *
 */
{
     extern double LeftMargin, RightMargin;

     RightMargin = 8.5 - LeftMargin - (double) numdel/1440.0;

     fprintf(fout, "\n\\formatnote{.ll %fi}\n", RightMargin);
     return CONTINUE;
}

int R2Par(command, numdel, tofind)
     char *command;
     int numdel;
     int tofind;
/*
 *
 *  Handle par command.  This function should be called for the
 *  first occurence of \par only.
 *
 */
{
   int index, counter;
   char ch, input[TMP_SIZE], tmp[TMP_SIZE * 5];

   counter = 1;
   index = 0;

   while(1)
   {
      tmp[index++] = ch = (char) fgetc(fin);
      if(ch=='\n' || ch=='\r')
      {
         CurrLine++;
         index--;
      }
      else if(isspace(ch))
         continue;
      else if(ch != '\\')
         break;
      else
      {
         strcpy(input, GetInstruction());
         if(!strcmp(input, "par"))
         {
            counter++;
            index = 0;
	 }
         else
         {
            tmp[index] = '\0';
            strcat(tmp, input);
            index += strlen(input);
            tmp[index++] = ' ';
	 }
      }
   }

   for(index--; index>=0; index--)
      ungetc(tmp[index], fin);
   for(; counter>=0; counter--)
      fputc('\n', fout);
   return CONTINUE;
}

int R2Font(command, numdel, tofind)
     char *command;
     int numdel;
     int tofind;
/*
 *
 *  Handle font changes within the input file by using the
 *  font table to map the font number to the font.
 *
 */
{
   FONT tmp = Font;

   while(tmp != NULL)
   {
      if(tmp->number == numdel)
         break;
      else
         tmp = tmp->next;
   }
   if(tmp == NULL)
   {
       /* Continue with old font */
       printf("Unknown font number %d\n", numdel);
       ParseText(tofind, NORMAL, PRINTTOFILE);
   }
   else
   {
       switch(tmp->ind)
       {
	  case ANDYSANS:
              fputs("\\sansserif{", fout);
              break;
	  case ANDYTYPE:
              fputs("\\typewriter{", fout);
              break;
	  case ANDYSYMBOL:
              fputs("\\symbol{", fout);
              break;
	  case ANDY:
	  case DEFAULT:
              ParseText(tofind, NORMAL, PRINTTOFILE);
              return;
       }
       ParseText(tofind, NORMAL, PRINTTOFILE);
       fputc('}', fout);
   }
   return CONTINUE;
}

int GetAttr(from, p1, p2, p3, p4)
     char *from, *p1, *p2, *p3, *p4;
/*
 *
 *  Get the 4 attributes associated with the command in
 *  the style sheet.
 *
 */
{
   static char *Attribs[ATTRIB_SIZE][5] = {
       {"plain", "FontFace", "Plain", "Int", "Set"},
       {"ql", "Justification", "LeftJustified", "Point", "0"},
       {"qr", "Justification", "RightJustified", "Point", "0"},
       {"qj", "Justification", "LeftAndRightJustified", "Point", "0"},
       {"qc", "Justification", "Centered", "Point", "0"},
       {"b", "FontFace", "Bold", "Int", "Set"},
       {"i", "FontFace", "Italic", "Int", "Set"},
       {"li", "LeftMargin", "ConstantMargin", "Inch", ""},
       {"ri", "RightMargin", "ConstantMargin", "Inch", ""},
       {"fi", "Indentation", "ConstantMargin", "Inch", ""},
       {"sb", "Above", "ConstantMargin", "Inch", ""},
       {"sa", "Below", "ConstantMargin", "Inch", ""},
       {"sl", "InterlineSpacing", "ConstantMargin", "Inch", ""},
       {"f", "FontFamily", "", "Int", "0"},
       {"fs", "FontSize", "ConstantFontSize", "Inch", ""},
       {"up", "Script", "ConstantScriptMovement", "Inch", ""},
       {"dn", "Script", "ConstantScriptMovement", "Inch", ""},
       {"tx", "Tabs", "LeftAligned", "Inch", ""}
   };
   int i, j, k;
   int len = strlen(from);
   int del = -1;
   int ret_val = 0;
   char tmp[TMP_SIZE], delstring[TMP_SIZE], *makelower();
   FONT tmpfont = Font;

   i = j = k = 0;

   while((i < len) && (from[i++] != '\\'))
      ;
   while((i < len) && (from[i] != '\\'))
   {
      if(isalpha(from[i]))
        tmp[j++] = from[i];
      else if(!isspace(from[i]))
        delstring[k++] = from[i];
      i++;
   }
   tmp[j] = '\0';
   delstring[k] = '\0';
   strcpy(tmp, makelower(tmp));
   if(k > 0)
     del = atoi(delstring);
   strcpy(from, &from[i]);

   for(i=0;i<ATTRIB_SIZE;i++)
      if(!strcmp(tmp, Attribs[i][0]))
      {
         ret_val++;
         break;
      }

   if(ret_val)
   {
      strcpy(p1, Attribs[i][1]);
      strcpy(p2, Attribs[i][2]);
      strcpy(p3, Attribs[i][3]);
      strcpy(p4, Attribs[i][4]);
      if(del>=0)
      {
         if((!strcmp(tmp,"b")) && !del)
            strcpy(p4, "Clear");
         else if((!strcmp(tmp,"i")) && !del)
            strcpy(p4, "Clear");
         else if(!strcmp(tmp,"f"))
         {
	     while(tmpfont != NULL)
	     {
		 if(tmpfont->number == del)
		     break;
		 else
		     tmpfont = tmpfont->next;
	     }
	     switch(tmpfont->ind)
	     {
		 case ANDYSANS:
		     strcpy(p2, "AndySans");
		     break;
		 case ANDY:
		     strcpy(p2, "Andy");
		     break;
		 case ANDYSYMBOL:
		     strcpy(p2, "AndySymbol");
		     break;
		 case ANDYTYPE:
                     strcpy(p2, "AndyType");
                     break;
		 case DEFAULT:
		     strcpy(p2, "Default");
		     break;
		 default:
                     ret_val = 0;
                     break;
	     }
	 }
         else
         {
             del = del * 65536 / 1440;
             if(tmp=="dn")
               del = -del;
             itoa(del, delstring);
             strcpy(p4, delstring);
	 }
      }
   }

   return(ret_val);
}


int R2StyleSheet(command, numdel, tofind)
     char *command;
     int numdel;
     int tofind;
/*
 *
 *  Process the style sheet declaration in the input file by
 *  defining the different styles in the output document, and
 *  adding them to a menu.
 *
 */
{
   static int done = 0;
   char ch,
        tmpstring[TMP_SIZE],
        name[TMP_SIZE],
        type[TMP_SIZE],
        basis[TMP_SIZE],
        unit[TMP_SIZE],
        param[TMP_SIZE];
   int in, check, a, n;

   printf("Found stylesheet.\n");   
 
   if(done)
      printf("Error - Multiple Style Sheet declarations exist.");
   else
   {
      while((in=fgetc(fin)) != '}')
      {
         while(in != '{')
            in = fgetc(fin);

	 /* fscanf(fin, "%[^;];}", tmpstring); */
	 input_read_up_to(tmpstring, TMP_SIZE, ';', fin);
	 input_match("}", fin);

         a = roffset(tmpstring, ' ');

         strcpy(name, &tmpstring[a+1]);
         tmpstring[a] = '\0';

         if((strcmp(tmpstring, "")) && (ULstrcmp(name, "Normal")))
         {
           fprintf(fout, "\\define{%s\n", name);
       	   fprintf(fout, "menu:[Style, %s]\n", name);
           while(strcmp(tmpstring, ""))
           { 
              check = GetAttr(tmpstring, type, basis, unit, param);
              if(check)
                fprintf(fout, "attr:[%s %s %s %s]\n", type, basis, unit, param);
	   }
	   fputs("}\n", fout);  
	 }
      }
   }

   done++;
   return CONTINUE;
}

int R2Plain(command, numdel, tofind)
   char *command;
   int numdel;
   int tofind;
/*
 *
 *  Process the plain command in the input file by closing
 *  off all styles that are currently in effect in the output
 *  file.
 *
 */
{
   CloseBraces();
   return CONTINUE;
}

int R2Size(command, numdel, tofind)
   char *command;
   int numdel;
   int tofind;
/*
 *
 *  Handle changes in the font size.
 *
 */
{
   int change = numdel - FontSize;
   int n = change / 4;
   int i, save;

   if(n<0)
     n = -n;
   FontSize = numdel;
   save = Levels;

   for(i=0; i<n; i++)
   {
      fputs(((change>0) ? "\\bigger{" : "\\smaller{"), fout);
      Levels++;
   }

   ParseText('}', NORMAL, PRINTTOFILE);

   if(Levels>save)
      FontSize -= change;
   for(; Levels>save; Levels--)
      fputc('}', fout);

   return(tofind);
}

int R2Hidden(command, numdel, tofind)
     char *command;
     int numdel;
     int tofind;
/*
 *
 *  Handles hidden text for annotations.  Currently only
 *  recognizes hidden text as either part of an index entry
 *  or an annotation.  It looks through the text to see if
 *  it's an index entry by checking if there is a group
 *  before a } that starts off "{\xe", or one of the four
 *  equivelent indexing commands, and if so, hands control
 *  over to R2Index(), otherwise it treats the hidden text
 *  as an annotation.
 *
 */
{
   int index;
   int token, token2;
   char ch, string[TMP_SIZE * 5], inst[TMP_SIZE];

   /* If the RTF document was originally run through
      2rtf, there will be a scripted style sheet to
      replace the RTF style sheet. */

   for(index = 0; index < 11; index++)
      string[index] = (char) fgetc(fin);
   string[index] = '\0';
   if(!strcmp(string, "STYLESHEET:"))
   {
      while((ch = (char) fgetc(fin)) != '}')
         switch(ch)
         {
	     case '|':
		 fputs("\\", fout);
		 break;
	     case '(':
		 fputs("{", fout);
		 break;
	     case ')':
		 fputs("}", fout);
		 break;
	     default:
		 fputc(ch, fout);
		 break;
	 }
      return(tofind);
   }
   else
      for(index = 10; index >= 0; index--)
         ungetc(string[index], fin);


   /* Check to see if a Style was scripted by 2rtf
      The expansion of the style needs to be eliminated. */


   for(index = 0; index < 7; index++)
      string[index] = (char) fgetc(fin);
   string[index] = '\0';
   if(!strcmp(string, "STYLE: "))
   {
      /* fscanf(fin, "%[^}]}", string); */
      input_read_up_to(string, TMP_SIZE, '}', fin);

      while((ch = (char) fgetc(fin)) == '\\')
         while(!isspace(ch))
            ch = (char) fgetc(fin);
      ungetc(ch, fin);
      fprintf(fout, "\\%s{", string);
      ParseText(tofind, NORMAL, PRINTTOFILE);
      fputs("}", fout);
      ungetc('}', fin);
      return(tofind);
   }
   else
      for(index = 6; index >= 0; index--)
         ungetc(string[index], fin);


   /* Check for template annotation from 2rtf. */
   for (index = 0; index < 10; index++)
       string[index] = (char) fgetc(fin);
   string[index] = '\0';
   if(!strcmp(string, "TEMPLATE: "))
   {
       input_read_up_to(string, TMP_SIZE, '}', fin);
       fprintf(fout, "\\template{%s}\n", string);

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

       fputs("\\define{rightindent\n", fout);
       fputs("menu:[Region, RightIndent]\n", fout);
       fputs("attr:[RightMargin RightMargin Cm 82074]}\n", fout);

       return(tofind);
   }


   /* Make sure it is not an index entry. */

   index = 0;
   ch = (char) fgetc(fin);
   while((ch != '}') && (ch != '{'))
   {
     string[index++] = ch;
     ch = (char) fgetc(fin);
   }
   if(ch == '{')
   {
     ch = (char) fgetc(fin);
     if(ch == '\\')
     {
        strcpy(inst, GetInstruction());
        if(strcmp(inst, "xe"))
          sscanf(inst, "%c%s", ch, inst);
        if(!strcmp(inst, "xe"))
        {
          if(ch != '\\')
            sprintf(inst, "%c%s", ch, inst);
          R2Index(inst, 0, tofind);
          ParseText('}', NORMAL, PRINTTOFILE);
          return CONTINUE;
	}
        else
        {
          sprintf(inst, "%c%s", ch, inst);
          string[index++] = '\\';
          string[index] = '\0';
          strcat(string, inst);
          while(string[index] != '\0')
             index++;
	}
     }
     else
     {
       string[index++] = '{';
       string[index++] = ch;
     }
   }
   else
     string[index++] = ch;
   while(index>0)
     ungetc(string[--index], fin);

   /* Annotation */

   token = R2UniqueID();
   token2 = R2UniqueID();
   fprintf(fout, "\n\\begindata{note,%d}\n", token);
   fprintf(fout, "200 100 1\n");

   /* Check to see if author field was scripted by 2rtf */
   for(index = 0; index < 9; index++)
      string[index] = (char) fgetc(fin);
   string[index] = '\0';
   if(!strcmp(string, "Author: "))
   {
      /* fscanf(fin, "%[^.].  ", string); */
      input_read_up_to(string, TMP_SIZE, '.', fin);
      input_skip_whitespace(fin);

      fprintf(fout, "\\title{%s}\n", string);
   }
   else
      for(index = 8; index >= 0; index--)
         ungetc(string[index], fin);

   fprintf(fout, "\\begindata{text,%d}\n", token2);
   fprintf(fout, "\\textdsversion{%d}\n", TextDSVersion);

   ParseText('}', NORMAL, PRINTTOFILE);

   fprintf(fout, "\\\n\\enddata{text,%d}\n", token2);
   fprintf(fout, "\\enddata{note,%d}\n", token);
   fprintf(fout, "\\view{noteview,%d,0,0,0}", token);
   return CONTINUE;
}

int R2Index(command, numdel, tofind)
     char *command;
     int numdel;
     int tofind;
/*
 *
 *  Handles index entries.  Due to the nature of rtf, it
 *  is difficult to determine whether or not an index entry
 *  is to be invisible or not.  This procedure works based
 *  on the assumption that all invisible index entries will
 *  start like this: "\xe\pard\plain {\v" with or without
 *  the space.  If something else is encountered, it is
 *  made into a visible index entry.  Due to the nature of
 *  EZ, it is not easy to keep the entries correct if the
 *  words are also formatted, so in both cases, formatting
 *  is stripped, and an invisible index entry is made.  For
 *  visible entries, the formatted text is parsed over again.
 *
 */
{
   int i, n;
   char tmp[TMP_SIZE], entry[TMP_SIZE], ch;

   n = 0;
   for(i=0; i<2; i++)
   {
      tmp[n++] = ch = (char) fgetc(fin);
      if(ch == '\\')
      {
         strcpy(entry, GetInstruction());
         tmp[n] = '\0';
         strcat(tmp, entry);
         n += strlen(entry);
         tmp[n++] = ' ';
         if(strcmp(entry, "pard"))
           break;
      }
      else
         break;
   }

   if(!strcmp(entry, "plain"))
   {
      /* Check for " {\v", or "{\v", if neither are
         there put everything back into the input
         file and declare it a visible index entry */

      AbsorbSpace();
      tmp[n++] = ch = (char) fgetc(fin);
      if(ch == '{')
      {
         tmp[n++] = ch = (char) fgetc(fin);
         if(ch == '\\')
         {
            tmp[n++] = ch = (char) fgetc(fin);
            if(ch == 'v')
            {
	       /* Invisible index entry.  Strip out
	          formatting comands.  Second ParseText
	          is there because the \v was skipped,
	          and its group needs to be handled. */

               while(1)
               {
                  ch = (char) fgetc(fin);
                  if(ch == '\\')
                     ParseText(' ', NORMAL, NOP);
                  else if(!isspace(ch))
                  {
                     ungetc(ch, fin);
                     break;
		  }
	       }
               fputs("\\indexi{", fout);
	       ParseText('}', NORMAL, PRINTTOFILE);
	       fputc('}', fout);
	       ParseText('}', NORMAL, PRINTTOFILE);
               return CONTINUE;
	    }
            else
            {
               ungetc(ch, fin);
               ungetc('\\', fin);
               n -= 2;
	    }
	 }
         else
         {
            ungetc(ch, fin);
            n -= 1;
	 }
      }
      else
      {
         ungetc(ch, fin);
         n -= 1;
      }
   }

   /* Turn visible index entry into an invisible one, while
      preserving the text without a visible index entry. */

   i = 0;
   while(1)
   {
      tmp[n++] = ch = (char) fgetc(fin);
      if(ch == '{')
         continue;
      else if(ch == '\\')
         while(!isspace(ch))
            tmp[n++] = ch = (char) fgetc(fin);
      else if(!isspace(ch))
      {
	  while(ch != '}')
          {
             entry[i++] = ch;
             tmp[n++] = ch = (char) fgetc(fin);
	  }
          entry[i] = '\0';
          break;
      }
   }

   for(n--; n>=0; n--)
      ungetc(tmp[n], fin);
   fprintf(fout, "\\indexi{%s}", entry);
   ParseText('}', NORMAL, PRINTTOFILE);
   return CONTINUE;
}

int R2Field(command, numdel, tofind)
     char *command;
     int numdel;
     int tofind;
/*
 *
 *  Handle fields only by finding \fldrslt and copying
 *  that.
 *
 */
{
   ParseText('}', NORMAL, PRINTTOFILE);
   if(!strcmp(command, "fldrslt"))
     ParseText('}', NORMAL, NOP);
   return CONTINUE;
}

int R2Header(command, numdel, tofind)
     char *command;
     int numdel;
     int tofind;
/*
 *
 *  Handle headers and footers.  Breaks up left, center,
 *  and right by tabs in the rtf document.  When it sets
 *  flag to 1, that tells the tab routine that it's
 *  handling a header/footer.
 *
 */
{
   int token1, token2;
   int head = 0;
   int done = 0;
   char special;

   if(!strncmp(command, "header", 6))
     head = 1;
   special = command[6];

   token1 = R2UniqueID();
   fprintf(fout, "\\begindata{header,%d}\n", token1);
   fprintf(fout, "where:%s\n", head ? "header" : "footer");
   fprintf(fout, "active:111\n");

   token2 = R2UniqueID();
   fprintf(fout, "\\begindata{text,%d}\n", token2);
   fprintf(fout, "\\textdsversion{%d}\n", TextDSVersion);

   flag = 1;
   /* Parse text until a tab or end is encountered. */
   ParseText('}', NORMAL, PRINTTOFILE);
   /* Set done if end is reached */
   if(flag)
     done = 1;

   fprintf(fout, "\\\n\\enddata{text,%d}\n", token2);

   token2 = R2UniqueID();
   fprintf(fout, "\\begindata{text,%d}\n", token2);
   fprintf(fout, "\\textdsversion{%d}\n", TextDSVersion);

   /* Parse text until a tab or end is encountered. */
   if(!done)
   {
      flag = 1;
      ParseText('}', NORMAL, PRINTTOFILE);
      /* Set done if end is reached */
      if(flag)
        done = 1;
   }

   fprintf(fout, "\\\n\\enddata{text,%d}\n", token2);

   token2 = R2UniqueID();
   fprintf(fout, "\\begindata{text,%d}\n", token2);
   fprintf(fout, "\\textdsversion{%d}\n", TextDSVersion);

   /* Parse text until the end of the group. */
   if(!done)
     ParseText('}', NORMAL, PRINTTOFILE);

   fprintf(fout, "\\\n\\enddata{text,%d}\n", token2);

   fprintf(fout, "\\enddata{header,%d}\n", token1);
   fprintf(fout, "\\view{headrtv,%d,%d,0,0}\n", token1,
	    head ? 15 : 17);

   flag = 0;
   return CONTINUE;
}

int R2Tab(command, numdel, tofind)
     char *command;
     int numdel;
     int tofind;
/*
 *
 *  Handle tabs.  If flag is set, then we are dealing
 *  with a header/footer.
 *
 */
{
   if(flag)
   {
     flag = 0;
     return('}');
   }
   else
     fputc('\t', fout);
   return CONTINUE;
}

int R2Current(command, numdel, tofind)
     char *command;
     int numdel;
     int tofind;
/*
 *
 *  Handle current date & time, as well as pagenumber.
 *
 */
{
   if(!strcmp(command, "chpgn"))
     fputs("  $page", fout);
   else if(!strcmp(command, "chdate"))
     fputs("$date", fout);
   else if(!strcmp(command, "chtime"))
     fputs("$timeofday", fout);
   return CONTINUE;
}

/*
int R2Indent(command, numdel, tofind)
     char *command;
     int numdel;
     int tofind;
 *
 *
 *  Handle local indentation.  Using formatnote.
 *
 *
{
   extern double LeftMargin, RightMargin;
   double m;
   int left;

   left = strcmp(command, "ri");

   if(left)
      m = LeftMargin + (double) numdel/1440.0;
   else
      m = RightMargin - (double) numdel/1440.0;

   fprintf(fout, "\\formatnote{%s %.2fi}\n\n", left ? ".po" : ".ll", m);

   indented = 1;
}
*/

int R2Indent(command, numdel, tofind)
     char *command;
     int numdel;
     int tofind;
/*
 *
 *  Handle local indentation.  Rounding to LeftIndent and
 *  RightIndent.
 *
 */
/* Note: This scheme is not sufficient to handle interleaved sequences of
 * left indents and right indents (.li 720, .ri 720, .li 0, .ri 0).  Perhaps
 * there is a better way of dealing with this case. --GBH */
{
   int is_left_indent;
   int i, target, *counter;

   /* Determine if indent is left indent or right indent. */
   is_left_indent = (strcmp(command, "ri") != 0);
   counter = (is_left_indent) ? &left_indented : &right_indented;

   /* Compute target number of "\leftindent"s or "\rightindent"s. */
   target = 0;
   while (numdel > 0) {
       numdel -= 720;
       target++;
   }
   if (target > 1 && numdel < -360)
       target--;

   /* Output appropriate number of indent commands or right braces to reach
    * target. */
   while (*counter < target) {
       fputs((is_left_indent) ? "\\leftindent{" : "\\rightindent{", fout);
       (*counter)++;
   }
   while (*counter > target) {
       fputs("}", fout);
       (*counter)--;
   }

   return CONTINUE;
}

int R2Pard(command, numdel, tofind)
     char *command;
     int numdel;
     int tofind;
/*
 *
 *  Reset default paragraph specifications.
 *
 */
{
   if(fnote)
      return;

/*
   if(indented)
   {
      fprintf(fout, "\\formatnote{.po %.2fi}\n\n", LeftMargin);
      fprintf(fout, "\\formatnote{.ll %.2fi}\n\n", RightMargin);
      indented = 0;
   }
*/

   for(; left_indented > 0; left_indented--)
       fputs("}", fout);
   for(; right_indented > 0; right_indented--)
       fputs("}", fout);

   if(tabs)
   {
      fprintf(fout, "\\formatnote{.ta}\n\n");
      tabs = 0;
   }

   return CONTINUE;
}


int R2TabChange(command, numdel, tofind)
     char *command;
     int numdel;
     int tofind;
/*
 *
 *  Handle local tab changes.
 *
 */
{
/*
   static char type = 'L';

   if(!strcmp(command, "tqr"))
   {
      type = 'R';
      return;
   }
   if(!strcmp(command, "tqc"))
   {
      type = 'C';
      return;
   }

   fprintf(fout, "\\formatnote{.ta %.2fi", numdel/1440.0);
   if(type != 'L')
   {
      fputc(type, fout);
      type = 'L';
   }
   fputs("}\n\n", fout);
   tabs = 1;
*/
    return CONTINUE;
}

