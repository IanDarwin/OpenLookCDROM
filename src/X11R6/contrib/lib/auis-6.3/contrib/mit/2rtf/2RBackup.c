/*  
 2RFunctions.c of

 2rtf: a facility to convert files in the ATK file format to
 RTF manuscript files.

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

 Rtf2 was written entirely by Jeremy Paul Kirby, jpkirby@ATHENA.MIT.EDU

 $Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/mit/2rtf/RCS/2RBackup.c,v 1.1 1994/02/02 19:05:20 susan Exp $
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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/mit/2rtf/RCS/2RBackup.c,v 1.1 1994/02/02 19:05:20 susan Exp $";
#endif


#include <stdio.h>
#include <sys/file.h>
#include <ctype.h>
#include "2rtf.h"

extern char *makelower();
extern TABLE Table;
extern IDSTACK IdStack;
extern STYLE Style;
TABLE FindNode();
void CloseFiles();
long int ParseText();
FP AssignFunc();
char *GetInstruction();
int RSLMargin(), RSRMargin(), RSTMargin(), RSBMargin(), RSIndent(), RSIpSpacing(), RSAbove(), RSBelow(), RSIlSpacing(), RSFontFamily(), RSFontSize(), RSFontScript(), RSTabChange(), RSFontFace(), RSJustify();
int RNOP(), RDelete(), RError(), RBegin(), RDSVer(), REnd(), RText(), RAnnotation(), RFootnote(), RSkip(), RTitle(), RNewpage(), RHeader(), RSize(), RFont(), RTemplate(), RIndent(), RIndex(), RStyleMain();


FP AssignFunc(rtfword)
     char *rtfword;
/*
 *
 *  Function that returns a pointer to the function
 *  associated with RTFWORD.
 *
 */
{
  static struct func_words fnlist[FUNCTION_SIZE] = {
      {"begin",		    RBegin},
      {"delete",	    RDelete},
      {"dsver",		    RDSVer},
      {"end",		    REnd},
      {"font",		    RFont},
      {"indent",	    RIndent},
      {"index",		    RIndex},
      {"nop",		    RNOP},
      {"size",		    RSize},
      {"skip",		    RSkip},
      {"style",		    RStyleMain},
      {"template",	    RTemplate},
      {"title",		    RTitle}
  };
  int i;

  for(i = 0; i < FUNCTION_SIZE; i++)
     if(!ULstrcmp(rtfword, fnlist[i].word))
       return(fnlist[i].fname);

  return(RNOP);
}

int RDelete(command, transform, tofind)
     char *command;
     int transform, tofind;
/*
 *
 *  Function that parses all of the text associated with COMMAND
 *  without processing it or writing it to the output file.
 *
 */
{
  ParseText(tofind, NORMAL, NOP);
}

  
int RNOP(command, transform, tofind)
     char *command;
     int transform, tofind;
/*
 *
 *  NOP function.
 *
 */
{
}

int RError(command, transform, tofind)
     char *command;
     int transform, tofind;
/*
 *
 *  Notification of errors.
 *
 */
{
  fprintf(ferr, "* Unknown error!\n* %s: unknown error in input file.\n", me);
}

int RBegin(command, transform, tofind)
     char *command;
     int transform, tofind;
{
   char type[TMP_SIZE];
   long int id = 0;
   IDSTACK tmp;
   int i;
   FP function = RNOP;
   static struct func_words typelist[TYPE_SIZE] = {
       {"text",			RText},
       {"note",			RAnnotation},
       {"fnote",		RFootnote},
       {"bp",			RNewpage},
       {"header",		RHeader}
   };

   fscanf(fin, "%[^,],%ld}", type, &id);

   for(i = 0; i < TYPE_SIZE; i++)
      if(!ULstrcmp(type, typelist[i].word))
      {
         function = typelist[i].fname;
         break;
      }

   tmp = (IDSTACK) malloc(sizeof(struct IdStackStruct));
   tmp->idnum = id;
   tmp->next = IdStack;
   IdStack = tmp;
   if(function == RNOP)
      ParseText('}', transform, PRINTTOFILE);
   else
      function(type, transform, '}');
}

int REnd(command, transform, tofind)
     char *command;
     int transform, tofind;
{
   char type[TMP_SIZE];
   long int id;

   fscanf(fin, "%[^,],%ld}", type, &id);
   if((IdStack != NULL) && (id == IdStack->idnum))
   {
      IdStack = IdStack->next;
      if(headerflag)
      {
	 ungetc('}', fin);
         headerflag = 0;
      }
      return('}');
   }
   else
   {
      printf("* Improper \\enddata encountered.  Id #: %ld", id);
   }
}

int RDSVer(command, transform, tofind)
     char *command;
     int transform, tofind;
{
   int version;

   fscanf(fin, "%d}", &version);
   if(version != TextDSVersion)
      fprintf(stderr, "* Error:\n* %s: Wrong TextDSVersion: %d.\n", me, version);
}

int RText(command, transform, tofind)
     char *command;
     int transform, tofind;
{
   ParseText(tofind, transform, PRINTTOFILE);
}

int RAnnotation(command, transform, tofind)
     char *command;
     int transform, tofind;
{
   int a, b, c;

   fscanf(fin, "%d %d %d", &a, &b, &c);
   fputs("{\\v \\pard\\plain ", fout);
   ParseText(tofind, NORMAL, PRINTTOFILE);
   fputs("}\n", fout);
}

int RFootnote(command, transform, tofind)
     char *command;
     int transform, tofind;
{
   fputs("{\\fs18\\up6 \\chftn {\\footnote \\pard\\plain\n", fout);
   fputs("\\s246 \\fs20 {\\fs18\\up6 \\chftn }", fout);
   ParseText(tofind, transform, PRINTTOFILE);
   fputs("}}\n", fout);
}

int RSkip(command, transform, tofind)
     char *command;
     int transform, tofind;
{
   ParseText(tofind, transform, PRINTTOFILE);
}

int RTitle(command, transform, tofind)
     char *command;
     int transform, tofind;
{
   fputs("Author: ", fout);
   ParseText(tofind, transform, PRINTTOFILE);
   fputs(".  ", fout);
}

int RNewpage(command, transform, tofind)
     char *command;
     int transform, tofind;
{
   fputs("\\page ", fout);
   ParseText(tofind, transform, PRINTTOFILE);
}

int RHeader(command, transform, tofind)
     char *command;
     int transform, tofind;
{
   char type[TMP_SIZE], ch;
   int header = 0;
   int active;

   fscanf(fin, "\nwhere:%s\n", type);
   if(strcmp(type, "footer"))
      header = 1;

   fscanf(fin, "active:%d\n", &active);
   CurrLine += 3;

   fprintf(fout, "{\\%s \\pard", header ? "header" : "footer");
   fprintf(fout, "\\plain \\s243\\tqc\\tx4320\\tqr\\tx8640 ");

   /* Parse 3 sections of header object, and separate them
      by tabs. */

   headerflag = 1;
   ParseText(tofind, HEADER, PRINTTOFILE);
   fputs("\\tab ", fout);

   headerflag = 1;
   ParseText(tofind, HEADER, PRINTTOFILE);
   fputs("\\tab ", fout);

   ParseText(tofind, HEADER, PRINTTOFILE);

   fputs("}\n", fout);
}

int RSize(command, transform, tofind)
     char *command;
     int transform, tofind;
{
   int big = 0;
   int oldfont = FontSize;

   if(strcmp(command, "smaller"))
      big = 1;

   if(big)
      FontSize += 4;
   else
      FontSize -= 4;

   fprintf(fout, "{\\fs%d ", FontSize);
   ParseText(tofind, transform, PRINTTOFILE);
   fputs("}", fout);

   FontSize = oldfont;
}

int RFont(command, transform, tofind)
     char *command;
     int transform, tofind;
{
   static struct FontStruct fonts[FONT_SIZE] = {
       {"sansserif",	CHICAGO},
       {"typewriter",	MONACO},
       {"symbol",	SYMBOL}
   };
   int i;
   int n = NEW_YORK;

   for(i=0; i<FONT_SIZE; i++)
      if(!strcmp(command, fonts[i].name))
      {
         n = fonts[i].num;
         break;
      }

   fprintf(fout, "{\\f%d ", n);
   ParseText(tofind, transform, PRINTTOFILE);
   fprintf(fout, "}");
}

int RTemplate(command, transform, tofind)
     char *command;
     int transform, tofind;
{
   char temp[TMP_SIZE];

   fscanf(fin, "%[^}]}", temp);
   if(strcmp(temp, "default"))
     printf("Warning, template other than default: %s.\n", temp);
}

int RIndent(command, transform, tofind)
     char *command;
     int transform, tofind;
{
   fputs("\\li720\\ri720 ", fout);
   ParseText(tofind, transform, PRINTTOFILE);
}

int RIndex(command, transform, tofind)
     char *command;
     int transform, tofind;
{
   int hidden = 0;

   if(!strcmp(command, "indexi"))
      hidden = 1;
   fputs("{\\v {\\xe\\pard\\plain ", fout);
   if(hidden)
   {
      fputs("{\\v ", fout);
      ParseText(tofind, INDEX, PRINTTOFILE);
      fputs("}}}", fout);
   }
   else
   {
      ParseText(tofind, NORMAL, PRINTTOFILE);
      fputs("}}", fout);
   }
}

int RStyleDefine()
{
   char name[TMP_SIZE], type[TMP_SIZE],
        p1[TMP_SIZE], p2[TMP_SIZE], p3[TMP_SIZE], p4[TMP_SIZE],
        ch;
   int i;
   STYLE tmp;
   static int styleid = 0;
   char stylestring[TMP_SIZE];

   tmp = (STYLE) malloc(sizeof(struct StyleStackStruct));

   fprintf(fout, "{\\s%d ", ++styleid);
   fscanf(fin, "%s", name);

   tmp->idnum = styleid;
   strcpy(tmp->name, name);
   tmp->next = Style;
   Style = tmp;

   stylestring[0] = '\0';

   while(1)
   {
      AbsorbNewlines();
      fscanf(fin, "%[^:}]:", type);
      if(!strcmp(type, "menu"))
      {
         ch = ' ';
         while(ch != ']')
            ch = (char) fgetc(fin);
      }
      else if(!strcmp(type, "attr"))
      {
          fscanf(fin, "[%s %s %s ", p1, p2, p3);
          i = 0;
          while((ch = (char) fgetc(fin)) != ']')
             p4[i++] = ch;
          p4[i] = '\0';
          /* handle actual conversion to rtf... */
          RStyleConvert(p1, p2, p3, p4, stylestring);
      }
      else
      {
          ch = ' ';
	  while(ch != ']' && ch != '}')
             ch = (char) fgetc(fin);
	  if(ch == '}')
	     ungetc(ch, fin);
      }

      AbsorbNewlines();
      ch = (char) fgetc(fin);
      if(ch != '}')
         ungetc(ch, fin);
      else
         break;
   }

   strcpy(Style->string, stylestring);
   fprintf(fout, "\\sbasedon0\\snext%d %s;}", styleid, name);
}

int RStyleMain(command, transform, tofind)
     char *command;
     int transform, tofind;
{
   char temp[TMP_SIZE], ch;
   int i;
   static int done = 0;

   if(done)
   {
      RDelete(command, transform, tofind);
      return;
   }
   else
      done = 1;

   fputs("{\\stylesheet", fout);
   fputs("{\\s243\\tqc\\tx4320\\tqr\\tx8640 \\sbasedon0\\snext243 footer;}", fout);
   fputs("{\\s244\\tqc\\tx4320\\tqr\\tx8640 \\sbasedon0\\snext244 header;}", fout);
   fputs("{\\s245 \\fs18\\up6 \\sbasedon0\\snext0 footnote reference;}", fout);
   fputs("{\\s246 \\fs20 \\sbasedon0\\snext246 footnote text;}", fout);
   fputs("{\\sbasedon222\\snext0 Normal;}", fout);

   RStyleDefine();
   while(1)
   {
      ch = ' ';
      while(isspace(ch))
      {
         AbsorbNewlines();
         ch = (char) fgetc(fin);
      }
      if(ch != '\\')
      {
         ungetc(ch, fin);
         break;
      }

      ch = (char) fgetc(fin);
      if(ch == '\\' || ch == '{' || ch == '}')
      {
          ungetc(ch, fin);
          ungetc('\\', fin);
          break;
      }

      ungetc(ch, fin);
      i = 0;
      while(1)
      {
         ch = (char) fgetc(fin);
         if(ch == '{')
         {
            ungetc(ch, fin);
            break;
	 }
         else if(isspace(ch))
            break;
         temp[i++] = ch;
      }
      temp[i] = '\0';

      if(!strcmp(temp, "define"))
      {
         if(ch = '{')
            fgetc(fin);           
	 RStyleDefine();
      }
      else
      {
         if(isspace(ch))
            ungetc(ch, fin);
         for(i--; i>=0; i--)
            ungetc(temp[i], fin);
         ungetc('\\', fin);
         break;
      }
   }

   fputs("}\n", fout);
}

int RStyleApply(tmp)
     STYLE tmp;
{
   char ch;

   fprintf(fout, "\\s%d%s ", tmp->idnum, tmp->string);

   ch = fgetc(fin);
   if(ch != '{')
      ungetc(ch, fin);
   
   ParseText('}', NORMAL, PRINTTOFILE);
}

int RStyleConvert(p1, p2, p3, p4, stylestring)
    char *p1, *p2, *p3, *p4, *stylestring;
{
   static struct func_words Attribs[15] = {
      {"LeftMargin",		RSLMargin},
      {"RightMargin",		RSRMargin},
      {"TopMargin",		RSTMargin},
      {"BottomMargin",		RSBMargin},
      {"Indentation",		RSIndent},
      {"InterparagraphSpacing",	RSIpSpacing},
      {"Above",			RSAbove},
      {"Below",			RSBelow},
      {"InterlineSpacing",	RSIlSpacing},
      {"FontFamily",		RSFontFamily},
      {"FontSize",		RSFontSize},
      {"FontScript",		RSFontScript},
      {"TabChange",		RSTabChange},
      {"FontFace",		RSFontFace},
      {"Justification",		RSJustify}
   };
   int i;

   for(i=0; i<15; i++)
      if(!strcmp(p1, Attribs[i].word))
      {
         Attribs[i].fname(p2, p3, p4, stylestring);
         break;
      }
}

int Delimeter(units, n)
   char *units, *n;
{
   struct divider
   {
       char *name;
       float div;
   };
   static struct divider types[UNITS] = {
      {"inch", 45.5},
      {"cm", 114.0},
      {"point", 20.0},
      {"rawdot", 20.0}
   };
   int i, numdel = -1;

   strcpy(units, makelower(units));

   for(i=0; i<UNITS; i++)
      if(!strcmp(units, types[i].name))
         numdel = ((float) atoi(n)) / types[i].div;

   if(numdel < 0)
      fprintf(ferr, "*Error:  Unknown unit of measurement, %s.\n", units);

   return(numdel);
}

int RSLMargin(basis, unit, operand, stylestring)
     char *basis, *unit, *operand, *stylestring;
/*
 *
 *   ConstantMargin
 *   LeftMargin
 *   LeftEdge
 *   RightMargin
 *   RightEdge
 *
 */
{
   int numdel;

   numdel = Delimeter(unit, operand);

   if(numdel>0 && (!strcmp(basis, "ConstantMargin") || !strcmp(basis, "LeftMargin")))
   {
      fprintf(fout, "\\li%d ", numdel);
      sprintf(stylestring, "%s\\li%d ", stylestring, numdel);
   }
}

int RSRMargin(basis, unit, operand, stylestring)
     char *basis, *unit, *operand, *stylestring;
/*
 *
 *   ConstantMargin
 *   LeftMargin
 *   LeftEdge
 *   RightMargin
 *   RightEdge
 *
 */
{
   int numdel;

   numdel = Delimeter(unit, operand);

   if(numdel>0 && (!strcmp(basis, "ConstantMargin") || !strcmp(basis, "RightMargin")))
   {
      fprintf(fout, "\\ri%d ", numdel);
      sprintf(stylestring, "%s\\ri%d ", stylestring, numdel);
   }
}

int RSTMargin(basis, unit, operand, stylestring)
     char *basis, *unit, *operand, *stylestring;
/*
 *
 *   ConstantMargin
 *   TopMargin
 *   TopEdge
 *   BottomMargin
 *   BottomEdge
 *
 */
{
   int numdel;

   numdel = Delimeter(unit, operand);

}

int RSBMargin(basis, unit, operand, stylestring)
     char *basis, *unit, *operand, *stylestring;
/*
 *
 *   ConstantMargin
 *   TopMargin
 *   TopEdge
 *   BottomMargin
 *   BottomEdge
 *
 */
{
   int numdel;

   numdel = Delimeter(unit, operand);

}

int RSIndent(basis, unit, operand, stylestring)
     char *basis, *unit, *operand, *stylestring;
/*
 *
 *   ConstantMargin
 *   PreviousIndention
 *
 */
{
   int numdel;

   numdel = Delimeter(unit, operand);

   if(numdel>0 && !strcmp(basis, "ConstantMargin"))
   {
      fprintf(fout, "\\li%d\\ri%d ", numdel, numdel);
      sprintf(stylestring, "%s\\li%d\\ri%d ", stylestring, numdel, numdel);
   }
}

int RSIpSpacing(basis, unit, operand, stylestring)
     char *basis, *unit, *operand, *stylestring;
{
   int numdel;

   numdel = Delimeter(unit, operand);

}

int RSAbove(basis, unit, operand, stylestring)
     char *basis, *unit, *operand, *stylestring;
/*
 *
 *   ConstantMargin
 *   AboveSpacing
 *
 */
{
   int numdel;

   numdel = Delimeter(unit, operand);

   if(numdel>0)
   {
      fprintf(fout, "\\sb%d ", numdel);
      sprintf(stylestring, "%s\\sb%d ", stylestring, numdel);
   }
}

int RSBelow(basis, unit, operand, stylestring)
     char *basis, *unit, *operand, *stylestring;
/*
 *
 *   ConstantMargin
 *   BelowSpacing
 *
 */
{
   int numdel;

   numdel = Delimeter(unit, operand);

   if(numdel>0)
   {
      fprintf(fout, "\\sa%d ", numdel);
      sprintf(stylestring, "%s\\sa%d ", stylestring, numdel);
   }
}

int RSIlSpacing(basis, unit, operand, stylestring)
     char *basis, *unit, *operand, *stylestring;
/*
 *
 *   ConstantMargin
 *   InterlineSpacing
 *
 */
{
   int numdel;

   numdel = Delimeter(unit, operand);

   if(numdel>0)
   {
      fprintf(fout, "\\sl%d ", numdel);
      sprintf(stylestring, "%s\\sl%d ", stylestring, numdel);
   }
}

int RSFontFamily(basis, unit, operand, stylestring)
     char *basis, *unit, *operand, *stylestring;
{
   static struct FontStruct type[FTSIZE] = {
       {"Andy",		    NEW_YORK},
       {"AndySans",	    CHICAGO},
       {"AndyType",	    MONACO},
       {"AndySymbol",	    SYMBOL},
       {"Default",	    NEW_YORK}
   };
   int i;

   for(i=0; i<FTSIZE; i++)
      if(!strcmp(basis, type[i].name))
         break;

   if(i<FTSIZE)
   {
      fprintf(fout, "\\f%d ", type[i].num);
      sprintf(stylestring, "%s\\f%d ", stylestring, type[i].num);
   }
}

int RSFontSize(basis, unit, operand, stylestring)
     char *basis, *unit, *operand, *stylestring;
/*
 *
 *   PreviousFontSize
 *   ConstantFontSize
 *
 */
{
   int numdel;

   numdel = Delimeter(unit, operand);

   if(numdel>0 && !strcmp(basis, "ConstantFontSize"))
   {
      fprintf(fout, "\\fs%d ", numdel);
      sprintf(stylestring, "%s\\fs%d ", stylestring, numdel);
   }
}

int RSFontScript(basis, unit, operand, stylestring)
     char *basis, *unit, *operand, *stylestring;
/*
 *
 *   PreviousScriptMovement
 *   ConstantScriptMovement
 *
 */
{
   int numdel;

   numdel = Delimeter(unit, operand);

   if(numdel != -1 && !strcmp(basis, "ConstantScriptMovement"))
   {
      if(numdel>0)
      {
         fprintf(fout, "\\up%d ", numdel);
         sprintf(stylestring, "%s\\up%d ", stylestring, numdel);
      }
      else
      {
         fprintf(fout, "\\dn%d ", -numdel);
         sprintf(stylestring, "%s\\dn%d ", stylestring, -numdel);
      }
   }
}

int RSTabChange(basis, unit, operand, stylestring)
     char *basis, *unit, *operand, *stylestring;
{
   static struct style_words type[TSIZE] = {
       {"LeftAligned",	    "tx"},
       {"RightAligned",	    "tqr\\tx"},
       {"Centered",	    "tqc\\tx"}
   };
   int i;
   int numdel;

   numdel = Delimeter(unit, operand);
   for(i=0; i<TSIZE; i++)
      if(!strcmp(basis, type[i].ezword))
         break;

   if(i<TSIZE)
   {
      fprintf(fout, "\\%s%d ", type[i].rtfword, numdel);
      sprintf(stylestring, "%s\\%s%d ", stylestring, type[i].rtfword, numdel);
   }
}

int RSFontFace(basis, unit, operand, stylestring)
     char *basis, *unit, *operand, *stylestring;
{
   static struct style_words type[FSIZE] = {
       {"Plain",	"plain"},
       {"Bold",		"b"},
       {"Italic",	"i"}
   };
   int i;

   for(i=0; i<FSIZE; i++)
      if(!strcmp(basis, type[i].ezword))
         break;

   if(i<FSIZE)
   {
      fprintf(fout, "\\%s ", type[i].rtfword);
      sprintf(stylestring, "%s\\%s ", stylestring, type[i].rtfword);
   }
}

int RSJustify(basis, unit, operand, stylestring)
     char *basis, *unit, *operand, *stylestring;
{
   static struct style_words type[JSIZE] = {
       {"LeftJustified",		"ql"},
       {"RightJustified",		"qr"},
       {"LeftAndRightJustified",	"qj"},
       {"LeftThenRightJustified",	"qj"},
       {"Centered",			"qc"}
   };
   int i;

   for(i=0; i<JSIZE; i++)
      if(!strcmp(basis, type[i].ezword))
         break;

   if(i<JSIZE)
   {
      fprintf(fout, "\\%s ", type[i].rtfword);
      sprintf(stylestring, "%s\\%s ", stylestring, type[i].rtfword);
   }
}
