%{
#include <browserpp.h>
extern int nametype;	/* pass back info to class preprocessor */
extern void errorexit();	/* what to do if something bad happens */
#undef YY_INPUT
#define YY_INPUT(buf,result,max_size) \
{ \
     int c = getc(yyin); \
     result = (c == EOF) ? YY_NULL : (buf[0] = c, 1); \
}
%}
%x COMMENT
%x STRING
%%
class[^A-Z0-9a-z_]		yyless(yyleng-1);return(class_Class);
package[^A-Z0-9a-z_]		yyless(yyleng-1);return(class_Package);
returns[^A-Z0-9a-z_]		yyless(yyleng-1);return(class_Returns);
onerror[^A-Z0-9a-z_]		yyless(yyleng-1);return(class_OnError);
methods[^A-Z0-9a-z_]		yyless(yyleng-1);return(class_Methods);
data[^A-Z0-9a-z_]		yyless(yyleng-1);return(class_Data);
overrides[^A-Z0-9a-z_]		yyless(yyleng-1);return(class_Overrides);
classprocedures[^A-Z0-9a-z_]	yyless(yyleng-1);return(class_ClassProcedures);
macromethods[^A-Z0-9a-z_]	yyless(yyleng-1);return(class_MacroMethods);
macrooverrides[^A-Z0-9a-z_]	yyless(yyleng-1);return(class_MacroOverrides);
macros[^A-Z0-9a-z_]		yyless(yyleng-1);return(class_Macros);
return[^A-Z0-9a-z_]		yyless(yyleng-1);return(class_Return);

InitializeObject[^A-Z0-9a-z_]	yyless(yyleng-1);return(nametype=name_InitializeObject,class_Name);
FinalizeObject[^A-Z0-9a-z_]	yyless(yyleng-1);return(nametype=name_FinalizeObject,class_Name);
Allocate[^A-Z0-9a-z_]		yyless(yyleng-1);return(nametype=name_Allocate,class_Name);
Deallocate[^A-Z0-9a-z_]		yyless(yyleng-1);return(nametype=name_Deallocate,class_Name);
InitializeClass[^A-Z0-9a-z_]	yyless(yyleng-1);return(nametype=name_InitializeClass,class_Name);

self[^A-Z0-9a-z_]		yyless(yyleng-1);return(nametype=name_Self,class_Name);
thisobject[^A-Z0-9a-z]		yyless(yyleng-1);return(nametype=name_ThisObject,class_Name);

unsigned[^A-Z0-9a-z_]		yyless(yyleng-1);return(nametype=name_Unsigned,class_Name);
int[^A-Z0-9a-z_]		yyless(yyleng-1);return(nametype=name_Int,class_Name);
short[^A-Z0-9a-z_]		yyless(yyleng-1);return(nametype=name_Int,class_Name);
char[^A-Z0-9a-z_]		yyless(yyleng-1);return(nametype=name_Int,class_Name);
long[^A-Z0-9a-z_]		yyless(yyleng-1);return(nametype=name_Long,class_Name);
float[^A-Z0-9a-z_]		yyless(yyleng-1);return(nametype=name_Float,class_Name);
double[^A-Z0-9a-z_]		yyless(yyleng-1);return(nametype=name_Float,class_Name);
void[^A-Z0-9a-z_]		yyless(yyleng-1);return(nametype=name_Void,class_Name);
pointer[^A-Z0-9a-z_]		yyless(yyleng-1);return(nametype=name_Pointer,class_Name);
boolean[^A-Z0-9a-z_]		yyless(yyleng-1);return(nametype=name_Boolean,class_Name);
NULL[^A-Z0-9a-z_]		yyless(yyleng-1);return(nametype=name_NULL,class_Name);
-1[^0-9]			yyless(yyleng-1);return(nametype=name_MinusOne,class_Name);	/* wrong, but convenient */
exit[^A-Z0-9a-z_]		yyless(yyleng-1);return(nametype=name_Exit,class_Name);

"/*"                            {
                                  BEGIN(COMMENT);
                                  yymore();
                                }
<COMMENT>[^*]*                  yymore();
<COMMENT>"*"+[^*/]*             yymore();
<COMMENT>"*"+"/"                return(BEGIN(INITIAL),class_Comment);

\"                              {
                                  BEGIN(STRING);
                                  yymore();
                                }
<STRING>.*\"                    return(BEGIN(INITIAL),class_String);

";"				return(class_Semi);
","				return(class_Comma);
":"				return(class_Colon);
"("				return(class_LeftParen);
")"				return(class_RightParen);
"{"				return(class_LeftBrace);
"}"				return(class_RightBrace);
"["				return(class_LeftSquareBracket);
"]"				return(class_RightSquareBracket);
\\\n				/* blow this off */
[ \n\t]*			return(class_WhiteSpace);
[A-Za-z_][0-9A-Za-z_]*		return(nametype=name_RegularName,class_Name);

.				return(class_Other);
%%
#ifndef NORCSID
static char rcsid[] = "$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/doc/mkbrowse/RCS/browserpp.flex,v 1.1 1993/10/13 20:18:33 gk5g Exp $";
#endif

static int FilePtr = 0;
static FILE *FileStack[FILESTACKSIZE];

void PushFile(file)
FILE *file;  {
    if (FilePtr >= FILESTACKSIZE) {
	errorexit(EXITCODE_BUG, "PushFile stack is full!");
    }
    FileStack[FilePtr] = file;
    FilePtr += 1;
    yyin =file;
}

void PopFile()  {
    if (FilePtr < 1) {
	errorexit(EXITCODE_BUG, "PopFile stack is empty!");
    }
    FilePtr -=1;
    if (FilePtr > 0)  {
	yyin = FileStack[FilePtr-1];
    }
    yyrestart(yyin);
}


/* ********************************************************************** *\
 *         Copyright IBM Corporation 1988,1991 - All Rights Reserved      *
 *        For full copyright information see:'andrew/config/COPYRITE'     *
\* ********************************************************************** */
/*
	$Disclaimer: 
# Permission to use, copy, modify, and distribute this software and its 
# documentation for any purpose is hereby granted without fee, 
# provided that the above copyright notice appear in all copies and that 
# both that copyright notice, this permission notice, and the following 
# disclaimer appear in supporting documentation, and that the names of 
# IBM, Carnegie Mellon University, and other copyright holders, not be 
# used in advertising or publicity pertaining to distribution of the software 
# without specific, written prior permission.
# 
# IBM, CARNEGIE MELLON UNIVERSITY, AND THE OTHER COPYRIGHT HOLDERS 
# DISCLAIM ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING 
# ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS.  IN NO EVENT 
# SHALL IBM, CARNEGIE MELLON UNIVERSITY, OR ANY OTHER COPYRIGHT HOLDER 
# BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY 
# DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, 
# WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS 
# ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE 
# OF THIS SOFTWARE.
#  $
*/
