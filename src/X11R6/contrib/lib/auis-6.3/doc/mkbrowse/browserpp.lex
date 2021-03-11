	/* $Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/doc/mkbrowse/RCS/browserpp.lex,v 1.1 1993/10/13 20:18:33 gk5g Exp $ */
%{
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
#include <browserpp.h>
extern int nametype;	/* pass back info to class preprocessor */
extern void errorexit();	/* what to do if something bad happens */
%}
%%
class[^A-Z0-9a-z_]		return(class_TokUnput(class_Class));
package[^A-Z0-9a-z_]		return(class_TokUnput(class_Package));
returns[^A-Z0-9a-z_]		return(class_TokUnput(class_Returns));
onerror[^A-Z0-9a-z_]		return(class_TokUnput(class_OnError));
methods[^A-Z0-9a-z_]		return(class_TokUnput(class_Methods));
data[^A-Z0-9a-z_]			return(class_TokUnput(class_Data));
overrides[^A-Z0-9a-z_]		return(class_TokUnput(class_Overrides));
classprocedures[^A-Z0-9a-z_]	return(class_TokUnput(class_ClassProcedures));
macromethods[^A-Z0-9a-z_]	return(class_TokUnput(class_MacroMethods));
macrooverrides[^A-Z0-9a-z_]	return(class_TokUnput(class_MacroOverrides));
macros[^A-Z0-9a-z_]		return(class_TokUnput(class_Macros));
return[^A-Z0-9a-z_]		return(class_TokUnput(class_Return));

InitializeObject[^A-Z0-9a-z_]	return(class_TokName(name_InitializeObject));
FinalizeObject[^A-Z0-9a-z_]	return(class_TokName(name_FinalizeObject));
Allocate[^A-Z0-9a-z_]		return(class_TokName(name_Allocate));
Deallocate[^A-Z0-9a-z_]		return(class_TokName(name_Deallocate));
InitializeClass[^A-Z0-9a-z_]		return(class_TokName(name_InitializeClass));

self[^A-Z0-9a-z_]			return(class_TokName(name_Self));
thisobject[^A-Z0-9a-z]		return(class_TokName(name_ThisObject));

unsigned[^A-Z0-9a-z_]		return(class_TokName(name_Unsigned));
int[^A-Z0-9a-z_]			return(class_TokName(name_Int));
short[^A-Z0-9a-z_]		return(class_TokName(name_Int));
char[^A-Z0-9a-z_]			return(class_TokName(name_Int));
long[^A-Z0-9a-z_]			return(class_TokName(name_Long));
float[^A-Z0-9a-z_]			return(class_TokName(name_Float));
double[^A-Z0-9a-z_]		return(class_TokName(name_Float));
void[^A-Z0-9a-z_]			return(class_TokName(name_Void));
pointer[^A-Z0-9a-z_]		return(class_TokName(name_Pointer));
boolean[^A-Z0-9a-z_]		return(class_TokName(name_Boolean));
NULL[^A-Z0-9a-z_]		return(class_TokName(name_NULL));
-1[^0-9]				return(class_TokName(name_MinusOne));	/* wrong, but convenient */
exit[^A-Z0-9a-z_]			return(class_TokName(name_Exit));

\/\*				return(class_TokComment());
\"				return(class_TokString());

";"				return(class_Semi);
","				return(class_Comma);
":"				return(class_Colon);
"("				return(class_LeftParen);
")"				return(class_RightParen);
"{"				return(class_LeftBrace);
"}"				return(class_RightBrace);
"["				return(class_LeftSquareBracket);
"]"				return(class_RightSquareBracket);
\\\n				return(class_TokWhitespace());
[ \n\t]*				return(class_TokWhitespace());
[A-Za-z_][0-9A-Za-z_]*		return(nametype=name_RegularName,class_Name);

.				return(class_Other);
%%
#undef YYLMAX
#define YYLMAX 1000

class_TokUnput(tok)
short tok;
{
    unput(yytext[--yyleng]);
    yytext[yyleng]='\0';
    return tok;
}

class_TokName(namtok)
short namtok;
{
    nametype=namtok;
    return class_TokUnput(class_Name);
}

class_TokWhitespace()
{
    /* convert an escaped newline into a space */	
    if(yytext[0]=='\\'){
	yytext[0]=' ';
	yytext[1]='\0';
    }

    return (class_WhiteSpace);
}

class_TokComment(str)
char *str;  {
    /* must search to the end of the comment */
    /* attempt to return it in str - up to the point that fits */

    int i = 2, c, SawStar = 0;
    
    yytext[YYLMAX-3] = '*';
    yytext[YYLMAX-2] = '/';
    yytext[YYLMAX-1] = '\0';
    
    while ((c = input()) > 0)  {
	if (i < YYLMAX - 3) yytext[i] = c;
	i += 1;
	if (SawStar && c == '/') break;
	SawStar = (c == '*');
    }
    if (i < (YYLMAX - 3)) yytext[i] = '\0';
    return (class_Comment);
}

class_TokString(str)
char *str;  {
    int i = 1, c, SawBackSlash = 0;
    
    yytext[YYLMAX-2] = '"';
    yytext[YYLMAX-1] = '0';
    
    while ((c = input()) > 0)  {
	if (i < YYLMAX - 2) yytext[i] = c;
	i += 1;
	if (! SawBackSlash && c == '"') break;
	SawBackSlash = (c == '\\');
    }
    if (i < (YYLMAX - 2)) yytext[i] = '\0';
    if (i > (YYLMAX - 2))  {
	fprintf(stderr, "String Constant too long - truncated\n");
    }
    return (class_String);
}


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
    fclose(yyin);
    if (FilePtr < 1) {
	errorexit(EXITCODE_BUG, "PopFile stack is empty!");
    }
    FilePtr -=1;
    if (FilePtr > 0)  {
	yyin = FileStack[FilePtr-1];
    }
}


