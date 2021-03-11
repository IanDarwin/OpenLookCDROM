/*
 main.c of 

 2rtf: a facility to convert files in the ATK file format to files compatible with the RTF format.

 Scribetext is copyright (c) 1989, 1990 by the Massachusetts Institute of
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

 2rtf was written by Scott Rixner, rixner@ATHENA.MIT.EDU and Jeremy Paul Kirby, jpkirby@ATHENA.MIT.EDU

 $Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/mit/2rtf/RCS/main.c,v 1.5 1994/03/31 16:19:21 rr2b Exp $
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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/mit/2rtf/RCS/main.c,v 1.5 1994/03/31 16:19:21 rr2b Exp $";
#endif


#include <andrewos.h>
#include <stdio.h>
#include <sys/file.h>  /* access() */
#ifdef POSIX_ENV
#include <unistd.h>
#endif
#include <sys/errno.h>
#include "2rtf.h"
#include "input.h"

char *AndrewDir();

struct TableStruct *Table=NULL;
struct ValuesStruct *Values=NULL;
/* struct FileStackStruct *FileStack=NULL; */
struct IdStackStruct *IdStack=NULL;
struct StyleStackStruct *Style=NULL;

char *Filein, *Fileout;

main(argc, argv)
     int argc;
     char *argv[];
/*
 *
 *  Handle command line arguments, and get pointers to the
 *  appropriate files.  Pass control on to ParseMain,
 *  with the names of the input and output files as
 *  arguments.
 *
 */
{
  char filein[TMP_SIZE], fileout[TMP_SIZE], filetrans[TMP_SIZE], 
  fileerr[TMP_SIZE], *GetInstruction(), *getenv(), datestamp[TMP_SIZE];
  FILE *FileProcess();
  int ExecuteSpecial(), i, CommandErr=FALSE, OptionErr=FALSE;
  long int ParseText();
  void usage(), CloseFiles(), MakeTable(), TempPrintList(), ParseMain(),
  SetupEnvironment(), InitStateVector();

  /*  Get date and time into char *datestamp as quick as posssible */

  filein[0] = '\0';
  fileout[0] = '\0';
  filetrans[0] = '\0';
  fileerr[0] = '\0';

  InitStateVector();
  paragraph = 1;

  me = (char *) calloc ((strlen(argv[0]) + 1), sizeof(char));
  strcpy(me, argv[0]);
  for(i=1; i<argc; i++)
    {
      if(strcmp(argv[i], "-t") == 0)
	{
	  if(i+1 < argc && argv[i+1][0] != '-')
	    strcpy(filetrans, argv[++i]);
	  else
	    {
	      fprintf(stderr, "%s: Filename must follow -t option.\n", me);
	      OptionErr = TRUE;
	    }
	}
      else if(strcmp(argv[i], "-e") == 0)
	{
	  if(i+1 < argc && argv[i+1][0] != '-')
	    strcpy(fileerr, argv[++i]);
	  else
	    {
	      fprintf(stderr, "* Error:\n* Filename must follow -e option.\n", me);
	      OptionErr=TRUE;
	    }
	}
      else
	{
	  if(argv[i][0] != '-')
	    {
	      if(strcmp(filein, "") == 0)
		strcpy(filein, argv[i]);
	      else if(strcmp(fileout,  "") == 0)
		strcpy(fileout, argv[i]);
	      else
		{
		  if(CommandErr == FALSE)
		    {
		      CommandErr = TRUE;
		      fprintf(stderr, "%s: Too many parameters.\n", me);
		    }
		  fprintf(stderr, "Extraneous parameter '%s'.\n", argv[i]);
		}
	    }
	  else
	    {
	      OptionErr = TRUE;	      
	      fprintf(stderr, "%s: Invalid command option %s\n", me, argv[i]);
	    }
	}
    if(CommandErr == TRUE || OptionErr == TRUE)
      usage();
    }

  printf("\n2rtf Version %s\n\n", VERSION);

  ferr = FileProcess("Press ENTER and CALL THE BUG POLICE!!!", fileerr, "e");

  ftrans = FileProcess("Translation file?", filetrans, "t");

  fin = FileProcess("EZ file to convert?", filein, "r");

  if(!strcmp(fileout, ""))
    fout = FileProcess("Output file?", filein, "n");
  else
    fout = FileProcess("Output file?", fileout, "w");

  MakeTable();

  SetupEnvironment(Filein);

  ParseMain(Filein, Fileout);
}


FILE *FileProcess(prompt, filename, mode)
     char *prompt;
     char *filename;
     char *mode;
/*
 *
 *  Return a pointer to the file associated with filename,
 *  or ask for filename, if appropriate, using prompt, and
 *  return a pointer to that associated file.
 *  
 *  modes:
 * 
 *     e - error file
 *     t - trans file
 *     r - input file/all read files
 *     n - output file with unknown name
 *     w - output file with known name/all write files
 *
 *  If no trans file is specified, use 2rtf.trans,
 *  ultimately, "$ANDREWDIR/lib/2rtf.trans", but currently
 *  "./2rtf.trans".
 *
 *  If no output file is specified, create one using the
 *  input file - strip off ".ez" if it is there, and add
 *  ".rtf" regardless. (this is the 'n' mode case)
 *
 *  If no error file is specified, stderr is used.
 *
 *  If no input file has been specified, always use prompt
 *  and ask for one - this is the general case; if at this
 *  point filename is still "" the function uses prompt to
 *  ask for a filename.
 *
 */
{
  int accessible, readable, len;
  FILE *fpt;
  char *filename2, *fullspec, number[20], *getenv(),
  instruction[TMP_SIZE];

  if(!strcmp(filename, "") && !strcmp(mode, "t"))
  {
      fullspec = AndrewDir("/lib/2rtf.trans");
      accessible = access(fullspec, F_OK);
      if(accessible != 0)
      {
	  printf("* Unable to find standard translation file.\n\n");
	  strcpy(filename, "");
      }
      else
      {
	  filename = (char *) malloc((strlen(fullspec) + 1)*sizeof(char));
	  strcpy(filename, fullspec);
      }
  }

  if(!strcmp(mode, "n"))
    {
      if(rindex(filename, '.') != NULL)
	{
	  if(!ULstrcmp(rindex(filename, '.'), ".ez"))
	    {
	      len = roffset(filename, '.');
	      filename2 = (char *) malloc ((len + 8) * sizeof(char));
	      filename[len] = '\0';
	    }	 
	}
      else
	filename2 = (char *) malloc ((strlen(filename) + 8) * sizeof(char));

      filename2 = strcat(filename, ".rtf");
      filename = (char *) malloc((strlen(filename2) + 1) * sizeof(char));
      filename = filename2;

      accessible = access(filename, F_OK);
      if(!accessible)
	{
	  sprintf(instruction, "mv %s %s~", filename, filename);
	  system(instruction);
	}
    }

  if(!strcmp(mode , "t"))
    mode = "r";

  if(!strcmp(mode, "n"))
    mode = "w";

  if(!strcmp(filename, ""))
    {
      if(!strcmp(mode, "e"))
	  return(stderr);      
      printf("%s ", prompt);
      /* scanf("%s", filename); */
      input_read_up_to_whitespace(filename, TMP_SIZE, stdin);
    }
  
  accessible = access(filename, F_OK);
  
  if(!strcmp(mode, "r"))
    strcpy(number, "does not exist");
  else
    {
      accessible = !accessible;
      strcpy(number, "exists");
    }
  if(accessible != 0)
    {  
      fprintf(stderr, "* Error!\n* %s: File %s %s\n", me, filename, number);
      usage();
    }
  
  if(!strcmp(mode, "r"))
    {
      readable = access(filename, R_OK);
      if(readable)
	{
	  fprintf(stderr, "* Error!\n* %s: Unauthorized to read from %s.\n", me, filename);
	  usage();
	}
    }
  
  if(!strcmp(mode, "e"))
    mode = "w";

  if((fpt=fopen(filename, mode)) == NULL)
    {
      fprintf(stderr, "* Unknown error!\n* %s: Unrecognizable error opening %s for access.\n\n", 
	      me, filename);
      fprintf(stderr, "* %s.debug: Error %d\n", me, errno);
      usage();
    }

  if(!strcmp(mode, "r"))
    {
      Filein = (char *) malloc((strlen(filename) + 1) * sizeof(char));
      strcpy(Filein, filename);
    }
  else if(!strcmp(mode, "w"))
    {
      Fileout = (char *) malloc((strlen(filename) + 1) * sizeof(char));
      strcpy(Fileout, filename);
    }

  return(fpt);
}


void MakeTable()
/*
 *
 *  Create internal translation table by reading in ftrans.
 *
 */
{
  struct TableStruct *tmp;
  char ch, *ezword, rtfword[TMP_SIZE], *makelower();  
  extern FP AssignFunc();
  int in, len, rv = 0, dsv=0;

  ezword = (char *) calloc(TMP_SIZE, sizeof(char));

  printf("* Creating internal translation table...");
  while(1)
    {
      in = fgetc(ftrans);
      if(in==EOF)
	break;
      ch = (char) in;
      if(ch == '#' || ch == '\n' || ch == '\r' || ch == ' ' || ch == '\t')
	{
	  while(ch != '\n' && ch != '\r')
	    ch = (char) fgetc(ftrans);
	  continue;
	}
      else
	ungetc(ch, ftrans);

      /* if(fscanf(ftrans, "%s%s", ezword, rtfword)==EOF)
	break; */
      if (input_read_up_to_whitespace(ezword, TMP_SIZE, ftrans) == EOF)
	  break;
      input_skip_whitespace(ftrans);
      if (input_read_up_to_whitespace(rtfword, TMP_SIZE, ftrans) == EOF)
	  break;

      ezword = makelower(ezword);

      if(rtfword[0]=='~')
	{
	  len = strlen(ezword) + 1;

          if(!strcmp(rtfword, "~RTFVersion"))
            {
              RTFVersion = atoi(ezword);
              rv = 1;
	    }
	  else if(!strcmp(rtfword, "~TextDSVersion"))
	    {
	      TextDSVersion = atoi(ezword);
	      dsv = 1;
	    }
	  else
	    {
	      fprintf(stderr, "\n* Error!\n* %s: %s\n%s%s%s%s\n", me,
		      "Corrupt translation table:  Presence of invalid environment entry:",
		      "Corrupt entry:  \"", ezword, "\" --> ", rtfword);
	    }
	}
      else
	{
	  tmp = (struct TableStruct *) malloc (sizeof(struct TableStruct));
	  tmp->ezword = (char *) calloc (strlen(ezword) + 1, 
					     sizeof(char));
	  strcpy(tmp->ezword, ezword);
	  tmp->mode = 0;

	  if( (rtfword[0]=='@') || (rtfword[0]=='!') || (rtfword[0]=='#') || (rtfword[0]=='^'))
	    {
	      switch ((char) rtfword[0])
		{
		case '@':
		  tmp->mode=COMMAND | EZCOMMAND;
		  break;
		case '!':
		  tmp->mode=COMMAND | RTFCOMMAND;
		  break;
      		case '#':
		  tmp->mode=COMMAND | EZCOMMAND | NAKED;
		  break;
		case '^':
		  tmp->mode=QUOTEDCHAR;
		}
	      if(rtfword[0]=='^')
		tmp->rtf.quote = (char) atoi(&rtfword[1]);
	      else
		tmp->rtf.fun = AssignFunc(&rtfword[1]);
	    }
	  else
	    {
	      tmp->mode = WORD;
	      tmp->rtf.word = (char *) calloc(strlen(rtfword) + 1, sizeof(char));
	      strcpy(tmp->rtf.word, rtfword);
	    }
	  if(Table == NULL)
	    {
	      Table = tmp;
	      Table->next = NULL;
	    }
	  else
	    {
	      tmp->next = Table;
	      Table = tmp;
	    }
	}
    }

  if(!rv || !dsv)
    {
      fprintf(stderr, "\n* Error!\n* %s: %s\n\t%s %s\n%s", me, 
	      "Fatal error in translation table:  environment entry:",
	      !rv ? "~RTFVersion" : "",
	      !dsv ? "~TextDSVersion" : "", 
	      "weren't found and are necessary for operation.");
      exit(0);    
    }
  fclose(ftrans);

  printf("done\n");
}


void SetupEnvironment(rootfile)
     char *rootfile;
/*
 *
 *  This adds a bunch of values.  I'm not sure why, because
 *  they never get used.
 *
 */
{
  void AddValue();
  char *username, *wd, *trimroot, *fullman;

  /*  Go through all predefined string fields and define them
      and add them to the transtable   */

  /*  Take care of device, devicename, filedate, fullmanuscript,
      genericdevice, manuscript, rootfiledate, site, sitename, time,
      timestamp, username, scribeversion and scribetextversion */

  wd = (char *) malloc(1024 * sizeof(char));
  trimroot = (char *) malloc(1024 * sizeof(char));
  fullman = (char *) malloc(1024 * sizeof(char));

  AddValue("device", "PostScript");
  AddValue("devicename",  "PostScript Page Description Language");
  AddValue("genericdevice",  "ScaleableLaser");
  AddValue("scribeversion", VERSION);
  AddValue("scribetextversion", VERSION);
  AddValue("site", "MIT");
  AddValue("sitename", "Project Athena");

  /* and now for filedate, fullmanuscript, manuscript, rootfiledate,
     time, timestamp, and username */

  username=getlogin();
  wd = getwd(wd);

  if(rootfile[0]=='/')
    fullman = rootfile;
  else
    fullman = strcat(wd, rootfile);

  trimroot = &rootfile[roffset(rootfile, '/')];

  AddValue("username", username);
  AddValue("fullmanuscript", fullman);
  AddValue("manuscript", trimroot);

}

void AddValue(name, value)
     char *name, *value;
/*
 *
 *  This adds a value to Values.
 *
 */
{
  struct ValuesStruct *tmp;

  tmp = (struct ValuesStruct *) malloc(sizeof(struct ValuesStruct));

  tmp->name = (char *) calloc(strlen(name) + 1, sizeof(char));
  tmp->name = name;
  if (value == NULL) 
    {
      tmp->value = (char *) malloc (sizeof(char));
      tmp->value[0] = '\0';
    }
  else
    {
      tmp->value = (char *) calloc(strlen(value) + 1, sizeof(char));
      tmp->value = value;
    }

  if(Values == NULL)
    {
      Values = tmp;
      Values->next = NULL;
    }
  else
    {
      tmp->next = Values;
      Values = tmp;
    }
}

void InitStateVector()
/*
 *
 *  This procedure initializes all the parts of the
 *  state vector to their standard values.
 *
 */
{
    State.CurLeftMargin = 0;
    State.CurRightMargin = 0;
    State.CurRightEdge = 1140;
    State.CurLeftEdge = 1140;
    State.CurTopMargin = 0;
    State.CurBottomMargin = 0;
    State.CurScriptMovement = 0;
    State.CurFontSize = 24;
    State.CurLeftIndentation = 0;
    State.CurRightIndentation = 0;
    State.CurJustification = style_LeftJustified;
    State.CurFontFamily = "andy";
}
