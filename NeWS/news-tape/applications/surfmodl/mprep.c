/*main MPREP; */
/***ERROR: Comment too long (sorry) ***//* Macro Preprocessor and version control program to give C-like preprocessing
  to programs written in any language (or general text).
  The following commands can be embedded in any ASCII file
  (They must start with a '#' as the first character of a new line):
     #define identifier [string]
     #ifdef identifier
     #ifndef identifier
     #endif
  In addition, using the identifier in any text following its definition
  results in replacement of the identifier by the string that followed
  it in the #define statement. (Macros can contain references to previously-
  defined macros.) MPREP is invoked as:
     MPREP [-E ext] [-D identifier] filename [filename2] [filename3] ...
  where ext is an optional filename extension, which is used to name the
  output file (the default is PAS). For example, the command:
     MPREP MAIN.PRE
  would result in the creation of an output file named MAIN.PAS.
  Identifiers specified with the -D flag will define the identifier.
  Note that all identifiers that are defined on the command line and
  in the files are defined for all files listed. Also, it should be noted
  that all commands in MPREP are case-sensitive. Thus, an identifier specified
  in capital letters will not be confused with the same identifier in lower
  case. The only exception is the letters '-D' and '-E' on the command line,
  which may be specified in either upper or lower case.
     The basics of how MPREP interprets your preprocessing commands follows.
  (C programmers take note: MPREP works just like the corresponding commands
  in the C language, except that:
  (1) In mine, macro definitions that contain previously-defined macros are
  expanded; (2) I have not bothered to implement the optional designation
  of an identifier after #endif. It always goes with the last open #ifdef or
  #ifndef; (3) Variables in the #define statement are not recognized; and
  (4) I have not bothered with the #else statement. You should also note
  that the #define statement inside an #ifdef doesn't work reliably.)
   */

/* CONST */ 
#define MAXDEF 200         /* max # defines */

    typedef char nametype;
    typedef char stringtype;
    typedef char filetype;

 nametype Defname[MAXDEF+1][41];   /* defined names */
    stringtype Defstring[MAXDEF+1][71]; /* strings defined by Defname*/
    char Extension[5];    /* filename extension for output files */
    int Param;      /* cmmd-line parameter # */
    FILE * Filin;         /* input files */
    FILE * Filout;         /* output files */
    int Numdef;      /* # defined names */
    boolean Nextext;      /* flag that next param is an extension */
    boolean Nextdef;      /* flag that next param is a definition */
    char Line[256];  /* line inputted from file */
    int Lenline;      /* length of line */
    int Startstr;      /* start pos of define string */
    int Endstr;      /* end pos of define string */
    boolean Defined;      /* flag OK to echo lines to file */
    nametype Name[41];     /* name of define macro */
    int Def;      /* define # */
    boolean Found;      /* flag found define name */
    int Nif;      /* # open ifdef's or ifndef's */
    int Ifdef;      /* flag the ifdef that stopped echoing */
    int Fdpos;      /* position where macro name was found */
    boolean Fileread;      /* flag that a file was read */
    int Nextc;      /* next character after macro names */

void USAGE()
{
  /*LINE*/printf ("usage: PREP [-E ext] [-D defname] flnm [flnm2] [flnm3] ...\n");
  exit(1);
}; /* procedure USAGE */

void MAXEXCEED()
{
  /*LINE*/printf ("PREP: Maximum # of defines (",MAXDEF,") exceeded.");
  exit(1);
}; /* procedure MAXEXCEED */

boolean ISALPHANUM(c)
char c;
{
  if ( ((c>="0") && (c<="9")) || ((c>="A") && (c<="Z")) ||
     ((c>="a") && (c<="z")) )
    return TRUE;
  ; else
    return FALSE;
}; /* function ISALPHANUM */

void OPENFILIN(Filename,Filin)
filetype Filename;
/* VAR */FILE * Filin;
 filetype Flnm;
    boolean Fileopen;

{
  Fileopen = FALSE;
  Flnm = Filename;
  while ( (! Fileopen) ) {
    assign (Filin, Flnm);
    /*$I-*/
    reset (Filin);
    /*$I+*/
    if ( (errno != 0) ) {
      printf ("Input file ",Flnm," cannot be opened. Enter new filename: ");
      /*LINE*/scanf (Flnm);
      if ( (Flnm == "") )
        ;
    } ; else
      Fileopen = TRUE;
  }; /* while */
}; /* procedure OPENFILIN */

void OPENFILOUT(Filename,Filout)
filetype Filename;
/* VAR */FILE * Filout;
 filetype Flnm;
    boolean Fileopen;
    int Period;   /* location of period in file name */

{
  Fileopen = FALSE;
  Flnm = Filename;
  while ( (! Fileopen) ) {
    Period = pos (".", Flnm);
    if ( (Period > 0) )
      Flnm = copy (Flnm, 1, Period-1);
    /* Check to see if the user put a period in the filename extension */
    if ( (Extension[1] == ".") )
      Flnm = Flnm + Extension
    ; else
      Flnm = Flnm + "." + Extension;
    if ( (Flnm == Filename) ) {
      /*LINE*/printf ("MPREP: Input and output filenames (",Flnm,") are same.");
      exit(1);
    };
    assign (Filout, Flnm);
    /*$I-*/
    rewrite (Filout);
    /*$I+*/
    if ( (errno != 0) ) {
      printf ("Output file ",Flnm," cannot be opened. Enter new file name: ");
      /*LINE*/scanf (Flnm);
      if ( (Flnm == "") )
        ;
    } ; else
      Fileopen = TRUE;
  }; /* while */
}; /* procedure OPENFILOUT */

{ /* program PREP */
  /* Initializations */
  Fileread = FALSE;
  Extension = "PAS";          /* default is for Pascal file extension */
  Nextext = FALSE;
  Nextdef = FALSE;
  Numdef = 0;
  Defined = TRUE;
  Nif = 0;
  Ifdef = 0;

  for ( Param = 1 ; paramcount ) {
    if ( (Nextext) ) {
      /* Last parameter was -E, so this parameter is the filename extension. */
      if ( (length (paramstr (Param)) > 3) )
        usage;
      Extension = paramstr (Param);
      Nextext = FALSE;
    } ; else if ( (Nextdef) ) {
      /* Last parameter was -D, so this parameter is a definition. */
      Numdef = Numdef + 1;
      if ( (Numdef > MAXDEF) )
        maxexceed;
      Defname[Numdef] = paramstr (Param);
      Defstring[Numdef] = "";
      Nextdef = FALSE;
    } ; else if ( (copy (paramstr(Param),1,2) == "-e") ||
                (copy (paramstr(Param),1,2) == "-E") ) {
      /* -E flag found; next parameter should contain filename extension */
      Nextext = TRUE;
    } ; else if ( (copy (paramstr(Param),1,2) == "-d") ||
                (copy (paramstr(Param),1,2) == "-D") ) {
      /* -D flag found; next parameter should contain definition */
      Nextdef = TRUE;
    } ; else {
      /* Just a normal file name */
      openfilin (paramstr (Param), Filin);
      openfilout(paramstr (Param), Filout);
      Fileread = TRUE;

      /* Now read the file & process the define's */
      do {
        /*LINE*/scanf (Filin, Line);
        Lenline = length (Line);
        if ( (copy (Line,1,8) == "#define ") ) {
          /* Find the #define name */
          /* (Starts at first non-blank & non-tab.) */
          Startstr = 9;
          while ( (Startstr <= Lenline) && ((Line[Startstr] == " ") ||
                (Line[Startstr] == [0]I)) )
            Startstr = Startstr + 1;
          if ( (Startstr <= Lenline) ) {
            /* Name was found; define it */
            Numdef = Numdef + 1;
            if ( (Numdef > MAXDEF) )
              maxexceed;
            Endstr = Startstr + 1;
            while ( (Endstr <= Lenline) && (Line[Endstr] != " ") &&
                  (Line[Endstr] != [0]I) )
              Endstr = Endstr + 1;
            if ( (Endstr > Lenline) )
              Endstr = Lenline;
            Defname[Numdef] = copy (Line, Startstr, Endstr-Startstr);

            /* Now find the string defined, if it exists */
            Startstr = Endstr + 1;
            while ( (Startstr <= Lenline) && ((Line[Startstr] == " ") ||
                  (Line[Startstr] == [0]I)) )
              Startstr = Startstr + 1;
            if ( (Startstr <= Lenline) ) {
              /* Definition was found */
              Defstring[Numdef] = copy (Line, Startstr, Lenline);
              /* Check for any other macros within the definition */
              for ( Def = 1 ; Numdef-1 )
                if ( (Defstring[Def] != "") ) {
                  Fdpos = pos (Defname[Def], Defstring[Numdef]);
                  if ( (Fdpos > 0) ) {
                    Found = TRUE;
                    if ( (Fdpos > 1) )
                      if ( isalphanum (Defstring[Numdef][Fdpos-1]) )
                        Found = FALSE;
                    Nextc = Fdpos + length (Defname[Def]);
                    if ( (Found) && (Nextc <= length(Defstring[Numdef])) )
                      if ( isalphanum (Defstring[Numdef][Nextc]) )
                        Found = FALSE;
                    if ( (Found) )
                      Defstring[Numdef] = copy (Defstring[Numdef],1,Fdpos-1)
                        + Defstring[Def] +
                        copy (Defstring[Numdef],Fdpos+length(Defname[Def]),
                        length(Defstring[Numdef]));
                  }; /* if Fdpos */
                }; /* if Defstring[Def] */
              /* for Def */
            } ; else
              /* No definition */
              Defstring[Numdef] = "";
          }; /* if Startstr */
        } ; else if ( (copy (Line,1,7) == "#ifdef ") ) {
          Nif = Nif + 1;
          if ( (Defined) ) {
            Startstr = 8;
            while ( (Startstr <= Lenline) && ((Line[Startstr] == " ") ||
                  (Line[Startstr] == [0]I)) )
              Startstr = Startstr + 1;
            if ( (Startstr <= Lenline) ) {
              /* Name was found; find the end of it */
              Endstr = Startstr + 1;
              while ( (Endstr <= Lenline) && (Line[Endstr] != " ") &&
                    (Line[Endstr] != [0]I) )
                Endstr = Endstr + 1;
              if ( (Endstr > Lenline) )
                Endstr = Lenline;
              Name = copy (Line, Startstr, Endstr-Startstr+1);
              /* Now see if the name was defined */
              Found = FALSE;
              for ( Def = 1 ; Numdef )
                if ( (Defname[Def] == Name) )
                  Found = TRUE;
              if ( (Found) )
                Defined = TRUE
              ; else {
                Defined = FALSE;
                Ifdef = Nif;
              }; /* if Found */
            }; /* if Startstr */
          }; /* if Defined */
        } ; else if ( (copy (Line,1,8) == "#ifndef ") ) {
          Nif = Nif + 1;
          if ( (Defined) ) {
            Startstr = 9;
            while ( (Startstr <= Lenline) && ((Line[Startstr] == " ") ||
                  (Line[Startstr] == [0]I)) )
              Startstr = Startstr + 1;
            if ( (Startstr <= Lenline) ) {
              /* Name was found; find the end of it */
              Endstr = Startstr + 1;
              while ( (Endstr <= Lenline) && (Line[Endstr] != " ") &&
                    (Line[Endstr] != [0]I) )
                Endstr = Endstr + 1;
              if ( (Endstr > Lenline) )
                Endstr = Lenline;
              Name = copy (Line, Startstr, Endstr-Startstr+1);
              /* Now see if the name was defined */
              Found = FALSE;
              for ( Def = 1 ; Numdef )
                if ( (Defname[Def] == Name) )
                  Found = TRUE;
              if ( (Found) ) {
                Defined = FALSE;
                Ifdef = Nif;
              } ; else
                Defined = TRUE;
            }; /* if Startstr */
          }; /* if Defined */
        } ; else if ( (copy (Line,1,6) == "#endif") ) {
          if ( (Ifdef == Nif) ) {
            Defined = TRUE;
            Ifdef = 0;
          };
          Nif = Nif - 1;
        } ; else if ( (Defined) ) {
          /* No preprocessor directives; just a normal line */
          /* Check for any defined macros */
          Def = 1;
          Found = FALSE;
          while ( (Def <= Numdef) && (! Found) ) {
            if ( (Defstring[Def] != "") ) {
              Fdpos = pos (Defname[Def], Line);
              if ( (Fdpos > 0) ) {
                Found = TRUE;
                if ( (Fdpos > 1) )
                  if ( (isalphanum (Line[Fdpos-1])) )
                    Found = FALSE;
                Nextc = Fdpos + length(Defname[Def]);
                if ( (Found) && (Nextc <= Lenline) )
                  if ( (isalphanum (Line[Nextc])) )
                    Found = FALSE;
                Line = copy (Line,1,Fdpos-1) + Defstring[Def] +
                        copy (Line,Fdpos+length(Defname[Def]),Lenline);
              }; /* if Fdpos > 0 */
            }; /* if Defstring[Def] */
            Def = Def + 1;
          }; /* while Def... */
          /*LINE*/printf (Filout, Line);
        }; /* if copy(Line,1,8)... */

      } until ( eof (Filin);
      close (Filin);
      close (Filout);
    }; /* if Nextext... */
  }; /* for Param */
  if ( (! Fileread) )
    usage;
}. /* program PREP */

