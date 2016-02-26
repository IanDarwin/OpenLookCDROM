/* Top Level Code: Group 7 readfile.c */

/* FILE readfile.c ************************************************************
 *
 * Description of Contents
 *
 *     This file contains the function readfile(). This function will be
 *   used to open the input file for the read and used to call the
 *   function yyparse.
 *   
 * Header files referenced
 *
 *     There are none within this file.
 *
 *  Author... Brian Gaubert
 *  Date .... 1992 Nov 2
 *
 * FILE readfile.c */

#include <stdio.h>
#include <string.h>
extern FILE *yyin;

/* FUNCTION readfile *********************************************************
*
*  Purpose:
*
*       The purpose of this function is to set up and open the input
*      file from the filename parameter that was given to it. This
*      function will also call the function yyparse. It will return a
*       value of 0 if the file was successfully opened and a 1 if the
*	file was not successfully opened.
*
*    Sample Call:
*
*        retval = readfile(filename);
*
*    INPUTS:
*
*     filename  -  This is the filename that is given to the routine
*                  from the user interface which intern will come from
*		  the user.
*
*    OUTPUTS:
*
*     This function will return a 0 if the file is successfully opened
*     and will return a 1 if there is an error in the proces of
*     openinmg the file
*
*    Processing narrative:
*    NOTE: This narrative came from the detailed design. I have also
*          included the relations that this function has with the
*	  functions yylex and yyparse, since those functions are made
*	  fron the tools Flex and Bison.
*
*
*      The tools Flex and Bison play an important part in reading a project
*     file The tool Flex will be used to generate code that scans a file and
*     looks for different tokens that are later assigned to the database
*      structure that is used in creating a task list. The Flex tool will
*      create tables that are used to look up the keywords that are used in
*      the file. The tool creates a file called lex.yy.c and will create a
*      function called yylex().  Bison, on the other hand, is used to create
*      a parser which in turn parses the file so that the database can get
*      the information that it needs to build the task list.  The set up of
*      the bison file correlates to the grammar that is used to read the
*      correct information from the file. Bison, like Flex, will use the
*      input file that parses the input. These files are called
*      parser.tab.c and parser.tab.h. The function that is created
*      from the Bison tool is called yyparse(). These functions use a file
*      pointer for the file to be read in called yyin. The function yyparse()
*      will call yylex(), the scanner. From here the scanner, yylex(), will
*      send the correct tokens to the parser from the input file.
*
*       The structure yylval will contain a token (taken from the scanner),
*      a string constant, and an integer (which will be used to insert into
*      the database). The string used in scanning the file will initially come
*      from the global variable created from the scanner called yytext. From
*      the scanner this variable will be placed into the stack yylval. Bison
*      will then use the information from the stack to update the database.
*      The file will be parsed in a bottom-up manner, so when each element of
*      a task is parsed, a local variable within the structure will be updated.
*       Then when all of the variables of that particular task have been 
*      parsed, a call will be made to insert the task into list, thus by
*      creating a list and a node that is appended to the list. If the task
*      has subtasks, a list will be created under that main task, and the the
*      subtasks will follow under it in a list of itself. This process will
*      take place for all of the task in the file until the file has been
*      totally read in. The grammar used in Bison will know when all of the
*      tasks for the file have been read in and the control will be issued
*      back to the read-file routine, which will end this routine. The
*      control will then return back to the user interface.
*
*       The function yyparse is created from the tool Bison. The files created
*      by Bison include the parser.tab.c and the parser.tab.h. These
*      Bison files are related directly to the files created by the Flex
*      tool. The purpose of using Bison is to allow the read from the file to
*      the user interface to be easy and easily changeable, thus creating a
*      file that can be made up from another source rather than the program
*      itself. The functionality of yyparse will involve parsing the file
*      according to the keywords used in yylex(). The parse will use a bottom
*      up approach which is done by Bison automatically. From the parsing
*      stage, Bison allows for code to be executed whenever it reaches a
*      certain point in the parse tree or rule in the grammar. Upon reaching
*      a single task in the parse of the file, the parser will make calls to
*      the database functions to create-main-task-list, create-task-node,
*      create-task-list, create-task-info, and add-task-to-end. The function
*      create-main-task-list will create the main task list from the first
*      task within the file. Other lists (subtask) and main task nodes will
*      added on to this main task node.  The function create-task-node will
*      create a node so the information can be stored within a node which
*      will make it easier in maintaining the list that is currently being
*      worked on. The function create-task-list will be used for creating any
*      other list , subtask or main task, for the database. The function
*      create-task-info will store all of the information of that task in the
*      node specified. Lastly, the function add-task-to-end will add the task
*      to the end of the list.  This process of adding a single task, along
*      with all of its subtask lists, dependencies, and resources will
*      involve a simple stack for that particular task. Once a task is
*      created it will be stored into the stack, then all of its subtasks
*      will be stored on top of the main task. Then, when the main task is
*      done being read from the file, contents of the stack will come off and
*      will be placed into the database.
*
*       The function yylex will involve the scanning part of the read function.
*      The scanner will scan through the file and look for specific keywords
*      and identifiers that will be used later in the parsing section of the
*      read. When an identifier is found, the scanner will return a token to
*      the caller of the function, yyparse. When a specific identifier is
*      found in the file that is a STRING or an INT the scanner will return a
*      token code and will assign it to the correct variable in the yylval
*      structure. The yylval structure will contain the token code, an long
*      integer variable and a character pointer. 
*
*      	 
*    PDL for the function:
*
*        PROCEDURE read-file EXTERNAL REFERENCE;
*        INTERFACE filename;
*          BEGIN
*            yyin = OPEN filename;
*           CALL yyparse PROCEDURE;
*            CLOSE yyin;
*          END
*
* FUNCTION readfile */ 
int readfile( char *filename )
{
  int i,j;

     i = 1;
     for(i=1;i<=strlen(filename)-1;i++)
        {
          yyin = fopen( filename, "r" );
          if( yyin == NULL )
             {
               /* Display the error message to the user */
	       return 1;
             }
          else
             {  
               j = yyparse();
	       if (j == 1)
	          {
		    /* Display parse error message */
		    return 1;
		  }
             }
             close(yyin);
             return 0;
        }
}


