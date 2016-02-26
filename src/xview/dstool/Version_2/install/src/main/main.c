/*  -------------------------------------------------------------------

This program is the property of:

                             Cornell University 
                        Center for Applied Mathematics 
                              Ithaca, NY 14853

and may be used, modified and distributed freely, subject to the 
following restrictions:

       Any product which incorporates source code from the dstool
       program or utilities, in whole or in part, is distributed
       with a copy of that source code, including this notice. You
       must give the recipients all the rights that you have with
       respect to the use of this software. Modifications of the
       software must carry prominent notices stating who changed
       the files and the date of any change.

DsTool is distributed in the hope that it will be useful, but 
WITHOUT ANY WARRANTY; without even the implied warranty of FITNESS 
FOR A PARTICULAR PURPOSE.  The software is provided as is without 
any obligation on the part of Cornell faculty, staff or students to 
assist in its use, correction, modification or enhancement.

  -----------------------------------------------------------------  */
/*
 * main.c
 *
 * Procedures:
 *   main()
 *   decode_cmd_line()
 *   stop_execution()
 *
 */

#include <stdio.h>

#include <portability.h>

#ifdef HAS_STRINGS_H
#include <strings.h>
#endif

#ifdef HAS_STRING_H
#include <string.h>
#endif


#include <defaults.h>
#include <version.h>
#include <constants.h>
#include <pm.h>


void
  main(argc, argv)
int argc;
char **argv;
{
  void 
    control_install_init(), decode_cmd_line(),
    sys_install(), user_install(),
    configure_dstool(),windows_install_init(),
    start_xv_loop(), start_tcl_loop(), 
    start_pvm_slave_loop();

  pm_init();
  control_install_init();
  decode_cmd_line(argc,argv);
  sys_install();
  user_install();

#ifdef USING_PVM

  if (*( (int *) pm(GET, "Control.Pvm_Host", NULL)))
    pm(EXEC, "Pvm.Start", NULL);

#endif

  configure_dstool();

  switch ( *( (int *) pm(GET, "Control.Mode", NULL)) ) 
    {
    case WINDOWS_MODE:
      windows_install_init();
      start_xv_loop(argc,argv);
      break;
    case TCL_MODE:
#ifdef USING_TCL
      start_tcl_loop();
#endif
      break;
    case PVM_SLAVE_MODE:
#ifdef USING_PVM
      start_pvm_slave_loop();
#endif
      break;
    default:
      /* ERROR */
      stop_execution("main: BAD control mode.");
    }

  if (*( (int *) pm(GET, "Control.Pvm_Host", NULL)))
    pm(EXEC, "Pvm.Finish", NULL);

  exit(0);
}


void
  decode_cmd_line(argc,argv)
int	argc;
char	**argv;
{
  int i;
  char *name;

  /* copy argc and argv into postmaster */
  pm(PUT, "Control.Argc", argc,
     INIT, "Control.Argv", argc, MAX_CMD_LINE_ARG_LEN,
     PUT_LIST, "Control.Argv", 0, argc-1, argv, NULL);

  /* find name of executable */

#ifdef HAS_STRRCHR
  name = ( (name = strrchr(argv[0],'/')) != (char *) NULL) ? name+1 : argv[0];
#endif

#ifdef HAS_RINDEX
  name = ( (name = rindex(argv[0],'/')) != (char *) NULL) ? name+1 : argv[0];
#endif


  pm(INIT, "Control.Program_Name", strlen(name)+1,
     PUT, "Control.Program_Name", name, 
     NULL);

  /* if program name contains nowin, then put in TCL_MODE */


  if ( strstr(name,"nowin") != (char *) NULL)


/*  if ( index(name,"nowin") != (char *) NULL)*/





    pm(PUT, "Control.Mode", TCL_MODE, NULL);

  /* if program name contains pvm, then put in PVM_SLAVE_MODE */

  if ( strstr(name,"nowin") != (char *) NULL)



/*  if ( index(name,"nowin") != (char *) NULL)*/


    pm(PUT, "Control.Mode", PVM_SLAVE_MODE, NULL);

  /* find out the mode from examining the command line and 
     look for control files */
  for(i=1; i<argc; i++)
    {
      
      if (!strcmp(argv[i],"-infile"))           /* INPUT FILE FLAG */
	{
	  if ( (++i == argc) || !check_file_to_read(argv[i]) )
	    {
	      fprintf(stderr,
		"dstool: configuration path or file cannot be read.\n");
	      stop_execution(USAGE_STR);
	    }
	  else
	    {
	      pm(INIT, "Control.Infile", strlen(argv[i])+1,
		 PUT, "Control.Infile", argv[i], NULL);
	    }
	}
 
      else if (!strcmp(argv[i],"-outfile"))      /* OUTPUT FILE FLAG */
	{
	  if ( (++i == argc) || !check_file_to_write(argv[i]) )
	    {
	      fprintf(stderr,
		"dstool: configuration path or file cannot be written.\n");
	      stop_execution(USAGE_STR);
	    }
	  else
	    {
	      pm(INIT, "Control.Outfile", strlen(argv[i])+1,
		 PUT, "Control.Outfile", argv[i], NULL);
	    }
	}
 
      else if (!strcmp(argv[i],"-windows"))     /* WINDOWS MODE FLAG */
	pm(PUT, "Control.Mode", WINDOWS_MODE, NULL);

      else if (!strcmp(argv[i],"-tcl"))             /* TCL MODE FLAG */
	pm(PUT, "Control.Mode", TCL_MODE, NULL);

      else if (!strcmp(argv[i],"-pvm"))             /* PVM HOST */
	pm(PUT, "Control.Pvm_Host", TRUE, NULL);

      else if (!strcmp(argv[i],"-debug"))              /* DEBUG FLAG */
	pm(PUT, "Control.Debug", TRUE, NULL);

      else if (!strcmp(argv[i],"-version"))         /* VERSIONS FLAG */
	stop_execution(VERSION);

      else if (!strcmp(argv[i],"-help"))         /* HELP FLAG */
	stop_execution(USAGE_STR);

    }
}



/*
 * stop_execution()
 * This procedure is called when an unrecoverable error is encountered
 *
 */
int
  stop_execution(s)
char *s;
{
	fprintf(stderr,"dstool: %s \n",s);
	fprintf(stderr,"dstool: exiting \n");
	exit(1);
}


