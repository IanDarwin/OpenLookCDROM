#include "help.h"
#define MAXMATCHES	24

 /* ************************************************************
  * 			H   E   L   P    !!
  *		     =========================
  *
  * 	This program emulates the VMS help facility in the UNIX
  *  environment.  The main routine HELP1 looks up help and 
  *  subtopics for help.  The help texts for various topix 
  *  are tree-structured.  A directory is defined as a "help"
  *  directory, it has four types of files in it:
  *		main help text: 	.HLP
  *		manual page name:	.MANUAL
  *		subtopic texts:		<topicname>.HLP
  *		subtopic directories:	<topicname>
  *             subtopic cross-refs:    <topicname>.XREF
  *
  *	Subtopic names must start with an alphanumeric
  *  character.  Preferably all subtopics will start with
  *  lowercase letters.
  *
  *	The routine help1 is recursive, it descends the	tree
  *  structure.
  */

  main(argc, argv)
  	int argc;
	char *argv[];
  {
	int i,j,k;
	int longlen;
	char yesno[10];
	char *opts;
		
	strcpy(progname,argv[0]);
	helpdir = NULL;
	getwd(olddir);
	dumb_flag = 0;
	list_flag = 0;
	frst_flag = 1;
	col_flag  = 1;

	argc--;
	if (argc != 0 && *(*++argv) == '-') {	/* must be options */
		for (opts = *argv; *opts != '\0'; opts++) {
			switch(*opts) {
			   case '-' :	
			   	break;

			   case 'd' :	
			   	if (argc > 1) {
					helpdir = *++argv;
					argc--;
				}
				break;

			   case 'l' :
			   	list_flag = 1;
				col_flag = 0;
				break;

			   case 'q' :
			   	dumb_flag = 1;
				break;

			   case 'C' :
			   	col_flag = 1;
				break;

			   case 'n' :
			   	col_flag = 0;

			   default:
			        fprintf(stderr,"%s: %c: bad option.\n",
					progname,*opts);
				break;
			}
		}
		argc--; argv++;
	}

	if (helpdir == NULL) helpdir = HELPDIR;

	if (chdir(helpdir) < 0) {
		fprintf(stderr,"%s: %s: help directory not found.\n",
			progname,helpdir);
		exit(1);
	}

#ifdef BSD
	init_funky();
#endif
	if (argc >= 1) {		/* treat vector as a help path */
#ifdef DEBUG
	    fprintf(stderr," help path vector, argc=%d, *argv=%s.\n",
		    argc,*argv);
#endif
	    help1( "", argv, 0, 0, 0);

	}
	else	help1( "", NULL, 0, 0, 0);

	exit(0);
 }




 /* **************************************************************
  * printhelp: given a string, pop .HLP on the end and do more(1).
  *
  *	This routine sends a help file to more(1).  A string
  *  is passed in, which is the name of a help.  If the string
  *  is nil, then just use the name HLP.
  *	The return value is -1 if no help file is accessible, 
  *  0 if the more(1) command was called okay with fkoff();
  */

  printhelp(hs, path)
  	char *hs;
  {
	char filename[MAXNAMELEN], comm[MAXNAMELEN + 20];

	if (hs == NULL) strcpy(filename,HELPEX);
	else if (strlen(hs) < 1) strcpy(filename, HELPEX);
	else {
		strcpy(filename, hs);
		strcat(filename,HELPEX);
	}

	if ( access(filename, R_OK) < 0 ) {
		printf("\n Sorry, no help text for %s.\n",
			(hs==NULL)?"this topic":hs );
		return(-1);
	}

	if (path != NULL) printf("\n HELP: %s\n",path);

	fkoff(VIEWPROGRAM,VIEWPROGOPTS1,VIEWPROGOPTS2,filename,NULL);

	return(0);
  }



/* *************************************************************
 * printtopix: print the topics available in this directory
 *		in a nice format.
 *
 *	This routine does a directory of help options, 
 *  and prints them out in a manner similar to that of ls(1).
 *  All filenames which start with anything other than numbers 
 *  or letters are not kept.  Extensions are stripped off.
 *	
 *	The number of subtopics found is returned, along with
 *  a pointer to a null-terminated vector of string pointers.
 *  If the mode is non-zero, it means just use the strings stored
 *  in topix.  If mode==0, then re-allocate space and re-check
 *  the directory.  If mode
 */

 printtopix( topix, mode, supress)
 	char *topix[];
	int mode, supress;
{
	int i,j,k,l,xrefs = 0;
	int  namewidth, totalnames;
	char *malloc(), *nbuf;
	char *thisname, *index();
	char *s1, *s2, **nxtname, *nxcnt;
	struct direct *readdir(), *filedat;
	DIR *dirp, *opendir();

	if ( mode ) {
		for(nxtname=topix, i=0; *nxtname++ != NULL; i++ );
		totalnames = i;
		goto inputtopix;
	}

	/* if mode is zero, then allocate space and search the directory */
	nbuf = malloc( MAXNAMES * MAXNAMELEN );
	nxcnt=nbuf;
	dirp = opendir(".");
	if (dirp == NULL) {
		fprintf(stderr,"%s: Cannot open help directory.\n",progname);
		return(-1);
	}

	for(nxtname=topix, i=0; i < (MAXNAMES-1) ; ) {
		filedat = readdir(dirp);
		if (filedat == NULL) break;
		thisname = filedat->d_name;
		if ( !(isalnum(*thisname)) )    /* if not in [0-9A-Za-z] */
				continue;	/* do the next one.      */
		*nxcnt = '\0';
		if ( (s1 = index(thisname,'.')) != NULL) {
			if (strcmp(s1, HELPEX) == 0) *s1 = '\0';
			else if ( strcmp(s1, MANEX) == 0) continue;
			else if ( strcmp(s1, XREFEX) == 0) {
			    /* mark xref with at sign in first char */
			    strcpy(nxcnt,"@");
			}
		}
		if (strlen(thisname) >= MAXNAMELEN - 1)
			*(thisname+MAXNAMELEN-1) = '\0';
		/* copy in data from this loop */
		if (*nxcnt == '\0') strcpy(nxcnt,thisname);
		else strcat(nxcnt,thisname);
		*nxtname++ = nxcnt; 
		/* update pointers for next loop */
		nxcnt += strlen(thisname) + ((*nxcnt=='@')?(2):(1));
		i++;
	}
	*nxtname++ = NULL;
	totalnames = i;
	closedir(dirp);

	if (totalnames == 0) return(0);
	
	/* sort the names in ascending order with exchange algorithm */
	for(i=0; i < totalnames-1; i++)
		for(j=i+1; j <totalnames; j++)
			if (strcmp(topix[i],topix[j]) > 0) {
				thisname = topix[i];
				topix[i] = topix[j];
				topix[j] = thisname;
			}
	    

 inputtopix:
	if (supress) return(totalnames);
	else {
	    if (col_flag) printf("\n Subtopics:\n");
	    printlist(topix, totalnames);
	    return(totalnames);
	}

}



 /* ****************************************************************
  * help1: descend recursive help tree and provide some
  *	     user services.
  *
  *	This routine is the heart of the new UNIX help facility.
  *  It climbs recursively around an n-tree of documentation files
  *  and directories.  The routine printhelp() outputs a file of
  *  help text, the routine printtopix prints out all subtopics.
  *
  *	This routine can operate in interactive mode, or not.  The
  *  basic cycle of operation is:
  *		if (not list-only-mode) print help.
  *		if (not quiet-mode)	print list.
  *		if (not interactive)	return.
  *		print prompt and do commands.
  *		return.
  *
  *	There are a number of commands available in the
  *  interactive mode, they are:
  *
  *		blank line:	up recursive level.
  *		subtopic name:	recurse to subtopic.
  *		?:		list topics again.
  *		*:		get list of commands
  *		$:		get info on topix
  *		^D:		quit help program.
  *		.:		man page (if any).
  *		#		list topics again
  *		
  */

  help1(ppt,svec,skip,xref,comp_flag)
  	char *ppt;
	char *svec[];
	short  skip;
	short  xref;       /* non-zero if doing xref */
	short  comp_flag;  /* non-zero if doing completions */
{                             /* this routine is too big, eh? */
	int i,j,k, err;
	int no_subs;
	char answer[MAXLINELEN];
	char fullpath[MAXNAMELEN + 29];
	char yesno[10];
	char *topix[MAXNAMES], *mx[MAXMATCHES];
	int  matchv();
	char *index(), *getenv();
	char *s1, *s2, *s3, *rest;
	char *wvec[MAXMATCHES];
	char cmdbuf[90];


	if (comp_flag) {
	    printtopix( topix, 0, 1);
	    if ( (k = matchv( *svec, topix, mx)) == 0) return(-1);
	    /* if there is more than one match, and we are not at end, flub */
	    if ( svec[1] != NULL  &&  k > 1 ) return(-1);
	    /* if we are not at end, go down one level */
	    if ( svec[1] != NULL ) {
		if ( chdir( mx[0] ) < 0) return(-1);
		else {
		    k = help1(ppt, svec+1, 0, 0, comp_flag);
		    chdir("..");
		    return(k);
		}
	    }
	    /* if we are at end, then do appropriate action on comp_flag */
	    else {
		strcpy(gbl_match, mx[0]);
		if ( comp_flag == CTRLD_COMP ) {
		    fputs("\n",stdout);
		    printlist(mx,k);
		}
	    }
	    return(0);
	}

	if ( svec != NULL  && *svec != NULL) {
	    printtopix( topix, 0, 1);
	    if ( (k = matchv(*svec, topix, mx)) == 0) {
		printf(" sorry, no direct help path to %s %s\n",ppt,*svec);
		if ( (k = xref_matchv(*svec, topix, mx)) == 0) {
		    printf("    no cross-references, either.\n\n");
		    if (strlen(ppt) == 0) help1(ppt,NULL,1,0, 0);
		    return(-1);
		}
		else {
		    do_xref(ppt,*svec,mx);
		    if (strlen(ppt) == 0) help1(ppt,NULL,1,0, 0);
		    return(-1);
		}
	    }

	    for(i = 0, j = 0, err = -1; mx[i] != NULL; ) {
		if (i > 0)
		  j = takeit(" Next help path: %s %s\nTry it?",ppt,mx[i]);
		if (j == 1) { i++; continue; }
		if (j < 0)  break;
		strcpy(fullpath,ppt);
		strcat(fullpath," ");
		strcat(fullpath,mx[i]);

		if ( chdir(mx[i]) < 0) {
		    if ( *(svec + 1) != NULL ) {
			i++;
			continue;
		    }
		    if (list_flag) 
			return(-1);
		    else
			printhelp( mx[i],fullpath);
		    printf("\n");
		    if (!dumb_flag) help1(ppt,NULL,1,0, 0);
		    err = 0;
		}
		else {
		    if ( help1(fullpath,(svec+1),0,0, 0) >= 0) {
			err = 0;
			chdir("..");
			printf("\n");
			if (!dumb_flag) help1(ppt,NULL,1,xref, 0);
		    }
		    else chdir("..");
		}
		i++;
	    }
	    return(err);
	}

	if ( !list_flag && !skip) 
	    if (printhelp(NULL, ppt) < 0) {
		return(-1);
	    }

	if ( !list_flag && !dumb_flag && ( access(MANEX, R_OK) >= 0) && !skip)
		printf("\n	Manual page IS available.\n");

	if ( !dumb_flag ) {
	    if ( printtopix( topix, 0, skip) <= 0 ) {
		no_subs = 1;
	    }
	    else no_subs= 0;
	}

	if ( dumb_flag  ||  list_flag ) return(0);

	while (xref == 0) {
		if (frst_flag) {
			printf(" (type '*' for commands)\n");
			frst_flag = 0;
		}


		/* now, prompt and wait for user response */

		if ( strlen(ppt) < 1 ) sprintf(gbl_ppt," Help %s",PROMPT);
		else		 sprintf(gbl_ppt,"  %s %s",ppt,SUBPROMPT);
		fputs(gbl_ppt,stdout);
#ifndef BSD
		if ( fgets(answer, MAXLINELEN-1, stdin) == NULL ) {
			printf("\nbye...\n");
			exit(1);
		}
#else
		if (read_gets(stdin->_file, answer, MAXLINELEN - 1, 1) < 0) {
		        printf("\nbye...\n");
			exit(1);
		}
#endif

		/* first remove any leading blanks or tabs */
		for(s1=answer; *s1 == ' ' || *s1 == '	'; s1++);

		/* chop off all of answer after first word. */
		s2 = index(s1,' ');
		if (s2 == NULL)  s2 = index(s1,'	');
		if (s2 == NULL)  s2 = index(s1,'\n');
		if (s2)
		  { *s2 = '\0'; rest = ++s2; }
		else {
		    rest = s2 = s1 + strlen(s1);
		}
		makewvec(rest,wvec,MAXMATCHES);

		if ( strlen(s1) == 0 )		/*  on blank line, */
			break;			/* pop up one level*/

		switch (*s1) {
	    	    case '?':			/* ?: print stuff again */
		    	printhelp(NULL, ppt);
			if ( access(MANEX, R_OK) >= 0 )
			    printf("\n	Manual page IS available.\n");
	    		if (!no_subs) printtopix(topix, 1,0);
			else	printf("\n Sorry, no subtopics.\n\n");
			break;

		    case '#':
			if (!no_subs) printtopix( topix, 1,0);
			else    printf("\n Sorry, no subtopics.\n\n");
			break;

		    case '*':			/* *: list commands */
			printf("\n");
		        for(i=0; helpcmds[i] != NULL; i++) {
				printf("	%s\n",helpcmds[i]);
			}
			printf("\n");
			break;

		    case '$':			/* $: find out about files */
		    	s2 = s1 + 1;
			if (no_subs) {
				printf("\n Sorry, no subtopics.\n\n");
		    		break;
			}
			strcpy(cmdbuf, INFOHEAD);
			strcat(cmdbuf,s2);
			strcat(cmdbuf,"* ");
			strcat(cmdbuf, INFOTAIL);
			printf("\n File information: \n");
			system(cmdbuf);
			printf("\n");
			break;
		        

	    	    case '.':			/* .: do manpage if any */
		    	s2 = s1 + 1;
			if (no_subs) {
				printf("\n Sorry, no subtopics.\n\n");
		    		break;
			}
			if ( *s2 == '\0' ) {
				mx[0] = "";  mx[1] = NULL; k = 1;
			}
			else if ( (k = matchv( s2, topix, mx)) == 0 ) {
			   printf("\n Sorry, no topics match %s\n",s2);
			   printf(" (list cmnds with '*', topix with '#')\n");
			}
			for( i=0; i < k; i++ ) {
				strcpy(cmdbuf,mx[i]);
				strcat(cmdbuf,MANEX);
				if (access(cmdbuf, R_OK|X_OK ) < 0) {
				        strcpy(cmdbuf,mx[i]);
					strcat(cmdbuf,MAN_SUBEX);
					if (access(cmdbuf,R_OK|X_OK) < 0) {
					    printf("\n Sorry, %s for %s.\n\n",
					      "No manual reference available",
					      (strlen(s2)==0)?ppt:mx[i]);
					    continue;
					}
				}
				if (i > 0) {
 				     k=
				     takeit(" Next man page: %s.\n Take it? ",
					mx[i]);
				     if (k ==  1) continue;
				     if (k == -1) break;
				}
				printf(" ...doing %s\n\n",cmdbuf);
				fkoff(SHELLPROG,SHELLOPTS,cmdbuf,NULL);
					/* source contents of .MANUAL file */
			}
			break;


		    default:			/* must be a topic spec */
		        if ( no_subs )  {
				printf("\n Sorry, no subtopics.\n\n");
		    		break;
			}
			if ( (k = matchv( s1, topix, mx)) == 0 ) {
			   printf("\n Sorry, no direct help for  %s\n",s1);
			   if ( (k = xref_matchv(s1,topix,mx)) == 0) {
			       printf(" no cross-references to  %s\n",s1);
			       printf(
				" (list cmnds with '*', topics with '#')\n");
			       break;	/* leave switch */
			   }
			   else {
			       do_xref(ppt,s1,mx);
			       break;   /* leave switch */
			   }
			}

			/* step thru sub-topics that match src */
		        for(i=0; i < k; i++) {
			    s3 = mx[i];

			    if (i > 0) {
			        j=takeit("\nNext %s subtopic: %s\nTake it?",
					 ppt,s3);
				if (j ==  1) continue;
				if (j == -1) break;
			    }

			    if ( chdir(s3) >= 0 ) {  /* directory  subtopic */
			    	strcpy(fullpath,ppt);
			    	strcat(fullpath," ");
			    	strcat(fullpath,s3);
			    	help1(fullpath,wvec,0,0, 0);	/* recurse */
				if ( strcmp(getwd(newdir),helpdir) )
					chdir("..");
			    }			/* else text subtopic */
			    else  {
				strcpy(fullpath, ppt);
				strcat(fullpath, " ");
				strcat(fullpath, s3);
				printhelp(s3, fullpath);
			    }
			}

		}	/* end of switch */

	}	/* end of while(1) */

	if (!no_subs && (*topix != NULL) ) free( *topix );

	return(0);

  }	/* end of help1 */


/* ****************************************************************
 * takeit: ask user whether to take next topic, return t or f
 *
 *	This routine takes a message, with format, and asks the 
 *  user whether he wants to do the action, not do the action,
 *  or quit the cycle of actions.
 *	y - return  0
 *	n - return  1
 *	q - return -1
 *	? - tell what y, n, and q do.
 *      * - tell what y, n, and q do.
 *     <CR> - carriage return
 *
 *	Naturally, case is insignificant.
 */

 takeit(fmt,m1,m2,m3)
   char *fmt, *m1, *m2, *m3;
 {

	char ans[40];
	int  done, ret;
	char *fgets();

	for(done = 0; !done; ) {
		if (fmt != NULL) printf(fmt, m1, m2, m3);
		printf(" [ynq] ");
		if (fgets(ans, 39, stdin) == NULL) exit(1);  /* abort */
		isupper(*ans)?(*ans=tolower(*ans)):0;
		if (*ans == 'n') {
			done = 1;
			ret = 1;
		}
		else if (*ans == 'y' || *ans == '\n') {
			done = 1;
			ret = 0;
		}
		else if (*ans == 'q') {
			done = 1;
			ret = -1;
		}
		else printf("  Answer y to get next subtopic, n to skip, q to quit.\n");
	}
	return(ret);
 }


 /* ***************************************************************
  * matchv: return all matches of a string in a vector.
  *
  *	This routine accepts a string and a vector of strings. 
  *  The string is supposed to be an abbreviation of one or more
  *  strings in the vector.  The full versions of the strings are
  *  placed in a another vector (which is in a static area) and
  *  a pointer to that vector returned.  Note that the input
  *  vector of pointers must have NULL as its terminating element.
  *  The output vector will also terminate with NULL.
  *	NOTE: The vector returned contains pointers into the input
  *  vector.  Do not, therefore, mess with the contents of the output
  *  vector.
  *
  *  If NULL is returned, nothing matched, or there was some other
  *  error.
  */

  matchv( src, vec, mx)
    char *src;
    char *vec[];
    char *mx[];
  {
	char *m[MAXMATCHES];
	char  *s1, *s2;
	int i,j, slen;

	if ( (slen = strlen(src)) == 0 ) return(NULL);  

	for(i=0, j=0; vec[i] != NULL && j < MAXMATCHES; i++) {
	    if  ( strcmp(src,vec[i]) == 0 ) {   /* exact match! */
		m[0] = vec[i];  j = 1;
		break;
	    }
	    else if ( strncmp(src,vec[i],slen) == 0   && 
		     strlen(vec[i]) >= slen )
		   		m[j++] = vec[i];
	}
	m[j] = NULL;
	for(i=0; i <= j; mx[i] = m[i], i++);
	return(j);
  }

 /* *****************************************************************
  * fkoff: fork a process and return the exit status of the process.
  *
  *    This routine takes a command line separated into words, and
  *  uses vfork(2) and execve(2) to quickly run the program.
  *
  *  This is a simplified version of the fkoff() routine from dcon(8).
  *  Here, a program name and up to four arguments may be passed in.
  *  If one of them is null, FINE, but the fifth 
  *
  */

  fkoff(prg,arg1,arg2,arg3,arg4)
    char *prg, *arg1, *arg2, *arg3, *arg4;

  {
	char *command, *malloc(), *argvec[6];
	int pid, stat, i;

	for(i=0; i < 6; argvec[i++] = NULL);
	argvec[0] = prg;
	argvec[1] = arg1;
	argvec[2] = arg2;
	argvec[3] = arg3;
	argvec[4] = arg4;
	if (argvec[0] == NULL) return(-1);
	command = malloc( strlen(argvec[0]) + 1);
	strcpy(command,argvec[0]);

	Set_Tc_Init( (stdin->_file) ) ;
#ifdef USG
	pid = fork();
#else
	pid = vfork();    /* fork 2 copies of us */
#endif
	if (pid < 0 )  {
	        fflush(stdout);
		perror("help: fork");
		Set_Tc_Here( (stdin->_file) ) ; 
		return(0);
	}
	else if (pid == 0)  { /* we are child, execve the program. */
   		pid = execve(command,argvec,environ);
		_exit(1);   
	}
	else {          /* we are parent, wait for child */
		pid = wait(&stat);
		Set_Tc_Here( (stdin->_file) ) ;
		if (pid == -1) return(pid);
		stat = stat / 0400;   /* get hi byte of status */
		return(stat);
        }
  }

/* **************************************************************
 * makewvec: make a vector of words, up to N of them
 *
 *    This routine uses index to simply parse a string
 *  made up of (possibly) several blank-separated words.
 *  The words are stored into the slots of a vector, as pointers
 *  into the original string.  Therefore, the original string
 *  is destroyed.
 */
 makewvec(sstr, rvec, veccnt)
     char *sstr;
     char *rvec[];
     int veccnt;
{
    int mcnt = 0;
    int done = 0;
    int i,j;
    char *s1, *s2, *index();

    if (strlen(sstr) == 0) {
	rvec[0] = NULL;
    }
    else {
	/* skip leading whitespace */
	for(s1=sstr; iswhite(*s1); s1++);
	for(mcnt = 0, done = 0; !done; mcnt++) {
	    s2 = index(s1,' ');
	    if (s2 == NULL) s2 = index(s1,'	');
	    if (s2 == NULL) s2 = index(s1,'\n');
	    if (s2 != NULL) *s2 = '\0';
	    rvec[mcnt] = s1;
	    if (mcnt + 1 >= veccnt) break;
	    if (s2 == NULL) done = 1;
	    else {
		/* skip more white space */
		for(s1 = s2+1; iswhite(*s1); s1++);
		if (*s1 == '\0') done = 1;
	    }
	}
	rvec[mcnt] = NULL;
    }

    return(mcnt);
}


 /* ***************************************************************
  * xref_matchv: return all cross-ref matches for a topic
  *
  *	This routine accepts a string and a vector of strings. 
  *  The string is supposed to be a referal to one or more
  *  cross-reference file names in the vector.  The matches are
  *  placed in a another vector (which is in a static area) and
  *  a pointer to that vector returned.  Note that the input
  *  vector of pointers must have NULL as its terminating element.
  *  The output vector will also terminate with NULL.
  *	NOTE: The vector returned contains pointers into the input
  *  vector.  Do not, therefore, mess with the contents of the output
  *  vector.  
  *     NOTE: cross-ref file names in the topix vector begin with 
  *  the flag char '@' and still have their extension of .XREF.
  *
  *  If 0 is returned, nothing matched, or there was some other error.
  */

  xref_matchv( src, vec, mx)
    char *src;
    char *vec[];
    char *mx[];
  {
	char *m[MAXMATCHES];
	char  *s1, *s2;
	char  one_ref[MAXNAMELEN];
	int i,j, slen;

	if ( (slen = strlen(src)) == 0 ) return(NULL);  

	for(i=0, j=0; vec[i] != NULL && j < MAXMATCHES; i++) {
	    if ( *vec[i] != '@' ) break;
	    for(s1 = vec[i], s2 = one_ref; 
		*s1 != '\0' && *s1 != '.';
		*s2++ = *s1++);
	    *s2 = '\0';
	    if ( strcmp(src,one_ref) == 0) {    /* exact match! */
		m[0] = vec[i]; j = 1;
		break;
	    }
	    else if
	      (strncmp(src,(vec[i] + 1),slen) == 0 && strlen(vec[i]) >= slen)
		      m[j++] = vec[i];
        }
	m[j] = NULL;
	for(i=0; i <= j; mx[i] = m[i], i++);
	return(j);
  }


/* **************************************************************
 * do_xref: given a vector of cross-reference filenames, do them
 *
 *     This routine follows a set of cross references.  
 *   Technically, there are two kinds of cross-reference files, 
 *   distinguished by the first character of their contents.
 *   If the first character is an at sign (@) the file is a 
 *   ``direct'' cross-reference and the rest of the line starting
 *   with the @ sign is a vector of words that give an ABSOLUTE
 *   help path.  If the first char is not @ then the file is an
 *   ``apologetic'' cross-reference, and the text in it is a 
 *   lame-brained excuse for why the topic in question is not
 *   documented in help.
 *
 *      For each filename in the vector, the following actions 
 *   are performed.
 *
 *        1. check that the file exists and is readable, by
 *           opening it for reading.
 *        2. If this is not the first xref, ask the user if
 *           he wants to look at this one, if so, proceed to 
 *        3. Read the first line of the file and check it.
 *               a. if first char is not '@', do a more(1)
 *                  on the file with fkoff.
 *               b. otherwise, read the first line, and
 *                  use makewvec to parse it into words.
 *                  pass these words to a new invokation
 *                  of help1.  Make the ppt for help1
 *                  be something that denotes a cross-ref.
 */
 do_xref(ppt,src,mx)
     char *ppt, *src;
     char *mx[];
{
    int i,j,k;
    char one_ref[MAXNAMELEN];
    char *s1, *index();

    for(i=0; mx[i] != NULL; i++) {
	strcpy(one_ref,mx[i]+1);
	if (i > 0) {
	    j = takeit("\nNext %s %s cross-reference: %s\nTake it?",
		       ppt,src,one_ref);
	    if (j == 1) continue;
	    if (j == -1) break;
	}
	do_one_xref(one_ref,src,ppt);
    }

    putchar('\n');
    return(0);
}

	
/* **************************************************************
 * do_one_xref: evaluate and do a single cross-reference
 *
 *      This routine accepts a single file name and src name
 *   for a single cross-reference.  It then checks the file
 *   for type, and performs the appropriate action.
 *
 *      If the file is not accessible, an error message is 
 *   printed.
 */
 do_one_xref(xref_file,src,ppt)
     char *xref_file, *src, *ppt;
{
    int i,j;
    FILE *xr_fp, *fopen();
    char *wvec[MAXMATCHES];
    char *index(), *s1, lbuf[MAXLINELEN], xbuf[MAXNAMELEN+15];

    if ( (xr_fp = fopen(xref_file,"r")) == NULL) {
	printf(" Sorry, %s cross-reference file inaccessible.\n",xref_file);
	return(-1);
    }

    if (fgets(lbuf,MAXLINELEN,xr_fp) != NULL) {
	if ( *lbuf != '@' ) {
	    s1 = index(xref_file,'.');
	    if (s1 != NULL) *s1 = '\0';
	    printf(" Cross reference text called \"%s\" is available\n",
		   xref_file);
	    if (ppt != NULL)
	      printf("\n HELP: %s <<%s>>\n",ppt,xref_file);
	    if (s1 != NULL) *s1 = '.';
	    fclose(xr_fp);
	    fkoff(VIEWPROGRAM,VIEWPROGOPTS1,VIEWPROGOPTS2,xref_file,NULL);
	}
	else {
	    char pptbuf[MAXNAMELEN+13], pathbuf[256];
	    makewvec(lbuf+1,wvec,MAXMATCHES);
	    printf("\n following cross-reference `help");
	    for(i=0; wvec[i] != NULL; i++) printf(" %s",wvec[i]);
	    s1 = index(xref_file,'.');
	    if (s1 != NULL) *s1 = '\0';
	    printf("' to find help for %s\n\n",xref_file);
	    sprintf(pptbuf,"<<%s>> ",xref_file);
	    if (s1 != NULL) *s1  = '.';
	    getwd(pathbuf);
	    chdir(helpdir);
	    help1(pptbuf,wvec,1,1, 0);
	    chdir(pathbuf);
	    putchar('\n');
	}
    }
    else {
	printf(" Sorry, cross-ref file %s seems to be empty!\n\n",xref_file);
    }
    return(0);
}


/* **************************************************************
 * printlist - print a list of strings in columnar or list form
 *
 *     This routine takes a list of names in a vector and prints
 *   the list according to the current help mode.  This code
 *   used to be in printtopix() but I decided to move it here
 *   so it could be used for other things.
 */
printlist(vec,namecnt)
     char *vec[];
     int namecnt;
{
    int i,j,k;
    int longlen, namewidth, rowcnt, colcnt, xrefs;
    char *s1, *s2;
    static char row[TERMWID+1];

	longlen = xrefs = 0;
	for(i=0; i < namecnt; i++ ) {
	        if (*vec[i] == '@') xrefs++;
		else
		  longlen = ((k=strlen(vec[i]))>longlen)?k:longlen;
	}

	/* here print the names out in nice columns */
	namewidth = longlen + COLUMNSPACE;
	rowcnt = TERMWID / namewidth;
	colcnt = (namecnt + (rowcnt-1)) / rowcnt ;
	if (colcnt <= 0) colcnt = 1;

	if (col_flag && rowcnt >= 1) {
	        printf("\n");
		for(i=0; i < colcnt ; i++ ) {
			for(k=0; k < TERMWID; row[k++] = ' ');
			row[k] = '\0';
			for(j=0, s1 = row; 
		    	    (i+j+xrefs) < namecnt; 
		    	    j += colcnt) {
				row[strlen(row)] = ' ';
				strcpy(s1,vec[i+j+xrefs]);
				s1 = s1 + namewidth;
			}
			printf("    %s\n",row);
		}
		printf("\n");
	}
	else {
		for(i=xrefs; i < namecnt; i++)
			printf("%s\n",vec[i]);
 	}
 
}

#ifdef BSD

/* ****************  funky keyboard stuff below here  ************* */

/* **************************************************************
 * pushback - push a string back onto the tty input queue
 *
 *     This routine accepts a string and pushes it onto the tty
 *   input queue, as if the user had typed it.
 *   The input is a file descriptor that points to a tty, and
 *   a pointer to the string.
 */
pushback(fd, str)
     int fd;
     char *str;
{
    char *s1;

    for(s1=str; *s1; ioctl( fd, TIOCSTI, s1++));
}


/* **************************************************************
 * getpending - get pending chars from tty, then push them back
 *
 *     This routine grabs chars pending on a specified file
 *   descriptor, puts them into the specified buffer, and pushes
 *   them back onto the tty input queue.
 *   The buffer passed by the caller had BETTER be long enough,
 *   or everything gets munched.
 *   If the parameter no_pushback is non-zero, the characters are
 *   not pushed back.
 *   The number of characters obtained is returned.
 */
getpending(fd, buf, buflen, no_pushback)
     int fd;
     char *buf;
     int buflen, no_pushback;
{
    int i,j;
    char x = '\n';

    ioctl(fd, FIONREAD, &i);
    if (i <= 0) return(0);
    j = read(fd, buf, buflen);
    buf[j-1] = '\0';
    for(i = j-2; buf[i] > '\030' ; buf[i--] = '\0' );
    if (i <= 0) return(0);
    if ( !no_pushback) pushback(fd, buf);
    return(i);
}


/* **************************************************************
 * init_funky - initialize funky stuff, and save startup values
 *
 *         This routine starts up breaking for ESC, and
 *   saves the initial tchars structure.
 */
init_funky()
{
    char *malloc();

    if ( isatty(0) ) {
	is_tty = 1;
	init_tchars = (struct tchars *) malloc( sizeof(struct tchars));
	here_tchars = (struct tchars *) malloc( sizeof(struct tchars));
	ioctl(0,TIOCGETC,init_tchars);
	ioctl(0,TIOCGETC,here_tchars);
	here_tchars->t_brkc = '\033';
	ioctl(0,TIOCSETC,here_tchars);
    }
    
    return;
}


/* **************************************************************
 * read_gets: do a gets by read(2)ing a line
 *
 *       Read a string into a buffer, from the given file
 *    descriptor.  There are four arguments:
 *
 *		fd	file descriptor (usually 0)  
 *  		buf     char * to buffer
 * 		blen    buffer length in bytes
 *		spec	do special ^[ and ^D processing
 *
 *    If the special processing is enabled, ESC invokes helpname
 *    completion, and ^D invokes possible completion listing.
 *    When the user finally inputs a line that does not require
 *    special processing, it is returned as a string to caller.
 *    If anything goes wrong, or a true EOF is given, -1 is
 *    returned.
 *    When special processing is not enabled, the input line is 
 *    simply returned to the caller as a string.
 */
read_gets(fd, buf, blen, do_special)
     int fd, blen, do_special;
     char *buf;
{
    char *s1, *s2, endc;
    static  char bufcopy[MAXLINELEN];
    static  char *wvec[MAXMATCHES];
    int i,j,k;


    for(endc = '\0'; endc != LF ; ) {
	fflush(stdout);
	i = read(fd, buf, blen);

	/* now, having read data, analyze it for special stuff */
	if (i <= 0) return(-1);
	else {
	    s1 = buf + (i - 1);
	    if (*s1 == ESC || *s1 == LF || *s1 == RET) {
		endc = (*s1 == ESC)?(*s1):(LF);
		*s1 = '\0';
	    }
	    else {
		*++s1 = '\0';
		endc = *s1;
	    }
	    for(s1 = buf + (strlen(buf) - 1);
		*s1 == ' '  ||   *s1 == '	';
		*s1-- = '\0');
	    for(s1 = buf; *s1 == ' ' || *s1 == '	'; s1++);

	    if (endc == LF || endc == RET || do_special == 0) return(i-1);
	    else {
		*gbl_match = '\0';

		/* if this is a non-topic command, just beep */
		if ( index( SPEC_CHARS, *s1) ) {
		    fputs("  ",stdout);
		    for(s1 = buf; *s1; s1++) fputs("",stdout);
		    fflush(stdout);
		    pushback(stdin->_file,buf);
		    continue;
		}
		else
		/* do special stuff here */
		if (endc == ESC) {
		    strcpy(bufcopy,buf);
		    i = makewvec(bufcopy, wvec, MAXMATCHES);
		    help1("", wvec, 0, 0, ESC_COMP);
		    fputs("  ",stdout);
		    for(s1 = buf; *s1; s1++) fputs("",stdout);
		    fflush(stdout);
		    pushback(stdin->_file,buf);
		    if ( *gbl_match ) {
			pushback(stdin->_file,(gbl_match + strlen(wvec[i-1])));
		    }
		    else fputs("",stdout);
		    fflush(stdout);
		}
		else
		  if (endc == '\0') {   /* must have been eof */
		      strcpy(bufcopy,buf);
		      i = makewvec(bufcopy, wvec, MAXMATCHES);
		      help1("" , wvec, 0, 0, CTRLD_COMP);
		      printf("\n%s ",gbl_ppt);
		      if (*gbl_match == '\0') fputs("", stdout);
		      fflush(stdout);
		      pushback(stdin->_file,buf);

		  }
	    }
	}
    }
}
		


#endif 
