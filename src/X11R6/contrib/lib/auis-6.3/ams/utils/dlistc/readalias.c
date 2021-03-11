/* ********************************************************************** *\
 *         Copyright IBM Corporation 1988,1991 - All Rights Reserved      *
 *        For full copyright information see:'andrew/config/COPYRITE'     *
\* ********************************************************************** */

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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/ams/utils/dlistc/RCS/readalias.c,v 1.6 1992/12/15 21:24:03 rr2b R6tape $";
#endif

/*

  readalias.c -- Read in an alias file

*/

#include <readalias.h>
#include <andrewos.h>
#include <stdio.h>
#include <sys/param.h>
#include <util.h>

/* Format of the .AMS_aliases file:

--  do not to leave any blank lines at the top of the .AMS_aliases file
--  comments can be placed in the file with a #
--  end every line in the file with a return
--  do not put any tabs or spaces before the alias ("joe")
--  put only one space (or one tab) between each alias and its address
--  when aliasing several addresses to one alias (explained below), separate 
    the addresses with a comma
--  aliases are case-insensitive, which means that the Andrew Message System 
    doesn't distinguish between upper case and lower case letters either in 
    your .AMS_aliases file or at the To: header when sending.
--  @shorthost @full.host.name is an abbreviation for hostnames
--  several commands:
       $forceformat user@host
       $forcestrip user@host
       $foretrust user@host

*/

#define WHITESPACE "\b\f\n\r\t\v "

static char *GetNextWordInString(w,s, iswhite)
char *w, *s;
int *iswhite;
{ /*
    Breaks the string s into alternating pieces of white space,
    and words.  Modifies s, in order to return the next chunk.
    Pass in NULL for w to start, otherwise pass in returned string.

    For example, if s is "abc def, hij  \t  klm",
    then successive calls would return:
    "abc", " ", "def", ", ", "hij", "  \t  ", "klm", NULL.
   */

  static char saved = '\0';
  int p;

  if (s[0] == '\0') return(NULL);

  if (w == NULL) {
    w = s;
  } else {
    w = w+strlen(w);
    w[0] = saved;
  }

  if ((p = strspn(w,WHITESPACE)) == 0) {
    p = strcspn(w, WHITESPACE);	/* starting on a word */
    *iswhite = 0;
  } else {
    *iswhite = 1;		/* extracting whitespace */
  }

  saved = w[p];
  w[p] = '\0';

  if (strlen(w) > 0) return(w);
  else return(NULL);
}

alias_t FindAlias(name, aliases)
char *name;
alias_set_t aliases;
{ 
  /* 
    If name is an alias, return a pointer to the alias structure,
    else return NULL.
    Does a binary search, so aliases had better be sorted.
   */
  int first, middle, last;
  int result;

  if (!(aliases->sorted)) {
    fprintf(stderr, "Internal error:  FindAlias called on an unsorted alias_set.\n");
    exit(10);
  }
  if (aliases->alias_count < 1) {
    return(NULL);
  }

  first = 0;
  last = aliases->alias_count - 1;
  do {
    middle = (first+last)/2;
    result= ULstrcmp(name, ALIASNAME(aliases->aliases[middle]));
    if (result == 0) {
      return(aliases->aliases[middle]);	/* found it */
    } else if (result < 0) {
      last = middle-1;		/* key < middle */
    } else {
      first = middle+1;		/* key > middle */
    }
  } while (!(last<first));
  return(NULL);			/* not found */
  
}

char *FindHost(shortname, aliases)
char *shortname;
alias_set_t aliases;
{
  /* 
    If shortname is a host alias, return the longname,
    else return NULL.
    Does a binary search, so aliases had better be sorted.
   */
  int first, middle, last;
  int result;

  if (!(aliases->sorted)) {
    fprintf(stderr, "Internal error:  FindHost called on an unsorted alias set.\n");
    exit(10);
  }
  if (aliases->host_count < 1) {
    return(NULL);
  }

  first = 0;
  last = aliases->host_count - 1;
  do {
    middle = (first+last)/2;
    result= ULstrcmp(shortname, SHORTNAME(aliases->hosts[middle]));
    if (result == 0) {
      return(LONGNAME(aliases->hosts[middle]));	/* found it */
    } else if (result < 0) {
      last = middle-1;		/* key < middle */
    } else {
      first = middle+1;		/* key > middle */
    }
  } while (!(last<first));
  return(NULL);			/* not found */
  
}

static char *GetNextLine(f)
FILE *f;
{
  static char *buf = NULL;
  static long bufsize = 0L;
  long count = 0L;
  int c;

  if (feof(f)) {
    if (buf) {
      free(buf);
      buf = NULL;
      bufsize = 0L;
    }
    return(NULL);
  }

  if (buf == NULL) {
    bufsize = 255L;
    if ((buf = (char *) malloc(bufsize)) == NULL) {
      buf = NULL;
      bufsize = 0L;
      return(NULL);
    }
  }

  while(!(feof(f)) && ((c=fgetc(f))>EOF)) {
    if (count+1>bufsize) {
      bufsize = (bufsize + 1)*2;
      if ((buf = (char *)realloc(buf,bufsize))==NULL) {
	buf = NULL;
	bufsize = 0L;
	return(NULL);
      }
    }

    if (c=='\n') {
      buf[count++] = '\0';
      return(buf);
    } else {
      buf[count++] = c;
    }
  }
  buf[count] = '\0';

  if (count>0) {
    return(buf);
  } else {
    if (buf) {
      free(buf);
      buf = NULL;
      bufsize = 0L;
    }
    return(NULL);
  }
}

static char *FlatUnparseSimpleAddress(pa)
PARSED_ADDRESS *pa;
{
  char *unbuf = NULL;
  long unbufsize = 255;
  int code;

  if (pa->Kind != SIMPLE_ADDRESS)
    return(NULL);

  if (unbuf == NULL)
    if ((unbuf = (char *)malloc(unbufsize)) == NULL)
      return(NULL);
  while (1) {
    switch (code = UnparseOneAddress(pa, UP_SPACES_TO_DOTS | UP_NO_COMMENTS, unbuf, unbufsize, "", unbufsize)) {
    case PA_OK:
      return(unbuf);
    case PA_NO_MEM:
      unbufsize *= 2;
      if(unbuf) free(unbuf);
      if ((unbuf = (char *)malloc(unbufsize)) == NULL)
	return(NULL);
      break;
    default:
      fprintf(stderr, "Unparse error (%d).\n", code);
      exit(9);
    }
  }
}

char *FlatUnparseAddress(pa)
PARSED_ADDRESS *pa;
{
  /*
    Takes a single address (NOT an address list), and Unparses it
    flatly.  That is, if there are nested group addresses, those
    addresses are burst out to the top level.  Returns a malloc'd
    string.
  */
  char *unbuf = NULL;
  char *res;

  switch(pa->Kind) {
  case SIMPLE_ADDRESS:
    return(FlatUnparseSimpleAddress(pa));
  case GROUP_ADDRESS:
    if (pa->Members == NULL) return(NULL);
    FOR_ALL_GROUP_MEMBERS
      (gm, pa,
       {
	 if ((res = FlatUnparseAddress(gm)) == NULL) {
	   if (unbuf) free(unbuf);

	   return(NULL);
	 }
	 if (unbuf == NULL) {
	   if ((unbuf = (char *)malloc(strlen(res)+1)) == NULL) {
	     free(res);
	     return(NULL);
	   }
	   strcpy(unbuf, res);
	   free(res);
	 } else {
	   if ((unbuf = (char *)realloc(unbuf, strlen(unbuf) + strlen(res)+ 3)) == NULL) {
	     free(res);
	     return(NULL);
	   }
	   strcat(unbuf, ", ");
	   strcat(unbuf, res);
	   free(res);
	 }
       }
       );
    break;
  }
  return(unbuf);
}

void DumpAliases(stream,aliases)
FILE *stream;
alias_set_t aliases;
{
  int i;
  host_t host;
  alias_t alias;
  char *unpa;

  if (aliases->host_count > 0) 
    for(i = 0; (host = aliases->hosts[i]) != NULL; ++i) {
      fprintf(stream, "\nHostname alias (%d) '%s' is short for '%s'.\n",
	      i, SHORTNAME(host), LONGNAME(host));
    }
  
  if (aliases->alias_count > 0)
    for(i = 0; (alias = aliases->aliases[i]) !=NULL; ++i) {
      fprintf(stream, "\nAlias (%d) '%s' has the following members:\n",
	      i, ALIASNAME(alias));
      if ((unpa = FlatUnparseAddress(alias->parsed_alias)) != NULL) {
	fprintf(stream, "\t%s\n", unpa);
	free(unpa);
      }
    }
}

static void AddAlias(aliases, name)
alias_set_t aliases;
char *name;
{
  alias_t newalias;
  
  aliases->alias_count++;
  if (aliases->aliases_size < (aliases->alias_count+1)*sizeof(alias_t)) {
    aliases->aliases_size = aliases->alias_count*sizeof(alias_t)*2;
    if (aliases->aliases) {
      if ((aliases->aliases = (alias_t *)realloc(aliases->aliases, aliases->aliases_size))==NULL) {
	fprintf(stderr, "Out of memory.\n");
	exit(5);
      }
    } else {
      if ((aliases->aliases = (alias_t *)malloc(aliases->aliases_size))==NULL) {
	fprintf(stderr, "Out of memory.\n");
	exit(5);
      }
    }
  }
  if ((newalias = (alias_t)malloc(sizeof(*newalias))) == NULL) {
    fprintf(stderr, "Out of memory.\n");
    exit(5);
  }

  if ((newalias->parsed_alias = (PARSED_ADDRESS *)malloc(sizeof(PARSED_ADDRESS))) == NULL) {
    fprintf(stderr, "Out of memory.\n");
    exit(5);
  }
  newalias->parsed_alias->Kind = GROUP_ADDRESS;
  if ((ALIASNAME(newalias) = (char *)malloc(strlen(name)+1)) == NULL) {
    fprintf(stderr, "Out of memory.\n");
    exit(5);
  }
  strcpy(ALIASNAME(newalias), name);
  newalias->parsed_alias->Hosts = NULL;
  newalias->parsed_alias->Members = NULL;
  newalias->parsed_alias->RoutePhrase = NULL;
  newalias->parsed_alias->Comments = NULL;
  newalias->parsed_alias->Next = NULL;
  newalias->parsed_alias->Prev = NULL;
  newalias->parsed_alias->MD = NULL;
  newalias->parsed_alias->Extra = NULL;
  newalias->resolving = 0;
  newalias->resolved = 0;
  newalias->circular = 0;
  aliases->aliases[aliases->alias_count-1] = newalias;
  aliases->aliases[aliases->alias_count] = NULL;
}

static void AddHost(aliases, shortname, longname)
alias_set_t aliases;
char *shortname, *longname;
{
  host_t newhost;
  
  aliases->host_count++;
  if (aliases->hosts_size < (aliases->host_count+1)*sizeof(host_t)) {
    aliases->hosts_size = aliases->host_count*sizeof(host_t)*2;
    if (aliases->hosts) {
      if ((aliases->hosts = (host_t *)realloc(aliases->hosts, aliases->hosts_size))==NULL) {
	fprintf(stderr, "Out of memory.\n");
	exit(5);
      }
    } else {
      if ((aliases->hosts = (host_t *)malloc(aliases->hosts_size))==NULL) {
	fprintf(stderr, "Out of memory.\n");
	exit(5);
      }
    }
  }
  if ((newhost = (host_t)malloc(sizeof(*newhost))) == NULL) {
    fprintf(stderr, "Out of memory.\n");
    exit(5);
  }

  if ((SHORTNAME(newhost) = (char *)malloc(strlen(shortname)+1)) == NULL) {
    fprintf(stderr, "Out of memory.\n");
    exit(5);
  }
  strcpy(SHORTNAME(newhost), shortname+1); /* skip the "@" */

  if ((LONGNAME(newhost) = (char *)malloc(strlen(longname)+1)) == NULL) {
    fprintf(stderr, "Out of memory.\n");
    exit(5);
  }
  strcpy(LONGNAME(newhost), longname+1); /* skip the leading "@" */
  aliases->hosts[aliases->host_count-1] = newhost;
  aliases->hosts[aliases->host_count] = NULL;

}

static void ResolveAddressList(aliases, al)
alias_set_t aliases;
PARSED_ADDRESS *al;
{
  alias_t refalias;
  extern void ResolveAliasMembers(); /* forward decl */


  if (al == NULL) return;

  FOR_ALL_ADDRESSES
    (a, al,

     switch(a->Kind) {
     case SIMPLE_ADDRESS:
       if (a->Hosts->Next == a->Hosts) { /* No hosts, could be an alias reference */
	 if ((refalias = FindAlias(a->LocalPart, aliases)) != NULL) {
	   ResolveAliasMembers(aliases,refalias);
	   /* now make a refer to the members of refalias */
	   if (!(refalias->circular)) {
	     a->Kind = GROUP_ADDRESS;
	     a->Members = ALIASMEMBERS(refalias);
	   } 
	 }
       } else {
	 char *longname;
				/* resolve hostname aliases */
	 FOR_ALL_REVERSE_HOSTS
	   (h, a,
	    
	    if ((longname = FindHost(h->Name, aliases)) != NULL) {
	      h->Name = longname;
	    }
	    );
       }
       break;
     case GROUP_ADDRESS:
				/* need to recurse on...  */
       ResolveAddressList(aliases, a->Members);
       break;
     }
     );
}

static void ResolveAliasMembers(aliases, alias)
alias_set_t aliases;
alias_t alias;
{
  if (alias->resolving) {
    fprintf(stderr, "Warning: circular alias reference in alias '%s'.\n", 
	    ALIASNAME(alias));
    alias->circular = 1;
    alias->resolved = 1;	/* short-circuit recursion */
  }
  if (alias->resolved) return;

  alias->resolving = 1;
  ResolveAddressList(aliases, ALIASMEMBERS(alias));
  alias->resolving = 0;
  alias->resolved = 1;
}

static void ResolveAliases(aliases)
alias_set_t aliases;
{
  int i;

  if (aliases->alias_count > 0)
    for(i = 0; aliases->aliases[i] != NULL; ++i) {
      ResolveAliasMembers(aliases, aliases->aliases[i]);
    }
}

static int AliasCmp(a1, a2)
alias_t *a1, *a2;
{
  return(ULstrcmp(ALIASNAME(*a1), ALIASNAME(*a2)));
}

static int HostCmp(h1, h2)
host_t *h1, *h2;
{
  return(ULstrcmp(SHORTNAME(*h1), SHORTNAME(*h2)));
}

static void ParseAndAddMembers(alias, upa)
alias_t alias;
char *upa;
{
  int code;
  PARSED_ADDRESS *pa;

  switch(code = ParseAddressList(upa, &pa)) {
  case PA_OK:
    ALIASMEMBERS(alias) = pa;
    break;
  default:
    fprintf(stderr, "Bad parse (%d).\n", code);
    exit(8);
  }
}

alias_set_t ReadAliases(aliasfile)
FILE *aliasfile;
{
  char *line, *word, *unparsed;
  int iswhite;
  alias_set_t aliases;
  alias_t newalias;

  if ((aliases = (alias_set_t)malloc(sizeof(*aliases))) == NULL)
    return(NULL);


  aliases->aliases = NULL;
  aliases->alias_count = 0;
  aliases->aliases_size = 0L;
  aliases->hosts = NULL;
  aliases->host_count = 0;
  aliases->hosts_size = 0L;
  aliases->sorted = 0;		/* sanity checking */
  unparsed = NULL;
  newalias = NULL;

  /* main loop */
  while ((line = GetNextLine(aliasfile)) != NULL) {
    if ((line[0]!='#')&&(line[0]!='\0')) { /* not a comment nor blank line */
      word = NULL;
      word = GetNextWordInString(word, line, &iswhite);
      if (!iswhite) {
	if (newalias && unparsed) ParseAndAddMembers(newalias, unparsed);
	newalias = NULL;
	if (unparsed) free(unparsed);
	unparsed = NULL;

	if (word[0] == '@') {
	  char *shorthost ;

	  if ((shorthost = (char *)malloc(strlen(word)+1)) == NULL) {
	    fprintf(stderr, "Out of memory.\n");
	    exit(5);
	  }
	  strcpy(shorthost,word);
	  word = GetNextWordInString(word, line, &iswhite); /* skip whitepace */
	  word = GetNextWordInString(word, line, &iswhite); /* get long name */
	  AddHost(aliases, shorthost, word);
	  if (shorthost) free(shorthost);
	} else {
	  AddAlias(aliases, word);
	  newalias = aliases->aliases[aliases->alias_count-1];
	}
      }

      if(newalias) {	/* only if we have an alias going */
	while ((word = GetNextWordInString(word,line, &iswhite)) != NULL) {
	  if (unparsed) {
	    unparsed = (char *)realloc(unparsed, 
				       strlen(unparsed) + strlen(word) + 1);
	    if (unparsed == NULL) {
	      fprintf(stderr, "Out of memory");
	      exit(5);
	    }
	    strcat(unparsed, word);
	  } else {
	    unparsed = (char *)malloc(strlen(word) + 1);
	    if (unparsed == NULL) {
	      fprintf(stderr, "Out of memory");
	      exit(5);
	    }
	    strcpy(unparsed, word);
	  }
	}
      }
    }
  }
  if (newalias && unparsed) ParseAndAddMembers(newalias, unparsed);

  fclose(aliasfile);

  /* sort the mess */
  qsort((char *)(aliases->aliases), aliases->alias_count,
	sizeof(alias_t), AliasCmp);
  qsort((char *)(aliases->hosts), aliases->host_count,
	sizeof(host_t), HostCmp);
  aliases->sorted = 1;

  /* resolve aliases within aliases */
  ResolveAliases(aliases);

  return(aliases);
}

#ifdef TESTINGONLYTESTING
main()
{
  DumpAliases(stdout,ReadAliases(stdin));
}
#endif TESTINGONLYTESTING
