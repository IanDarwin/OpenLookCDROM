/*	Function Name: CreateManpageName
 *	Description: Creates the manual page name for a given item.
 *	Arguments: entry - the entry to convert.
 *	Returns: the manual page properly allocated.
 */

/*
 * If the filename is foo.3     - Create an entry of the form:  foo
 * If the filename is foo.3X11  - Create an entry of the form:  foo(X11)
 * IF the filename is a.out.1   - Create an entry of the form:  a.out
 */

char *
CreateManpageName(entry)
char * entry;
{
  char * cp;
  char page[BUFSIZ];

  ParseEntry(entry, NULL, NULL, page);

  if ( (cp = rindex(page, '.')) != NULL)
    if ( (strlen(cp) > 2) ) {
      *cp++ = '(';
      while( (cp[1] != '\0') ) {
	*cp = *(cp + 1); 
	cp++;
      }
      *cp++ = ')';
      *cp = '\0';
    }
    else
      *cp = '\0';  
  
  return(StrAlloc(page));
}
