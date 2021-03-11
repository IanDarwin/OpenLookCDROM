/* File doload.c for any SYSVR4 dynamic loader. 
   tried on the a Motorola 88000 computer with an Unix System V Release 4
         on the a Motorola 68040 computer with an Unix System V Release 4
*/

#include <stdio.h>
#include <dlfcn.h>
#include <string.h>
#include <libgen.h>
#include "mapping.h"
#include "resolvinfo.h"
#include <sys/param.h>

#define MAX_CLASSLIST 512

struct resolv_class {
  char *name;         /* name of the class to be load */
  int loaded;         /* is loaded ? */
  struct resolv_info* all_list; /* list of all the classes that this class needs */
} infoclasslist[MAX_CLASSLIST];

int infoclasslist_num=-1;

#ifndef RTLD_NOW
#include <dlfcn.h> /* using the libdl.so */
#endif


char doload_extension[] = ".do";

int doload_trace=0;		/* nonzero if debugging */

/* doload: Load a dynamic object.
 *
 * Basically, this just calls the dynamic loader and relies on the
 * system to do the right thing.
 */
char *doload(inFD, name, bp, lenP, path) /* return pointer to entry point, */
				/* or NULL if error */
/* UNUSED */ int inFD;		/* open fd for package file. */
char *name;			/* name of package being loaded */
char **bp;			/* base address of package */
long *lenP;			/* size of text segment */
char *path;			/* Pathname of package being loaded */
{
    char *dummy = NULL;
    void *handle = NULL;
    char *EntryPoint = NULL;
    char epname[MAXPATHLEN] = "\0";	/* XXX - allocate dynamically? */
    register char *p = NULL;
    char *tmp_local_str;
    char local_str[MAXPATHLEN];
    struct file_list* file_elem = NULL;
    int i=0;
    char *namekey = NULL;
    struct MapEntryStruct *mps;
    struct stat buf;

    dummy = (char *) malloc(1); /* is memory yet availbe ? */
    if (dummy == NULL)
        return NULL;
    if (bp)
	*bp = dummy;
    if (lenP)
        *lenP = 1;

    /*
     * We assume class_Error and class_RoutineStruct are in the
     * dynamic-loading symbol table of the executable.
     */

    /*
     * Load package
     */


    /* Is the class of name or key <name> already loaded ? */
    mps = RetrieveByName(name,class_VERSIONNOTKNOWN);
    if (mps == NULL)
      mps = RetrieveByKey(name,class_VERSIONNOTKNOWN);

    if (mps != NULL) { /* class not already loaded */
      if ((tmp_local_str = (char*)doload_FileExist(mps->Data)) == NULL) {
	/* put a default path to /usr/andrew/dlib/atk 
	   if CLASSPATH variable is not defined */
	strcpy(local_str,"/usr/andrew/dlib/atk/");
	strcat(local_str,mps->Data);
      } else 
	strcpy(local_str,tmp_local_str);
	if (doload_trace > 1)
	  printf("mps: name(%s) key(%s) data(%s)\n",mps->Name, mps->Key, mps->Data);
    } else {
      int n = strlen(name);
      if ( n>3 && strcmp(name+n-3,"app") == 0) {
	strncpy(local_str,name,n-3);
	local_str[n-3] = '\0';
	mps = RetrieveByName(local_str,class_VERSIONNOTKNOWN);
	if (mps == NULL)
	  mps = RetrieveByKey(local_str,class_VERSIONNOTKNOWN);
	if (mps != NULL) {
	  if ((tmp_local_str = (char*)doload_FileExist(mps->Data)) == NULL) {
	    /* put a default path to /usr/andrew/dlib/atk 
	       if CLASSPATH variable is not defined */
	    strcpy(local_str,"/usr/andrew/dlib/atk/");
	    strcat(local_str,mps->Data);
	  } else 
	      return NULL;
	} else {
	  strcpy(local_str,"/usr/andrew/dlib/atk/");
	  strcat(local_str,name);
	  strcat(local_str,doload_extension);
	  if (doload_trace > 1)
	    printf("mps(NULL): local_str(%s) suff(%s)\n",local_str,name+n-3);
	}
      }
    }

    /* find if class is already stored in infoclasslist struct. 
       It can be found, and not be loaded */
    if (infoclasslist_num > -1)
      while ( (i<=infoclasslist_num) && (strcmp(name,infoclasslist[i].name)!=0) ) i++;

    if (i>infoclasslist_num) { /* case not found in infoclasslist struct */
      int fn = open(local_str,O_RDONLY); /* try to open the file <name[key]>.do */
      if (fn == -1) { /* If not found, try to open in the current directoty */
	strcpy(local_str,getcwd(NULL,MAXPATHLEN));
	strcat(local_str,"/");
	strcat(local_str,name);
	strcat(local_str,doload_extension);
	fn = open(local_str,O_RDONLY);
	if (fn == -1) { /* If not found, class does not exist */
	  close(fn);
	  return NULL;
	}
      }
      close(fn);

      /* If class found, store informations in infoclasslist */
      infoclasslist_num++;
      infoclasslist[infoclasslist_num].name=(char *) malloc(strlen(name)+1);
      strcpy(infoclasslist[infoclasslist_num].name,name);
      infoclasslist[infoclasslist_num].loaded=0;
      infoclasslist[infoclasslist_num].all_list = (struct resolv_info*) resolv_file(local_str);

      file_elem=infoclasslist[infoclasslist_num].all_list->classheader_list;

      /* befor loading this class, we must before load all the classes
	 that this one needs */
      while (file_elem != NULL) {
	struct MapEntryStruct *mps2;

	mps2 = RetrieveByName(file_elem->name,class_VERSIONNOTKNOWN);
	if (mps2 == NULL)
	  mps2 = RetrieveByKey(file_elem->name,class_VERSIONNOTKNOWN);
	if (mps2 == NULL)
	  fprintf(stderr," Entree mps2 de (%s) non trouvee\n",file_elem->name);
	else {
	  int k = 0;
	  int objfile_isloaded = -1;
	  while (k<=infoclasslist_num) {
	    if ( (strcmp(infoclasslist[k].name,mps2->Data) == 0) ||
		 (strcmp(infoclasslist[k].name,mps->Data) == 0)) {
	      objfile_isloaded = 1;
	      break;
	    }
	    k++;
	  }
	  if ( (mps != mps2) && (objfile_isloaded == -1) &&
	      (strcmp(mps->Data,mps2->Data) != 0) ) {
	    if (doload_trace > 1)	  
	      printf("doload: name(%s) mp_fname(%s) (**)\n",file_elem->name,mps2->Data);
	    class_Load(mps2->Name);
	  }
	}
	file_elem=file_elem->prev;
      }
    } else
      return NULL;

    infoclasslist[infoclasslist_num].loaded=1;
    if (stat(local_str,&buf) == -1 || ((buf.st_mode & S_IFMT) == S_IFDIR))
    {
	    return NULL;
    }

    handle = dlopen(local_str, RTLD_NOW);

    if ( handle == NULL )
    {
        fprintf( stderr, "doload: Error loading package \"%s\" - %s\n", local_str, dlerror() ); 
        return NULL;
    }
        
    /*
     * Construct name of GetClassInfo symbol from the package name
     * by stripping off the suffix and appending "__GetClassInfo".
     * name can be the name of the class, the key or the basename
     * of the object class.
     */

    /*
     * Return entry point
     */
    
    if (mps == NULL)
      (void) strcpy(epname, name);
    else /* If mps found, We want the name of the class, and not its key */
      (void) strcpy(epname, mps->Name);

    p = strrchr(epname, '.');
    if (p == NULL)
	p = epname + strlen(epname);
    (void) strcpy(p, "__GetClassInfo");

    EntryPoint = (char *)dlsym( handle, epname );
    if ( EntryPoint == NULL )
    {
        fprintf( stderr, "doload: Error finding entry point of package \"%s\" - %s\n", local_str, dlerror() );
        return NULL;
    }
    if ( doload_trace )
        printf(" %s: entry = 0x%.8x\n", name, EntryPoint);
        
    return( EntryPoint );
}



/* doload: Load a dynamic object.
 *
 * Basically, this just calls the dynamic loader and relies on the
 * system to do the right thing.
 */
char *doload2(inFD, name, bp, lenP, path) /* return pointer to entry point, */
				/* or NULL if error */
/* UNUSED */ int inFD;		/* open fd for package file. */
char *name;			/* name of package being loaded */
char **bp;			/* base address of package */
long *lenP;			/* size of text segment */
char *path;			/* Pathname of package being loaded */
{
    char *dummy;
    void *handle;
    char *EntryPoint = NULL;
    char epname[MAXPATHLEN];	/* XXX - allocate dynamically? */
    register char *p;
    char local_str[MAXPATHLEN];
    int i = 0;
    char *tmp_local_str;

    dummy = (char *) malloc(1);
    if (dummy == NULL)
        return NULL;
    if (bp)
	*bp = dummy;
    if (lenP)
        *lenP = 1;

    if (*path != '/' && (tmp_local_str = (char*)doload_FileExist(path)) == NULL) {
      strcpy(local_str,"/usr/andrew/dlib/atk/");
      strcat(local_str,path);
    } else if (*path == '/')
      strcpy(local_str,path);
    else
      strcpy(local_str,tmp_local_str);

    handle = dlopen(local_str, RTLD_NOW);
    if ( handle == NULL ) {
	strcpy(local_str,getcwd(NULL,MAXPATHLEN));
	strcat(local_str,"/");
        strcat(local_str,name);
	handle = dlopen(local_str, RTLD_NOW);
	if ( handle == NULL ) {
	  fprintf( stderr, "doload: Error openning  the file \"%s\" - %s\n", local_str, dlerror() );
	  return NULL;
	}
      }
        
    (void) strcpy(epname, name);
    p = strrchr(epname, '.');
    if (p == NULL)
	p = epname + strlen(epname);
    (void) strcpy(p, "__GetClassInfo");

    /*
     * Return entry point
     */
    EntryPoint = (char *)dlsym( handle, epname );
    if ( EntryPoint == NULL )
    {
        fprintf( stderr, "doload: Error finding entry point of package \"%s\" - %s\n", local_str, dlerror() );
        return NULL;
    }
        
    if ( doload_trace )
        printf(" %s: entry2 = 0x%.8x\n", name, EntryPoint);
        
    return( EntryPoint );
}

