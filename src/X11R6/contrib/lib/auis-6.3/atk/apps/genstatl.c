/* Copyright 1992 Carnegie Mellon University All rights reserved.
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

#include <andrewos.h>
#include <class.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <sys/param.h>

#include <util.h>

FILE *pathopen();
char *AndrewDirStr=NULL;
char *XBaseDirStr=NULL;
char *AFSBaseDirStr=NULL;

char builddir[MAXPATHLEN+1];
char *buildpath=NULL;
char rbuf[MAXPATHLEN+1];
static char ext[]=".ref";

#define HASHMAX 57
struct hasht {
    char *classname;
    struct hasht *next;
};

static struct hasht *need[HASHMAX];
static struct hasht *have[HASHMAX];

struct hashf {
    dev_t dev;
    ino_t ino;
    char *pathname;
    struct hashf *next;
    int priority;
    struct hashf *pnext;
};

static struct hashf *havefile[HASHMAX];

#define HASH(n, len) (((n[0]<<10)^(n[len-1]<<5)^(n[len/2]))%HASHMAX)
#define HASHFILE(s) (((s).st_dev^(s).st_ino)%HASHMAX)

boolean HaveIt(name, add)
char *name;
boolean add;
{
    int len=strlen(name);
    int ind=HASH(name, len);
    struct hasht *result;
    if (have[ind]) {
	struct hasht *s=have[ind];
	while (s && strcmp(s->classname, name)) {
	    s=s->next;
	}
	if(s) return TRUE;
    }
    if(add) {
	result=(struct hasht *)malloc(sizeof(struct hasht));
	if(result==NULL) {
	    fprintf(stderr, "out of memory.\n");
	    exit(-1);
	}
	result->classname=NewString(name);
	result->next=have[ind];
	have[ind]=result;
    }
    return FALSE;
}

static boolean AddClass(name)
char *name;
{
    int len=strlen(name);
    int ind=HASH(name, len);
    struct hasht *result;
    if (need[ind]) {
	struct hasht *s=need[ind];
	while (s && strcmp(s->classname, name)) {
	    s=s->next;
	}
	if(s) return FALSE;
    }
    result=(struct hasht *)malloc(sizeof(struct hasht));
    if(result==NULL) {
	fprintf(stderr, "out of memory.\n");
	exit(-1);
    }
    result->classname=NewString(name);
    result->next=need[ind];
    need[ind]=result;
    return TRUE;
}

static struct hashf *AddFile(path, exists)
char *path;
boolean exists;
{
    struct stat statbuf;
    struct hashf *result;
    int ind;
    boolean flag=FALSE;
    if (path[0]!='-') {
	if(stat(path, &statbuf)!=0) {
	    if(exists) perror(path);
	    return NULL;
	}
	ind=HASHFILE(statbuf);
    } else {
	int len=strlen(path);
	ind=HASH(path, len);
	flag=TRUE;
    }
    if (havefile[ind]) {
	struct hashf *s=havefile[ind];
	while (s && strcmp(s->pathname, path) && (flag || (s->dev!=statbuf.st_dev || s->ino!=statbuf.st_ino))) {
	    s=s->next;
	}
	if(s) {
	    if(s->priority<0) s->priority=(-s->priority);
	    return s;
	}
    }
    result=(struct hashf *)malloc(sizeof(struct hashf));
    if(result==NULL) {
	fprintf(stderr, "out of memory.\n");
	exit(-1);
    }
    result->pathname=NewString(path);
    result->dev=statbuf.st_dev;
    result->ino=statbuf.st_ino;
    result->next=havefile[ind];
    havefile[ind]=result;
    if(flag) result->priority=1000000+(path[1]=='l'?1:0);
    else result->priority=0;
    return result;
}

static void ProcessList(fp)
FILE *fp;
{
    char filebuf[MAXPATHLEN+1];
    boolean defs=(fp!=stdin);
    while(!feof(fp) && fgets(rbuf, sizeof(rbuf)-1-sizeof(ext), fp)) {
	char *p=rindex(rbuf, '\n');
	FILE *fp2;
	if(p) *p='\0';
	if(rbuf[0]=='=') {
	    defs=FALSE;
	    continue;
	}
	if(rbuf[0]=='\0') continue;
	if(!defs) {
	    if(HaveIt(rbuf, FALSE)) continue;
	    strcpy(filebuf, rbuf);
	    strcat(filebuf, ext);
	    if(!AddClass(rbuf)) continue;
	    fp2=pathopen(buildpath, filebuf, "r");
	    if(fp2==NULL) {
		perror(filebuf);
		continue;
	    }
	    ProcessList(fp2);
	    fclose(fp2);
	} else {
	    HaveIt(rbuf, TRUE);
	}
    }
}

static void DumpStatl()
{
    FILE *sfp;
    int i;
    
    sfp=fopen("statl.c", "w");
    if(sfp==NULL) {
	perror("statl.c");
	exit(-1);
    }
    fprintf(sfp, "#include <class.h>\n#include <andrewos.h>\n#define class_StaticEntriesOnly\n#define class_StaticInfoOnly\n");
    for(i=0;i<HASHMAX;i++) {
	struct hasht *s=need[i];
	while(s) {
	    fprintf(sfp, "#include <%s.ih>\n", s->classname);	    
	    s=s->next;
	}
    }
    fprintf(sfp, "#undef class_StaticEntriesOnly\n#undef class_StaticInfoOnly\n\ndoStaticLoads() {\n");
    for(i=0;i<HASHMAX;i++) {
	struct hasht *s=need[i];
	while(s) {
	    fprintf(sfp, "%s_StaticEntry;\n", s->classname);
	    s=s->next;
	}
    }
    fprintf(sfp, "}\n");
    if(fclose(sfp)!=0) {
	fprintf(stderr, "genstatl: error writing statl.c...\n");
	exit(-1);
    }
}

static void DumpList(ifp, extn, ordering)
FILE *ifp;
char *extn;
FILE *ordering;
{
    int i;
    int c;
    boolean syslibs=FALSE;
    struct hashf *printing=NULL;
    for(i=0;i<HASHMAX;i++) {
	if (havefile[i]) {
	    struct hashf *s=havefile[i];
	    while(s) {
		struct hashf *t=s->next;
		free(s->pathname);
		free(s);
		s=t;
	    }
	    havefile[i]=NULL;
	}
    }
    if(ordering) {
	long priority=1;
	while(!feof(ordering) && fgets(rbuf, sizeof(rbuf)-1-sizeof(ext), ordering)) {
	    char filebuf[MAXPATHLEN+1];
	    char *p=rindex(rbuf, '\n');
	    char *q=rbuf;
	    struct hashf *result=NULL;
	    if(p) *p='\0';
	    if(rbuf[0]=='\0') continue;
	    p=index(rbuf, '/');
	    if(!p) continue;
	    filebuf[0]='\0';
	    if(q[0]=='-' && q[1]=='L') {
		strcat(filebuf, "-L");
		q+=2;
	    }
	    if(q[0]=='$') {
		q++;
		if(strncmp(q, "ANDREWDIR", 9)==0) {
		    strcat(filebuf, AndrewDirStr);
		} else if(strncmp(q, "XBASEDIR", 8)==0) {
		    strcat(filebuf, XBaseDirStr);
		} else if(strncmp(q, "AFSBASEDIR", 10)==0) {
		    strcat(filebuf, AFSBaseDirStr);
		} else {
		    char *v;
		    p[0]='\0';
		    v=(char *)getenv(q);
		    if(v) strcat(filebuf, v);
		    else p=(--q);
		}
	    } else p=q;
	    strcat(filebuf, p);
	    result=AddFile(filebuf, FALSE);
	    if(result) {
		if(result->priority==0) result->priority=(-(priority++));
		else result->priority=(-result->priority);
	    }
	}
    }
    for(i=0;i<HASHMAX;i++) {
	struct hasht *s=need[i];
	while(s) {
	    char filebuf[MAXPATHLEN+1], *p;
	    FILE *fp;
	    strcpy(filebuf, s->classname);
	    strcat(filebuf, extn);
	    fp=pathopen(buildpath, filebuf, "r");
	    if(fp==NULL) {
		perror(filebuf);
		goto skip;
	    }
	    filebuf[0]='\0';
	    p=filebuf;
	    while(!feof(fp)) {
		c=fgetc(fp);
		if(c==EOF) break;
		if(c==' ' || c=='\n') {
		    *p='\0';
		    if(*filebuf) AddFile(filebuf, TRUE);
		    p=filebuf;
		    *p='\0';
		} else {
		    *p=(char)c;
		    p++;
		}
	    }
	    if(*filebuf) AddFile(filebuf, TRUE);
	    fclose(fp);
	    skip:	    
	    s=s->next;
	}
    }
    for(i=0;i<HASHMAX;i++) {
	struct hashf *s=havefile[i];
	while(s) {
	    struct hashf *t, **u;
	    t=printing;
	    u=(&printing);
	    if(s->priority<0) {
	    } else if(printing==NULL) {
		printing=s;
		s->pnext=NULL;
	    } else {
		while(t) {
		    if(s->priority<=t->priority) {
			s->pnext=t;
			*u=s;
			break;
		    }
		    u=(&t->pnext);
		    t=t->pnext;
		}
		if(t==NULL) {
		    s->pnext=NULL;
		    *u=s;
		}
	    }
	    s=s->next;
	}
    }
    while(printing) {
	if(printing->pathname[0]=='-' && !syslibs) {
	    syslibs=TRUE;
	    fprintf(ifp, "\n\nSYSLIBS=");
	}
	fprintf(ifp, "\\\n %s",printing->pathname);
	printing=printing->pnext;
    }
    fprintf(ifp, "\n");
}


static void usage()
{

    fprintf(stderr, "usage: genstatl <directory> [-e <executable-name>] [-l <library-ordering-file>] [-m <main-object-files>]\n");
    exit(-1);
}

int main(argc, argv)
int argc;
char **argv;
{
    int i=0;
    FILE *ifp;
    FILE *orderfile;
    char *executablename="runapp";
    char *objects=NULL;
    char *directory=".";
    char *orderfilename= (char *)AndrewDir("/lib/genstatl/liborder");

    argv++;
    argc--;
    if(argc<=0) usage();
    while(argc>0) {
	if(argv[0][0]=='-') {
	    switch(argv[0][1]) {
		case 'e':
		    executablename=argv[1];
		    break;
		case 'l':
		    orderfilename=argv[1];
		    break;
		case 'm':
		    objects=argv[1];
		    break;
		default:
		    usage();
	    }
	    argv+=2;
	    argc-=2;
	} else {
	    directory=argv[0];
	    argv++;
	    argc--;
	}
    }

   orderfile=fopen(orderfilename, "r");
    if(orderfile==NULL) {
	perror(orderfilename);
	exit(-1);
    }


    if(chdir(directory)) {
	if(mkdir(directory, 0000777)!=0 || chdir(directory)!=0) {
	    perror(directory);
	    exit(-1);
	}
    }
       
    AndrewDirStr=(char *)AndrewDir("");
    XBaseDirStr=(char *)XBaseDir("");
    AFSBaseDirStr=(char *)getenv("AFSBASEDIR");
    if(AFSBaseDirStr==NULL) AFSBaseDirStr=AFSBASEDIR;
    
    buildpath=(char *)getenv("ATKBUILDPATH");
    if(buildpath==NULL) {
	strcpy(builddir, AndrewDirStr);
	strcat(builddir, "/build/");
	buildpath=builddir;
    }
    
    for(i=0;i<HASHMAX;i++) {
	need[i]=NULL;
	havefile[i]=NULL;
    }
    ProcessList(stdin);
    
    ifp=fopen("Imakefile", "w");
    if(ifp==NULL) {
	perror("Imakefile");
	exit(-1);
    }
    
    fprintf(ifp, "NormalObjectRule()\n");
    
    if(objects==NULL) {
	fprintf(ifp, "runapp.c: $(BASEDIR)/lib/genstatl/runapp.c\n");
	fprintf(ifp, "\tcp $(BASEDIR)/lib/genstatl/runapp.c .\n");
	fprintf(ifp, "clean::\n\t$(RM) runapp.c\n");
	objects="runapp.o";
    }
    
    fprintf(ifp, "OBJS=\\\n");
    DumpList(ifp, ".ols", NULL);
    
    fprintf(ifp, "\n\nLIBS=\\\n");
    DumpList(ifp, ".lls", orderfile);
    fprintf(ifp, "\n\n");
 
    
    fprintf(ifp, "ClassProgramTarget(%s, %s statl.o $(OBJS), $(LIBS), -L$(BASEDIR)/lib $(SYSLIBS) -lclass)\n", executablename, objects);
    
    if(fclose(ifp)!=0) {
	perror("Imakefile");
	exit(-1);
    }
    
    DumpStatl();
    
    fclose(orderfile);
    exit(0);
}
