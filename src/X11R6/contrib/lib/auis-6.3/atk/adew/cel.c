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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/adew/RCS/cel.c,v 2.25 1994/04/17 22:06:26 rr2b Exp $";
#endif


 

#include <andrewos.h>
#include <class.h>
#include <ctype.h>

#include <dataobj.ih>
#include <dict.ih>
#include <proctbl.ih>
#include <envrment.ih>
#include <text.ih>
#include <filetype.ih>
#include <observe.ih>
#include <arbiter.ih>
#include <cel.eh>

#define VALUE 10
static long viewID = 0;


void cel__ObservedChanged(self, changed, value)
struct cel *self;
struct observable *changed;
long value;
{
    if(changed==(struct	observable *)self->dataObject) {
	
	if(value==observable_OBJECTDESTROYED) {
	    self->dataObject=NULL;
	    cel_Destroy(self);
	} else cel_NotifyObservers(self,value);
    }
    super_ObservedChanged(self, changed, value);
}

short cel__Get(self,property, type, rock)
struct cel *self;
struct atom *property;
struct atom **type;
long *rock;
{
    short result=super_Get(self,property,type,rock);
    if(self->dataObject && !result)
	return dataobject_Get(self->dataObject, property, type, rock);
    else return result;
}

void cel__FinalizeObject(classID,self)
struct classheader *classID;
struct cel *self;
{
    if(self->dataObject) { dataobject_RemoveObserver(self->dataObject,self);
       dataobject_Destroy(self->dataObject);
       self->dataObject=NULL;
    }
    
    /* cel_NotifyObservers(self,observable_OBJECTDESTROYED);
      this doesn't appear to be needed and this should never be
      done since observable will take care of notifying observers
      of the destruction */
    if(self->chain != self){
	struct cel *nlink;
	if(self->ab == NULL || self->ab->first == (struct cel *) self->ab) return;
	nlink = self->ab->first;
	if(nlink == self){
	    self->ab->first = self->chain;
	}
	else for(; nlink != NULL && nlink != nlink->chain ; nlink = nlink->chain){
	    if(nlink->chain == self){
		nlink->chain = self->chain;
		break;
	    }
	}
    }
}

static SetVisible(self)
struct cel *self;
{
    cel_SetVisible(self);
}
static SetInvisible(self)
struct cel *self;
{
    cel_SetInvisible(self);
}
boolean cel__InitializeObject(classID, self)
struct classheader *classID;
struct cel *self;
{
    self->refname = NULL;
    self->viewType = NULL;
    self->dataType = NULL;
    self->dataatm = NULL;
    self->viewatm = NULL;
    self->refatm = NULL;
    self->linkatm = NULL;
    self->linkname = NULL;
    self->viewID = viewID++;
    self->dataObject = NULL;
    self->desw = self->desh = 0;
    self->script = NULL;
    self->application = FALSE;
    self->script = NULL;
    self->readfromfile = 0;
    self->usedefaultview = 0;
    self->mode = cel_VISIBLE;
    cel_SetDefaultStream(self,NULL);
    cel_SetInitFile(self,NULL);
    cel_SetWriteChild(self,TRUE);
    self->count = 0;
    self->chain = self;
    self->ab = NULL;
    return TRUE;
}

struct cel *cel__Create(classID, viewType, dataObject)
struct classheader *classID;
    char *viewType;
    struct dataobject *dataObject;
{
    struct cel *newvr;
    
    if ((newvr = cel_New()))  {
	if ((newvr->viewatm = atom_Intern(viewType))!= NULL) {
	    newvr->viewType = atom_Name(newvr->viewatm);
	    newvr->dataObject = dataObject;
	    dataobject_Reference(dataObject);
	    dataobject_AddObserver(dataObject,newvr);
	    
	    return newvr;
	}
    }
    fprintf(stderr, "Could not allocate cel structure - exiting\n");
    exit(1);
}
char *cel__SetRefName(self,refname)
struct cel *self;
char *refname;
{
    if(refname){
	if((self->refatm = atom_Intern(refname)) != NULL)
	    self->refname = atom_Name(self->refatm);
	}
    return self->refname;
}
void cel__UnsetRefName(self)
struct cel *self;
{
    if(self->refname && *self->refname){
	free(self->refname);
	self->refname = NULL;
    }
}
struct dataobject *cel__GetObject(self)
struct cel *self;
{
    return (self->dataObject);
}
boolean cel__SetChildObject(self,newobject,viewName)
struct cel *self;
struct dataobject *newobject;
char *viewName;
{
    if(viewName == NULL || *viewName == '\0')
	cel_SetViewName(self,viewName,TRUE);
    else cel_SetViewName(self,viewName,FALSE);
    cel_SetObject(self,newobject);
    cel_SetRefName(self,self->dataType);
    return TRUE;
}
long cel__GetModified(self)
register struct cel *self;
{
    register long mod = super_GetModified(self);
    if(self->NoSave) return mod;
    if(self->dataObject)
	mod += dataobject_GetModified(self->dataObject);
    if(self->script)
	mod += text_GetModified(self->script);
    return mod;
}
boolean cel__SetObject(self,newobject)
struct cel *self;
struct dataobject *newobject;
{
    if(newobject){
	newobject->id = dataobject_UniqueID(newobject); 
	/* 	    Register the object with the dictionary */
	dictionary_Insert(NULL,(char *)newobject->id,(char *) newobject);
	self->dataObject = newobject;
	
	dataobject_Reference(newobject);
	dataobject_AddObserver(newobject,self);
	
	
	cel_SetObjectName(self,class_GetTypeName(newobject));
	if(self->usedefaultview)
	    cel_SetViewName(self,NULL,TRUE);
	if(strcmp(self->dataType,"value")== 0) 
	    self->application =  VALUE;
	return TRUE;
    }
    return FALSE;
}
boolean cel__SetObjectByName(self,dataname)
struct cel *self;
char *dataname;
{
    struct dataobject *newobject;
    if((dataname == NULL || *dataname == '\0')) return FALSE;
    if((newobject = (struct dataobject *) class_NewObject(dataname)) != NULL) {
	cel_SetObject(self,newobject);
	dataobject_Destroy(newobject); /* won't really destroy since SetObject got a ref. */
	return TRUE;
    }
    return FALSE;
}
void cel__SetObjectName(self,dataname)
struct cel *self;
char *dataname;
{
    if(dataname && *dataname && 
	(self->dataatm = atom_Intern(dataname))!= NULL) {
	self->dataType = atom_Name(self->dataatm);
	if(strcmp(self->dataType,"value")== 0) 
	    self->application =  VALUE;
    }
}
    
void cel__SetViewName(self,viewname,usedefaultview)
struct cel *self;
char *viewname;
int usedefaultview;
{
    if(viewname && *viewname){
	self->usedefaultview = FALSE;
    }
    else {
	self->usedefaultview = usedefaultview;
	if(usedefaultview && self->dataObject)
	   viewname = dataobject_ViewName(self->dataObject);
    }
    if (viewname && *viewname && 
	 (self->viewatm = atom_Intern(viewname))!= NULL) {
	    self->viewType = atom_Name(self->viewatm);
    }
}
void cel__SetLinkName(self,linkname)
struct cel *self;
char *linkname;
{
    if (linkname && *linkname && 
	 (self->linkatm = atom_Intern(linkname))!= NULL) {
	    self->linkname = atom_Name(self->linkatm);
    }
    else{
	self->linkatm = NULL;
	self->linkname = NULL;
    }
}
void cel__SetApplication(self,app)
struct cel *self;
int app;
{
    if(self->dataType != NULL){
	if(app != cel_VALUE && strcmp(self->dataType,"value") == 0) {
	self->application = cel_VALUE;
	return ;
	}
    }
    self->application = app;
}

void cel__InsertObject (self,newobject, dataname,viewname,usedefaultview)
struct cel *self;
struct dataobject *newobject;
char *dataname;
char *viewname;
int usedefaultview;
{
    char buf[128];
    if(newobject != NULL){
	dataname = class_GetTypeName(newobject);
    }
    else if((dataname == NULL || *dataname == '\0')) /* no object */;
    else{
	if((newobject = (struct dataobject *) class_NewObject(dataname)) == NULL) {
	    if(self->application == VALUE){
		if((newobject = (struct dataobject *) class_NewObject("value")) == NULL) return;
		if(viewname == NULL || *viewname == '\0'){
		    viewname = buf;
		    sprintf(buf,"%sV",dataname);
		}
	    }
	    else return;
	}
    }
    if(newobject){
	newobject->id = dataobject_UniqueID(newobject); 
	/* 	    Register the object with the dictionary */
	dictionary_Insert(NULL,(char *)newobject->id,(char *) newobject);
	self->dataObject = newobject;

	dataobject_AddObserver(newobject,self);
	
    }
  
 
    if(dataname && *dataname){
	 if ((self->dataatm = atom_Intern(dataname))!= NULL) {
	    self->dataType = atom_Name(self->dataatm);

	}
    }
    if(self->dataType && (strcmp(self->dataType,"value")== 0)) 
	self->application =  VALUE;
#ifdef DEBUG
printf("Initing v = %s, d = %s, r = %s\n",self->viewType,self->dataType,self->refname);
#endif /* DEBUG */
}
struct atom *getline(place)
char **place;
{
    char tmpbuf[512];
    char *c = tmpbuf;
    char *buf;
    buf = *place;
#ifdef DEBUG
printf("GETLINE GOT ---- %s XXXXXXX\n",*place);
#endif /* DEBUG */
/* printf("Getting line from %s\n",buf); */
    if(buf == NULL || *buf == '\0'){
	*c = '\0';
	return NULL;
    }
    if(*buf == '>' && strchr(buf,'<') != NULL) {
	buf = strchr(buf,'<');
	buf++;
    }
    while(*buf == ' ') buf++;
    while((*c = *buf++) != '\n') {
/*	putchar(*c); */
	if(*c++ == '\0') return NULL;
    }
    *c = '\0';
    *place = buf;
    return atom_Intern(tmpbuf);
}
long cel__ReadSup(self, file, id)
    struct cel *self;
    FILE *file;
    long id;
{
	return dataobject_NOREADERROR;
    }
long cel__ReadFile(self,thisFile)
struct cel *self;
FILE *thisFile;
{  
    int objectID;
    long result;
    char *objectName;
    objectName = filetype_Lookup(thisFile, NULL, &objectID, NULL); /* For now, ignore attributes. */
    if(objectName == NULL) objectName = "text";
    if(class_IsTypeByName("cel",objectName) || class_IsTypeByName(objectName,"cel")){ 
	result = cel_Read(self,thisFile,objectID);
/* 	if(self->arb) arbiterview_InitCell(self->arb,self); */
    }
    else{

	if(/* objecttest(self,objectName,"dataobject") && */ cel_SetObjectByName(self,objectName) && cel_GetObject(self) != NULL){
	    char *nm = NULL;
	    struct cel *arb = NULL;
	    cel_SetViewName(self,NULL,TRUE);
	    cel_SetRefName(self,"");
	    result = dataobject_Read(cel_GetObject(self),thisFile,objectID);
	    if(strcmp(objectName,"arbiter") == 0){
		arb = (struct cel *) cel_GetObject(self);
		if((nm = cel_GetRefName(arb)) == NULL || *nm == '\0'){
		    cel_SetRefName(arb,objectName);
		}
		cel_SetRefName(self,"arbcel");
	    }
	    else{
		cel_SetRefName(self,objectName);
	    }
	}
    }
    cel_NotifyObservers(self,0);
    return result;
}

long cel__Read(self, file, id)
    struct cel *self;
    FILE *file;
    long id;
{
    long endcount = 1;
    boolean begindata;
    char *s;
    long c;
    struct arbiter *master;
    long status;
    char objectname[200],*cp;
    long objectid;
    struct dataobject *newobject = NULL;
    char cbuf[2048];
    char *buf;
    long textpending;
    long did,version = 0l;
    did = 0l;
    textpending = 0;
    buf = cbuf;
/* printf("In Cel Read\n"); */
    self->count++;
    cel_SetID(self,cel_UniqueID(self));/* change id to unique number */
    while (endcount != 0)  {
        while ((c = getc(file)) != EOF && c != '\\')  {
	    if(endcount == 1){
		/* Place actual read code here */
	    *buf++ = c;
	    }
        }
        if (c == EOF) return dataobject_NOREADERROR;
        if ((c = getc(file)) == EOF)
            return dataobject_PREMATUREEOF;
        if (c == 'b')  {
            begindata = TRUE;
            s = "egindata";
        }
        else if (c == 'e')  {
            begindata = FALSE;
            s = "nddata";
        }
        else  {
	    if(endcount == 1){
		/* Place handling of \x characters here */
		if(c == 'V') {
		    version = 0;
		    while ((c = getc(file)) != EOF && c != '\n')
			if(isdigit(c)) version = (version * 10) + (c - '0');
		    if (c == EOF) return dataobject_NOREADERROR;
		    if((status = cel_ReadSup(self, file, id)) != dataobject_NOREADERROR){
			return status;
		    }
		}
	    }
            continue;
        }
        while ((c = getc(file)) != EOF && c == *s) s++;
        if (c == '{' && *s == '\0')  {
            if (begindata) {
                s = objectname;
                while ((c = getc(file)) != EOF && c != ',')
                    *s++ = c;
                if (c == EOF) return dataobject_PREMATUREEOF;
                *s = '\0';
                objectid = 0;
                while ((c = getc(file)) != EOF && c != '}')
                    if(c >= '0' && c <= '9')objectid = objectid * 10 + c - '0';
                if (c == EOF) return dataobject_PREMATUREEOF;
		if((c = getc(file))!= '\n' || (strcmp(objectname,"zip") == 0)) ungetc(c,file);
                /* Call the New routine for the object */
		if( buf == cbuf && endcount == 1 && version == 0 && id == 0 &&
		   (strcmp(objectname,class_GetTypeName(self)) == 0) ){
		    /* reading the begindata for this object */
		    id = 1;
		    continue;
		}
		cp=objectname;
		if(!class_Load(cp)) {
		    cp="unknown";
		}
                if ((newobject = (struct dataobject *) class_NewObject(cp)))  {
                    /* Register the object with the dictionary */
		    dictionary_Insert(NULL,(char *)objectid, (char *)newobject);
		    /* Call the read routine for the object */
		    dataobject_UnReference(newobject);
                    status = dataobject_Read(newobject, file, objectid);
		    if (status != dataobject_NOREADERROR) {
			printf("ERROR reading %s, %d\n",objectname,status);
			return status;
		    }
		}
                else {
                    endcount += 1;
		    /* return dataobject_OBJECTCREATIONFAILED; */
		}

	    }
            else  {
                endcount -= 1;
                while ((c = getc(file)) != EOF && c != '}');
		if((c = getc(file))!= '\n') ungetc(c,file);
            }
        }
        else if(endcount == 1){
	    
        /* 	    Place Handling of characters following \  
           */	
	    *buf++ = c;
	}
    }
/*    cp = strchr(cbuf,'\n'); cp++; */
    cp = cbuf;
    *buf = '\0';
    if (*cp == '\n') cp++;
    switch(version){
	case 2:
	    sscanf(cp,"%d %ld %d %d %d %d\n" ,&(self->application),
		   &did,&textpending,&(self->desw), &(self->desh),&(self->mode));
	    break;
	default:
	    sscanf(cp,"%d %ld %d %d %d\n" ,&(self->application),
		   &did,&textpending,&(self->desw), &(self->desh));
    }
    while (*cp != '\n') cp++;
    cp++;

    if((self->dataatm = getline(&cp)) != NULL)self->dataType = atom_Name(self->dataatm);
    else self->dataType = NULL;
    if((self->viewatm = getline(&cp)) != NULL) self->viewType =atom_Name(self->viewatm);
    else self->viewType = NULL;
    if((self->refatm = getline(&cp)) != NULL) self->refname =atom_Name(self->refatm);
    else self->refname = NULL;
    if((self->linkatm = getline(&cp)) != NULL) self->linkname =atom_Name(self->linkatm);
    else self->linkname = NULL;
    *buf = '\0';
    if(textpending){
	self->script = (struct text *) newobject;
	textpending = FALSE;
    }
    self->readfromfile = TRUE;
#ifdef DEBUG
   printf("<%s>\n",cbuf);
   printf("['%s' '%s' '%s' %ld]\n",self->dataType,self->viewType,self->refname,did); fflush(stdout); 
#endif /* DEBUG */
   
    if(did) {
	self->dataObject = (struct dataobject *) dictionary_LookUp(NULL,(char *) did);
	dataobject_Reference(self->dataObject);
	dataobject_AddObserver(self->dataObject,self);
    }
    else if((self->linkname == NULL  || *(self->linkname) == '\0') && self->dataType != NULL){
	if(self->viewType == NULL || *(self->viewType) == '\0')
	    self->usedefaultview = TRUE;
	cel_SetObjectByName(self,self->dataType);
    }
/*    registerobject(self); Assume arbiter will do this when view is linked.
      Unfortunately, this meens it will be unavailable until then .
      Maybe the arbiter should save a list of the objects and names */
#ifdef DEBUG
    printf("dobj = %d\n",self->dataObject);
#endif /* DEBUG */
    if(self->dataObject == NULL) self->count = 0;
    if((self->ab = arbiter_GetMaster()) != NULL)
	arbiter_DeclareRead(self->ab,self);
    return dataobject_NOREADERROR;
}

long cel__WriteLink(self,file ,writeid,level)
struct cel *self;
FILE *file;
long writeid;
int level;
{
    long val;
    struct dataobject *dob;
    dob = self->dataObject;
    self->dataObject = NULL;
    val = cel_Write(self,file ,writeid,level);
    self->dataObject = dob;
    return val;
}
long cel__WriteSup(self,file ,writeid,level)
struct cel *self;
FILE *file;
long writeid;
int level;
{
return TRUE;
}
long cel__Write(self,file ,writeid,level)
struct cel *self;
FILE *file;
long writeid;
int level;
{
    long did;
    did = 0l;
    if (self->header.dataobject.writeID == writeid)  return cel_GetID(self);
    if(level == 0 && (self->refname==NULL || *(self->refname) == '\0') && self->script == NULL && 
	self->application == cel_APPLICATION && 
	self->dataObject != NULL && 
	strcmp(class_GetTypeName(self),"arbiter") == 0 &&
	class_IsTypeByName(class_GetTypeName(self->dataObject),"text") &&
	( text_GetExportEnvironments((struct text *) self->dataObject) == FALSE ||  
	 environment_NumberOfChildren(((struct text *) self->dataObject)->rootEnvironment) <= 0))
	/* don't write out self over plain text */
	level--;
    if(level != -1){
	self->header.dataobject.writeID = writeid;
	fprintf(file,"\\begindata{%s,%ld}\n",class_GetTypeName(self),cel_GetID(self));
	fprintf(file,"\\V 2\n"); /* Version Number */
	cel_WriteSup(self,file ,writeid,level);
	if(self->WriteChild == FALSE){
	    fprintf(file,"1 0 0 0 0 0 \n>OBJ< \n>VIEW< \n>REF< \n");	    fprintf(file,"\\enddata{%s,%ld}\n",class_GetTypeName(self),cel_GetID(self));
	    return cel_GetID(self);
	}
    }
#ifdef WOULDWORKBUT
    if(self->dataObject) did = dataobject_Write(self->dataObject,file,writeid,level+1);
#else /* WOULDWORKBUT */
    if(self->dataObject){dataobject_Write(self->dataObject,file,writeid,level+1); did = dataobject_UniqueID(self->dataObject);}
#endif /* WOULDWORKBUT */
    if(level != -1){
	if(self->linkname)
	    fprintf(file,"%d %ld %d %d %d %d \n>OBJ< %s\n>VIEW< %s\n>REF< %s\n>LINK< %s\n" ,self->application,
		    did,(self->script != NULL),self->desw, self->desh,self->mode,(self->dataType)?self->dataType:"", (self->viewType)?self->viewType:"",(self->refname)?self->refname:"",(self->linkname)?self->linkname:"");
	else 
	    fprintf(file,"%d %ld %d %d %d %d \n>OBJ< %s\n>VIEW< %s\n>REF< %s\n" ,self->application,
		    did,(self->script != NULL),self->desw, self->desh,self->mode,(self->dataType)?self->dataType:"", (self->viewType)?self->viewType:"",(self->refname)?self->refname:"");
	if(self->script){
	    text_Write(self->script,file,writeid,level+1);
	}
	fprintf(file,"\\enddata{%s,%ld}\n",class_GetTypeName(self),cel_GetID(self));
    }
    return cel_GetID(self);
}
void cel__SetVisibilityBit(self,mode)
struct cel *self;
int mode;
{
    if(mode != self->mode){
	self->mode = mode;
	cel_NotifyObservers(self,0);
    }
}
boolean cel__InitializeClass(classID)
struct classheader *classID;
{
    proctable_DefineProc("cel-set-visible", SetVisible,&cel_classinfo,NULL, "Make cel visible");
    proctable_DefineProc("cel-set-invisible", SetInvisible,&cel_classinfo,NULL, "Make cel invisible");
    return TRUE;
}

static searchatt(self,attname,len)
struct cel *self;
char *attname;
long *len;
{
    long tlen,i,attlen,j;
    attlen = strlen(attname);
    tlen = text_GetLength(self->script);
    for(i = 0; ((i = text_Index(self->script,i,'<',tlen - i)) != EOF);){
	i++;
	if((i + attlen < tlen) && (text_GetChar(self->script,i + attlen) == '>') && text_Strncmp(self->script,i,attname,strlen(attname)) == 0){
	    i += attlen;
	    j = text_Index(self->script,i,'(', tlen - i);
	    if(j != EOF) {
		j++;
		i = text_Index(self->script,j,')', tlen - j);
		if(i == EOF) return -1;
		*len = i - j;
		return j;
	    }
	}
    }
    return -1;
}
void cel__SetStringAtt(self,attname,attval)
struct cel *self;
char *attname,*attval;
{
    char buf[256];
    long i,len;
    if(self->script == NULL) self->script = text_New();
    if( (i = searchatt(self,attname,&len)) >= 0){
	if(attval == NULL) text_DeleteCharacters(self->script,i,len);
	else text_ReplaceCharacters(self->script,i,len,attval,strlen(attval));
    }
    else if(attval != NULL){
	sprintf(buf,"[string] <%s> (%s)\n",attname,attval);
	text_InsertCharacters(self->script,0,buf,strlen(buf));
    }
    cel_NotifyObservers(self,cel_NeedsRepost);
}
void cel__SetLongAtt(self,attname,val)
struct cel *self;
char *attname;
long val;
{
    char buf[256],attval[64];
    long i,len;
    if(val == cel_UNDEFINEDVALUE) *attval = '\0';
    else sprintf(attval,"%ld",val);
    if(self->script == NULL) self->script = text_New();
    if( (i = searchatt(self,attname,&len)) >= 0){
	text_ReplaceCharacters(self->script,i,len,attval,strlen(attval));
    }
    else if(val != cel_UNDEFINEDVALUE){
	sprintf(buf,"[long] <%s> (%s)\n",attname,attval);
	text_InsertCharacters(self->script,0,buf,strlen(buf));
    }
    cel_NotifyObservers(self,cel_NeedsRepost);
}
long cel__GetLongAtt(self,attname)
struct cel *self;
char *attname;
{
    long i,len;
    char buf[256],*c;
    if(self->script == NULL) return 0;
    if( (i = searchatt(self,attname,&len)) >= 0){
	if(len > 255) len = 255;
	if(len == 0) return cel_UNDEFINEDVALUE;
	for(c = buf; len; len--,i++) *c++ = text_GetChar(self->script,i);
	*c = '\0';
	return(atoi(buf));
    }
    return cel_UNDEFINEDVALUE;
}
char * cel__GetStringAtt(self,attname,buf,buflen)
struct cel *self;
char *attname,*buf;
long buflen;
{
    char *c;
    long i,len;
    if(self->script == NULL) return NULL;
    if( (i = searchatt(self,attname,&len)) >= 0){
	if(len >= buflen) len = buflen - 1;
	for(c = buf; len; len--,i++) *c++ = text_GetChar(self->script,i);
	*c = '\0';
	return(buf);
    }
    return NULL;
}
void cel__FinializeObject(classID, self)
struct classheader *classID;
struct cel *self;
{
    
}
long cel__InitDefault(self)
struct cel *self;
{
    
    FILE *f;
    long ret;
    char fnm[20];
    self->count++;
    if(self->initfile && *self->initfile &&
	(f = fopen(self->initfile,"r")) != NULL){
	ret = cel_ReadFile(self,f);
	fclose(f);
	return ret;
    }
    if(self->defaultStream){
	strcpy(fnm, "/tmp/celXXXXXX");
	mktemp(fnm);
	/* need to generate tmpfile name */
	if((f = fopen(fnm,"w")) == NULL){
	    fputs("Can't write init file for cel in /tmp\n",stderr);
	    fflush(stderr);
	    self->count--;
	    return dataobject_OBJECTCREATIONFAILED;
	}
	fwrite(self->defaultStream,strlen(self->defaultStream),1,f);
	fclose(f);
	f = fopen(fnm,"r");
	ret = cel_ReadFile(self,f);
	fclose(f);
	unlink(fnm);
	return ret;
    }
    return dataobject_OBJECTCREATIONFAILED;

}
#if 0
void cel__ClearChain(self)
struct cel *self;
{
    struct cel *nlink;
    
    nlink = self->chain;
    while(self != nlink){
	self->chain = self;
	self = nlink;
 	nlink = self->chain;
   }
}
#endif
