/*
 *
 * 	file.c
 * 	directory and file management
 *
 * 	Modification :  03/05/94
 *
 *	Copyright (c) 1993,1994 Bruno RIVAS
 *	All Rights Reserved
 *
 *
 * Permission to use, copy, modify, distribute and sell this software and its
 * documentation for any purpose and without fee is hereby granted, provided
 * that the above copyright notice appears in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the name of Bruno RIVAS not be used in advertising
 * or publicity pertaining to distribution of the software without specific,
 * written prior permission.  Bruno RIVAS makes no representations about the
 * suitability of this software for any purpose.  It is provided "as is"
 * without express or implied warranty.
 *
 * Bruno RIVAS disclaims all warranties with regard to this software,
 * including all implied warranties of merchantability and fitness,
 * in no event shall Bruno RIVAS be liable for any special,
 * indirect or consequential damages or any damages
 * whatsoever resulting from loss of use, data or profits,
 * whether in an action of contract, negligence or other tortious
 * action, arising out of or in connection with the use 
 * or performance of this software.
 *
 *
 *
 *      libMfile.a  version 1.0
 *
 *	Internet:       rivas@email.teaser.com
 *	Surface Mail:   Bruno RIVAS,
 *			30 avenue Marie
 *			93250 Villemomble -- FRANCE
 *	Voice phone:    (33) (1) 49.35.97.27
 *	Fax: 		(33) (1) 48.94.27.91
 *
 */


#include <stdio.h>
#include <fcntl.h>
#include <sys/types.h>
#include <dirent.h>
#include <sys/stat.h>
#include <time.h>
#include <errno.h>
#include <unistd.h>

#include "file.h"


/*
 *
 * Obtention d'informations sur les fichiers
 *
 *
 */



int file_GetNumValidFiles(directory,mask)
char *directory;
char *mask;
{

  int i, j;
  int numfiles, validfiles;
  DIR *dir;
  struct dirent *entries;
  char **names;



  /*fprintf(stderr,"mask=%s\n",mask);*/
  if(mask==NULL||directory==NULL)
    return -1;


  dir=opendir(directory);
  if(dir==NULL)
    return -1;


  numfiles=validfiles=0;
  while((entries=readdir(dir))!=NULL)
  {
    numfiles++;
    if(VerifyMask(entries->d_name,mask)==0)
      validfiles++;
    
  }
  rewinddir(dir);
  return validfiles;
}




char **file_GetNames(directory,mask,numf)
char *directory;
char *mask;
int *numf;
{

  int i, j;

  int numfiles, validfiles;
  DIR *dir;
  struct dirent *entries;
  char **names;



  /*fprintf(stderr,"mask=%s\n",mask);*/
  if(numf==NULL||mask==NULL)
  {
    *numf=0;
    return NULL;
  }
  if(directory==NULL)
    directory="./";

  
  dir=opendir(directory);
  if(dir==NULL)
  {
    *numf=0;
    return NULL;
  }


  numfiles=validfiles=0;
  while((entries=readdir(dir))!=NULL)
  {
    /*fprintf(stderr,"entries=%s\n",entries->d_name);*/
    numfiles++;
    if(VerifyMask(entries->d_name,mask)==0)
      validfiles++;
    
  }
  rewinddir(dir);
  i=j=0;
  names=(char **)malloc(sizeof(char *)*validfiles);
  if(names==NULL)
  {
    *numf=0;
    return NULL;
  }

  j=0;
  if(validfiles>0) for(i=0;i<numfiles;i++)
  {
    entries=readdir(dir);
    if(entries==NULL)
    {
	*numf=0;
	return NULL;
    }
    /*fprintf(stderr,"j=%d ",j);*/
    if(VerifyMask(entries->d_name,mask)==0)
    {
      names[j]=(char *)malloc(strlen(entries->d_name)+2);
      /*fprintf(stderr,"j=%d ",j);*/
      if(names[j]==(char *)NULL)
      {
	*numf=0;
	fprintf(stderr,"malloc error \n");
	return NULL;
      }
      /*fprintf(stderr,"j=%d ",j);*/
      memset(names[j],0,strlen(entries->d_name)+2);
      names[j]=strcpy(names[j],entries->d_name);
      j=j+1;
    }
  }

/*  for(j=0;j<validfiles;j++)
    fprintf(stderr,"%s\n",names[j]);*/
  closedir(dir);

  *numf=validfiles;
  return names;

}





char **file_GetOrderedNames(directory,mask,numf)
char *directory;
char *mask;
int *numf;
{

  int i, j, k;

  int numfiles, validfiles;
  DIR *dir;
  struct dirent *entries;
  char **names;
  

  /*fprintf(stderr,"mask=%s\n",mask);*/
  if(numf==NULL)
  {
    *numf=0;
    return NULL;
  }

  if(mask==NULL)
  {
    *numf=0;
    return NULL;
  }
  if(directory==NULL)
    directory="./";

  dir=opendir(directory);
  if(dir==NULL)
  {
    *numf=0;
    return NULL;
  }


  numfiles=validfiles=0;
  while((entries=readdir(dir))!=NULL)
  {
    /*fprintf(stderr,"entries=%s\n",entries->d_name);*/
    numfiles++;
    if(VerifyMask(entries->d_name,mask)==0)
      validfiles++;
    
  }
  rewinddir(dir);
  i=j=0;

  names=(char **)malloc(sizeof(char *)*(validfiles+2));
  /*fprintf(stderr," passe %d  valid:%d\n",numfiles,validfiles);*/



  if(names==(char **)NULL)
  {
    *numf=0;
    return NULL;
  }

  j=0;
  if(validfiles>0) for(i=0;i<numfiles;i++)
  {
    entries=readdir(dir);
    if(entries==NULL)
    {
	*numf=0;
	return NULL;
    }
    /*fprintf(stderr,"j=%d ",j);*/
    if(VerifyMask(entries->d_name,mask)==0)
    {
      names[j]=(char *)malloc(strlen(entries->d_name)+2);
      /*fprintf(stderr,"j=%d ",j);*/
      if(names[j]==(char *)NULL)
      {
	*numf=0;
	fprintf(stderr,"malloc error \n");
	return NULL;
      }
      /*fprintf(stderr,"j=%d ",j);*/
      memset(names[j],0,strlen(entries->d_name)+2);
      names[j]=strcpy(names[j],entries->d_name);
      j=j+1;
    }
  }
  closedir(dir);

  /*if(validfiles>0) for(j=0;j<validfiles;j++)
    fprintf(stderr,"%s\n",names[j]);*/

  /*names[validfiles]=(char *)malloc(FILENAME_LENGTH+1);*/



  if(validfiles>=2) for(i=1;i<validfiles;i++)
  {
    /*fprintf(stderr,"i=%d ",i);*/
    for(j=i-1;j>=0;j--)
    {
        if(strcmp(names[i],names[j])<0)
	  goto ORDER_SUITE;
	if(strcmp(names[i],names[j])>=0)
  	{
 	  j++;
	  if(j>=0 && j<i)
	  {
	    /*fprintf(stderr,"Cas 1  decalage sur %d\n",j);*/
    	    names[validfiles]=names[i];	
    	    /*fprintf(stderr,"Avant boucle ");*/
    	    for(k=i-1;k>=j;k--)
    	      names[k+1]=names[k];	
    	    /*fprintf(stderr,"Apres boucle  ");*/
    	    names[j]=names[validfiles];	
    	    /*fprintf(stderr,"OK \n");*/
	    goto ORDER_SUITE2;
	  }
	  else goto ORDER_SUITE2;
	}
ORDER_SUITE:
    	j=j;
    }
ORDER_SUITE2:
    j=j;

  }

  /*if(validfiles>0) for(i=0;i<validfiles;i++)
    fprintf(stderr,"%s\n",names[i]);*/

  *numf=validfiles;
  return names;

}






FileInfo **file_GetInfos(directory,mask,numfiles)
char *directory, *mask;
int *numfiles;
{
  int seconde, minute, hour, day, month, year;
  int i, j, k, size;
  DIR *dir;
  struct dirent *entries;  
  struct stat buf;
  char **names;
  FileInfo **infos;
  char *fullname;
  struct tm *time;

  names=file_GetNames(directory,mask,numfiles);
  if(names==NULL)
    return NULL;
  
  if(*numfiles>0)
  {  
    infos=(FileInfo **)malloc(sizeof(FileInfo *)*(*numfiles));
    if(infos==NULL)
    {
      fprintf(stderr,"infos error\n");
      file_FreeNames(names,*numfiles);
      *numfiles=0;
      return NULL;
    }
    size=strlen(directory)+1+FILENAME_LENGTH+1;
    fullname=(char *)malloc(size);
    if(fullname==NULL)
    {
      fprintf(stderr,"full error\n");
      file_FreeNames(names,*numfiles);
      *numfiles=0;
      return NULL;
    }
    /*fprintf(stderr,"numinfosfiles:%d   size:%d\n",*numfiles,size);*/
    k=0;

    for(i=0;i<*numfiles;i++)    
    {
  	size=strlen(directory)+1+strlen(names[i])+2;
    	fullname=(char *)realloc(fullname,size);
        memset(fullname,0,size);
	sprintf(fullname,"%s/%s",directory,names[i]);
	j=stat(fullname,&buf);
	if(j==-1)
	{
   	  /*fprintf(stderr,"stat get error = -1\n");*/
	  free(names[i]);
	  goto GET_SUITE;
	}
	infos[k]=(FileInfo *)malloc(sizeof(FileInfo));
	if(infos[k]==(FileInfo *)NULL)
	{
   	  fprintf(stderr,"Memory error = -1\n");
	  file_FreeNames(names,*numfiles);
	  return (FileInfo **)NULL;
	}

	infos[k]->mode=(unsigned short)buf.st_mode;
	infos[k]->name=names[i];
	infos[k]->owner=(unsigned short)buf.st_uid;
	infos[k]->group=(unsigned short)buf.st_gid;
	infos[k]->size=buf.st_size;

	time=gmtime(&buf.st_mtime);
	infos[k]->year=(short)time->tm_year+1900;
	infos[k]->month=(short)time->tm_mon+1;
	infos[k]->day=(short)time->tm_mday;
	infos[k]->hour=(short)time->tm_hour;
	infos[k]->minute=(short)time->tm_min;
	infos[k]->seconde=(short)time->tm_sec;

        /*if((infos[i]->mode&S_IFDIR)==S_IFDIR)
 	  fprintf(stderr,"%-100s  %-6d  %-6d directory\n",fullname,infos[i]->size,infos[i]->mode);
	else fprintf(stderr,"%-100s  %-6d  %-6d fichier\n",fullname,infos[i]->size,infos[i]->mode);
	fprintf(stderr,"%02d/%02d/%04d ",infos[i]->day,infos[i]->month,infos[i]->year);
	fprintf(stderr,"%s\n ",infos[i]->name);*/
	
	k++;
	GET_SUITE:
	  i=i;
    }
    free(fullname);
    *numfiles=k;
    /*fprintf(stderr,"\n");*/

    free(names);
  }
  return infos;

}






FileInfo **file_GetOrderedInfos(directory,mask,numfiles)
char *directory, *mask;
int *numfiles;
{
  int seconde, minute, hour, day, month, year;
  int i, j, k, size;
  DIR *dir;
  struct dirent *entries;  
  struct stat buf;
  char **names;
  FileInfo **infos;
  char *fullname;
  struct tm *time;

  names=file_GetOrderedNames(directory,mask,numfiles);
  if(names==NULL)
    return NULL;
  
  
  if(*numfiles>0)
  {
    infos=(FileInfo **)malloc(sizeof(FileInfo *)*(*numfiles));
    if(infos==NULL)
    {
      fprintf(stderr,"infos error\n");
      file_FreeNames(names,*numfiles);
      *numfiles=0;
      return NULL;
    }
    size=strlen(directory)+1+FILENAME_LENGTH+1;
    fullname=(char *)malloc(size);
    if(fullname==NULL)
    {
      fprintf(stderr,"full error\n");
      file_FreeNames(names,*numfiles);
      *numfiles=0;
      return NULL;
    }
    /*fprintf(stderr,"numinfosfiles:%d   size:%d\n",*numfiles,size);*/
    k=0;
    if(*numfiles>0) for(i=0;i<*numfiles;i++)    
    {
	size=strlen(directory)+1+strlen(names[i])+2;
    	fullname=(char *)realloc(fullname,size);
        memset(fullname,0,size);
	sprintf(fullname,"%s/%s",directory,names[i]);
	j=stat(fullname,&buf);
	if(j==-1)
	{
	  /*fprintf(stderr,"ENOTDIR=%d  ENOENT=%d EACCES=%d EFAULT=%d EINTR=%d ENOLINK=%d\n",ENOTDIR,ENOENT,EACCES,EFAULT,EINTR,ENOLINK);*/
   	  fprintf(stderr,"%s  : stat get error = -1 errno=%d\n",names[i],errno);
	  free(names[i]);
	  goto GET_SUITE;
	}
	infos[k]=(FileInfo *)malloc(sizeof(FileInfo));
	if(infos[k]==(FileInfo *)NULL)
	{
   	  fprintf(stderr,"Memory error = -1\n");
	  file_FreeNames(names,*numfiles);
	  return (FileInfo **)NULL;
	}

	infos[k]->mode=(unsigned short)buf.st_mode;
	infos[k]->name=names[i];
	infos[k]->owner=(unsigned short)buf.st_uid;
	infos[k]->group=(unsigned short)buf.st_gid;
	infos[k]->size=buf.st_size;

	time=gmtime(&buf.st_mtime);
	infos[k]->year=(short)time->tm_year+1900;
	infos[k]->month=(short)time->tm_mon+1;
	infos[k]->day=(short)time->tm_mday;
	infos[k]->hour=(short)time->tm_hour;
	infos[k]->minute=(short)time->tm_min;
	infos[k]->seconde=(short)time->tm_sec;

        /*if((infos[i]->mode&S_IFDIR)==S_IFDIR)
 	  fprintf(stderr,"%-100s  %-6d  %-6d directory\n",fullname,infos[i]->size,infos[i]->mode);
	else fprintf(stderr,"%-100s  %-6d  %-6d fichier\n",fullname,infos[i]->size,infos[i]->mode);
	fprintf(stderr,"%02d/%02d/%04d ",infos[i]->day,infos[i]->month,infos[i]->year);
	fprintf(stderr,"%s\n ",infos[i]->name);*/
	
	k++;
	GET_SUITE:
	  i=i;
    }
    free(fullname);

    *numfiles=k;
    free(names);
  }
  return infos;

}






int VerifyMask(name,mask)
char *name, *mask;
{
  int i, j;
  int length;

  i=j=0;
  if(mask==NULL||name==NULL||strlen(mask)==0||strlen(name)==0)
    return -1;

  length=strlen(mask);
  for(i=0;i<length;i++)
  {
     /*fprintf(stderr,"%c",mask[i]);*/
     if(j>=strlen(name))
       return -1;

     if(mask[i]==name[j])
	goto VERIFY_SUITE;

     if(mask[i]!=name[j]&&mask[i]=='*')
     {
	if(i+1<strlen(mask)&&mask[i+1]=='.')
	{
	  if(j+1>=strlen(name))
	    return -1;
	  j++;
	  while(j<strlen(name)&&name[j]!='.')
	    j++;
	  if(j>=strlen(name)) return -1;
	  j--;
	  goto VERIFY_SUITE;
	}
	else return 0;
     }
     else return -1;

VERIFY_SUITE:
    j++;
  }
  if(j<strlen(name))
    return -1;
  return 0;
}





int file_VerifyAccess(name)
char *name;
{
  return access(name,F_OK);
}





char *file_ExtractPath(path)
char *path;
{
  char *pathnm;
  int length;
  int i, j;
  struct stat buf;


  /*fprintf(stderr,"Origine: %s  ",path);*/

  if(path==NULL||strlen(path)==0)
    return NULL;
  
  length=strlen(path);
  if(length==0)
    return NULL;

  /*fprintf(stderr,"malloc %d ",length);*/
  pathnm=(char *)calloc(length+1,1);
  if(pathnm==NULL)
  {
    fprintf(stderr,"Erreur memoire\n");
    return NULL;
  }
  /*fprintf(stderr,"strcpy ");*/
  strcpy(pathnm,path);


  if(file_VerifyAccess(path)==0)
  {
    /*fprintf(stderr,"dans Verify - ");*/
    if(file_IsDirectory(path)==1)
    {
      /*fprintf(stderr,"Is directory - ");*/
      return pathnm;
    }
    else
    {
      if(path[length-1]=='/')
      {
        pathnm[length-1]=0;
        /*fprintf(stderr,"path: %s  \n",pathnm);*/
        return pathnm;
      }
      i=length-1;
    }
  }  
  else i=length-1;


  while(i>=0&&path[i]!='/')
    i--;

EXTRACT_SUITE:
  /*fprintf(stderr,"i=%d\n",i);*/

  if(i<0)
    pathnm[0]=0;
  else if(i==0)
    pathnm[1]=0;
  else pathnm[i]=0;

  /*fprintf(stderr,"path: %s\n",pathnm);*/
  return pathnm;
}




char *file_ExtractName(path)
char *path;
{
  char *name;
  int length;
  int i, j;
  struct stat buf;


  /*fprintf(stderr,"Origine: %s  ",path);*/
  if(path==(char *)NULL || strlen(path)==0)
    return (char *)NULL;
  
  length=strlen(path);
  if(length==0)
    return (char *)NULL;
  
  i=length-1;
  while(i>=0 && path[i]!='/')
    i--;


  name=(char *)malloc(length-i+2);
  if(name==(char *)NULL)
    return (char *)NULL;
  memset(name,0,length-i+2);
  strcpy(name,path+i+1);

  fprintf(stderr,"name:%s----\n",name);
  return (char *)name;
}





int file_IsDirectory(name)
char *name;
{
  struct stat buf;
  int ret;

  ret=stat(name,&buf);
  if(ret==-1)
    return -1;
  if((buf.st_mode&S_IFDIR)==S_IFDIR && (buf.st_mode&S_IFCHR)!=S_IFCHR &&  (buf.st_mode&S_IFBLK)!=S_IFBLK)
    return 1;
  else return 0;
}



int file_IsSpecial(name)
char *name;
{
  struct stat buf;
  int ret;

  ret=stat(name,&buf);
  if(ret==-1)
    return -1;
  if((buf.st_mode&S_IFCHR)==S_IFCHR)
    return 1;
  if((buf.st_mode&S_IFBLK)==S_IFBLK)
    return 1;
  else return 0;
}


int file_IsNormal(name)
char *name;
{
  struct stat buf;
  int ret;

  ret=stat(name,&buf);
  if(ret==-1)
    return -1;
  if((buf.st_mode&S_IFDIR)!=S_IFDIR && (buf.st_mode&S_IFCHR)!=S_IFCHR && (buf.st_mode&S_IFBLK)!=S_IFBLK )
    return 1;
  else return 0;
}





int file_FreeNames(names,numfiles)
char **names;
int numfiles;
{
  int i;

  for(i=0;i<numfiles;i++)
    free(names[i]);
  free(names);

}



int file_FreeInfos(infos,numfiles)
FileInfo **infos;
int numfiles;
{
  int i;

  for(i=0;i<numfiles;i++)
  {
    free(infos[i]->name);
    free(infos[i]);
  }
  free(infos);

}










/* 
 *
 * Modification des fichiers
 *
 *
 */



int file_Delete(name)
char *name;
{
  return unlink(name);
}






/*
 *
 * Fonctions sur les r‚pertoires
 *
 *
 */



TreeInfo **dir_GetTree(directory,numdirs)
char *directory;
int *numdirs;
{
  int i, j;

  if(directory==NULL||numdirs==NULL)
    return NULL;
  
  if(file_VerifyAccess(directory)==-1)
    return NULL;
  
}



int dir_FreeTree(tree,numdirs)
TreeInfo **tree;
int numdirs;
{
  int i;

  if(tree==NULL)
    return -1;

  for(i=0;i<numdirs;i++)
  {
    free(tree[i]->name);
    free(tree[i]);
  }
  free(tree);
  return 0;
}



char *dir_GetCurrent()
{
  char *path;
  int size;
  
  size=32;
START_GETCWD:
  path=(char *)getcwd((char *)NULL,size);
  if(path==NULL&&errno==ERANGE)
  {  
    size=size+16;
    goto START_GETCWD;
  }
  return (char *)path;
}





int dir_SetCurrent(directory)
char *directory;
{
  return chdir(directory);
}





/*
 *
 *
 *  Main
 *
 *
 */


/*
main(argc,argv)
int argc;
char **argv;
{
  FileInfo **infos;
  int numfiles;
  int i;
  char *name;
  char *path;
  char **names;

  if(argc<=1)
  {
    fprintf(stderr,"Pas d'argument\n");
    exit(-1);
  }
  name=file_ExtractName(argv[1]);
  fprintf(stderr,"Name=%s\n",name);
  path=file_ExtractPath(argv[1]);
  fprintf(stderr,"Path=%s\n",path);
  fprintf(stderr,"dir_set: %d\n",dir_SetCurrent(path));
  fprintf(stderr,"access: %d\n",file_VerifyAccess(path));
  names=file_GetOrderedNames(path,name,&i);
  fprintf(stderr,"numfiles=%d\n",i);
  if(names!=NULL)
    file_FreeNames(names,i);
  if(path!=NULL)
    free(path);
  if(name!=NULL)
    free(name);
  exit(0);
}
*/





