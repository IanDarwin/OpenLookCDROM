#include <stdio.h>
#include <dirent.h>
#include <sys/types.h>
#include <sys/stat.h>

class file_or_directory {
public:   
     char* name;
     enum {is_file,is_directory} type;
     virtual ~file_or_directory() {delete name;}
};

 class dirlist {
   file_or_directory* first;
   dirlist* rest;
   public:
     file_or_directory* car(){ return first;}
     dirlist* cdr(){ return rest;}

     dirlist(file_or_directory* a, dirlist* b) {
       first=a; rest=b;}
     dirlist() { first = 0; rest = 0;}
     ~dirlist() { 
       delete first; 
       if (cdr()) delete cdr();
     }

};

class file : public file_or_directory {
public:
     FILE* open (char* mode="r") {return fopen(name,mode);}
     file(char* filename) { name = strdup(filename); type = is_file; }
       
};

class directory : public file_or_directory {
     dirlist *contents;
     int expanded;
   public:
      directory(char* dirname,int expand) {
	contents = 0;
	name = strdup(dirname);
	type = is_directory;
	if (expand) {
	  expand_directory();
	}
      }
     ~directory() {if(contents) delete contents;}
     void expand_directory() {
       DIR* fred;
       fred =opendir(name);
       if(fred==0) {return;}
       
       struct dirent* item;
       struct stat statbuf;
       while((item = readdir(fred)) != 0) {
	 char buf[1024];
	 sprintf(buf,"%s/%s",name,item->d_name);
	 stat(buf,&statbuf);
	 if(S_ISDIR(statbuf.st_mode)) {
	   contents = new dirlist( new directory(item->d_name,0), contents);
	 } else {
	   contents = new dirlist( new file(item->d_name),contents);
	 }
       }
       closedir(fred);
       expanded =1;
     }
     dirlist* items() { if(!expanded) {expand_directory();} return contents;}
 
}; 
