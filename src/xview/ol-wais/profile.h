// profile.h
// defined the profile object which manages user and system wide profiles
//

// dispatch is an object used by the profile; it is used to select a
// process to handle a document of a given type

class dispatch {
  char* type;
  char* command_string;
  dispatch* next;
public:
  dispatch(char* t,char* command,dispatch* l) {
    type = t;
    command_string = command;
    next = l;
  }
  char* get_match(char* t,char* c,char*fn) {
    if(!strcmp(type,t)) {
      sprintf(c,command_string,fn);
      return t;
    } else {
      if (next) {
	return next->get_match(t,c,fn);
      } else {
	return NULL;
      }
    }
  }
  char* best_match(char** types,char* command_buffer,char* fn) {
    while(*types) {
      char* result;
      result = get_match(*types, command_buffer,fn);
      if (result) {
	return result;
      } else {
	types++;
      }
    }
    return 0;
  }
};

static dispatch* parse_dispatch_file(FILE* f) {
  char buf[1024];
  char type[1024];
  
  int n;
  n = fscanf(f,"%s %[^\n]",type,buf);
  if (n == EOF) {
    return NULL;
  } else {
    return new dispatch(strdup(type),strdup(buf),parse_dispatch_file(f));
  }
}
    
    

static dispatch* load_dispatches(char* filename) {
  FILE* f = fopen(filename,"r");
  if(f == 0) {
    return 0;
  }
  dispatch* tmp = parse_dispatch_file(f);
  fclose(f);
  return tmp;
}
  

//
// profile contains the generic user profile
class profile {
 public:
  char* source_path;
  dispatch* handlers;
};
