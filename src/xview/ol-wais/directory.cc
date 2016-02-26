#include "directory.h"
#include <stdio.h>
main() {
  directory fred("/bin",1);
  if(fred.type == file_or_directory::is_directory) {
    List<file_or_directory>* p;
    p = fred.contents;
    while(p) {
       printf("%s\n", (p->car())->name);
      p = p->cdr();
    }
  }
}

	
