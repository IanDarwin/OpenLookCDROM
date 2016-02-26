#include <stdio.h>
#include "mchoose.h"
#include <sys/time.h>

/*
 * mchoose
 * Copyright (C) 1988 by Don Hopkins
 */

char *label = "Choose!";
char *programname;
FILE *input_file = stdin;
int key_count = 0;
int max_keys = 64;
char **keys = NULL;
int max_keys_per_menu = 8;
int no_newlines = 0;
char *menuclassname = "DefaultMenu";

main(argc, argv)
int argc;
char **argv;
{
    char *key;
    char buf[256];
    int i;
    int width, readfds, writefds, execptfds;

    programname = argv[0];

    while (--argc > 0 && (++argv)[0][0] == '-')
	switch (argv[0][1]) {
	    case 'l':
	      if (--argc <= 0) {
		fprintf(stderr, "%s: not enough arguments\n", programname);
		exit(1);
	      }
	      label = (++argv)[0];
	      break;
	    case 'm':
	      if (--argc <= 0) {
		fprintf(stderr, "%s: not enough arguments\n", programname);
		exit(1);
	      }
	      menuclassname = (++argv)[0];
	      break;
	    case 'f':
	      if (--argc <= 0) {
		fprintf(stderr, "%s: not enough arguments\n", programname);
		exit(1);
	      }
	      if ((input_file = fopen((++argv)[0], "r")) == NULL) {
		fprintf(stderr, "%s: can't open %s\n", programname, argv[0]);
		exit(1);
  	      }
	      break;
	    case 'n':
	      no_newlines++;
	      break;
	    case 'k':
	      if (--argc <= 0) {
		fprintf(stderr, "%s: not enough arguments\n", programname);
		exit(1);
	      }
	      max_keys_per_menu = atoi((++argv)[0]);
	      break;
	    default:
		fprintf(stderr, "%s: invalid switch '%s'\n",
			programname, argv[0]);
		exit(1);
	    }

    if (argc > 0)
	while (--argc >= 0)
	    remember_key((argv++)[0]);
    else
	while ((fgets(buf, 256, input_file) != NULL) &&
    	       (*buf != '\n')) {
            buf[strlen(buf)-1] = '\0';
	    key = (char *)malloc(strlen(buf) + 1);
	    strcpy(key, buf);
	    remember_key(key);
	}

    ps_open_PostScript();
    choose_init(label, menuclassname);
    if (key_count) {
      make_menus();
      start_popup("choosemenu");
    }
    ps_flush_PostScript();

    while (1) {
        if (get_key(buf)) {
	    if(no_newlines)
		printf("%s",buf);
	    else
		printf("%s\n",buf);
	    fflush(stdout);
	} 
	else if (get_string(buf)) {
	    printf("%s",buf);
	    fflush(stdout);
	} 
	else if (get_zap()) {
	    exit(0);
	} 
	else {
	    if ((i=getchar(PostScript_Input)) == EOF)
	      exit(0);
	    putchar(i);
	}
    }
}

remember_key(key)
    char *key;
{
    if (keys == NULL)
	keys = (char **)malloc(max_keys * sizeof(char *));
    else if (key_count == max_keys) {
	max_keys <<= 1;
	keys = (char **)realloc(keys, max_keys * sizeof(char *));
    }
    keys[key_count++] = key;
}

make_menus()
{
    int k, m, i;
    int submenus;
    char submenu_id[256], submenu_key[256];

    if ((submenus = ((key_count - 1) / max_keys_per_menu) + 1) == 1) {
        for (k = 0; k < key_count; k++)
	    add_key(keys[k]);
    } else {
	for (m = 0, k = 0; m < submenus; m++) {
	    for (i = 0; i < max_keys_per_menu && k < key_count; i++, k++)
		add_key(keys[k]);
	    sprintf(submenu_id, "submenu_%d", m);
	    name_menu(submenu_id);
	}
	for (m = 0; m < submenus; m++) {
	    sprintf(submenu_key, "%s...", keys[m * max_keys_per_menu]);
	    sprintf(submenu_id, "submenu_%d", m);
	    add_submenu(submenu_key, submenu_id);
	}
    }
    name_menu("choosemenu");
}
