#!/bin/sh
## main.tcl main file for tkWWW  user interface
## ==============
## Copyright (C) 1992-1993
## Globewide Network Academy
## Macvicar Institute for Educational Software Development
##
## See the file COPYRIGHT for conditions 

# where can I find the executable
if test "$TK_WWW_TK_CMD" = ""; then
  TK_WWW_TK_CMD=tk_www_tk_cmd
fi

# where can I find my files
if test "$TK_WWW_SRC_DIR" = ""; then
  TK_WWW_SRC_DIR=tk_www_src_dir
fi

# where can I find my files
if test "$TK_WWW_HOME_PAGE" = ""; then
  TK_WWW_HOME_PAGE=tk_www_home_page
fi

# where can I find my files
if test "$TK_WWW_START_PAGE" = ""; then
  TK_WWW_START_PAGE=tk_www_start_page
fi

# the mailing program
if test "$TK_WWW_MAIL" = ""; then
  TK_WWW_MAIL=tk_www_mail
fi

PATH=$PATH:tk_www_extra_path
export PATH

# Backup in case tk command cannot be found

if test \!  -f $TK_WWW_TK_CMD ; then
    echo "Cannot find $TK_WWW_TK_CMD"
    if test -f ../Server/wwwish ; then
	TK_WWW_TK_CMD=../Server/wwwish
        echo "Using $TK_WWW_TK_CMD as executable"
    elif test -f ./wwwish ; then
	TK_WWW_TK_CMD=./wwwish
        echo "Using $TK_WWW_TK_CMD as executable"
    fi
fi

# Backup in case main.tcl cannot be found

if test \!  -f $TK_WWW_SRC_DIR/main.tcl ; then
    echo "Cannot find $TK_WWW_SRC_DIR"
    if test -f ../Tcl/main.tcl ; then
	TK_WWW_SRC_DIR=../Tcl
        echo "Using $TK_WWW_SRC_DIR as source directory"
    elif test -f ./main.tcl ; then
	TK_WWW_SRC_DIR=.
        echo "Using $TK_WWW_SRC_DIR as source directory"
    fi
fi

export TK_WWW_SRC_DIR
export TK_WWW_HOME_PAGE
export TK_WWW_START_PAGE
export TK_WWW_MAIL

exec $TK_WWW_TK_CMD \
	-f $TK_WWW_SRC_DIR/main.tcl \
	-name tkWWW $@
