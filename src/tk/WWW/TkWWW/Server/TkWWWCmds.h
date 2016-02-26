#ifndef TKWWWCMDS_H
#define TKWWWCMDS_H

#include "HTAnchor.h"
#include "HTFormat.h"
#include "tcl.h"

extern void HTTkSetOutputFile PARAMS((HTRequest *, HTFormat,
				      CONST char *, HTFormat));
extern BOOL HTTkUseInPlace PARAMS((HTFormat, HTRequest *));

extern int WWW_AppInit PARAMS((Tcl_Interp *));

#endif
