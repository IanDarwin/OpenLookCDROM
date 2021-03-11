/* $NCDId: @(#)globals.c,v 1.2 1994/05/02 17:42:25 greg Exp $ */
/*
 * $XConsortium: globals.c,v 1.13 91/07/12 15:54:41 gildea Exp $
 *
 * Copyright 1989 Massachusetts Institute of Technology
 *
 */

/*
Permission to use, copy, modify, distribute, and sell this software and its
documentation for any purpose is hereby granted without fee, provided that
the above copyright notice appear in all copies and that both that
copyright notice and this permission notice appear in supporting
documentation, and that the name of M.I.T. not be used in advertising or
publicity pertaining to distribution of the software without specific,
written prior permission.  M.I.T. makes no representations about the
suitability of this software for any purpose.  It is provided "as is"
without express or implied warranty.
*/

/*
 *
 *                                 Global data
 *
 * This file should contain only those objects which must be predefined.
 */
#include <audio/Alibint.h>


#ifdef STREAMSCONN


/* The following are how the Austream connections are used:              */
/*      1)      Local connections over pseudo-tty ports.                */
/*      2)      SVR4 local connections using named streams or SVR3.2    */
/*              local connections using streams.                        */
/*      3)      SVR4 stream pipe code. This code is proprietary and     */
/*              the actual code is not included in the MIT distribution.*/
/*      4)      remote connections using tcp                            */
/*      5)      remote connections using StarLan                        */

/*
 * descriptor block for streams connections
 */

#include "Astreams.h"

char _AusTypeOfStream[100] = { 0 };

extern int write();
extern int close();
#ifdef SVR4
extern int _AusSetupSpStream();
extern int _AusSetupNamedStream();
#endif 
extern int _AusSetupLocalStream();
extern int _AusConnectLocalClient();
extern int _AusCallLocalServer();
extern int _AusReadLocalStream();
extern int _AusErrorCall();
extern int _AusWriteLocalStream();
extern int _AusCloseLocalStream(); 
extern int _AusSetupTliStream();
extern int _AusConnectTliClient();
extern int _AusCallTliServer(); 
extern int _AusReadTliStream(); 
extern int _AusWriteTliStream();
extern int _AusCloseTliStream();


Austream _AusStream[] = {

    { 
	/* local connections using pseudo-ttys */

	_AusSetupLocalStream,
	_AusConnectLocalClient,
	_AusCallLocalServer,
	_AusReadLocalStream,
	_AusErrorCall,
	write,
	close,
	NULL
    },
    { 
#ifdef SVR4
	/* local connections using named streams */

        _AusSetupNamedStream,
#else
	/* local connections using streams */
        _AusSetupLocalStream,
#endif
        _AusConnectLocalClient,
        _AusCallLocalServer,
        _AusReadLocalStream,
        _AusErrorCall,
        write,
        close,
        NULL
    },
    /* Enhanced Application Compatibility Support */
    {
#ifdef SVR4
	/* SVR4 stream pipe code */
	_AusSetupSpStream,
#else
	_AusSetupLocalStream,
#endif
	_AusConnectLocalClient,
	_AusCallLocalServer,
	_AusReadLocalStream,
	_AusErrorCall,
	write,
	close,
	NULL
    },
    /* End Enhanced Application Compatibility Support */

    {
	/* remote connections using tcp */
        _AusSetupTliStream,
        _AusConnectTliClient,
        _AusCallTliServer,
        _AusReadLocalStream,
        _AusErrorCall,
	write,
	close,
	NULL
    },
    {
	/* remote connections using StarLan */
        _AusSetupTliStream,
        _AusConnectTliClient,
        _AusCallTliServer,
        _AusReadLocalStream,
        _AusErrorCall,
        write,
        close,
        NULL
    }
};


#endif /* STREAMSCONN */
