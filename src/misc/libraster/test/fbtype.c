#include <stdio.h>
#include <sys/types.h>
#include <fcntl.h>
#include <sys/ioctl.h>

#ifdef OLD_FBIO_H_LOC
#include <sun/fbio.h>
#else /*OLD_FBIO_H_LOC*/
#include <sys/fbio.h>
#endif /*OLD_FBIO_H_LOC*/

static char* typename();

void
main( argc, argv )
    int argc;
    char* argv[];
    {
    int fd;
    struct fbgattr attr;
    struct fbtype type;

    fd = open( "/dev/fb", O_RDWR );
    if ( fd < 0 )
	{
	perror( "/dev/fb" );
	exit( 1 );
	}

    if ( ioctl( fd, FBIOGATTR, &attr ) == 0 )
	printf(
	    "FBIOGATTR says fbtype is %d (%s)\n",
	    attr.fbtype.fb_type, typename( attr.fbtype.fb_type ) );
    else
	perror( "FBIOGATTR failed" );

    if ( ioctl( fd, FBIOGTYPE, &type ) == 0 )
	printf(
	    "FBIOGTYPE says fbtype is %d (%s)\n",
	    type.fb_type, typename( type.fb_type ) );
    else
	perror( "FBIOGTYPE failed" );
    
    exit( 0 );
    }

static char*
typename( type )
    int type;
    {
    switch ( type )
	{
	case FBTYPE_SUN1BW:
	return "FBTYPE_SUN1BW";
	case FBTYPE_SUN1COLOR:
	return "FBTYPE_SUN1COLOR";
	case FBTYPE_SUN2BW:
	return "FBTYPE_SUN2BW";
	case FBTYPE_SUN2COLOR:
	return "FBTYPE_SUN2COLOR";
	case FBTYPE_SUN2GP:
	return "FBTYPE_SUN2GP";
#ifdef FBTYPE_SUN5COLOR
	case FBTYPE_SUN5COLOR:
	return "FBTYPE_SUN5COLOR";
#endif /*FBTYPE_SUN5COLOR*/
	case FBTYPE_SUN3COLOR:
	return "FBTYPE_SUN3COLOR";
#ifdef FBTYPE_MEMCOLOR
	case FBTYPE_MEMCOLOR:
	return "FBTYPE_MEMCOLOR";
#endif /*FBTYPE_MEMCOLOR*/
	case FBTYPE_SUN4COLOR:
	return "FBTYPE_SUN4COLOR";
#ifdef FBTYPE_SUNFAST_COLOR
	case FBTYPE_SUNFAST_COLOR:
	return "FBTYPE_SUNFAST_COLOR";
#endif /*FBTYPE_SUNFAST_COLOR*/
#ifdef FBTYPE_SUNROP_COLOR
	case FBTYPE_SUNROP_COLOR:
	return "FBTYPE_SUNROP_COLOR";
#endif /*FBTYPE_SUNROP_COLOR*/
#ifdef FBTYPE_SUNFB_VIDEO
	case FBTYPE_SUNFB_VIDEO:
	return "FBTYPE_SUNFB_VIDEO";
#endif /*FBTYPE_SUNFB_VIDEO*/
#ifdef FBTYPE_SUNGIFB
	case FBTYPE_SUNGIFB:
	return "FBTYPE_SUNGIFB";
#endif /*FBTYPE_SUNGIFB*/
#ifdef FBTYPE_SUNGPLAS
	case FBTYPE_SUNGPLAS:
	return "FBTYPE_SUNGPLAS";
#endif /*FBTYPE_SUNGPLAS*/
#ifdef FBTYPE_SUNGP3
	case FBTYPE_SUNGP3:
	return "FBTYPE_SUNGP3";
#endif /*FBTYPE_SUNGP3*/
#ifdef FBTYPE_SUNGT
	case FBTYPE_SUNGT:
	return "FBTYPE_SUNGT";
#endif /*FBTYPE_SUNGT*/
	default:
	return "unknown";
	}
    }
