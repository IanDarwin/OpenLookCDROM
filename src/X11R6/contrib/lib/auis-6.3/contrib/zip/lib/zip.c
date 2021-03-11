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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/zip/lib/RCS/zip.c,v 1.6 1993/05/04 01:51:05 susan Exp $";
#endif


 

/* zip.c	Zip Data-object					      */
/* Author	TC Peters					      */
/* Information Technology Center	   Carnegie-Mellon University */


/**  SPECIFICATION -- External Facility Suite  *********************************

TITLE	The Zip Data-object

MODULE	zip.c

NOTICE	IBM Internal Use Only

DESCRIPTION
	This is the suite of Methods that support the Zip Data-object.

    NB: The comment-symbol "===" indicates areas which are:
	    1 - Questionable
	    OR
	    2 - Arbitrary
	    OR
	    3 - Temporary Hacks
    Such curiosities need be resolved prior to Project Completion...

HISTORY
  06/16/87	Created (TCP)
  08/06/87	Once more to the breach, dear friends ...
  10/30/87	return long instead of void for zip__Read (TCP)
  11/09/87	Support "Absolute" as well as "Relative" sizing (TCP)
  12/09/87	Fixed reading code to be able to recognize EOF when reading.
		Also changed fgetc to getc. (ajp)
  03/31/88	Revised to ATK (TCP)
  03/16/89	Accomodate short file-names for objects (TCP)
  08/07/89	Override GetModified to check for changes to Imbedded objects (TCP)

END-SPECIFICATION  ************************************************************/

#include <andrewos.h>
#include <dataobj.ih>
#include <environ.ih>
#include <zipobj.ih>
#include <zip.eh>
#include <sys/stat.h>
#include <ctype.h>

static boolean debug;

#define	 Data			      self
#define	 Objects(i)		      ((*self->objects)[i])


/*===*/  static char   *object_names[] =
	{
/* NULL	       */ "zipobj",
/* A Caption   */ "zipocapt",
/* B FlexCapt  */ "zipofcap",
/* C Line      */ "zipoline",
/* D PolyLine  */ "zipoplin",
/* E Polygon   */ "zipopoly",
/* F Trapezoid */ "zipotrap",
/* G Rectangle */ "ziporect",
/* H Path      */ "zipopath",
/* I Imbed     */ "zipoimbd",
/* J Circle    */ "zipocirc",
/* K Photo     */ "zipobj",
/* L Ellipse   */ "zipoelli",
/* M Arc       */ "zipoarc",
/* N RoundAngle*/ "ziporang",
/* O Arrow     */ "zipoarrw",
/* P Symbol    */ "ziposym",
NULL,
/* Q	       */ "zipobj",
/* R           */ "zipobj",
/* S           */ "zipobj",
/* T           */ "zipobj",
/* U           */ "zipobj",
/* V           */ "zipobj",
/* W           */ "zipobj",
/* X           */ "zipobj",
/* Y           */ "zipobj",
/* Z           */ "zipobj",
NULL
	};


boolean
zip__InitializeClass( classID )
  register struct classheader	     *classID;
  {
  IN(zip_InitializeClass);
  debug = 0;
  OUT(zip_InitializeClass);
  return TRUE;
  }


static Generate_Temp_File();
static Write_View_Info();
static Write_Object_Info();

static long Init_Message_Writer();
static long Init_Message_Clearer();
static long Init_Message_Acknowledger();

boolean
zip__InitializeObject( classID, self )
  register struct classheader	     *classID;
  register struct zip		     *self;
  {
  register long			      status = zip_ok;
  register char			     *font_name = NULL;
  register int			      i;

  IN(zip_InitializeObject);
  Stream = NULL;
  StreamFileName = NULL;
  StreamAnchor = NULL;
  ImageAnchor = NULL;
  FigureAnchor = NULL;
  Paths = NULL;
  Fonts = NULL;
  Status = StatusAddenda = NULL;
  Facility = NULL;
  generalExceptionHandler = StreamExceptionHandler = ImageExceptionHandler =
    FigureExceptionHandler = NULL;
  Id = NULL;
  PageCount = 0;
  DesiredWidth = DesiredHeight = ObjectWidth = ObjectHeight = NULL;
  MessageWriter  = Init_Message_Writer;
  MessageClearer = Init_Message_Clearer;
  MessageAcknowledger = Init_Message_Acknowledger;
  if ( (font_name = (char *) environ_GetProfile( "zip.bodyfont" )) == NULL )
      font_name = "andy12";
  zip_Define_Font( self, font_name, NULL );
  self->objects = (struct zipobject *((*)[])) calloc( 28, sizeof(char *) );
  for ( i = 0; object_names[i]  &&  status == zip_ok; i++ )
    {
    DEBUGst(Loading,object_names[i]);
    Objects(i) = (struct zipobject *) class_NewObject( object_names[i] );
    if ( Objects(i) )
      {
      zipobject_Set_Data_Object( Objects(i), Data );
      }
      else
      {
      DEBUG(NewObject FAILED);
/*===*/status = zip_failure; /*===s/b Missing Object===*/
      }
    }
  OUT(zip_InitializeObject);
  return (status == zip_ok);
  }

void 
zip__FinalizeObject( classID, self )
  register struct classheader	      *classID;
  register struct zip		      *self;
  {
  IN(zip_FinalizeObject);
/*===*/
  OUT(zip_FinalizeObject);
  }

void 
zip__Set_Debug( self, state )
  register struct zip		     *self;
  register char			      state;
  {
  register long			      i;

  IN(zip_Set_Debug);
  debug = state;
  if ( self->objects )
    for ( i = 0; (i < 26); i++ )
      if ( Objects(i) )
        zipobject_Set_Debug( Objects(i), state );
  OUT(zip_Set_Debug);
  }

static long
Check_Image( self, image, modified )
  register struct zip		     *self;
  register zip_type_image	      image;
  register long			      modified;
  {
  register zip_type_figure	      figure;

  IN(Check_Image);
  if ( image )
    {
    figure = image->zip_image_figure_anchor;
    while ( figure )
      {
      if ( zipobject_Object_Modified( Objects(figure->zip_figure_type), figure ) > modified )
	modified = zipobject_Object_Modified( Objects(figure->zip_figure_type), figure );
      figure = figure->zip_figure_next;
      }
    if ( image->zip_image_inferior )
      modified = Check_Image( self, image->zip_image_inferior, modified );
    if ( image->zip_image_peer )
      modified = Check_Image( self, image->zip_image_peer, modified );
    }
  OUT(Check_Image);
  return  modified;
  }

long
zip__GetModified( self )
  register struct zip		     *self;
  {
  register long			      modified = super_GetModified( self );

  IN(zip_GetModified);
  if ( Stream )
    modified = Check_Image( self, Stream->zip_stream_image_anchor, modified );
  OUT(zip_GetModified);
  return  modified;
  }

long
zip__Read( self, file, id )
  register struct zip		     *self;
  register FILE			     *file;
  register long			      id;
  {
  register long			      status;
  char				     *generated_file_name;

  IN(zip_Read);
  if ( (status = Generate_Temp_File( self, file, &generated_file_name )) == zip_ok )
    {
    if ( status = zip_Open_Stream( self, &Stream, generated_file_name, zip_default ) )
      { DEBUGdt(Open Status,status);
      status = dataobject_BADFORMAT;
      }
    if ( status = zip_Read_Stream( self, Stream ) )
      { DEBUGdt(Read Status,status);
      status = dataobject_BADFORMAT;
      }
    unlink( generated_file_name );
    }
  OUT(zip_Read);
  return status;
  }

long
zip__Write( self, file, id, level )
  register struct zip		     *self;
  register FILE			     *file;
  register long			      id;
  register long			      level;
  {
  register long			      status;

  IN(zip_Write);
  DEBUGdt( Headerwriteid, self->header.dataobject.writeID );
  DEBUGdt( Given Id, id );
  DEBUGdt( Given Level, level );
  WriteStreamFile = file;
  WriteStreamId = id;
  WriteStreamLevel = level;
  if ( self->header.dataobject.writeID != id ) /*===???avoid recursive writes */
    {
    self->header.dataobject.writeID = id;
    if ( level )
      { DEBUG(Not Parent -- Write To Datastream);
      fprintf( file, "\\begindata{%s,%d}\n",
		class_GetTypeName( self ),
		dataobject_UniqueID( &self->header.dataobject ) );
      Write_View_Info( self, file );
      Write_Object_Info( self, file );
      if ( Stream->zip_stream_attributes.zip_stream_attribute_reference )
	{ DEBUG(Write File-reference);
	fprintf( file, "!%s", Stream->zip_stream_file_full_name );
	if ( Stream->zip_stream_states.zip_stream_state_modified )
          status = zip_Write_Stream( self, Stream );
	}
	else
	{ DEBUG(Write File-substance);
	Stream->zip_stream_file = file;
        status = zip_Enparse_Stream( self, Stream );
	}
      DEBUGdt(Status,status);
      fprintf( file, "\n\\enddata{%s,%d}\n",
		class_GetTypeName( self ),
		dataobject_UniqueID( &self->header.dataobject ) );
      }
      else
      { DEBUG(Parent -- Write To Raw File);
      Write_Object_Info( self, file );
      Stream->zip_stream_file = file;
      status = zip_Enparse_Stream( self, Stream );
      DEBUGdt(Enparse_Stream Status,status);
      }
    }
  OUT(zip_Write);
  return (long) self;
  }

static
Write_View_Info( self, file )
  register struct zip		     *self;
  register FILE			     *file;
  {
  if ( DesiredWidth )
    fprintf( file, "%%ViewWidth %d\n", DesiredWidth );
  if ( DesiredHeight )
    fprintf( file, "%%ViewHeight %d\n", DesiredHeight );
  }

static
Write_Object_Info( self, file )
  register struct zip		     *self;
  register FILE			     *file;
  {
  if ( ObjectWidth )
    fprintf( file, "%%ObjectWidth %d\n", ObjectWidth );
  if ( ObjectHeight )
    fprintf( file, "%%ObjectHeight %d\n", ObjectHeight );
  }

static
Generate_Temp_File( self, file, generated_file_name )
  register struct zip		     *self;
  register FILE			     *file;
  register char			    **generated_file_name;
  {
  register long			      status = dataobject_NOREADERROR;
  static char			     *temp_name_template = "/tmp/ZIPxxxxxx",
				      temp_name[512];
  register FILE			     *temp_file;
  register long			      level = 0;
  char				      line[32000];


  IN(Generate_Temp_File);
  sprintf( temp_name, "%s.zip", mktemp( temp_name_template ) );
  DEBUGst(Temp-Name,temp_name);
  *generated_file_name = temp_name;
  if ( (temp_file = fopen( temp_name, "w+" )) == NULL )
    { DEBUG(Open Temp-File ERROR);
    status = zip_failure;
    }
    else
    { DEBUG(Open Temp-File OK);
    while ( level >= 0 )
      {
      if ( fgets( line, sizeof( line ) - 1, file ) )
	{ DEBUGst(Line,line);
        if ( strncmp( line, "\\begindata", 10 ) == 0 )
  	  level++;
        else
        if ( strncmp( line, "\\enddata", 8 ) == 0 )
	  level--;
        if ( level >= 0 )
          fputs( line, temp_file );
	}
	else
	{ DEBUG(EOF);
	if ( level > 0 )
	  status = dataobject_PREMATUREEOF;
	break;
	}
      }
    }
  fclose( temp_file );
  OUT(Generate_Temp_File);
  return  status;
  }

void zip__Show_Statistics( self, options )
  register struct zip		      *self;
  register int			       options;
  {
  register zip_type_stream_chain	  stream_chain;
  register zip_type_stream		  stream;
  register zip_type_image		  image;
  register zip_type_figure		  figure;
  register int				  bytes,
					  figure_count = 0,
					  image_count = 0,
					  stream_count = 0,
					  stream_byte_count = 0,
					  image_byte_count = 0,
					  figure_byte_count = 0;
/*===  if ( options ... ) ===*/
    {

      {
      stream_chain = StreamAnchor;
      while ( stream_chain )
	{
	stream_count++;
	stream = stream_chain->zip_stream_chain_ptr;
	stream_byte_count += sizeof(struct zip_stream);
	if ( zip_Stream_Name( self, stream ) )
	  stream_byte_count += strlen( zip_Stream_Name( self, stream ) );
	image = stream->zip_stream_image_anchor;
	while ( image )
	  {
	  image_count++;
	  bytes = sizeof(struct zip_image);
	  if ( zip_Image_Name( self, image ) )
	    bytes += strlen( zip_Image_Name( self, image ) );
	  image_byte_count += bytes;
	  stream_byte_count += bytes;
	  figure = image->zip_image_figure_anchor;
	  while ( figure )
	    {
	    figure_count++;
	    bytes = sizeof(struct zip_figure) +
		((figure->zip_figure_points) ?
		  (figure->zip_figure_points->zip_points_count *
			 sizeof(struct zip_point_pair))
		    :
		   0);
	    if ( zip_Figure_Name( self, figure ) )
	      bytes += strlen( zip_Figure_Name( self, figure ) );
	    figure_byte_count += bytes;
	    stream_byte_count += bytes;
	    figure = figure->zip_figure_next;
	    }
	  image = zip_Next_Image( self, image );
	  }
	printf( "Stream '%s' (%d Bytes)\n\t%d Images ( %d Bytes)\n\t%d Figures  (%d Bytes)\n",
		 zip_Stream_Name( self, stream ), stream_byte_count,
		 image_count, image_byte_count,
		 figure_count, figure_byte_count );
        stream_chain = stream_chain->zip_stream_chain_next;
	}
      }
    }
  }

static long
Init_Message_Writer( self, msg )
  register struct zip		     *self;
  register char			     *msg;
  {
/*===  apt_Announce( msg );===*/
  return zip_success;
  }

static long
Init_Message_Clearer( self )
  register struct zip		     *self;
  {
/*===  apt_Unannounce();===*/
  return zip_success;
  }

static long
Init_Message_Acknowledger( self, msg )
  register struct zip		     *self;
  register char			     *msg;
  {
/*===
  while ( apt_Acknowledge( msg ) == -1 )
	;
===*/
  return zip_success;
  }
/*=== === ===*/

int 
apt_MM_Compare( s1, s2 )
  /* Assumes "s1" must be shifted to lower-case
             "s2" must be shifted to lower-case
  */
  register unsigned char		 *s1, *s2;
  {
  register unsigned char		  c1, c2;
  register int				  result = 0;

  do
    {
    c1 = isupper( *s1 ) ? tolower( *s1++ ) : *s1++;
    c2 = isupper( *s2 ) ? tolower( *s2++ ) : *s2++;
    if ( c1 != c2 )
      break;
    } while ( c1 );
  if ( c1 != c2 )
    if ( c1 > c2 )
      result = 1;
      else result = -1;
  return result;
  }


