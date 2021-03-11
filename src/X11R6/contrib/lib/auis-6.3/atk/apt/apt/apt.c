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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/apt/apt/RCS/apt.c,v 1.13 1993/05/04 01:06:01 susan Exp $";
#endif

/* $Header $ */
/* $Source $ */

#ifndef lint
static char *rcsidapt = "$Header $";
#endif

/*
    $Log: apt.c,v $
 * Revision 1.13  1993/05/04  01:06:01  susan
 * RCS Tree Split
 *
 * Revision 1.12.1.1  1993/02/02  00:42:16  rr2b
 * new R6tape branch
 *
 * Revision 1.12  1992/12/15  21:25:24  rr2b
 * more disclaimerization fixing
 *
 * Revision 1.11  1992/12/14  20:33:21  rr2b
 * disclaimerization
 *
 * Revision 1.10  1992/08/31  23:32:41  rr2b
 * renaming debug var to avoid name clashes.
 * .
 *
 * Revision 1.9  1991/09/12  15:56:20  bobg
 * Update copyright notice and rcsid
 *
 * Revision 1.8  1991/09/09  23:32:44  gk5g
 * Added forward declarations.
 *
 * Revision 1.7  1990/04/11  14:10:12  gk5g
 * Removed initialization of variable debug from apt.h and put it in apt.c:apt__InitializeClass.  Create apt__InitializeClass.
 *
 * Revision 1.6  89/12/12  14:57:47  ghoti
 * sync with MIT tape
 * 
 * Revision 1.2  89/11/28  16:05:37  xguest
 * Added Gary Keim's diffs.
 * 
 * Revision 1.1  89/11/28  15:51:35  xguest
 * Initial revision
 * 
 * Revision 1.5  89/08/30  12:24:30  gk5g
 * Removed include of andrewos.h because apt.h does that.
 * 
 * Revision 1.4  89/08/03  12:25:32  ghoti
 * added include of andrewos.h (HPUX)
 * changed #inlcude "" to #include <>
 * 
 * Revision 1.3  89/05/24  19:46:11  tom
 * Fix Free_Vector; use apt_LeftArea, etc.
 * 
 * Revision 1.2  89/05/18  20:32:44  tom
 * Utilize apts.c (apts_CompareStrings).
 * 
 * Revision 1.1  89/04/28  17:45:51  tom
 * Initial revision
 * 
*/

/**  SPECIFICATION -- External Facility Suite  *********************************

TITLE	The Apt Data-object

MODULE	apt.c

VERSION	0.0

AUTHOR	TC Peters
	Information Technology Center, Carnegie-Mellon University 

DESCRIPTION
	This is the suite of Methods that support the Apt Data-object.

    NB: The comment-symbol "===" indicates areas which are:
	    1 - Questionable
	    OR
	    2 - Arbitrary
	    OR
	    3 - Temporary Hacks
    Such curiosities need be resolved prior to Project Completion...


HISTORY
  02/23/88	Created (TCP)
  05/18/89	Utilize apts facility apts_CompareStrings (TCP)
  05/22/89	Fix Free_Vector (TCP)
		Use apt_LeftArea, etc.
   08/30/89	Removed include of andrewos.h because that is in apt.h. (GW Keim)

END-SPECIFICATION  ************************************************************/
#include <dataobj.ih>
#include <apt.h>
#include <apt.eh>
#include <apts.ih>

int apt_debug = 0;
#define debug apt_debug

#define	 Title(p)	 	    (*self->titles[p].strings)[0]
#define	 TitlesAnchor(p) 	     (self->titles[p].strings)
#define	 TitlesCount(p) 	     (self->titles[p].strings_count)
#define	 Titles(p,i)	 	    (*self->titles[p].strings)[i]
#define	 TitleMode(p)	 	     (self->titles[p].mode)
#define	 TitleFontName(p)	     (self->titles[p].font_name)

#define	 Legend(p)	 	    (*self->legends[p].strings)[0]
#define	 LegendsAnchor(p) 	     (self->legends[p].strings)
#define	 LegendsCount(p) 	     (self->legends[p].strings_count)
#define	 Legends(p,i)	 	    (*self->legends[p].strings)[i]
#define	 LegendMode(p)	 	     (self->legends[p].mode)
#define	 LegendFontName(p)	     (self->legends[p].font_name)

#define  Id			      self->id

#define  Fields			      self->fields
#define  FieldsFile		      self->field_file
#define  FieldIndex		      self->field_index
#define  Field(i)		      self->fields->field[i]
#define  FieldCount		      self->fields->count
#define  FieldName(i)		      self->fields->field[i].name
#define  FieldContent(i)	      self->fields->field[i].content


static char			     *areas[] =
				     { "Left", "Top", "Right", "Bottom" };

static Assign_String();
static Assign_Strings();
static Write_Strings();
static Parse_Field();


static
Free_Vector( vector )
  register char			    *((*vector)[]);
  {
  register long			      i = 0;

  IN(Free_Vector);
  if ( vector )
    {
    while ( (*vector)[i] )
      {
      DEBUGst(String,(*vector)[i]);
      free( (*vector)[i] );
      i++;
      }
    free( vector );
    }
  OUT(Free_Vector);
  }

boolean
apt__InitializeClass( classID )
  register struct classheader	    *classID;
  {
  IN(apt_InitializeClass);
  apt_debug = 0;
  OUT(apt_InitializeClass);
  return TRUE;
  }

boolean
apt__InitializeObject( classID, self )
  register struct classheader	     *classID;
  register struct apt		     *self;
  {
  IN(apt_InitializeObject);
  DEBUGst(RCSID,rcsidapt);
  bzero( self->titles,  4 * sizeof(struct apt_strings) );
  bzero( self->legends, 4 * sizeof(struct apt_strings) );
  Fields = NULL;
  OUT(apt_InitializeObject);
  return TRUE;
  }

void 
apt__FinalizeObject( classID, self )
  register struct classheader	      *classID;
  register struct apt		      *self;
  {
  register long			       i,j;

  IN(apt_FinalizeObject);
  for(i = 0; i < 4; i++) {
      if(TitlesAnchor(i))
	  for(j = 0; j < TitlesCount(i) ; j++)
	      if(Titles(i,j)) free(Titles(i,j));
      if(LegendsAnchor(i))
	  for(j = 0; j < LegendsCount(i); j++)
	      if(Legends(i,j)) free(Legends(i,j));
      if(LegendFontName(i)) free(LegendFontName(i));
      if(TitleFontName(i)) free(TitleFontName(i));
  }
  if(Fields) {
      for(i = 0; i < FieldCount; i++) {
	  if(FieldName(i)) free(FieldName(i));
	  if(FieldContent(i)) free(FieldContent(i));
      }
      free(Fields);
  }
  OUT(apt_FinalizeObject);
  }

void
apt__SetAreaTitle( self, title, area )
  register struct apt		     *self;
  register char			     *title;
  register long			      area;
  {
  char				     *titles[2];

  IN(apt_SetAreaTitle);
  titles[0] = title;
  titles[1] = NULL;
  apt_SetAreaSpreadTitle( self, titles, 1, area, apt_Center );
  OUT(apt_SetAreaTitle);
  }

void
apt__SetAreaSpreadTitle( self, titles, count, area, mode )
  register struct apt		     *self;
  register char			    **titles;
  register long			      count, area, mode;
  {
  register long			      i = 0;

  IN(apt_SetAreaSpreadTitle);
  DEBUGdt(Position,area);
  Free_Vector( TitlesAnchor(area) );
  TitlesAnchor(area) = NULL;
  TitlesCount(area) = count;
  if ( titles )
    {
    TitlesAnchor(area) = (char *(*)[]) calloc( count + 1, sizeof(char *) );
    while ( i < count  &&  *titles )
      { DEBUGst(Title,*titles);
      Titles(area,i) = (char *) malloc( strlen( *titles ) + 1 );
      strcpy( Titles(area,i), *titles );
      i++;
      titles++;
      }
    }
  OUT(apt_SetAreaSpreadTitle);
  }

void
apt__SetAreaTitleFontName( self, font_name, area )
  register struct apt		     *self;
  register char			     *font_name;
  register long			      area;
  {
  IN(apt_SetAreaTitleFontName);
  if ( TitleFontName(area) )  free( TitleFontName(area) );
  TitleFontName(area) = NULL;
  if ( font_name  &&  *font_name )
    {
    DEBUGst(TitleFontName,font_name);
    TitleFontName(area) = (char *) malloc( strlen( font_name ) + 1 );
    strcpy( TitleFontName(area), font_name );
    }
  OUT(apt_SetAreaTitleFontName);
  }

void
apt__SetAreaLegend( self, legend, area )
  register struct apt		     *self;
  register char			     *legend;
  register long			      area;
  {
  char				     *legends[2];

  IN(apt_SetAreaLegend);
  legends[0] = legend;
  legends[1] = NULL;
  apt_SetAreaSpreadLegend( self, legends, 1, area, apt_Center );
  OUT(apt_SetAreaLegend);
  }

void
apt__SetAreaSpreadLegend( self, legends, count, area, mode )
  register struct apt		     *self;
  register char			    **legends;
  register long			      count, area, mode;
  {
  register long			      i = 0;

  IN(apt_SetAreaSpreadLegend);
  Free_Vector( LegendsAnchor(area) );
  LegendsAnchor(area) = NULL;
  LegendsCount(area) = count;
  if ( legends )
    {
    LegendsAnchor(area) = (char *(*)[]) calloc( count + 1, sizeof(char *) );
    while ( i < count  &&  *legends )
      {
      DEBUGst(Legend,*legends);
      Legends(area,i) = (char *) malloc( strlen( *legends ) + 1 );
      strcpy( Legends(area,i), *legends );
      i++;
      legends++;
      }
    }
  OUT(apt_SetAreaSpreadLegend);
  }

void
apt__SetAreaLegendFontName( self, font_name, area )
  register struct apt		     *self;
  register char			     *font_name;
  register long			      area;
  {
  IN(apt_SetAreaLegendFontName);
  if ( LegendFontName(area) )  free( LegendFontName(area) );
  LegendFontName(area) = NULL;
  if ( font_name  &&  *font_name )
    {
    DEBUGst(LegendFontName,font_name);
    LegendFontName(area) = (char *) malloc( strlen( font_name ) + 1 );
    strcpy( LegendFontName(area), font_name );
    }
  OUT(apt_SetAreaLegendFontName);
  }


struct apt_field *
apt__ReadObjectField( self )
  register struct apt		     *self;
  {
  register struct apt_field	     *field = NULL;

  IN(apt_ReadObjectField);
  if ( FieldIndex < FieldCount )
    {
    field = &Field(FieldIndex);
    FieldIndex++;
    }
  OUT(apt_ReadObjectField);
  return field;
  }

long
apt__ReadObject( self, file, id, reader )
  register struct apt		     *self;
  register FILE			     *file;
  register long			      id;
  register void			    (*reader)();
  {
  register long			      i, j, found, status = dataobject_NOREADERROR;
  char				      line[256 + 1];

  IN(apt_ReadObject);
  if ( Id = id )
    {
    Fields = (struct apt_fields *)
			malloc( sizeof(struct apt_fields) );
    /* Extract all fields */
    FieldCount = FieldIndex = i = 0;
    while ( fgets( line, 256, file ) )
      {
      DEBUGst(Line,line);
      if ( *line == '\\' )
	{
	DEBUG(BeginData | EndData Read);
	if ( strncmp( line, "\\begindata", 10 ) == 0 )
	  continue;
	break;
	}
        else
        {
	if ( *line  &&  *line != '\n' )
	  {
          Parse_Field( self, line, &Field(i) );
          FieldCount = ++i;
          Fields = (struct apt_fields *)
	    realloc( Fields, sizeof(struct apt_fields) +
		     FieldCount * sizeof(struct apt_field) );
	  }
        }
      }
    DEBUGdt(FieldCount,FieldCount);
    for ( i = 0; i < FieldCount; i++ )
      {
      found = 0;
      for ( j = 0; ! found && j < 4; j++ )
	if ( Assign_String( self, areas[j], "TitleFontName", FieldName(i),
			    FieldContent(i), &TitleFontName(j) ) )
	  found++;
      for ( j = 0; ! found && j < 4; j++ )
	if ( Assign_Strings( self, areas[j], "Title", FieldName(i),
			    FieldContent(i), &TitlesAnchor(j), &TitlesCount(j) ) )
	  found++;
      for ( j = 0; ! found && j < 4; j++ )
	if ( Assign_String( self, areas[j], "LegendFontName", FieldName(i),
			    FieldContent(i), &LegendFontName(j) ) )
	  found++;
      for ( j = 0; ! found && j < 4; j++ )
	if ( Assign_Strings( self, areas[j], "Legend", FieldName(i),
			    FieldContent(i), &LegendsAnchor(j), &LegendsCount(j) ) )
	  found++;
      }
    if ( reader )
      (*reader)( self );
    }
    else /* Id != id */
    {/*===*/}
  OUT(apt_Read_Object);
  return  status;
  }

static
Assign_String( self, prefix, desire, candidate, source, target )
  register struct apt		     *self;
  register char			     *prefix, *desire, *candidate, *source, **target;
  {
  char				      name[257];
  register long			      status = 0;

  IN(Assign_String);
  sprintf( name, "%s%s", prefix, desire );
  DEBUGst(Name,name);
  if ( apts_CompareStrings( name, candidate ) == 0 )
    {
    DEBUG(Satisfied);
    status = 1;
    if ( *target )  free( *target );
    *target = (char *) malloc( strlen( source ) + 1 );
    strcpy( *target, source );
    }
  OUT(Assign_String);
  return status;
  }

static
Assign_Strings( self, prefix, desire, candidate, source, target, count )
  register struct apt		     *self;
  register char			     *prefix, *desire, *candidate, *source;
  register char			   ***target;
  register long			     *count;
  {
  char				      name[257], **strings;
  register long			      status = 0;

  IN(Assign_Strings);
  sprintf( name, "%s%s", prefix, desire );
  DEBUGst(Name,name);
  if ( apts_CompareStrings( name, candidate ) == 0 )
    {
    DEBUG(Satisfied);
    status = 1;
    *target = apt_ParseFieldContent( self, source );
    *count = 0;
    strings = *target;
    while ( *(strings++) ) (*count)++;
    DEBUGdt(Count,*count);
    }
  OUT(Assign_Strings);
  return status;
  }

void
apt__WriteObjectField( self, field )
  register struct apt		     *self;
  register struct apt_field	     *field;
  {
  IN(apt_WriteObjectField);
  fprintf( FieldsFile, "%s %s\n", field->name, field->content );
  OUT(apt_WriteObjectField);
  }

void
apt__WriteObject( self, file, id, level, writer )
  register struct apt		     *self;
  register FILE			     *file;
  register long			      id, level;
  register void			    (*writer)();
  {
  char				      bracket[256 + 2];
  register long			      i;

  IN(apt_WriteObject);
  if ( self->header.dataobject.writeID != id ) /*avoid recursive writes */
    { DEBUG(Not Recursive);
    FieldsFile = file;
    self->header.dataobject.writeID = id;
/*===    if ( level )===*/ /* not parent, use datastream */
      {
      DEBUG( Write To Datastream );
      sprintf( bracket, "data{%s, %d}\n", class_GetTypeName( self ),
			  dataobject_UniqueID( &self->header.dataobject ) );
      fprintf( file, "\\begin%s", bracket );
      for ( i = 0; i < 4; i++ )
	{
	Write_Strings( self, file, "Title",  areas[i], TitlesCount(i), TitlesAnchor(i) );
        if ( TitleFontName(i) )
	  fprintf( file, "%sTitleFontName %s\n",  areas[i], TitleFontName(i) );
	Write_Strings( self, file, "Legend", areas[i], LegendsCount(i), LegendsAnchor(i) );
        if ( LegendFontName(i) )
	  fprintf( file, "%sLegendFontName %s\n", areas[i], LegendFontName(i) );
	}
      if ( writer )
	(*writer)( self );
      fprintf( file, "\\end%s", bracket );
      }
    FieldsFile = NULL;
    }
  OUT(apt_WriteObject);
  }

static
Write_Strings( self, file, name, prefix, count, anchor )
  register struct apt		     *self;
  register FILE			     *file;
  register char			     *name, *prefix;
  register long			      count;
  register char			     *anchor[];
  {
  register long			      i;

  IN(Write_Strings);
  if ( anchor )
    {
    fprintf( file, "%s%s ", prefix, name );
    for ( i = 0; i < count; i++ )
      {
      if ( i )  fprintf( file, ";" );
      fprintf( file, "%s", anchor[i] );
      DEBUGst(String,anchor[i]);
      }
    fprintf( file, "\n" );
    }
  OUT(Write_Strings);
  }

static
Parse_Field( self, line, field )
  register struct apt		     *self;
  register char			     *line;
  register struct apt_field	     *field;
  {
  register int			      status = 0;
  char				      work[257];
  register char			     *t = work;

  IN(Parse_Field);
  DEBUGst(Given-Line,line);
  while ( *line  &&  *line != ' ' )  *t++ = *line++;
  *t = 0;
  field->name = (char *) malloc( strlen( work ) + 1 );
  strcpy( field->name, work );
  while ( *line  &&  *line == ' ' )  line++;
  t = work;
  while ( *line  &&  *line != '\n' )  *t++ = *line++;
  *t = 0;
  field->content = (char *) malloc( strlen( work ) + 1 );
  strcpy( field->content, work );
  OUT(Parse_Field);
  return  status;
  }

#define  max_field_content_items    50

char **
apt__ParseFieldContent( self, string )
  register struct apt		     *self;
  register char			     *string;
  {
  register char			    **fields;
  register char			     *s = string, *s2, *t;
  register long			      i, length;

  IN(apt_ParseFieldContent);
  DEBUGst(String,string);
  fields = (char **) calloc( max_field_content_items, sizeof (char *) );
  i = 0;
  while ( *s )
    {
    s2 = s;
    length = 0;
    while ( *s2  &&  *s2 != ';' )
      {
      s2++;
      length++;
      }
    t = fields[i] = (char *) malloc( length + 1 );
    while ( *s  &&  *s != ';' )
      *t++ = *s++;
    *t = 0;
    if ( *s )
      s++;
    DEBUGst(Field,fields[i]);
    i++;
    }
  OUT(apt_ParseFieldContent);
  return  fields;
  }

struct apt_field_contents *
apt__ParseFieldContents( self, string )
  register struct apt		     *self;
  register char			     *string;
  {
  register char			    **content, **field;
  register struct apt_field_contents *contents;
  register char			     *s, *s2, *t;
  register long			      i = 0, length = 0;

  IN(apt_ParseFieldContents);
  DEBUGst(String,string);
  contents = (struct apt_field_contents *)
    malloc( sizeof(struct apt_field_contents) +
	    max_field_content_items * sizeof(struct apt_field) );
  contents->count = 0;
  field = content = apt_ParseFieldContent( self, string );
  while ( *field )
    {
    s = s2 = *field;
    while ( *s2  &&  *s2 != '(' )
      { length++; s2++; }
    t = contents->field[i].name = (char *) malloc( length + 1 );
    while ( *s  &&  *s != '(' )
      *t++ = *s++;
    *t = 0;
    DEBUGst(Name,contents->field[i].name);
    if ( *s )  s++;
    s2 = s;
    length = 0;
    while( *s2  &&  *s2 != ')' )
      { length++; s2++; }
    t = contents->field[i].content = (char *) malloc( length + 1 );
    while ( *s  &&  *s != ')' )
      *t++ = *s++;
    *t = 0;
    DEBUGst(Content,contents->field[i].content);
    free( *field );
    field++;
    contents->count = ++i;
    }
  free( content );
  OUT(apt_ParseFieldContents);
  return  contents;
  }
