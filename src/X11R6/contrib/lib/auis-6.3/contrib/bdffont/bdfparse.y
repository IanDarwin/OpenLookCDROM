/* bdfparse.y - grammar for bdf font descriptions */
/*
	Copyright Carnegie Mellon University 1991, 1992 - All rights reserved
	$Disclaimer: 
# Permission to use, copy, modify, and distribute this software and its 
# documentation for any purpose is hereby granted without fee, 
# provided that the above copyright notice appear in all copies and that 
# both that copyright notice, this permission notice, and the following 
# disclaimer appear in supporting documentation, and that the names of 
# IBM, Carnegie Mellon University, and other copyright holders, not be 
# used in advertising or publicity pertaining to distribution of the software 
# without specific, written prior permission.
# 
# IBM, CARNEGIE MELLON UNIVERSITY, AND THE OTHER COPYRIGHT HOLDERS 
# DISCLAIM ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING 
# ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS.  IN NO EVENT 
# SHALL IBM, CARNEGIE MELLON UNIVERSITY, OR ANY OTHER COPYRIGHT HOLDER 
# BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY 
# DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, 
# WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS 
# ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE 
# OF THIS SOFTWARE.
#  $
*/

%token STARTFONT
%token COMMENT
%token FONT
%token SIZE
%token FONTBOUNDINGBOX
%token STARTPROPERTIES
%token ENDPROPERTIES
%token CHARS
%token STARTCHAR
%token ENCODING
%token SWIDTH
%token DWIDTH
%token BBX
%token ATTRIBUTES
%token BITMAP
%token ENDCHAR
%token ENDFONT

%token FOUNDRY
%token DEFAULTCHAR
%token DEFAULTWIDTH
%token DEFAULTHEIGHT
%token DEFAULTX
%token DEFAULTY
%token DEFAULTDX
%token DEFAULTDY
%token ASCENT
%token DESCENT
%token FAMILY
%token RESX
%token RESY
%token WEIGHTNAME
%token WEIGHT

%token TOKEN

%start bdffont
%%
bdffont :	STARTFONT TOKEN
		{
		    char *tok = tlex_GetTokenText(self->lex);
		    self->version = (char *)malloc(strlen(tok) + 1);
		    strcpy(self->version, tok);

		    self->comments = (char *) malloc(bdfparse_Increment);
		    *self->comments = '\0';
		    bdfparse_Next = self->comments;
		    bdfparse_Limit = self->comments + bdfparse_Increment;
		}
		comments
		FONT	/* bdffont.tlx combines in FONT the rest of the line, up to newline */
		{
		    char *tmp = tlex_GetTokenText(self->lex);
		    self->fontname = (char *)malloc(strlen(tmp)+1);
		    for (; *tmp && !isspace(*tmp); tmp++);
		    for (; *tmp && isspace(*tmp); tmp++);
		    if (*tmp == '\0') {
			/* should report some error, really */
			tmp = tlex_GetTokenText(self->lex);
		    }
		    strcpy(self->fontname, tmp);
		    for (tmp = self->fontname; *tmp && *tmp!='\n'; tmp++);
		    *tmp = '\0';
		}
		SIZE TOKEN
		{
		    /* point size */

		    /* sample check */
		    /* if (sscanf(tlex_GetTokenText(self->lex),
				"%d", &self->pointsize) < 1)
		    {
			return parse_ERROR;
		    } */

		    self->pointsize = atol(tlex_GetTokenText(self->lex));
		}
		TOKEN
		{
		    /* x resolution */
		    self->resx = atol(tlex_GetTokenText(self->lex));
		}
		TOKEN
		{
		    /* y resolution */
		    self->resy = atol(tlex_GetTokenText(self->lex));
		}
		FONTBOUNDINGBOX TOKEN
		{
		    /* box width */
		    self->bbw = atol(tlex_GetTokenText(self->lex));
		}
		TOKEN
		{
		    /* box height */
		    self->bbh = atol(tlex_GetTokenText(self->lex));
		}
		TOKEN
		{
		    /* x origin */
		    self->bbx = atol(tlex_GetTokenText(self->lex));
		}
		TOKEN
		{
		    /* y origin */
		    self->bby = atol(tlex_GetTokenText(self->lex));
		}
		opt_properties
		CHARS TOKEN
		comments2 char_defns
		ENDFONT
;

comments:	/* empty */
    |		comments COMMENT
		{
		    char *tok = tlex_GetTokenText(self->lex);
		    long length = strlen(tok);
		    bdfparse_EnsureStorage(&self->comments, length + 1
				+ sizeof ("COMMENT\n"));
		    strcpy(bdfparse_Next, "COMMENT");
		    strcat(bdfparse_Next, tok);
		    strcat(bdfparse_Next, "\n");
		    bdfparse_Next += length;
		}
;

comments1:	/* empty */
    |		comments1 COMMENT
		{
		}
;

comments2:	/* empty */
    |		comments2 COMMENT
		{
		}
;

opt_properties :/* empty */
    |		STARTPROPERTIES TOKEN
		{
		    self->properties = (char *) malloc(bdfparse_Increment);
		    *self->properties = '\0';
		    bdfparse_Next = self->properties;
		    bdfparse_Limit = self->properties + bdfparse_Increment;
		}
		comments1 properties ENDPROPERTIES
		{
		    self->proplength = bdfparse_Next - self->properties;
		}
;

properties :	/* empty */
    |
		properties TOKEN
		{
		    long length;

		    bdfparse_Property = $2;
		    switch ($2) {
			case DEFAULTCHAR:
			    bdfparse_PropertyValue = &self->defaultchar;
			    break;
			case DEFAULTWIDTH:
			    bdfparse_PropertyValue = &self->defaultw;
			    break;
			case DEFAULTHEIGHT:
			    bdfparse_PropertyValue = &self->defaulth;
			    break;
			case DEFAULTX:
			    bdfparse_PropertyValue = &self->defaultx;
			    break;
			case DEFAULTY:
			    bdfparse_PropertyValue = &self->defaulty;
			    break;
			case DEFAULTDX:
			    bdfparse_PropertyValue = &self->defaultdx;
			    break;
			case DEFAULTDY:
			    bdfparse_PropertyValue = &self->defaultdy;
			    break;
			case ASCENT:
			    bdfparse_PropertyValue = &self->ascent;
			    break;
			case DESCENT:
			    bdfparse_PropertyValue = &self->descent;
			    break;
			case RESX:
			    bdfparse_PropertyValue = &self->resx;
			    break;
			case RESY:
			    bdfparse_PropertyValue = &self->resy;
			    break;
			case WEIGHT:
			    bdfparse_PropertyValue = &self->fontweight;
			    break;
			case FAMILY:
			case FOUNDRY:
			case WEIGHTNAME:
			    /* nothing else to do */
			    break;
			default:
			    bdfparse_Property = 0;
			    bdfparse_PropertyValue = NULL;
			    break;
		    }

		    if ((bdfparse_Property != FAMILY) &&
			(bdfparse_Property != WEIGHTNAME))
		    {
			char *tok = tlex_GetTokenText(self->lex);
			length = strlen(tok);
			bdfparse_EnsureStorage(&self->properties, length + 2);
			strcpy(bdfparse_Next, tok);
			bdfparse_Next += length;
			*bdfparse_Next++ = ' ';
			*bdfparse_Next = '\0';
		    }
		}
		TOKEN
		{
		    long l;

		    if (bdfparse_Property == FAMILY) {
			char *tok = tlex_GetTokenText(self->lex);
			l = strlen(tok);
			self->fontfamily = (char *) malloc(l - 1);
			strncpy(self->fontfamily, tok + 1, l - 2);
			self->fontfamily[l-2] = '\0';
		    }
		    else if (bdfparse_Property != WEIGHTNAME) {
			if ((*tlex_GetTokenText(self->lex) == '"') ||
			    (bdfparse_PropertyValue == NULL))
			{
			    char *tok = tlex_GetTokenText(self->lex);
			    l = strlen(tok);
			    bdfparse_EnsureStorage(&self->properties, l + 2);
			    strcpy(bdfparse_Next, tok);
			    bdfparse_Next += l;
			    *bdfparse_Next++ = '\n';
			    *bdfparse_Next = '\0';
			}
			else {
			    l = atol(tlex_GetTokenText(self->lex));

			    bdfparse_EnsureStorage(&self->properties,
						   bdfprop_INT_LEN + 2);
			    sprintf(bdfparse_Next,
				    "%*d\n", bdfprop_INT_LEN, l);
			    bdfparse_Next += bdfprop_INT_LEN + 1;

			    *bdfparse_PropertyValue = l;
			}
		    }
		}
		comments1
;

char_defns :	/* empty */
    |		char_defns char_defn
;

char_defn :	STARTCHAR  /* TOKEN (the token is reported in token text) */
		{
		    /* character name */
		    self->activedefns++;
		    strncpy(bdffont_ReadCharDefn.name,
			    tlex_GetTokenText(self->lex),
			    sizeof(bdffont_ReadCharDefn.name));
		    bdffont_ReadCharDefn.name[sizeof(bdffont_ReadCharDefn.name) - 1] = '\0';
		}
		ENCODING char_encoding
		SWIDTH TOKEN
		{
		    /* swidth x */
		    bdffont_ReadCharDefn.swx = $6;
		}
		TOKEN
		{
		    /* swidth y */
		    bdffont_ReadCharDefn.swy = $8;
		}
		DWIDTH TOKEN
		{
		    /* dwidth x */
		    bdffont_ReadCharDefn.dwx = $11;
		}
		TOKEN
		{
		    /* dwidth y */
		    bdffont_ReadCharDefn.dwy = $13;
		}
		BBX TOKEN
		{
		    /* char width */
		    bdffont_ReadCharDefn.bbw = $16;
		}
		TOKEN
		{
		    /* char height */
		    bdffont_ReadCharDefn.bbh = $18;
		}
		TOKEN
		{
		    /* char origin x */
		    bdffont_ReadCharDefn.bbx = $20;
		}
		TOKEN
		{
		    /* char origin y */
		    bdffont_ReadCharDefn.bby = $22;
		}
		opt_attrs
		BITMAP
		ENDCHAR
		{
		    if (bdffont_ReadCharDefn.encoding >= 0) {
			EnsureDefns(self, bdffont_ReadCharDefn.encoding);
			self->defns[bdffont_ReadCharDefn.encoding] =
			  bdffont_ReadCharDefn;
			self->lastcharloaded = bdffont_ReadCharDefn.encoding;
		    }
		    else {
			int cnum;
			self->lastcharloaded++;
			cnum = self->lastcharloaded;
			EnsureDefns(self, cnum);
			self->defns[cnum] = bdffont_ReadCharDefn;
		    }
		}
;

char_encoding :	TOKEN
		{
		    /* character's index */
		    bdffont_ReadCharDefn.encoding = $1;
		}
    |		TOKEN TOKEN
		{
		    /* character's index */
		    bdffont_ReadCharDefn.encoding = $2;
		}
;

opt_attrs :	/* empty */
		{
		    bdffont_ReadCharDefn.attributes = 0;
		}
    |		ATTRIBUTES TOKEN
		{
		    /* character's index */
		    sscanf(tlex_GetTokenText(self->lex),
			   "%X",
			   &bdffont_ReadCharDefn.attributes);
		}
;
