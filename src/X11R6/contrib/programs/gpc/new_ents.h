/* $XConsortium: new_ents.h,v 5.1 91/02/16 10:07:48 rws Exp $ */

/*
 */
/*--------------------------------------------------------------------*\
|
|  Copyright (C) 1989,1990, 1991, National Computer Graphics Association
|
|  Permission is granted to any individual or institution to use, copy, or
|  redistribute this software so long as it is not sold for profit, provided
|  this copyright notice is retained.
|
|                         Developed for the
|                National Computer Graphics Association
|                         2722 Merrilee Drive
|                         Fairfax, VA  22031
|                           (703) 698-9600
|
|                                by
|                 SimGraphics Engineering Corporation
|                    1137 Huntington Drive  Unit A
|                      South Pasadena, CA  91030
|                           (213) 255-0900
|---------------------------------------------------------------------
|
| Author        :	SimGraphics Engineering Corportation
|
| File          :	new_ents.h
| Date          :	Fri Feb  9 10:46:55 PST 1990
| Project       :	PLB
| Description   :	
| Status        :	Version 1.0
|
| Revisions     :	
|
\*--------------------------------------------------------------------*/
BIF_Activeview			*new_activeview();
BIF_Anno_text3			*new_annotext3();
BIF_Applytoglobal3		*new_applytoglobal3();
BIF_Applytolocal3		*new_applytolocal3();
BIF_Index			*new_backgroundcolorindex();
BIF_Beginstructure		*new_beginstructure();
BIF_Begintest			*new_begintest();
BIF_Callstructure		*new_callstructure();
BIF_Charheight			*new_charheight();
BIF_Charupvector		*new_charupvector();
BIF_Cleargeometry		*new_cleargeometry();
BIF_Colormodel			*new_colormodel();
BIF_Concatmatrix3		*new_concatmatrix3();
BIF_Defaultviewspecification	*new_defaultviewspecification();
BIF_Definecolor			*new_definecolor();
BIF_Definelight			*new_definelight();
BIF_Defineviewspecification	*new_defineviewspecification();
BIF_Endtest			*new_endtest();
BIF_Executestructure		*new_executestructure();
BIF_Fillareaset			*new_fillareaset();
BIF_Fillareaset3		*new_fillareaset3();
BIF_All				*new_generic();
BIF_Globaltransformation3	*new_globaltransformation3();
BIF_Hlhsremoval			*new_hlhsremoval();
BIF_Identity3			*new_identity3();
BIF_Indexpolygons3		*new_indexpolygons3();
BIF_Invertmatrix3		*new_invertmatrix3();
BIF_Lightstate			*new_lightstate();
BIF_Line			*new_line();
BIF_Line3			*new_line3();
BIF_Localtransformation3	*new_localtransformation3();
BIF_Marker			*new_marker();
BIF_Marker3			*new_marker3();
BIF_Matrix3			*new_matrix3();
BIF_Pause			*new_pause();
BIF_Pixelmap3			*new_pixelmap3();
BIF_Polygon			*new_polygon();
BIF_Polygon3			*new_polygon3();
BIF_Quadmesh3			*new_quadmesh3();
BIF_Readgeometry		*new_readgeometry();
BIF_Rotate3			*new_rotate3();
BIF_Rotatexyz3			*new_rotatexyz3();
BIF_Scale3			*new_scale3();
BIF_Simple_triple		*new_simple_triple();
BIF_Sleep			*new_sleep();
BIF_Text			*new_text();
BIF_Text3			*new_text3();
BIF_Textalign			*new_textalign();
BIF_Index			*new_textcolorindex();
BIF_Textfont			*new_textfont();
BIF_Textpath			*new_textpath();
BIF_Textprec			*new_textprec();
BIF_Translate3			*new_translate3();
BIF_Triangle3			*new_triangle3();
BIF_Vieworientation3		*new_vieworientation3();
BIF_Withdata3			*new_withdata3();
BIF_Nubc			*new_nubc();
BIF_Nubs			*new_nubs();

