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

/* zipedit.h	Internal Macros						      */
/* Author	TC Peters						      */
/* Information Technology Center		   Carnegie-Mellon University */


/*
    $Log: zipedit.h,v $
 * Revision 1.4  1993/05/04  01:51:05  susan
 * RCS Tree Split
 *
 * Revision 1.2.1.1  1993/02/02  06:55:30  rr2b
 * new R6tape branch
 *
 * Revision 1.2  1992/12/15  04:00:36  rr2b
 * disclaimerization
 *
 * Revision 1.1  1992/10/06  18:33:00  susan
 * Initial revision
 *
 * Revision 2.5  1991/09/12  20:07:27  bobg
 * Update copyright notice
 *
 * Revision 2.4  1989/02/08  16:49:25  ghoti
 * change copyright notice
 *
 * Revision 2.3  88/11/16  18:55:35  tom
 * Add ManualRefresh macro.
 * 
 * Revision 2.2  88/10/12  14:37:33  tom
 * Add BackgroundExposed macro.
 * 
 * Revision 2.1  88/09/27  18:12:17  ghoti
 * adjusting rcs #
 * 
 * Revision 1.2  88/09/15  17:29:30  ghoti
 * copyright fix
 * 
 * Revision 1.1  88/09/14  17:43:49  tom
 * Initial revision
 * 
*/

/**  SPECIFICATION -- Internal Facility Suite  *********************************

TITLE	Zip Editor

MODULE	zipedit.h

NOTICE	IBM Internal Use Only

DESCRIPTION
	Internal Editor symbolics.

HISTORY
  03/31/88	Create for ATK (TCP)
  10/27/88	Add ManualRefresh (TCP)

END-SPECIFICATION  ************************************************************/

#define  Data			(self->data_object)
#define  View			(self->view_object)
#define  Edit			(self)
#define  IconFont		(self->icon_font)
#define  PointsFont		(self->points_font)
#define  DotsFont		(self->dots_font)
#define  KeyboardProcessor	(self->keyboard_processor)
#define  KeyboardAnchor		(self->keyboard_anchor)
#define  PendingProcessor	(self->pending_processor)
#define  PendingAnchor		(self->pending_anchor)

#define  Pane			(View->pane)
#define  ManualRefresh		(View->options.manual_refresh)
#define  Abeyant		(View->states.abeyant)
#define  Application		(View->states.application)
#define  CurrentPane		(View->current_pane)
#define  ViewWidth		(View->block.width)
#define  ViewHeight		(View->block.height)
#define  StreamModified		(Data->stream->zip_stream_states.zip_stream_state_modified)
#define  SetStreamModified	{Data->stream->zip_stream_states.zip_stream_state_modified=1;}
#define  Action			(self->action)
#define  Menu			(self->menu)
#define  ViewMenu		(View->menu)
#define  KeyState		(self->keystate)

#define  Objects(i)		((*View->objects)[i])

#define  GridExposed		(self->states.grid_exposed)
#define  CoordinatesExposed	(self->states.coordinates_exposed)
#define  PalettesExposed	(self->states.palettes_exposed)
#define  BackgroundExposed	(self->states.background_exposed)
#define  BackgroundSelected	(self->states.background_selected)
#define  BackgroundLightened	(self->states.background_lightened)
#define  EnclosureExposed	(self->states.enclosure_exposed)
#define  DuplicateSelection	(self->states.duplicate_selection)
#define  ForegroundPanning      (self->states.foreground_panning)
#define  Moving		        (self->states.moving)

#define  PriorX			(self->prior_x)
#define  PriorY			(self->prior_y)
#define  FontFamily		(self->font_family)
#define  FontHeight		(self->font_height)
#define  FontBold		(self->font_bold)
#define  FontItalic		(self->font_italic)
#define  FontVertical		(self->font_vertical)
#define  FontHorizontal		(self->font_horizontal)
#define  EnclosureLeft		(self->enclosure_left)
#define  EnclosureTop		(self->enclosure_top)
#define  EnclosureWidth		(self->enclosure_width)
#define  EnclosureHeight	(self->enclosure_height)
#define  EnclosureShadowStartX	(self->enclosure_shadow_start_x)
#define  EnclosureShadowStartY	(self->enclosure_shadow_start_y)
#define  EnclosureShadowLastX	(self->enclosure_shadow_last_x)
#define  EnclosureShadowLastY	(self->enclosure_shadow_last_y)
#define  EnclosedFigures	(self->enclosed_figures)

#define  LastPointX		((pane)->zip_pane_edit->zip_pane_edit_last_x_point)
#define  LastPointY		((pane)->zip_pane_edit->zip_pane_edit_last_y_point)
#define  LastPixelX		((pane)->zip_pane_edit->zip_pane_edit_last_x_pixel)
#define  LastPixelY		((pane)->zip_pane_edit->zip_pane_edit_last_y_pixel)
#define  SelectionLevel		((pane)->zip_pane_edit->zip_pane_edit_selection_level)
#define  BuildPending		((pane)->zip_pane_edit->zip_pane_edit_build_pending)
#define  Building		((pane)->zip_pane_edit->zip_pane_edit_build_figure)
#define  BackgroundPane		((pane)->zip_pane_edit->zip_pane_edit_background_pane)
#define  BackgroundData		((pane)->zip_pane_edit->zip_pane_edit_background_data)
#define  BackgroundView		((pane)->zip_pane_edit->zip_pane_edit_background_view)


#define  Flip			((pane)->zip_pane_x_flip)
#define  Flop			((pane)->zip_pane_y_flop)
#define  OriginX		((pane)->zip_pane_x_origin_offset)
#define  OriginY		((pane)->zip_pane_y_origin_offset)
#define  Multiplier		((pane)->zip_pane_stretch_zoom_multiplier)
#define  Divisor		((pane)->zip_pane_stretch_divisor)

#define  PaneLeft		(zipview_Pane_Left( View, pane ))
#define  PaneRight		(zipview_Pane_Right( View, pane ))
#define  PaneTop		(zipview_Pane_Top( View, pane ))
#define  PaneBottom		(zipview_Pane_Bottom( View, pane ))
#define  PaneWidth		(zipview_Pane_Width( View, pane ))
#define  PaneHeight		(zipview_Pane_Height( View, pane ))

#define ZIP_double_click_parameter  500
#define ZIP_motion_factor 1 /*===*/


#define  PaletteMode	    ((pane)->zip_pane_edit->zip_pane_edit_palette_mode)
#define  FontPalette	    (PaletteMode & zip_font_palette)
#define  NamePalette	    (PaletteMode & zip_name_palette)
#define  ShadePalette	    (PaletteMode & zip_shade_palette)
#define  AttributePalette   (PaletteMode & zip_attribute_palette)
#define  FigurePalette	    (PaletteMode & zip_figure_palette)
#define  HierarchyPalette   (PaletteMode & zip_hierarchy_palette)

#define  FigurePane	    (((ZIP_ecd)pane->zip_pane_client_data)->\
				editing_pane->zip_pane_edit->zip_pane_edit_prior_pane)

/*===
#define  EditPane	    (pane->zip_pane_edit->zip_pane_edit_containing_pane)
#define  ZIP_Editing_Pane( pane )\
	    (((ZIP_ecd)pane->zip_pane_client_data)->editing_pane)
===*/

#define  PalettePanes	    (pane->zip_pane_palette_panes)
#define  EditPane	    PalettePanes->zip_pane_palette_vector[ 0]
#define  NamesPane	    PalettePanes->zip_pane_palette_vector[ 1]
#define  ShadesPane	    PalettePanes->zip_pane_palette_vector[ 2]
#define  FiguresPane	    PalettePanes->zip_pane_palette_vector[ 3]
#define  AttributesPane	    PalettePanes->zip_pane_palette_vector[ 4]
#define  TLPane		    PalettePanes->zip_pane_palette_vector[ 5]
#define  TRPane		    PalettePanes->zip_pane_palette_vector[ 6]
#define  BLPane		    PalettePanes->zip_pane_palette_vector[ 7]
#define  BRPane		    PalettePanes->zip_pane_palette_vector[ 8]
#define  FontsPane	    PalettePanes->zip_pane_palette_vector[ 9]
#define  FontFamilyPane	    PalettePanes->zip_pane_palette_vector[10]
#define  FontHeightPane	    PalettePanes->zip_pane_palette_vector[11]
#define  FontItalicPane	    PalettePanes->zip_pane_palette_vector[12]
#define  FontBoldPane	    PalettePanes->zip_pane_palette_vector[13]
#define  FontSamplePane	    PalettePanes->zip_pane_palette_vector[14]
#define  HierarchyPane	    PalettePanes->zip_pane_palette_vector[15]

#define  TextPane	    PalettePanes->zip_pane_palette_vector[20]
#define  DotPane	    PalettePanes->zip_pane_palette_vector[21]
#define  LinePane	    PalettePanes->zip_pane_palette_vector[22]
#define  PolyLinePane	    PalettePanes->zip_pane_palette_vector[23]
#define  PolygonPane	    PalettePanes->zip_pane_palette_vector[24]
#define  TrapezoidPane	    PalettePanes->zip_pane_palette_vector[25]
#define  RectanglePane	    PalettePanes->zip_pane_palette_vector[26]
#define  PathPane	    PalettePanes->zip_pane_palette_vector[27]
#define  SprayPane	    PalettePanes->zip_pane_palette_vector[28]
#define  CirclePane	    PalettePanes->zip_pane_palette_vector[29]
#define  PicturePane	    PalettePanes->zip_pane_palette_vector[30]
#define  EllipsePane	    PalettePanes->zip_pane_palette_vector[31]
#define  RoundanglePane	    PalettePanes->zip_pane_palette_vector[32]
#define  SlantanglePane	    PalettePanes->zip_pane_palette_vector[33]
#define  ArcPane	    PalettePanes->zip_pane_palette_vector[34]

#define  PalettePaneCount   50

#define  PriorPane	    (pane->zip_pane_edit->zip_pane_edit_prior_pane)
#define  PriorBorderPattern (((ZIP_ecd)pane->zip_pane_client_data)->prior_border_pattern)
#define  PriorBorderFont    (((ZIP_ecd)pane->zip_pane_client_data)->prior_border_font)
#define  PriorBorderThickness  (((ZIP_ecd)pane->zip_pane_client_data)->prior_border_thickness)
#define  PriorXOrigin	    (((ZIP_ecd)pane->zip_pane_client_data)->prior_x_origin)
#define  PriorYOrigin	    (((ZIP_ecd)pane->zip_pane_client_data)->prior_y_origin)
#define  PriorWidth	    (((ZIP_ecd)pane->zip_pane_client_data)->prior_width)
#define  PriorHeight	    (((ZIP_ecd)pane->zip_pane_client_data)->prior_height)
#define  PriorClientData    (((ZIP_ecd)pane->zip_pane_client_data)->prior_client_data)

#define  EditingPane(pane)  (((ZIP_ecd)(pane)->zip_pane_client_data)->editing_pane)
#define  EditingIcon(pane)  (((ZIP_ecd)(pane)->zip_pane_client_data)->cursor_icon)
#define  EditingType(pane)  (((ZIP_ecd)(pane)->zip_pane_client_data)->figure_type)
#define  ForegroundPinning  (((ZIP_ecd)pane->zip_pane_client_data)->foreground_pinning)

#define  PendingFigureType  (((ZIP_ecd)pane->zip_pane_client_data)->\
				editing_pane->zip_pane_edit->zip_pane_edit_build_pending)

#define  EditGrid	    ((pane)->zip_pane_edit->zip_pane_edit_coordinate_grid)


#define  ZIP_current_stream_text_name		"ZIP_current_stream_name"
#define  ZIP_current_image_text_name		"ZIP_current_image_name"
#define  ZIP_current_figure_text_name		"ZIP_current_figure_name"
#define  ZIP_current_x_point_text_name		"ZIP_current_x_point_name"
#define  ZIP_current_y_point_text_name		"ZIP_current_y_point_name"
#define  ZIP_current_font_text_name		"ZIP_current_font_name"


#define  PointSelection			1
#define  FigureSelection		2
#define  ImageSelection			3

zip_type_figure				zipedit_Next_Selected_Figure();
