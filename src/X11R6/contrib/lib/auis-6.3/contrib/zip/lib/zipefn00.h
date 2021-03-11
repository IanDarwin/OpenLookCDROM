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

/* zipefn00.h	Zip Subroutine Library Symbolic Facility-names		      */
/* Author	TC Peters						      */
/* Information Technology Center		   Carnegie-Mellon University */

/*
$Log: zipefn00.h,v $
 * Revision 1.3  1993/05/04  01:51:05  susan
 * RCS Tree Split
 *
 * Revision 1.2.1.1  1993/02/02  06:55:47  rr2b
 * new R6tape branch
 *
 * Revision 1.2  1992/12/15  04:00:36  rr2b
 * disclaimerization
 *
 * Revision 1.1  1992/10/06  18:33:00  susan
 * Initial revision
 *
 * Revision 2.4  1991/09/12  20:07:40  bobg
 * Update copyright notice
 *
 * Revision 2.3  1989/02/08  16:49:31  ghoti
 * change copyright notice
 *
 * Revision 2.2  89/02/07  19:07:15  ghoti
 * first pass porting changes: filenames and references to them
 * 
 * Revision 2.1  88/09/27  18:12:41  ghoti
 * adjusting rcs #
 * 
 * Revision 1.2  88/09/15  17:30:20  ghoti
 * copyright fix
 * 
 * Revision 1.1  88/09/14  17:43:57  tom
 * Initial revision
 * 
 * Revision 1.1  87/10/28  21:38:23  tom
 * Initial revision
 * 
 * Revision 3.2  86/07/01  18:15:47  rl0t
 * ===== Log of changes checked in on Tue Jul  1 18:15:35 EDT 1986
 * Checked in zipefn00.h
 * Change log:
 * 
 * 
 * Revision 3.1  86/02/06  02:30:06  daemon
 * new root
 * 
 * Revision 2.1  86/02/06  02:30:01  daemon
 * release root
 * 
 * Revision 1.1  85/12/31  19:30:53  tom
 * Initial revision
 * 
*/

/**  SPECIFICATION -- External Facility Suite  *********************************

TITLE	Zip Subroutine Library Symbolic Facility-names

MODULE	zipefn00.h

NOTICE	IBM Internal Use Only

DESCRIPTION
	This file is to be included in the compilation of both client-programs
	and the Zip Subroutine Library modules. It defines the symbolc "Facility-
	names of Zip Subroutine Procedures/Functions.

HISTORY
  05/01/85	Created (TCP)
  05/16/86	Added names (RML)

END-SPECIFICATION  ************************************************************/

/* Symbolic External Facility Names (EFN) */


#define zip_lowest_figure_facility_name			0	/* Lowest Base-facility name defined */

#define zip_Unspecified_facility_Name_EFN	0
#define zip_Initialize_EFN			0
#define zip_Terminate_EFN			0
#define zip_Set_general_Exception_Handler_EFN	0
#define zip_Reset_general_Exception_Handler_EFN	0

#define zip_figure_EFN				0
#define zip_Image_Figure_EFN			0
#define zip_Figure_Image_EFN			0
#define zip_Figure_Name_EFN			0
#define zip_Figure_Type_EFN			0 /* ???? */
#define zip_01_EFN				0

#define zip_Containing_Stream_Figure_EFN	0
#define zip_Containing_Image_EFN		0
#define zip_Set_Figure_Exception_Handler_EFN	0
#define zip_Reset_Figure_Exception_Handler_EFN	0  
#define zip_Set_Figure_Text_EFN			0
#define zip_Set_Figure_Pattern_EFN		0
#define zip_Set_Figure_Font_EFN			0
#define zip_Display_Figure_EFN			0
#define zip_Draw_Figure_EFN			0
#define zip_Clear_Figure_EFN			0
#define zip_Which_Figure_EFN			0
#define zip_Highlight_Figure_Points_EFN		0
#define zip_Normalize_Figure_Points_EFN		0
#define zip_Hide_Figure_Points_EFN		0
#define zip_Expose_Figure_Points_EFN		0
#define zip_Change_Figure_Point_EFN		0
#define zip_Remove_Figure_Point_EFN		0
#define zip_Add_Figure_Point_EFN		0

#define zip_Acknowledge_Image_Status_Message_EFN 0 /* ???
#define zip_Issue_Image_Status_Message		0  ??? */


#define zip_highest_figure_facility_name	0	/* Highest Figure-facility name defined */

#define zip_lowest_image_facility_name		0	/* Lowest Image-facility name defined */

#define zip_Image_EFN				0
#define zip_Stream_Image_EFN			0
#define zip_Image_Name_EFN			0
#define zip_Image_01_EFN			0

#define zip_Containing_Stream_EFN		0
#define zip_Containing_Image_Image_EFN		0
#define zip_Containing_Image_EFN		0
#define zip_Set_Image_Exception_Handler_EFN	0
#define zip_Reset_Image_Exception_Handler_EFN	0 
#define zip_Set_Image_Text_EFN			0
#define zip_Set_Image_Pattern_EFN		0
#define zip_Set_Image_Font_EFN			0
#define zip_Display_Image_EFN			0
#define zip_Draw_Image_EFN			0
#define zip_Clear_Image_EFN			0
#define zip_Which_Image_EFN			0
#define zip_Highlight_Image_Points_EFN		0
#define zip_Normalize_Image_Points_EFN		0
#define zip_Hide_Image_Points_EFN		0
#define zip_Expose_Image_Points_EFN		0

#define zip_highest_image_facility_name		0	/* Highest Image-facility name defined */

#define zip_lowest_pane_facility_name		0	/* Lowest Pane-facility name defined */

#define zip_Pane_EFN				0
#define zip_Pane_Name_EFN			0
#define zip_Pane_Stream_EFN			0

#define zip_Create_Window_Pane_EFN		0
#define zip_Create_Panel_Layout_EFN		0
#define zip_Create_Layout_Pane_EFN		0
#define zip_Destroy_Pane_EFN			0
#define zip_Redisplay_Panes_EFN			0

#define zip_Set_Pane_Exception_Handler_EFN	0
#define zip_Reset_Pane_Exception_Handler_EFN	0 
#define zip_Set_Pane_Coordinates_EFN		0
#define zip_Set_Pane_Border_EFN			0
#define zip_Set_Pane_Stream_EFN			0
#define zip_Set_Pane_Image_EFN			0
#define zip_Set_Pane_Figure_EFN			0
#define zip_Set_Pane_Zoom_Factor_EFN		0


#define zip_Set_Pane_Text_EFN			0
#define zip_Set_Pane_Pattern_EFN		0
#define zip_Set_Pane_Font_EFN			0
#define zip_Draw_Pane_EFN			0
#define zip_Which_Pane_EFN			0
#define zip_Highlight_Pane_Points_EFN		0
#define zip_Normalize_Pane_Points_EFN		0
#define zip_Hide_Pane_Points_EFN		0
#define zip_Expose_Pane_Points_EFN		0

#define zip_Display_Pane_EFN			0
#define zip_Print_Pane_EFN			0
#define zip_Clear_Pane_EFN			0
#define zip_Invert_Pane_EFN			0
#define zip_Zoom_Pane_EFN			0
#define zip_Scale_Pane_EFN			0
#define zip_Handle_Planning_EFN			0
#define zip_Pan_Pane_EFN			0
#define zip_Pan_Pane_To_Edge_EFN		0
#define zip_Flip_Pane_EFN			0
#define zip_Flop_Pane_EFN			0
#define zip_Balance_Pane_EFN			0
#define zip_Hide_Pane_EFN			0
#define zip_Expose_Pane_EFN			0
#define zip_Hide Pane_Points_EFN		0
#define zip_Expose_Pane_Points_EFN		0
#define zip_Hide_Pane_Coordinates_EFN		0
#define zip_Expose_Pane_Coordinates_EFN		0
#define zip_Which_Pane_EFN			0

#define zip_highest_pane_facility_name		0	/* Highest Pane-facility name defined */

#define zip_lowest_stream_facility_name		0	/* Lowest Stream-facility name defined */

#define zip_Stream_EFN				0
#define zip_Stream_Name_EFN			0
#define zip_Create_Stream_EFN			0
#define zip_Destroy_Stream_EFN			0
#define zip_Open_Stream_EFN			0
#define zip_Close_Stream_EFN			0
#define zip_Read_Stream_EFN			0
#define zip_Write_Stream_EFN			0

#define zip_Set_Stream_Exception_Handler_EFN	0
#define zip_Reset_Stream_Exception_Handler_EFN	0  
#define zip_Set_Stream_Text_EFN			0
#define zip_Set_Stream_Pattern_EFN		0
#define zip_Set_Stream_Font_EFN			0
#define zip_Set_Stream_Source_EFN		0

#define zip_Display_Stream_EFN			0
#define zip_Draw_Stream_EFN			0
#define zip_Which_Stream_EFN			0
#define zip_Copy_Stream_EFN			0


#define zip_highest_stream_facility_name		80	/* Highest Stream-facility name defined */


