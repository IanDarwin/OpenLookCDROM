/*
 * This file is a product of Sun Microsystems, Inc. and is provided for
 * unrestricted use provided that this legend is included on all tape
 * media and as a part of the software program in whole or part.
 * Users may copy, modify or distribute this file at will.
 * 
 * THIS FILE IS PROVIDED AS IS WITH NO WARRANTIES OF ANY KIND INCLUDING THE
 * WARRANTIES OF DESIGN, MERCHANTIBILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE, OR ARISING FROM A COURSE OF DEALING, USAGE OR TRADE PRACTICE.
 * 
 * This file is provided with no support and without any obligation on the
 * part of Sun Microsystems, Inc. to assist in its use, correction,
 * modification or enhancement.
 * 
 * SUN MICROSYSTEMS, INC. SHALL HAVE NO LIABILITY WITH RESPECT TO THE
 * INFRINGEMENT OF COPYRIGHTS, TRADE SECRETS OR ANY PATENTS BY THIS FILE
 * OR ANY PART THEREOF.
 * 
 * In no event will Sun Microsystems, Inc. be liable for any lost revenue
 * or profits or other special, indirect and consequential damages, even
 * if Sun has been advised of the possibility of such damages.
 * 
 * Sun Microsystems, Inc.
 * 2550 Garcia Avenue
 * Mountain View, California  94043
 *
 * Modifications to the original Sun Microsystems, Inc. source code
 * made by the Grasshopper Group are in the Public Domain.
 *
 * Extensions to this file by Eric Messick of the Grasshopper Group.
 *
 * Grasshopper Group
 * 212 Clayton St
 * San Francisco, CA 94117
 *
 */

#ifndef lint
static char sccsid[] = "@(#)selection.c 9.4 88/01/19 Copyright 1987 Sun Micro";
static	char RCSid[] =
	"@(#)$Header: /it/grass/gterm/RCS/selection.c,v 2.4 1991/04/23 06:52:17 hugh Grass2 $";
#endif

/*
 * Copyright (c) 1987 by Sun Microsystems, Inc. 
 */

/*-
	selection.c

	selection.c, Fri Feb 27 23:51:49 1987

 */

/*
 * Selection module 
 *
 * The external interfaces of this module are
 * tc_extend_to(col, row, rank, rect)
 * tc_report_selection(rank)
 * tc_deselect(rank)
 * tc_refresh_selection()
 */

#include	<sys/types.h>
#ifdef REF
#include	<ref/config.h>
#endif
#include	"screen.h"
#include	"tcap.h"

extern int	CharsPerLine;
extern int	LinesPerScreen;
extern int	ScrollY;
extern int	ScrollLength;
extern int	ScrollLinesSaved;

static int FixedRow, FixedCol, FloatRow, FloatCol;
static int ValidSelectionRange = 0 ;
static int FixedPointSet = 0 ;
static int Selecting = 0 ;
static int RectSelection = 0 ;
static char spaces[MaxCharsPerLine];

#define SelectionThreshold 32000

StartSelection(col, row)
int col, row;
{
	if (FixedPointSet)
		return;
	if (row<ScrollLinesSaved-ScrollLength)
		row = ScrollLinesSaved - ScrollLength ;
	if (row>=ScrollLength) row = ScrollLength - 1 ;
	if (col<0) col = 0 ;
	if (RectSelection) {
		if (col>=CharsPerLine) col = CharsPerLine - 1 ;
		}
	else	{
		if (col>lines[row]->length) col = lines[row]->length ;
		}
	DeSelect();
	FixedRow = row ;
	FixedCol = col ;
	FixedPointSet = 1 ;
}

ExtendTo(col, row, strokeoutline)
int col, row, strokeoutline;
{
/*	if (row<ScrollY)
		scroll_to(ScrollY - ScrollLength + ScrollLinesSaved - row); */
	if (row<ScrollLength-ScrollLinesSaved)
		row = ScrollLength - ScrollLinesSaved ;
/*	if (row>=ScrollY+LinesPerScreen)
		scroll_to(ScrollY - ScrollLength + ScrollLinesSaved +
				LinesPerScreen - row) ; */
	if (row>=ScrollLength) row = ScrollLength - 1 ;
	if (col<0) col = 0 ;
	if (RectSelection) {
		if (col>=CharsPerLine) col = CharsPerLine - 1 ;
		}
	else	{
		if (col>lines[row]->length) col = lines[row]->length ;
		}
	if (ValidSelectionRange) {
		if (FloatRow==FixedRow) {
			if (FloatCol > FixedCol)
				ChooseFixed(FixedCol, FixedRow,
						FloatCol, FloatRow, col, row);
			else	ChooseFixed(FloatCol, FloatRow,
						FixedCol, FixedRow, col, row);
			}
		else	if (FloatRow > FixedRow)
			ChooseFixed(FixedCol, FixedRow,
					FloatCol, FloatRow, col, row);
		else	ChooseFixed(FloatCol, FloatRow,
					FixedCol, FixedRow, col, row);
		}
	else	StartSelection(col, row);
	FloatRow = row ;
	FloatCol = col ;
	if (FloatRow==FixedRow) {
		if (FloatCol > FixedCol)
			HiLight(FixedCol, FixedRow,
					FloatCol, FloatRow, strokeoutline);
		else	HiLight(FloatCol, FloatRow,
					FixedCol, FixedRow, strokeoutline);
		}
	else	if (FloatRow > FixedRow)
		HiLight(FixedCol, FixedRow, FloatCol, FloatRow, strokeoutline);
	else	HiLight(FloatCol, FloatRow, FixedCol, FixedRow, strokeoutline);
}

ReportSelection()
{
	if (FloatRow==FixedRow) {
		if (FloatCol > FixedCol)
			Report(FixedCol, FixedRow, FloatCol, FloatRow);
		else	Report(FloatCol, FloatRow, FixedCol, FixedRow);
		}
	else	if (FloatRow > FixedRow)
		Report(FixedCol, FixedRow, FloatCol, FloatRow);
	else	Report(FloatCol, FloatRow, FixedCol, FixedRow);
	FixedPointSet = 0 ;
}

ChooseFixed(startcol, startrow, endcol, endrow, col, row)
int startcol, startrow, endcol, endrow, col, row;
{
	if (RectSelection) {
		if (abs(startrow-row) > abs(endrow-row))
			row = startrow ;
		else	row = endrow ;
		if (abs(startcol-col) > abs(endcol-col))
			col = startcol ;
		else	col = endcol ;
		StartSelection(col, row) ;
		return;
		}
	if ((row < startrow) || ((row == startrow) && (col <= startcol))) {
		StartSelection(endcol, endrow);
		return;
		}
	if ((row > endrow) || ((row == endrow) && (col >= endcol))) {
		StartSelection(startcol, startrow);
		return;
		}
	if (countchars(startcol, startrow, col, row) <
	    countchars(col, row, endcol, endrow)) {
		StartSelection(endcol, endrow);
		}
	else	{
		StartSelection(startcol, startrow);
		}
}

countchars(startcol, startrow, endcol, endrow)
int startcol, startrow, endcol, endrow;
{
	int count ;
	struct line **sp;

	count = endcol-startcol ;
	for (sp = &lines[startrow]; startrow<endrow; sp++, startrow++)
		count += (*sp)->length ;
	return count;
}

HiLight(startcol, startrow, endcol, endrow, strokeoutline)
int startcol, startrow, endcol, endrow, strokeoutline;
{
	int row, col;
	struct line **sp;
	extern int fontisfixedwidth;

	startrow -= ScrollY ;
	endrow -= ScrollY ;

	if (RectSelection) {
		if (startrow>endrow) {
			row = startrow ;
			startrow = endrow ;
			endrow = row ;
			}
		if (startcol>endcol) {
			col = startcol ;
			startcol = endcol ;
			endcol = col ;
			}
		if (endrow < 0 || startrow >= LinesPerScreen) return;
		if (startrow < 0) startrow = 0 ;
		if (endrow >= LinesPerScreen) endrow = LinesPerScreen - 1 ;
		if (fontisfixedwidth) {
			HiLightRect(startcol, startrow,
					endcol, endrow, strokeoutline);
			}
		else	{
			HiLightRect(startcol, startrow,
					endcol, endrow, strokeoutline);
				/* punt for now */
			}
		}
	else	{
		if (startrow < 0) {
			startrow = 0 ;
			startcol = 0 ;
			}
		if (endrow >= LinesPerScreen) {
			endrow = LinesPerScreen - 1 ;
			endcol = screen[endrow]->length ;
			}
		StartHiLighting(strokeoutline);
		for (sp = &lines[startrow+ScrollY], row = startrow;
				row<endrow;
				sp++, row++)
			if (fontisfixedwidth)
				HiLightLine((*sp)->length);
			else	{
				(*sp)->body[(*sp)->length] = ' ' ;
				StrHiLightLine((*sp)->body, (*sp)->length + 1);
				}
		if (fontisfixedwidth)
			EndHiLighting(endcol, startcol, startrow);
		else	{
			(*sp)->body[(*sp)->length] = ' ' ;
			StrEndHiLighting((*sp)->body, endcol+1,
					lines[startrow+ScrollY]->body,
					startcol, startrow);
			}
		}
}

Report(startcol, startrow, endcol, endrow)
int startcol, startrow, endcol, endrow;
{
	int row, col, selection_length, extra;
	struct line **sp;

	selection_length = 0 ;

	StartSavingSelection();
	if (RectSelection) {
		RePaintHiLight();
		if (startrow>endrow) {
			row = startrow ;
			startrow = endrow ;
			endrow = row ;
			}
		if (startcol>endcol) {
			col = startcol ;
			startcol = endcol ;
			endcol = col ;
			}
		for (sp = &lines[startrow];
				startrow<=endrow;
				startrow++, sp++) {
			selection_length += endrow - startrow + 1 ;
			if (selection_length > SelectionThreshold) {
				bugprintf("Selection truncated. Max %d chars",
					SelectionThreshold, 0, 0);
				break;
				}
			if ((*sp)->length < startcol) {
				SaveSelectionPiece(spaces, endcol-startcol+1);
				}
			else if ((*sp)->length < endcol) {
				SaveSelectionPiece(&((*sp)->body[startcol]),
						(*sp)->length - startcol);
				SaveSelectionPiece(spaces,
						endcol - (*sp)->length + 1);
				}
			else	{
				SaveSelectionPiece(&((*sp)->body[startcol]),
						endcol - startcol + 1);
				}
			SaveSelectionPiece("\r", 1);
			}
		}
	else	{
		sp = &lines[startrow] ;
		if (startrow == endrow) {
			if (endcol >= (*sp)->length)
				extra = 0 ;
			else	extra = 1 ;
			if ((*sp)->buffer_length > 0)
				SaveSelectionPiece(&((*sp)->body[startcol]),
						endcol - startcol + extra);
			(*sp)->changeposition = 0 ;
			(*sp)->end_of_changes = CharsPerLine ;
			if ((*sp)->buffer_length > 0)
				for (; startcol<=endcol; startcol++)
					(*sp)->prop[startcol] |= PrimSelMode ;
			}
		else	{
			selection_length = (*sp)->length - startcol + 1 ;
			SaveSelectionPiece(&((*sp)->body[startcol]),
						(*sp)->length - startcol);
			if ((((*sp)->flags) & LINE_WRAPPED) == 0)
				SaveSelectionPiece("\r", 1);
			(*sp)->changeposition = 0 ;
			(*sp)->end_of_changes = CharsPerLine ;
			if ((*sp)->buffer_length > 0)
				for (; startcol<=(*sp)->length; startcol++)
					(*sp)->prop[startcol] |= PrimSelMode ;
			for (sp++, startrow++;
					startrow<endrow;
					sp++, startrow++) {
				selection_length += (*sp)->length + 1 ;
				if (selection_length > SelectionThreshold) {
					break;
					}
				if ((*sp)->buffer_length > 0)
					SaveSelectionPiece((*sp)->body,
							(*sp)->length);
				if ((((*sp)->flags) & LINE_WRAPPED) == 0)
					SaveSelectionPiece("\r", 1);
				(*sp)->changeposition = 0 ;
				(*sp)->end_of_changes = CharsPerLine ;
				if ((*sp)->buffer_length > 0)
					for (startcol=0;
						startcol<=(*sp)->length;
						startcol++)
						(*sp)->prop[startcol] |=
							PrimSelMode ;
				}
			selection_length += (endcol >= (*sp)->length) ?
						(*sp)->length+1 : endcol+1;
			if ((*sp)->buffer_length > 0)
				SaveSelectionPiece((*sp)->body,
						(endcol >= (*sp)->length) ?
						(*sp)->length : endcol+1);
			if (selection_length > SelectionThreshold) {
				bugprintf("Selection truncated. Max %d chars",
						SelectionThreshold, 0, 0);
				}
			else	{
				(*sp)->changeposition = 0 ;
				(*sp)->end_of_changes = CharsPerLine ;
				if ((*sp)->buffer_length > 0)
					for (startcol=0;
						startcol<=((endcol >=
							(*sp)->length) ?
							(*sp)->length :
							endcol);
						startcol++)
						(*sp)->prop[startcol] |=
							PrimSelMode ;
				}
			}
		if (endcol>=(*sp)->length)
			SaveSelectionPiece("\r", 1);
		}
	FinishSavingSelection();
	TakeDownOutline();
}

DeSelect()
{
	int row, col, endrow;

	if (ValidSelectionRange == 0) return;
	if (RectSelection) {
		RePaintHiLight();
		}
	else	{
		if (FixedRow > FloatRow) {
			row = FloatRow;
			endrow = FixedRow;
			}
		else	{
			row = FixedRow;
			endrow = FloatRow;
			}
		for (; row<=endrow; row++) {
			lines[row]->changeposition = 0 ;
			lines[row]->end_of_changes = CharsPerLine;
			if (lines[row]->buffer_length > 0)
				for(col=0; col<lines[row]->length+1; col++)
					lines[row]->prop[col] &= ~PrimSelMode ;
			}
		}
	ClearSelectionPath();
}

/* --------------- External Routines Below ------------------- */
tc_init_selection()
{
	int i;
	char *s;

	for (i=0, s=spaces; i<MaxCharsPerLine; i++, s++)
		*s = ' ' ;
}
/*ARGSUSED*/
tc_extend_selection(col, row, rank, rect)
    int	col, row, rank, rect;
{
	if (rect)
		RectSelection = 1 ;
	ExtendTo(col, row + ScrollY, 1);
	ValidSelectionRange = 1 ;
	Selecting = 1 ;
}
/*ARGSUSED*/
tc_report_selection(rank)
int rank;
{
	if (Selecting) {
		ReportSelection();
		Selecting = 0 ;
		}
}
/*ARGSUSED*/
tc_deselect(rank)
    int rank;
{
	if (!Selecting) {
		DeSelect();
		ValidSelectionRange = 0 ;
		RectSelection = 0 ;
		}
}

tc_take_down_selection()
{
	if (!Selecting && ValidSelectionRange && RectSelection) {
		RePaintHiLight();
		}
}

tc_refresh_selection()
{
	ClearSelectionPath();
	if (!Selecting && ValidSelectionRange && RectSelection) {
		ExtendTo(FloatCol, FloatRow, 0);
		FixedPointSet = 0 ;
		RePaintHiLight();
		}
}
