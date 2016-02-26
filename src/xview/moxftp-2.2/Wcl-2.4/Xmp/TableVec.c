/*LINTLIBRARY*/
/*
 * SCCS_data:    @(#) TableVec.c	1.10 92/12/04 07:06:00
 */

#include <X11/Xmp/COPY>
#include <X11/IntrinsicP.h>

#ifdef sun
#include <X11/ObjectP.h>        /* why don't they just use X from mit!?! */
#include <X11/RectObjP.h>
#endif

#include <X11/Xmp/TableP.h>

/* TableVector Methods
**=====================**
   Each Table instance has two TableVectors: one describes the columns, and the
   other describes the rows.

   The table vectors are created based on information in the real_layout
   TableLoc array, hence they must be created after the real_layout, and they
   must be updated when the real_layout changes.  The real_layout data upon
   which the vectors depend is: number of cols, number of rows, options (only
   TBL_SM_WIDTH and TBL_SM_HEIGHT).
*/

XmpTableVector XmpTableVectorNew( size, tw, do_col )
    int			size;
    XmpTableWidget	tw;
    int			do_col;
{
    int			minimize, dontStretch, first_slot, last_slot, slot;
    XmpTableLoc  	loc = tw->table.real_layout;
    XmpTableVector	vec;

    if ( (XmpTableLoc)0 == loc || 0 == size )
	return (XmpTableVector)0;

    vec = (XmpTableVector)XtCalloc( size+1, sizeof(XmpTableVectorRec));

    /* Determine which slots need to be minimized
    */
    for (  ;  loc->w  ;  loc++ )
    {
	if ( do_col )
	{
	    minimize	= loc->options & TBL_SM_WIDTH;
	    dontStretch	= loc->options & TBL_LK_WIDTH;
	    first_slot	= loc->col;
	    last_slot	= loc->col + loc->col_span;
	}
	else
	{
	    minimize	= loc->options & TBL_SM_HEIGHT;
	    dontStretch	= loc->options & TBL_LK_HEIGHT;
	    first_slot	= loc->row;
	    last_slot	= loc->row + loc->row_span;
	}

	if ( minimize )
	    for ( slot = first_slot ; slot < last_slot ; slot++ )
		vec[ slot ].options |= TBL_VEC_MINIMIZE;

	if ( dontStretch )
	    for ( slot = first_slot ; slot < last_slot ; slot++ )
		vec[ slot ].options |= TBL_VEC_LOCK;
    }
    XmpTableVectorMinimize( vec, size, tw, do_col );
    return vec;
}

void XmpTableVectorFree( vec )
    XmpTableVector vec ;
{
    XtFree( (char*)vec );
}


/* Minimize Column Widths and Row Heights
**========================================**
   Change the vector to be its minimum size in the direction indicated.
   If TBL_VEC_MINIMIZE (i.e., TBL_SM_WIDTH (W) or TBL_SM_HEIGHT (H)) then
   the widget is kept to its original size. 
   TBL_VEC_LOCK (i.e., TBL_LK_WIDTH (w) or TBL_LK_HEIGHT (h) ) not checked,
   because such widgets DO grow to the minimum size of the column or row.
*/
void XmpTableVectorMinimize( vec, vec_len, tw, do_col )
    XmpTableVector	vec;
    int			vec_len;
    XmpTableWidget	tw;
    int			do_col;
{
    int         i;
    XmpTableLoc loc                 = tw->table.real_layout;
    Widget	resize_child        = tw->table.resize_child;
    int         resize_width        = tw->table.resize_width;
    int         resize_height       = tw->table.resize_height;
    int         resize_border_width = tw->table.resize_border_width;

    if ( (XmpTableVector)0 == vec || 0 == vec_len )
	return;

    /* Sort real_layout (in-place) by the number of columns or rows each child
    ** spans so we first compute sizes of individual columns or rows, then 
    ** compute the spanned columns or rows.
    */
    if ( do_col )
	qsort( (char*)loc,			/* sort real_layout	*/
		XmpTableLocLen(loc),
		sizeof(XmpTableLocRec),
		XmpTableLocCompareColSpan );	/* compare column spans	*/
    else
	qsort( (char*)loc,			/* sort real_layout	*/
		XmpTableLocLen(loc),
		sizeof(XmpTableLocRec),
		XmpTableLocCompareRowSpan );	/* compare row spans	*/

    /* Reset all width|heights to zero, then expand to fit
    */
    for ( i = 0 ; i < vec_len ; i++ )
	vec[i].value = 0;

    for (  ;  loc->w  ;  loc++ )
    {
	int pref;

	/* Check for simple case (span of 1), where col or row just becomes
	** large enough for largest child in that col or row.
	*/
	if (  do_col && loc->col_span == 1 )
	{
	    pref = XmpTableLocPreferredWidth( loc, tw );

	    if ( pref > vec[ loc->col ].value )
		vec[ loc->col ].value = pref;
	}
	else if ( !do_col && loc->row_span == 1 )
	{
	    pref = XmpTableLocPreferredHeight( loc, tw );

	    if ( pref > vec[ loc->row ].value )
		vec[ loc->row ].value = pref;
	}

	else
	{
	    /* Spans multiple columns or rows.  We have already set each
	    ** column or row to the individual size requirements, now we can
	    ** see which spanned columns or rows need to be stretched.  The
	    ** span width includes inter-column or inter-row spacing.
	    */
	    int to_stretch, span_size, first_slot, stop_before, slot;
	    int can_stretch = 0;

	    if ( do_col )
	    {
		span_size   = tw->table.col_spacing * (loc->col_span-1);
		first_slot  = loc->col;
		stop_before = loc->col + loc->col_span;
	    }
	    else
	    {
		span_size   = tw->table.row_spacing * (loc->row_span-1);
		first_slot  = loc->row;
		stop_before = loc->row + loc->row_span;
	    }
	    for ( slot = first_slot  ;  slot < stop_before  ;  slot++ )
	    {
		if ( 0 == (vec[ slot ].options & TBL_VEC_LOCK) )
		    can_stretch++;
		span_size += vec[ slot ].value;
	    }

	    /* If none of the slots can stretch, then we still must force
	    ** them all to stretch at least to the orig_size of this widget.
	    */
	    if ( 0 == can_stretch )
	    {
		if ( do_col )
		{
		    to_stretch	= loc->col_span;
		    if ( loc->w == resize_child )
			pref = resize_width + 2 * resize_border_width;
		    else
			pref = loc->orig_width + 2 * loc->orig_border_width;
		}
		else
		{
		    to_stretch	= loc->row_span;
		    if ( loc->w == resize_child )
			pref = resize_height + 2 * resize_border_width;
		    else
			pref = loc->orig_height + 2 * loc->orig_border_width;
		}
	    }
	    else
	    {
		to_stretch = can_stretch;
		if ( do_col )
		{
		    pref = XmpTableLocPreferredWidth( loc, tw );
		}
		else
		{
		    pref = XmpTableLocPreferredHeight( loc, tw );
		}
	    }

	    if ( span_size < pref )
	    {
		/* Increase size of some or all slots: if nothing
		** can stretch, expand every slot, else expand only
		** those which are not locked small.
		*/
		int excess	= pref - span_size;
		int amt		= excess / to_stretch;
		int truncated	= excess - amt*to_stretch;

		for ( slot = first_slot  ;  slot < stop_before  ;  slot++ )
		{
		    if ( 0 == can_stretch
		      || 0 == (vec[ slot ].options & TBL_VEC_LOCK) )
		    {
			if ( truncated )
			{
			    vec[ slot ].value += amt + 1;
			    --truncated;
			}
			else
			    vec[ slot ].value += amt;
		    }
		}
	    }
	}
    }
    /* The vector is minimized: set pref_value from value
    */
    for ( i = 0 ; i < vec_len ; i++ )
	vec[i].pref_value = vec[i].value;
}

/* Total Width or Height
**=======================**
    Including inter-column and inter-row spacing, and margins.  Works
    even when there are no columns or rows (vec==num==0).
*/
#define DO_ACTUAL 1
#define DO_PREFERRED 0

static int XmpTableVectorSize( vec, num, tw, do_col, do_actual )
    XmpTableVector	vec;
    int			num;
    XmpTableWidget	tw;
    int			do_col;
    int			do_actual;
{
    int slot, size, space;

    size = 2*tw->manager.shadow_thickness;
    if (do_col)
    {
	space = tw->table.col_spacing;
	size += 2*tw->bulletin_board.margin_width;
    }
    else
    {
	space = tw->table.row_spacing;
	size += 2*tw->bulletin_board.margin_height;
    }

    if ( 0 != num && (XmpTableVector)0 != vec )
    {
	if (do_actual)
	{
	    for ( size -= space, slot = 0  ;  slot < num  ;  slot++ )
		size += vec[ slot ].value + space;
	}
	else
	{
	    for ( size -= space, slot = 0  ;  slot < num  ;  slot++ )
		size += vec[ slot ].pref_value + space;
	}
    }

    if ( size > 0 )
	return size;
    else
	return 1;	/* minimum size */
}

int XmpTableVectorTotalSize( vec, num, tw, do_col )
    XmpTableVector	vec;
    int			num;
    XmpTableWidget	tw;
    int			do_col;
{
    return XmpTableVectorSize( vec, num, tw, do_col, DO_ACTUAL );
}

int XmpTableVectorPreferredSize( vec, num, tw, do_col )
    XmpTableVector	vec;
    int			num;
    XmpTableWidget	tw;
    int			do_col;
{
    return XmpTableVectorSize( vec, num, tw, do_col, DO_PREFERRED );
}

#undef DO_ACTUAL
#undef DO_PREFERRED


/* Adjust rows or columns
**========================**
   When a parent re-sizes a Table, it can make it larger or smaller.  The
   adjustment is distributed as a ratio of the preferred sizes of the
   col/row, so small ones change slower than larger ones.

   If the table wants to restrict making things smaller than preferred,
   then it must simply never respond to resize commands which make
   the table smaller than its preferred size.  Nowhere in the logic below
   is there any mechanism which prevents things from shrinking smaller
   than the preferred size.  There is, however, mechanisms to prevent any
   col or row from becoming smaller than 1.

   If resize makes the Table larger than before:  First, all col/row
   smaller that preferred size are stretched up until their preferred
   size.  The rest of the change is distributed evenly to un-locked col/row,
   but if all are locked, then all are stretched.

   If the table is being made smaller, then the inverse is applied: all
   unlocked (or all if all are locked) are made smaller down to their
   preferred sizes, then all are made smaller by the same amount.

   Adjustments to the vectors are made on a relative basis.  Big slots
   change more than small slots.  Therefore, the adjustment delta is
   computed for each slot which might change.

   While adjusting the vectors, there are too things to watch out for: lots of
   slots to change, yet not much change, integer truncation then leaves the
   delta zero.  In this case we make the delta 1, which means the change gets
   used up before all the slots are seen.  We use the same algorithm for 
   growing and shrinking, so there should be no perceivable problems.  The
   second problem is when the delta would consume too much change, again due
   to integer truncation.  In this case, we must simply make the delta equal
   to the remaining change.

   Notice that there are two resize algorithms used: one applies when everything
   is smaller than preferred, and another applies when everything is bigger.
   When smaller, everything gets changed relatively.  When larger, change
   is influenced by the table slot being locked (TBL_VEC_LOCK).  Slots which
   are locked are not adjusted unless ALL slots are locked, then all are
   adjusted relatively.
*/

void XmpTableVectorAdjust( vec, vec_len, change )
    XmpTableVector vec;
    int            vec_len, change;
{
    int vec_inx, remaining, amt;
    int total_pref;
    int can_change, can_change_pref;
    int too_small;
    int too_big, too_big_pref;

    if ( (XmpTableVector)0 == vec || 0 == vec_len || 0 == change )
	return;

    total_pref = can_change = can_change_pref = too_big = too_big_pref = 0;
    for ( vec_inx=0  ;  vec_inx < vec_len  ;  vec_inx++ )
    {
	/* NOTE: total_pref can be zero if all pref_value are 0!
	*/
	total_pref += vec[ vec_inx ].pref_value;
	if ( change > 0 )
	{
	    if ( 0 == ( vec[ vec_inx ].options & TBL_VEC_LOCK ) )
	    {
		/* NOTE: can_change_pref can be zero if all pref_value are 0!
		*/
		can_change++;
		can_change_pref += vec[ vec_inx ].pref_value;
	    }
	    if ( vec[ vec_inx ].value < vec[ vec_inx ].pref_value )
	    {
		too_small++;
	    }
	}
	else
	{
	    if ( vec[ vec_inx ].value > vec[ vec_inx ].pref_value )
	    {
		/* NOTE: too_big_pref can be zero if all pref_value are 0!
		*/
		too_big++;
		too_big_pref += vec[ vec_inx ].pref_value;
	    }
	}
    }

    if ( change > 0 )
    {
	/**************** Make columns wider or rows taller ***************
	*/
	int still_too_small;
	remaining = change;
	do
	{
	    /* Expand everything smaller than preferred up to preferred
	    */
	    still_too_small = 0;

	    for ( vec_inx=0  ;  vec_inx < vec_len  ;  vec_inx++ )
	    {
		if ( vec[ vec_inx ].value < vec[ vec_inx ].pref_value )
		{
		    /* Make this one bigger, up to preferred size
		    */
		    if ( 0 == total_pref )
			amt = change / (too_small?too_small:vec_len);
		    else
			amt = change * vec[ vec_inx ].pref_value / total_pref;
		    if ( 0 == amt )
			amt = 1;
		    else if ( remaining < amt )
			amt = remaining;

		    if (  vec[vec_inx].value + amt
			< vec[vec_inx].pref_value )
		    {
			vec[ vec_inx ].value += amt;
			++still_too_small;
		    }
		    else
		    {
			amt = vec[vec_inx].pref_value - vec[vec_inx].value;
			vec[vec_inx].value = vec[vec_inx].pref_value;
		    }
		    remaining -= amt;

		    if ( remaining <= 0 )
			return;	/* used up all change */
		}
	    }

	    change = remaining;
	}
	while ( still_too_small );

	/* All are at least preferred size, and there is change remaining.
	 * If none of the vector slots can stretch, then we still must
	 * force them all to stretch.
	 */
	if ( 0 == can_change )
	    can_change_pref = total_pref;	/* maintain relative sizes */

	do
	{
	    for ( vec_inx = 0  ;  vec_inx < vec_len  ;  vec_inx++ )
	    {
		if ( 0 == can_change
		  || 0 == ( vec[ vec_inx ].options & TBL_VEC_LOCK ) )
		{
		    /* Add relative amount to all which can change.
		    */
		    if ( 0 == can_change_pref )
			amt = change / (can_change?can_change:vec_len);
		    else
			amt = change * vec[vec_inx].pref_value/can_change_pref;
		    if ( 0 == amt )
			amt = 1;
		    else if ( remaining < amt )
			amt = remaining;

		    vec[ vec_inx ].value += amt;
		    remaining -= amt;

		    if ( remaining <= 0 )
			return; /* used up all change */
		}
	    }

	    /* We have gone thru vector, adding space, but due to truncation
	     * there may still be more change to distribute.
	     */
	    change = remaining;
	}
	while ( 1 ); /* until remaining goes to zero or negative above */
	/*NOTREACHED*/
    }
    else /*  (change < 0)  */
    {
	/**************** Make columns narrower or rows shorter ***************
	*/
	int still_too_big, num_larger_than_1;

	/* For conceptual clarity, switch the sign on change
	*/
	change = -change;
	remaining = change;

	still_too_big = too_big;
	while ( still_too_big )
	{
	    /* Shrink all which are larger than preferred
	    */
	    still_too_big = 0;
	    for ( vec_inx = 0  ;  vec_inx < vec_len  ;  vec_inx++ )
	    {
		if ( vec[ vec_inx ].value > vec[ vec_inx ].pref_value )
		{
		    if ( 0 == too_big_pref )
			amt = change / (too_big?too_big:vec_len);
		    else
			amt = change * vec[vec_inx].pref_value / too_big_pref;
		    if ( 0 == amt )
			amt = 1;
		    else if ( remaining < amt )
			amt = remaining;

		    if ( vec[ vec_inx ].value - amt < vec[ vec_inx ].pref_value)
		    {
			amt = vec[ vec_inx ].value - vec[ vec_inx ].pref_value;
			vec[ vec_inx ].value = vec[ vec_inx ].pref_value;
		    }
		    else
		    {
			vec[ vec_inx ].value -= amt;
			still_too_big++;
		    }

		    remaining -= amt;

		    if ( remaining <= 0 )
			return; /* used up all change */
		}
	    }

	    /* We have made a pass through all slots
	    */
	    change = remaining;
	}
	/* Now all stretchable are preferred sizes, or all were already smaller
	 * than preferred sizes, yet more change is remaining to be absorbed.
	 *
	 * Shrink evenly, but since none can become smaller than 1, we may need
	 * to make multiple passes over vector until total change is absorbed,
	 * or all are of size 1.
	 */
	do
	{
	    num_larger_than_1 = 0;

	    for ( vec_inx = 0  ;  vec_inx < vec_len  ;  vec_inx++ )
	    {
		if ( 0 == total_pref )
		    amt = change / vec_len;
		else
		    amt = change * vec[vec_inx].pref_value / total_pref;
		if ( 0 == amt )
		    amt = 1;
		else if ( remaining < amt )
		    amt = remaining;

		if ( amt < vec[vec_inx].value )
		{
		    vec[vec_inx].value -= amt;
		    ++num_larger_than_1;
		}
		else
		{
		    amt = vec[vec_inx].value - 1;
		    vec[vec_inx].value = 1;
		}
		remaining -= amt;
		if ( remaining <= 0 )
		    return; /* used up all change */
	    }

	    /* We have made a pass through all slots
	    */
	    change = remaining;
	}
	while ( num_larger_than_1 );
	return; /* all are shrunk to absolute minimum size (1) */
    }
    /*NOTREACHED*/
}

/* Set Upper Left Corner Coordinates of Each Cell
**================================================**
   Note that it is not worth doing this until the actual correct size of
   the rows and columns have been computed.
*/

void XmpTableVectorComputeOffsets( vec, vec_len, margin, gap )
    XmpTableVector vec;
    int            vec_len, margin, gap;
{
    int i;
    int offset = margin;

    if ( (XmpTableVector)0 == vec || 0 == vec_len )
	return;

    for ( i = 0  ;  i < vec_len  ;  i++ )
    {
	vec[i].offset = offset;
	offset = offset + vec[i].value + gap;
    }
}
