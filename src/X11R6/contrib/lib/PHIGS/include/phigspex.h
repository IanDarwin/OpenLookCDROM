/* $XConsortium: phigspex.h,v 5.6 94/04/17 20:41:54 rws Exp $ */

/***********************************************************

Copyright (c) 1989, 1990, 1991  X Consortium

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL THE
X CONSORTIUM BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN
AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

Except as contained in this notice, the name of the X Consortium shall not be
used in advertising or otherwise to promote the sale, use or other dealings
in this Software without prior written authorization from the X Consortium.

Copyright 1989, 1990, 1991 by Sun Microsystems, Inc. 

                        All Rights Reserved

Permission to use, copy, modify, and distribute this software and its 
documentation for any purpose and without fee is hereby granted, 
provided that the above copyright notice appear in all copies and that
both that copyright notice and this permission notice appear in 
supporting documentation, and that the name of Sun Microsystems,
not be used in advertising or publicity pertaining to distribution of 
the software without specific, written prior permission.  

SUN MICROSYSTEMS DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, 
INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT 
SHALL SUN MICROSYSTEMS BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL 
DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,
ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
SOFTWARE.
******************************************************************/

#ifndef PHIGSPEX_H_INCLUDED
#define PHIGSPEX_H_INCLUDED

#ifndef DEBUG
#define ASSERT(a)
#else
#define ASSERT(__assertion__)                                   \
  if (!(__assertion__)) {                                       \
     fprintf(stderr, "Assertion Failed: File %s, Line %d\n",    \
                __FILE__, __LINE__);                            \
     fprintf(stderr, "Assertion: __assertion__\n\n");           \
  }
#endif /* DEBUG */

#define PEX_WS_SUPPORT( _pex_info ) \
    ( !(_pex_info)->subset_info || (_pex_info)->subset_info & 0x2 )

/*
 * Macros for converting certain PHIGS data types to PEX data types and
 * back again.
 */
#define PEX_CONV_FROM_Ppoint4(phigsdataPtr,pexdataPtr) { \
        (pexdataPtr)->x = (PEXFLOAT)(phigsdataPtr)->x; \
        (pexdataPtr)->y = (PEXFLOAT)(phigsdataPtr)->y; \
        (pexdataPtr)->z = (PEXFLOAT)(phigsdataPtr)->z; \
        (pexdataPtr)->w = (PEXFLOAT)(phigsdataPtr)->w; \
}

#define PEX_CONV_FROM_Ppoint3(phigsdataPtr,pexdataPtr) { \
        (pexdataPtr)->x = (PEXFLOAT)(phigsdataPtr)->x; \
        (pexdataPtr)->y = (PEXFLOAT)(phigsdataPtr)->y; \
        (pexdataPtr)->z = (PEXFLOAT)(phigsdataPtr)->z; \
}

#define PEX_CONV_FROM_Ppoint(phigsdataPtr,pexdataPtr) { \
        (pexdataPtr)->x = (PEXFLOAT)(phigsdataPtr)->x; \
        (pexdataPtr)->y = (PEXFLOAT)(phigsdataPtr)->y; \
}

#define PEX_CONV_FROM_Pvec3(phigsdataPtr,pexdataPtr) { \
        (pexdataPtr)->x = (PEXFLOAT)(phigsdataPtr)->delta_x; \
        (pexdataPtr)->y = (PEXFLOAT)(phigsdataPtr)->delta_y; \
        (pexdataPtr)->z = (PEXFLOAT)(phigsdataPtr)->delta_z; \
}

#define PEX_CONV_FROM_Pvec(phigsdataPtr,pexdataPtr) { \
        (pexdataPtr)->x = (PEXFLOAT)(phigsdataPtr)->delta_x; \
        (pexdataPtr)->y = (PEXFLOAT)(phigsdataPtr)->delta_y; \
}

#define PEX_CONV_TO_Ppoint4(pexdataPtr,phigsdataPtr) { \
	(phigsdataPtr)->x = (pexdataPtr)->x; \
	(phigsdataPtr)->y = (pexdataPtr)->y; \
	(phigsdataPtr)->z = (pexdataPtr)->z; \
	(phigsdataPtr)->w = (pexdataPtr)->w; \
}

#define PEX_CONV_TO_Ppoint3(pexdataPtr,phigsdataPtr) { \
	(phigsdataPtr)->x = (pexdataPtr)->x; \
	(phigsdataPtr)->y = (pexdataPtr)->y; \
	(phigsdataPtr)->z = (pexdataPtr)->z; \
}

#define PEX_CONV_TO_Ppoint(pexdataPtr,phigsdataPtr) { \
	(phigsdataPtr)->x = (pexdataPtr)->x; \
	(phigsdataPtr)->y = (pexdataPtr)->y; \
}

#define PEX_CONV_TO_Pvec3(pexdataPtr,phigsdataPtr) { \
	(phigsdataPtr)->delta_x = (pexdataPtr)->x; \
	(phigsdataPtr)->delta_y = (pexdataPtr)->y; \
	(phigsdataPtr)->delta_z = (pexdataPtr)->z; \
}

#define PEX_CONV_TO_Pvec(pexdataPtr,phigsdataPtr) { \
	(phigsdataPtr)->delta_x = (pexdataPtr)->x; \
	(phigsdataPtr)->delta_y = (pexdataPtr)->y; \
}

#define PEX_CONV_TO_Pfloat_size(pexdataPtr,phigsdataPtr) { \
	(phigsdataPtr)->size_x = (pexdataPtr)->x; \
	(phigsdataPtr)->size_y = (pexdataPtr)->y; \
}

#define PEX_CONV_TO_Pmatrix3(pexdataPtr,phigsdataPtr) { \
	(phigsdataPtr)[0][0] = (pexdataPtr)[0][0]; \
	(phigsdataPtr)[0][1] = (pexdataPtr)[0][1]; \
	(phigsdataPtr)[0][2] = (pexdataPtr)[0][2]; \
	(phigsdataPtr)[0][3] = (pexdataPtr)[0][3]; \
	(phigsdataPtr)[1][0] = (pexdataPtr)[1][0]; \
	(phigsdataPtr)[1][1] = (pexdataPtr)[1][1]; \
	(phigsdataPtr)[1][2] = (pexdataPtr)[1][2]; \
	(phigsdataPtr)[1][3] = (pexdataPtr)[1][3]; \
	(phigsdataPtr)[2][0] = (pexdataPtr)[2][0]; \
	(phigsdataPtr)[2][1] = (pexdataPtr)[2][1]; \
	(phigsdataPtr)[2][2] = (pexdataPtr)[2][2]; \
	(phigsdataPtr)[2][3] = (pexdataPtr)[2][3]; \
	(phigsdataPtr)[3][0] = (pexdataPtr)[3][0]; \
	(phigsdataPtr)[3][1] = (pexdataPtr)[3][1]; \
	(phigsdataPtr)[3][2] = (pexdataPtr)[3][2]; \
	(phigsdataPtr)[3][3] = (pexdataPtr)[3][3]; \
}

#define PEX_CONV_FROM_Pmatrix3(phigsdataPtr,pexdataPtr) { \
	(pexdataPtr)[0][0] = (phigsdataPtr)[0][0]; \
	(pexdataPtr)[0][1] = (phigsdataPtr)[0][1]; \
	(pexdataPtr)[0][2] = (phigsdataPtr)[0][2]; \
	(pexdataPtr)[0][3] = (phigsdataPtr)[0][3]; \
	(pexdataPtr)[1][0] = (phigsdataPtr)[1][0]; \
	(pexdataPtr)[1][1] = (phigsdataPtr)[1][1]; \
	(pexdataPtr)[1][2] = (phigsdataPtr)[1][2]; \
	(pexdataPtr)[1][3] = (phigsdataPtr)[1][3]; \
	(pexdataPtr)[2][0] = (phigsdataPtr)[2][0]; \
	(pexdataPtr)[2][1] = (phigsdataPtr)[2][1]; \
	(pexdataPtr)[2][2] = (phigsdataPtr)[2][2]; \
	(pexdataPtr)[2][3] = (phigsdataPtr)[2][3]; \
	(pexdataPtr)[3][0] = (phigsdataPtr)[3][0]; \
	(pexdataPtr)[3][1] = (phigsdataPtr)[3][1]; \
	(pexdataPtr)[3][2] = (phigsdataPtr)[3][2]; \
	(pexdataPtr)[3][3] = (phigsdataPtr)[3][3]; \
}

#define PEX_CONV_FROM_Peditmode(_m) \
    ((_m) == PEDIT_INSERT ? PEXStructureInsert : PEXStructureReplace)

#define PEX_CONV_FROM_Pedgef(_m) \
    ((_m) == PEDGE_ON ? PEXOn : PEXOff)
#define PEX_CONV_TO_Pedgef(_m) \
    ((_m) == PEXOn ? PEDGE_ON : PEDGE_OFF )

#define PEX_CONV_FROM_Ptxprec(_p) \
    ((_p) == PPREC_CHAR ? PEXCharPrecision : \
	(_p) == PPREC_STRING ? PEXStringPrecision : PEXStrokePrecision)

#define PEX_CONV_TO_Ptxprec(_p) \
    ((_p) == PEXCharPrecision ? PPREC_CHAR : \
	(_p) == PEXStringPrecision ? PPREC_STRING : PPREC_STROKE)

#define PEX_CONV_FROM_Ptxpath(_p) \
    ((_p) == PPATH_LEFT ? PEXPathLeft : \
	(_p) == PPATH_UP ? PEXPathUp : \
	    (_p) == PPATH_DOWN ? PEXPathDown : PEXPathRight)
#define PEX_CONV_TO_Ptxpath(_p) \
    ((_p) == PEXPathLeft ? PPATH_LEFT : \
	(_p) == PEXPathUp ? PPATH_UP : \
	    (_p) == PEXPathDown ? PPATH_DOWN : PPATH_RIGHT)

#define PEX_CONV_FROM_Ptxver(_a) \
    ((_a) == PVERT_NORM ? PEXValignNormal : \
	(_a) == PVERT_TOP ? PEXValignTop : \
	    (_a) == PVERT_CAP ? PEXValignCap : \
		(_a) == PVERT_HALF ? PEXValignHalf : \
		    (_a) == PVERT_BASE ? PEXValignBase: \
			(_a) == PVERT_BOTTOM ? PEXValignBottom: PEXValignNormal)
#define PEX_CONV_TO_Ptxver(_a) \
    ((_a) == PEXValignNormal ? PVERT_NORM : \
	(_a) == PEXValignTop ? PVERT_TOP : \
	    (_a) == PEXValignCap ? PVERT_CAP : \
		(_a) == PEXValignHalf ? PVERT_HALF : \
		    (_a) == PEXValignBase ? PVERT_BASE: \
			(_a) == PEXValignBottom ? PVERT_BOTTOM: PVERT_NORM)

#define PEX_CONV_FROM_Ptxhor(_a) \
    ((_a) == PHOR_NORM ? PEXHalignNormal : \
	(_a) == PHOR_LEFT ? PEXHalignLeft : \
	    (_a) == PHOR_CTR ? PEXHalignCenter : \
		(_a) == PHOR_RIGHT ? PEXHalignRight : PEXHalignNormal)
#define PEX_CONV_TO_Ptxhor(_a) \
    ((_a) == PEXHalignNormal ? PHOR_NORM : \
	(_a) == PEXHalignLeft ? PHOR_LEFT : \
	    (_a) == PEXHalignCenter ? PHOR_CTR : \
		(_a) == PEXHalignRight ? PHOR_RIGHT : PHOR_NORM)

#define PEX_CONV_FROM_Pinterstyle(_s) \
    ((_s) == PSTYLE_EMPTY ? PEXInteriorStyleEmpty : \
	(_s) == PSTYLE_SOLID ? PEXInteriorStyleSolid : \
	    (_s) == PSTYLE_HATCH ? PEXInteriorStyleHatch : \
		(_s) == PSTYLE_PAT ? PEXInteriorStylePattern : \
		    (_s) == PSTYLE_HOLLOW ? PEXInteriorStyleHollow:PEXInteriorStyleHollow)
#define PEX_CONV_TO_Pinterstyle(_s) \
    ((_s) == PEXInteriorStyleEmpty ? PSTYLE_EMPTY : \
	(_s) == PEXInteriorStyleSolid ? PSTYLE_SOLID : \
	    (_s) == PEXInteriorStyleHatch ? PSTYLE_HATCH : \
		(_s) == PEXInteriorStylePattern ? PSTYLE_PAT : \
		    (_s) == PEXInteriorStyleHollow ? PSTYLE_HOLLOW : PSTYLE_HOLLOW)

#define PEX_CONV_FROM_Pdistgmode(_m) ((_m) == PDISTING_YES ? 1 : 0)
#define PEX_CONV_TO_Pdistgmode(_m) ((_m) == 1 ? PDISTING_YES : PDISTING_NO)

#define PEX_CONV_FROM_Pcullmode(_m) \
    ((_m) == PCULL_BACKFACE ? PEXBackFaces : \
	(_m) == PCULL_FRONTFACE ? PEXFrontFaces : 0)
#define PEX_CONV_TO_Pcullmode(_m) \
    ((_m) == PEXBackFaces ? PCULL_BACKFACE : \
	(_m) == PEXFrontFaces ? PCULL_FRONTFACE : PCULL_NONE)

#define PEX_CONV_FROM_Pcomptype(_t) \
    ((_t) == PTYPE_PRECONCAT ? PEXPreConcatenate : \
	(_t) == PTYPE_POSTCONCAT ? PEXPostConcatenate : \
	    (_t) == PTYPE_REPLACE ? PEXReplace : PEXReplace)
#define PEX_CONV_TO_Pcomptype(_t) \
    ((_t) == PEXPreConcatenate ? PTYPE_PRECONCAT : \
	(_t) == PEXPostConcatenate ? PTYPE_POSTCONCAT : \
	    (_t) == PEXReplace ? PTYPE_REPLACE : PTYPE_REPLACE)

#define PEX_CONV_FROM_Pasf(_a) ((_a) == PASF_BUNDLED ? PEXBundled : PEXIndividual)
#define PEX_CONV_TO_Pasf(_a) ((_a) == PEXBundled ? PASF_BUNDLED : PASF_INDIV)

#define PEX_CONV_FROM_Pclip(_a) ((_a) == PIND_CLIP ? 1 : 0)
#define PEX_CONV_TO_Pclip(_a) ((_a) ? PIND_CLIP: PIND_NO_CLIP)

#define PEX_CONV_FROM_Pattrid(_a, _p) \
    switch ( _a ) { \
	case PASPECT_LINETYPE: (_p) = PEXLineTypeAsf; break; \
	case PASPECT_LINEWIDTH: (_p) = PEXLineWidthAsf; break; \
	case PASPECT_LINE_COLR_IND: (_p) = PEXLineColourAsf; break; \
	case PASPECT_MARKER_TYPE: (_p) = PEXMarkerTypeAsf; break; \
	case PASPECT_MARKER_SIZE: (_p) = PEXMarkerScaleAsf; break; \
	case PASPECT_MARKER_COLR_IND: (_p) = PEXMarkerColourAsf; break; \
	case PASPECT_TEXT_FONT: (_p) = PEXTextFontIndexAsf; break; \
	case PASPECT_TEXT_PREC: (_p) = PEXTextPrecAsf; break; \
	case PASPECT_CHAR_EXPAN: (_p) = PEXCharExpansionAsf; break; \
	case PASPECT_CHAR_SPACE: (_p) = PEXCharSpacingAsf; break; \
	case PASPECT_TEXT_COLR_IND: (_p) = PEXTextColourAsf; break; \
	case PASPECT_INT_STYLE: (_p) = PEXInteriorStyleAsf; break; \
	case PASPECT_INT_STYLE_IND: (_p) = PEXInteriorStyleIndexAsf; break; \
	case PASPECT_INT_COLR_IND: (_p) = PEXSurfaceColourAsf; break; \
	case PASPECT_EDGE_FLAG: (_p) = PEXSurfaceEdgesAsf; break; \
	case PASPECT_EDGETYPE: (_p) = PEXSurfaceEdgeTypeAsf; break; \
	case PASPECT_EDGEWIDTH: (_p) = PEXSurfaceEdgeWidthAsf; break; \
	case PASPECT_EDGE_COLR_IND: (_p) = PEXSurfaceEdgeColourAsf; break; \
	case PASPECT_CURVE_APPROX_CRIT: (_p) = PEXCurveApproxAsf; break; \
	case PASPECT_SURF_APPROX_CRIT: (_p) = PEXSurfaceApproxAsf; break; \
	case PASPECT_LINE_SHAD_METH: (_p) = PEXPolylineInterpAsf; break; \
	case PASPECT_REFL_PROPS: (_p) = PEXReflectionAttrAsf; break; \
	case PASPECT_INT_REFL_EQN: (_p) = PEXReflectionModelAsf; break; \
	case PASPECT_INT_SHAD_METH: (_p) = PEXSurfaceInterpAsf; break; \
	case PASPECT_BACK_INT_STYLE: (_p) = PEXBfInteriorStyleAsf; break; \
	case PASPECT_BACK_INT_STYLE_IND: \
	    (_p) = PEXBfInteriorStyleIndexAsf; break; \
	case PASPECT_BACK_INT_COLR: (_p) = PEXBfSurfaceColourAsf; break; \
	case PASPECT_BACK_REFL_PROPS: (_p) = PEXBfReflectionAttrAsf; break; \
	case PASPECT_BACK_INT_REFL_EQN: \
	    (_p)=PEXBfReflectionModelAsf;break;\
	case PASPECT_BACK_INT_SHAD_METH: (_p) = PEXBfSurfaceInterpAsf; break; \
    }

#define PEX_CONV_TO_Pattrid(_a, _p) \
    switch ( _a ) { \
	case PEXLineTypeAsf: (_p) = PASPECT_LINETYPE; break; \
	case PEXLineWidthAsf: (_p) = PASPECT_LINEWIDTH; break; \
	case PEXLineColourAsf: (_p) = PASPECT_LINE_COLR_IND; break; \
	case PEXMarkerTypeAsf: (_p) = PASPECT_MARKER_TYPE; break; \
	case PEXMarkerScaleAsf: (_p) = PASPECT_MARKER_SIZE; break; \
	case PEXMarkerColourAsf: (_p) = PASPECT_MARKER_COLR_IND; break; \
	case PEXTextFontIndexAsf: (_p) = PASPECT_TEXT_FONT; break; \
	case PEXTextPrecAsf: (_p) = PASPECT_TEXT_PREC; break; \
	case PEXCharExpansionAsf: (_p) = PASPECT_CHAR_EXPAN; break; \
	case PEXCharSpacingAsf: (_p) = PASPECT_CHAR_SPACE; break; \
	case PEXTextColourAsf: (_p) = PASPECT_TEXT_COLR_IND; break; \
	case PEXInteriorStyleAsf: (_p) = PASPECT_INT_STYLE; break; \
	case PEXInteriorStyleIndexAsf: (_p) = PASPECT_INT_STYLE_IND; break; \
	case PEXSurfaceColourAsf: (_p) = PASPECT_INT_COLR_IND; break; \
	case PEXSurfaceEdgesAsf: (_p) = PASPECT_EDGE_FLAG; break; \
	case PEXSurfaceEdgeTypeAsf: (_p) = PASPECT_EDGETYPE; break; \
	case PEXSurfaceEdgeWidthAsf: (_p) = PASPECT_EDGEWIDTH; break; \
	case PEXSurfaceEdgeColourAsf: (_p) = PASPECT_EDGE_COLR_IND; break; \
	case PEXCurveApproxAsf: (_p) = PASPECT_CURVE_APPROX_CRIT; break; \
	case PEXSurfaceApproxAsf: (_p) = PASPECT_SURF_APPROX_CRIT; break; \
	case PEXPolylineInterpAsf: (_p) = PASPECT_LINE_SHAD_METH; break; \
	case PEXReflectionAttrAsf: (_p) = PASPECT_REFL_PROPS; break; \
	case PEXReflectionModelAsf: (_p) = PASPECT_INT_REFL_EQN; break;\
	case PEXSurfaceInterpAsf: (_p) = PASPECT_INT_SHAD_METH; break; \
	case PEXBfInteriorStyleAsf: (_p) = PASPECT_BACK_INT_STYLE; break; \
	case PEXBfInteriorStyleIndexAsf: (_p) = PASPECT_BACK_INT_STYLE_IND; \
	    break; \
	case PEXBfSurfaceColourAsf: (_p) = PASPECT_BACK_INT_COLR; break; \
	case PEXBfReflectionAttrAsf: (_p) = PASPECT_BACK_REFL_PROPS; break; \
	case PEXBfReflectionModelAsf: (_p)= PASPECT_BACK_INT_REFL_EQN; \
	    break;\
	case PEXBfSurfaceInterpAsf: (_p) = PASPECT_BACK_INT_SHAD_METH; break;\
    }

#define PEX_CONV_PHIGS_COLOUR_TYPE( _t ) \
    ((_t) == PINDIRECT ? PEXIndexedColour : \
	(_t) == PMODEL_RGB ? PEXRgbFloatColour : \
	    (_t) == PMODEL_CIELUV ? PEXCieFloatColour : \
		(_t) == PMODEL_HSV ? PEXHsvFloatColour : \
		    (_t) == PMODEL_HLS ? PEXHlsFloatColour : PEXIndexedColour)

#define PEX_CONV_PEX_COLOUR_TYPE( _t ) \
    ((_t) == PEXIndexedColour ? PINDIRECT : \
	(_t) == PEXRgbFloatColour ? PMODEL_RGB : \
	    (_t) ==  PEXCieFloatColour ? PMODEL_CIELUV : \
		(_t) == PEXHsvFloatColour ? PMODEL_HSV : \
		    (_t) == PEXHlsFloatColour ? PMODEL_HLS : PINDIRECT)

#define PEX_CONV_FROM_Pgcolr( _c, _p ) \
    switch ( (_c)->type ) { \
	case PINDIRECT: \
	default: \
	    (_p)->type.colourType = PEXIndexedColour; \
	    if ( (_c)->type == PINDIRECT ) \
		(_p)->format.indexed.index = (_c)->val.ind; \
	    else /* Can't map it to PEX so use PHIGS default. */ \
		(_p)->format.indexed.index = 1; \
	    break; \
	case PMODEL_RGB: \
	    (_p)->type.colourType = PEXRgbFloatColour; \
	    (_p)->format.rgbFloat.red = (_c)->val.general.x; \
	    (_p)->format.rgbFloat.green = (_c)->val.general.y; \
	    (_p)->format.rgbFloat.blue = (_c)->val.general.z; \
	    break; \
	case PMODEL_CIELUV: \
	    (_p)->type.colourType = PEXCieFloatColour; \
	    (_p)->format.cieFloat.x = (_c)->val.general.x; \
	    (_p)->format.cieFloat.y = (_c)->val.general.y; \
	    (_p)->format.cieFloat.z = (_c)->val.general.z; \
	    break; \
	case PMODEL_HSV: \
	    (_p)->type.colourType = PEXHsvFloatColour; \
	    (_p)->format.hsvFloat.hue = (_c)->val.general.x; \
	    (_p)->format.hsvFloat.saturation = (_c)->val.general.y; \
	    (_p)->format.hsvFloat.value = (_c)->val.general.z; \
	    break; \
	case PMODEL_HLS: \
	    (_p)->type.colourType = PEXHlsFloatColour; \
	    (_p)->format.hlsFloat.hue = (_c)->val.general.x; \
	    (_p)->format.hlsFloat.lightness = (_c)->val.general.y; \
	    (_p)->format.hlsFloat.saturation = (_c)->val.general.z; \
	    break; \
    }

#define PEX_CONV_TO_Pgcolr( _p, _c ) \
    switch ( (_p)->type.colourType ) { \
	case PEXIndexedColour: \
	default: \
	    (_c)->type = PINDIRECT; \
	    if ( (_p)->type.colourType == PEXIndexedColour ) \
		(_c)->val.ind = (_p)->format.indexed.index; \
	    else /* Can't map it so use PHIGS default. */ \
		(_c)->val.ind = 1; \
	    break; \
	case PEXRgbFloatColour: \
	    (_c)->type = PMODEL_RGB; \
	    (_c)->val.general.x = (_p)->format.rgbFloat.red; \
	    (_c)->val.general.y = (_p)->format.rgbFloat.green; \
	    (_c)->val.general.z = (_p)->format.rgbFloat.blue; \
	    break; \
	case PEXCieFloatColour: \
	    (_c)->type = PMODEL_CIELUV; \
	    (_c)->val.general.x = (_p)->format.cieFloat.x; \
	    (_c)->val.general.y = (_p)->format.cieFloat.y; \
	    (_c)->val.general.z = (_p)->format.cieFloat.z; \
	    break; \
	case PEXHsvFloatColour: \
	    (_c)->type = PMODEL_HSV; \
	    (_c)->val.general.x = (_p)->format.hsvFloat.hue; \
	    (_c)->val.general.y = (_p)->format.hsvFloat.saturation; \
	    (_c)->val.general.z = (_p)->format.hsvFloat.value; \
	    break; \
	case PEXHlsFloatColour: \
	    (_c)->type = PMODEL_HLS; \
	    (_c)->val.general.x = (_p)->format.hlsFloat.hue; \
	    (_c)->val.general.y = (_p)->format.hlsFloat.lightness; \
	    (_c)->val.general.z = (_p)->format.hlsFloat.saturation; \
	    break; \
    }

#define PEX_CONV_FROM_Pcoval( _t, _c, _p ) \
    switch ( _t ) { \
	case PINDIRECT: \
	{ \
	    pexIndexedColour *_colr = ((pexIndexedColour *)(_p)); \
	    _colr->index = (_c)->ind; \
	    (_p) += sizeof(pexIndexedColour)/sizeof(*(_p)); \
	    break; \
	} \
	case PMODEL_RGB: \
	{ \
	    pexRgbFloatColour *_colr = (pexRgbFloatColour *)(_p); \
	    _colr->red = (_c)->direct.rgb.red; \
	    _colr->green = (_c)->direct.rgb.green; \
	    _colr->blue = (_c)->direct.rgb.blue; \
	    (_p) += sizeof(pexRgbFloatColour)/sizeof(*(_p)); \
	    break; \
	} \
	case PMODEL_CIELUV: \
	{ \
	    pexCieColour *_colr = (pexCieColour *)(_p); \
	    _colr->x = (_c)->direct.cieluv.cieluv_x; \
	    _colr->y = (_c)->direct.cieluv.cieluv_y; \
	    _colr->z = (_c)->direct.cieluv.cieluv_y_lum; \
	    (_p) += sizeof(pexCieColour)/sizeof(*(_p)); \
	    break; \
	} \
	case PMODEL_HSV: \
	{ \
	    pexHsvColour *_colr = (pexHsvColour *)(_p); \
	    _colr->hue = (_c)->direct.hsv.hue; \
	    _colr->saturation = (_c)->direct.hsv.satur; \
	    _colr->value = (_c)->direct.hsv.value; \
	    (_p) += sizeof(pexHsvColour)/sizeof(*(_p)); \
	    break; \
	} \
	case PMODEL_HLS: \
	{ \
	    pexHlsColour *_colr = (pexHlsColour *)(_p); \
	    _colr->hue = (_c)->direct.hls.hue; \
	    _colr->lightness = (_c)->direct.hls.lightness; \
	    _colr->saturation = (_c)->direct.hls.satur; \
	    (_p) += sizeof(pexHlsColour)/sizeof(*(_p)); \
	    break; \
	} \
	default: \
	    fprintf( stderr, \
		"Can't convert unknown colour type, %d: file %s, line %d:\n",\
		(_t), __FILE__, __LINE__); \
	    break; \
    }

/* takes a PHIGS colour type, a pointer into a PEX OC buffer (_p), and
 * a pointer to a Pcoval (_c), and fills in _c.  RGB, CIE, HSV, HLS all 
 * take the same amount of space */
#define PEX_CONV_TO_Pcoval(_t, _p, _c) \
    switch (_t) {							\
	case PINDIRECT :						\
	    (_c)->ind = ((pexIndexedColour *)(_p))->index;		\
	    break;							\
	case PMODEL_RGB :						\
	case PMODEL_CIELUV :						\
	case PMODEL_HSV :						\
	case PMODEL_HLS :						\
	    (_c)->direct.rgb.red=((pexRgbFloatColour *)(_p))->red;	\
	    (_c)->direct.rgb.green=((pexRgbFloatColour *)(_p))->green;	\
	    (_c)->direct.rgb.blue =((pexRgbFloatColour *)(_p))->blue;	\
	    break;							\
	default: \
	    fprintf( stderr, \
		"Can't convert unknown colour type, %d: file %s, line %d:\n",\
		(_t), __FILE__, __LINE__); \
	    break; \
    }

#define PEX_CONV_FROM_VertexFlag( _f ) \
    ((_f) == PVERT_COORD ? 0x0000 : \
      (_f) == PVERT_COORD_COLOUR ? PEXGAColour : \
	(_f) == PVERT_COORD_NORMAL ? PEXGANormal : \
	  (_f) == PVERT_COORD_COLOUR_NORMAL ? PEXGAColour | PEXGANormal : 0)

#define PEX_CONV_TO_VertexFlag(_f) \
    ((_f) == 0x0000 ? PVERT_COORD : \
      (_f) == PEXGAColour ? PVERT_COORD_COLOUR :    \
        (_f) == PEXGANormal ? PVERT_COORD_NORMAL : PVERT_COORD_COLOUR_NORMAL)

#define PEX_CONV_FROM_FacetFlag( _f ) \
    ((_f) == PFACET_NONE ? 0x0000 : \
	(_f) == PFACET_COLOUR ? PEXGAColour : \
	    (_f) == PFACET_NORMAL ? PEXGANormal : \
		(_f) == PFACET_COLOUR_NORMAL ? PEXGAColour | PEXGANormal : 0)

#define PEX_CONV_TO_FacetFlag( _f ) \
    ((_f) == 0x0000 ? PFACET_NONE : \
	(_f) == PEXGAColour ? PFACET_COLOUR : \
	    (_f) == PEXGANormal ? PFACET_NORMAL : PFACET_COLOUR_NORMAL)

#define PEX_CONV_FROM_Prational( _r ) ((_r) == PRATIONAL ? PEXRational : PEXNonRational)
#define PEX_CONV_TO_Prational( _r ) ((_r) == PEXRational ? PRATIONAL : PNON_RATIONAL)

#define PEX_CONV_FROM_Pedge_flag( _v ) ((_v) == PEDGE_OFF ? PEXOff : PEXOn)
#define PEX_CONV_TO_Pedge_flag( _v ) ((_v) == PEXOff ? PEDGE_OFF : PEDGE_ON)

#define PEX_CONV_TO_Pmodtype( _t ) \
    ((_t) == PEXIRG ? PDYN_IRG : \
	(_t) == PEXIMM ? PDYN_IMM : \
	    (_t) == PEXCBS ? PDYN_CBS : PDYN_IRG)

#define PEX_CONV_FROM_Pdcuemode(_m) \
    ((_m) == PALLOWED ? PEXOn : PEXOff)
#define PEX_CONV_TO_Pdcuemode(_m) \
    ((_m) == PEXOn ? PALLOWED : PSUPPRESSED )

#define PEX_COLOUR_SIZE( _t ) \
    ((_t) == PINDIRECT ? sizeof(pexIndexedColour) : \
	(_t) == PMODEL_RGB ? sizeof(pexRgbFloatColour) : \
	    (_t) == PMODEL_CIELUV ? sizeof(pexCieColour) : \
		(_t) == PMODEL_HSV ? sizeof(pexHsvColour) : \
		    (_t) == PMODEL_HLS ? sizeof(pexHlsColour) : \
			/* unknown colour type, no way to determine size */ 0)

#define PEX_CONV_TO_Pupdatest(_m) \
    ((_m) == PEXPending ? PUPD_PEND : PUPD_NOT_PEND )

#define PEX_CONV_PHIGS_HLHSR_MODE( _m ) \
    ((_m) == PHIGS_HLHSR_MODE_NONE ? PEXHlhsrOff : \
	(_m) == PHIGS_HLHSR_MODE_ZBUFF ? PEXHlhsrZBuffer : \
	    (_m) == PHIGS_HLHSR_MODE_PAINTERS ? PEXHlhsrPainters : \
		(_m) == PHIGS_HLHSR_MODE_SCANLINE ? PEXHlhsrScanline : \
		(_m) == PHIGS_HLHSR_MODE_LINE_ONLY ? PEXHlhsrHiddenLineOnly : \
			(_m))

#define PEX_CONV_PEX_HLHSR_MODE( _m ) \
    ((_m) == PEXHlhsrOff ? PHIGS_HLHSR_MODE_NONE : \
	(_m) == PEXHlhsrZBuffer ? PHIGS_HLHSR_MODE_ZBUFF : \
	    (_m) == PEXHlhsrPainters ? PHIGS_HLHSR_MODE_PAINTERS : \
		(_m) == PEXHlhsrScanline ? PHIGS_HLHSR_MODE_SCANLINE : \
		(_m) == PEXHlhsrHiddenLineOnly ? PHIGS_HLHSR_MODE_LINE_ONLY: \
			(_m))

#define PEX_CONV_FROM_Pvpri( _p ) \
    ((_p) == PPRI_HIGHER ? PEXHigher : PEXLower)

#define PEX_CONV_TO_Pvpri( _p ) \
    ((_p) == PEXHigher ? PPRI_HIGHER : PPRI_LOWER)

#define PEX_CONV_TO_Pdisp_surf_empty( _m ) \
    ((_m) == PEXEmpty ? PSURF_EMPTY : PSURF_NOT_EMPTY)

#define PEX_CONV_TO_Pvisualrep( _m ) \
    ((_m) == PEXCorrect ? PVISUAL_ST_CORRECT : \
	(_m) == PEXDeferred ? PVISUAL_ST_DEFER : PVISUAL_ST_SIMULATED)

#define PEX_CONV_FROM_Psrchdir( _d ) \
    ((_d) == PDIR_FORWARD ? PEXForward : PEXBackward)

#define PEX_CONV_TO_Psrchstatus( _s ) \
    ((_s) == PEXFound ? PSEARCH_STATUS_SUCCESS : PSEARCH_STATUS_FAILURE)

#define PEX_CONV_PHIGS_BUF_MODE( _m ) \
    ((_m) == PHIGS_BUF_SINGLE ? PEXSingleBuffered : PEXDoubleBuffered)

#define PEX_CONV_FROM_Pinq_type( _t ) \
    ((_t) == PINQ_SET ? PEXSetValue : PEXRealizedValue)

/* this macro is "unformatted" to minimize its length, because at least one
 * preprocessor we encountered could not handle it when formatted.
 */
#define PEX_CONV_FROM_Psrcheltype( _s, _d ) \
switch ( _s ) { \
case PELEM_ALL: (_d) = PEXOCAll; break;\
case PELEM_NIL: (_d) = PEXOCNil; break;\
case PELEM_POLYLINE3: (_d) = PEXOCPolyline; break;\
case PELEM_POLYLINE: (_d) = PEXOCPolyline2D; break;\
case PELEM_POLYMARKER3: (_d) = PEXOCMarker; break;\
case PELEM_POLYMARKER: (_d) = PEXOCMarker2D; break;\
case PELEM_TEXT3: (_d) = PEXOCText; break;\
case PELEM_TEXT: (_d) = PEXOCText2D; break;\
case PELEM_ANNO_TEXT_REL3: (_d) = PEXOCAnnotationText; break;\
case PELEM_ANNO_TEXT_REL: (_d) = PEXOCAnnotationText2D; break;\
case PELEM_FILL_AREA3: (_d) = PEXOCFillArea; break;\
case PELEM_FILL_AREA: (_d) = PEXOCFillArea2D; break;\
case PELEM_FILL_AREA_SET3: (_d) = PEXOCFillAreaSet; break;\
case PELEM_FILL_AREA_SET: (_d) = PEXOCFillAreaSet2D; break;\
case PELEM_CELL_ARRAY3: (_d) = PEXOCCellArray; break;\
case PELEM_CELL_ARRAY: (_d) = PEXOCCellArray2D; break;\
case PELEM_GDP3: (_d) = PEXOCGdp; break;\
case PELEM_GDP: (_d) = PEXOCGdp2D; break;\
case PELEM_LINE_IND: (_d) = PEXOCLineBundleIndex; break;\
case PELEM_MARKER_IND: (_d) = PEXOCMarkerBundleIndex; break;\
case PELEM_TEXT_IND: (_d) = PEXOCTextBundleIndex; break;\
case PELEM_INT_IND: (_d) = PEXOCInteriorBundleIndex; break;\
case PELEM_EDGE_IND: (_d) = PEXOCEdgeBundleIndex; break;\
case PELEM_LINETYPE: (_d) = PEXOCLineType; break;\
case PELEM_LINEWIDTH: (_d) = PEXOCLineWidth; break;\
case PELEM_LINE_COLR_IND: (_d) = PEXOCLineColourIndex; break;\
case PELEM_MARKER_TYPE: (_d) = PEXOCMarkerType; break;\
case PELEM_MARKER_SIZE: (_d) = PEXOCMarkerScale; break;\
case PELEM_MARKER_COLR_IND: (_d) = PEXOCMarkerColourIndex; break;\
case PELEM_TEXT_FONT: (_d) = PEXOCTextFontIndex; break;\
case PELEM_TEXT_PREC: (_d) = PEXOCTextPrecision; break;\
case PELEM_CHAR_EXPAN: (_d) = PEXOCCharExpansion; break;\
case PELEM_CHAR_SPACE: (_d) = PEXOCCharSpacing; break;\
case PELEM_TEXT_COLR_IND: (_d) = PEXOCTextColourIndex; break;\
case PELEM_CHAR_HT: (_d) = PEXOCCharHeight; break;\
case PELEM_CHAR_UP_VEC: (_d) = PEXOCCharUpVector; break;\
case PELEM_TEXT_PATH: (_d) = PEXOCTextPath; break;\
case PELEM_TEXT_ALIGN: (_d) = PEXOCTextAlignment; break;\
case PELEM_ANNO_CHAR_HT: (_d) = PEXOCAtextHeight; break;\
case PELEM_ANNO_CHAR_UP_VEC: (_d) = PEXOCAtextUpVector; break;\
case PELEM_ANNO_PATH: (_d) = PEXOCAtextPath; break;\
case PELEM_ANNO_ALIGN: (_d) = PEXOCAtextAlignment; break;\
case PELEM_ANNO_STYLE: (_d) = PEXOCAtextStyle; break;\
case PELEM_INT_STYLE: (_d) = PEXOCInteriorStyle; break;\
case PELEM_INT_STYLE_IND: (_d) = PEXOCInteriorStyleIndex; break;\
case PELEM_INT_COLR_IND: (_d) = PEXOCSurfaceColourIndex; break;\
case PELEM_EDGE_FLAG: (_d) = PEXOCSurfaceEdgeFlag; break;\
case PELEM_EDGETYPE: (_d) = PEXOCSurfaceEdgeType; break;\
case PELEM_EDGEWIDTH: (_d) = PEXOCSurfaceEdgeWidth; break;\
case PELEM_EDGE_COLR_IND: (_d) = PEXOCSurfaceEdgeColourIndex; break;\
case PELEM_PAT_SIZE: (_d) = PEXOCPatternSize; break;\
case PELEM_PAT_REF_POINT_VECS: (_d) = PEXOCPatternAttr; break;\
case PELEM_PAT_REF_POINT: (_d) = PEXOCPatternRefPt; break;\
case PELEM_ADD_NAMES_SET: (_d) = PEXOCAddToNameSet; break;\
case PELEM_REMOVE_NAMES_SET: (_d) = PEXOCRemoveFromNameSet; break;\
case PELEM_INDIV_ASF: (_d) = PEXOCSetAsfValues; break;\
case PELEM_HLHSR_ID: (_d) = PEXOCHlhsrIdentifier; break;\
case PELEM_LOCAL_MODEL_TRAN3: (_d) = PEXOCLocalTransform; break;\
case PELEM_LOCAL_MODEL_TRAN: (_d) = PEXOCLocalTransform2D; break;\
case PELEM_GLOBAL_MODEL_TRAN3: (_d) = PEXOCGlobalTransform; break;\
case PELEM_GLOBAL_MODEL_TRAN: (_d) = PEXOCGlobalTransform2D; break;\
case PELEM_MODEL_CLIP_VOL3: (_d) = PEXOCModelClipVolume; break;\
case PELEM_MODEL_CLIP_VOL: (_d) = PEXOCModelClipVolume2D; break;\
case PELEM_MODEL_CLIP_IND: (_d) = PEXOCModelClip; break;\
case PELEM_RESTORE_MODEL_CLIP_VOL: (_d) = PEXOCRestoreModelClip; break;\
case PELEM_VIEW_IND: (_d) = PEXOCViewIndex; break;\
case PELEM_EXEC_STRUCT: (_d) = PEXOCExecuteStructure; break;\
case PELEM_LABEL: (_d) = PEXOCLabel; break;\
case PELEM_APPL_DATA: (_d) = PEXOCApplicationData; break;\
case PELEM_GSE: (_d) = PEXOCGse; break;\
case PELEM_PICK_ID: (_d) = PEXOCPickId; break;\
case PELEM_POLYLINE_SET3_DATA: (_d) = PEXOCPolylineSet; break;\
case PELEM_FILL_AREA_SET3_DATA: (_d) = PEXOCExtFillAreaSet; break;\
case PELEM_TRI_STRIP3_DATA: (_d) = PEXOCTriangleStrip; break;\
case PELEM_QUAD_MESH3_DATA: (_d) = PEXOCQuadrilateralMesh; break;\
case PELEM_NUNI_BSP_CURVE: (_d) = PEXOCNurbCurve; break;\
case PELEM_NUNI_BSP_SURF: (_d) = PEXOCNurbSurface; break;\
case PELEM_CELL_ARRAY3_PLUS: (_d) = PEXOCExtCellArray; break;\
case PELEM_TEXT_COLR: (_d) = PEXOCTextColour; break;\
case PELEM_MARKER_COLR: (_d) = PEXOCMarkerColour; break;\
case PELEM_EDGE_COLR: (_d) = PEXOCSurfaceEdgeColour; break;\
case PELEM_LINE_COLR: (_d) = PEXOCLineColour; break;\
case PELEM_CURVE_APPROX_CRIT: (_d) = PEXOCCurveApproximation; break;\
case PELEM_LINE_SHAD_METH: (_d) = PEXOCPolylineInterp; break;\
case PELEM_INT_COLR: (_d) = PEXOCSurfaceColour; break;\
case PELEM_BACK_INT_COLR: (_d) = PEXOCBfSurfaceColour; break;\
case PELEM_BACK_INT_STYLE: (_d) = PEXOCBfInteriorStyle; break;\
case PELEM_BACK_INT_STYLE_IND: (_d) = PEXOCBfInteriorStyleIndex; break;\
case PELEM_REFL_PROPS: (_d) = PEXOCSurfaceReflAttr; break;\
case PELEM_BACK_REFL_PROPS: (_d) = PEXOCBfSurfaceReflAttr; break;\
case PELEM_INT_SHAD_METH: (_d) = PEXOCSurfaceInterp; break;\
case PELEM_BACK_INT_SHAD_METH: (_d) = PEXOCBfSurfaceInterp; break;\
case PELEM_INT_REFL_EQN: (_d) = PEXOCSurfaceReflModel; break;\
case PELEM_BACK_INT_REFL_EQN: (_d) = PEXOCBfSurfaceReflModel; break;\
case PELEM_SURF_APPROX_CRIT: (_d) = PEXOCSurfaceApproximation; break;\
case PELEM_FACE_DISTING_MODE: (_d) = PEXOCDistinguishFlag; break;\
case PELEM_FACE_CULL_MODE: (_d) = PEXOCCullingMode; break;\
case PELEM_LIGHT_SRC_STATE: (_d) = PEXOCLightState; break;\
case PELEM_DCUE_IND: (_d) = PEXOCDepthCueIndex; break;\
case PELEM_COLR_MAP_IND: (_d) = PEXOCColourApproxIndex; break;\
case PELEM_SET_OF_FILL_AREA_SET3_DATA: (_d) = PEXOCSOFAS; break;\
case PELEM_RENDERING_COLR_MODEL: (_d) = PEXOCRenderingColourModel; break;\
}

#define PEX_CONV_FROM_Ppathorder( _o ) \
    ((_o) == PORDER_TOP_FIRST ? PEXTopPart : PEXBottomPart)

/* map pex color approx type to phigs colour mapping method
 * PEXColourSpace really maps to PSEUDO_N and TRUE. 
 * This only returns PSEUDO_N. code calling this must account for TRUE, too
 */
#define PEX_CONV_COLRMAP_METHOD( _m )  \
    ((_m) == PEXColourRange ? PCOLR_MAP_PSEUDO : PCOLR_MAP_TRUE)

#define PEXCloseFont(_dpy, _xid) \
	PEXResourceIdNoReplyFunc(PEX_CloseFont, _dpy, _xid)

#define PEXFreeLookupTable(_dpy, _xid) \
	PEXResourceIdNoReplyFunc(PEX_FreeLookupTable, _dpy, _xid)

#define PEXCreateNameSet(_dpy, _xid) \
	PEXResourceIdNoReplyFunc(PEX_CreateNameSet, _dpy, _xid)

#define PEXFreeNameSet(_dpy, _xid) \
	PEXResourceIdNoReplyFunc(PEX_FreeNameSet, _dpy, _xid)

#define PEXFreePickMeasure(_dpy, _xid) \
	PEXResourceIdNoReplyFunc(PEX_FreePickMeasure, _dpy, _xid)

#define PEXFreePipelineContext(_dpy, _xid) \
	PEXResourceIdNoReplyFunc(PEX_FreePipelineContext, _dpy, _xid)

#define PEXFreeRenderer(_dpy, _xid) \
	PEXResourceIdNoReplyFunc(PEX_FreeRenderer, _dpy, _xid)

#define PEXEndStructure(_dpy, _xid) \
	PEXResourceIdNoReplyFunc(PEX_EndStructure, _dpy, _xid)

#define PEXFreeSearchContext(_dpy, _xid) \
	PEXResourceIdNoReplyFunc(PEX_FreeSearchContext, _dpy, _xid)

#define PEXCreateStructure(_dpy, _xid) \
	PEXResourceIdNoReplyFunc(PEX_CreateStructure, _dpy, _xid)

#define PEXFreePhigsWks(_dpy, _xid) \
	PEXResourceIdNoReplyFunc(PEX_FreePhigsWks, _dpy, _xid)

#define PEXRedrawAllStructures(_dpy, _xid) \
	PEXResourceIdNoReplyFunc(PEX_RedrawAllStructures, _dpy, _xid)

#define PEXUpdateWorkstation(_dpy, _xid) \
	PEXResourceIdNoReplyFunc(PEX_UpdateWorkstation, _dpy, _xid)

#define PEXExecuteDeferredActions(_dpy, _xid) \
	PEXResourceIdNoReplyFunc(PEX_ExecuteDeferredActions, _dpy, _xid)

#define PEXUnpostAllStructures(_dpy, _xid) \
	PEXResourceIdNoReplyFunc(PEX_UnpostAllStructures, _dpy, _xid)

#define PEXCreateStructure(_dpy, _xid) \
	PEXResourceIdNoReplyFunc(PEX_CreateStructure, _dpy, _xid)

#define X_MAX_COLOR_VALUE	65535

#define	X_CONV_COLOR_FROM_Pcolr_rep( _pc, _xc ) \
	(_xc)->red = (unsigned short)((_pc)->rgb.red * ((Pfloat)X_MAX_COLOR_VALUE));	\
	(_xc)->green = (unsigned short)((_pc)->rgb.green * ((Pfloat)X_MAX_COLOR_VALUE));	\
	(_xc)->blue = (unsigned short)((_pc)->rgb.blue * ((Pfloat)X_MAX_COLOR_VALUE))

#define	X_CONV_COLOR_TO_Pcolr_rep( _xc, _pc )	\
	(_pc)->rgb.red = ((Pfloat)(_xc)->red) / ((Pfloat)X_MAX_COLOR_VALUE);	\
	(_pc)->rgb.green = ((Pfloat)(_xc)->green) / ((Pfloat)X_MAX_COLOR_VALUE);	\
	(_pc)->rgb.blue = ((Pfloat)(_xc)->blue) / ((Pfloat)X_MAX_COLOR_VALUE)

#define X_CONV_COLOR_FROM_Pfloat( _pf, _xc ) \
	(_xc) = (unsigned short)((_pf) * ((Pfloat)X_MAX_COLOR_VALUE))

#define X_CONV_COLOR_TO_Pfloat( _xc, _pf ) \
	(_pf) = ((Pfloat)(_xc)) / ((Pfloat)X_MAX_COLOR_VALUE)

#endif
