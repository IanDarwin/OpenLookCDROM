#include <X11/Fresco/fresco.h>
#include <X11/Fresco/Impls/region.h>
#include <X11/Fresco/Ox/schema.h>
#include <X11/Fresco/Ox/typeobjs.h>
#include "commands.h"
#include "manipulators.h"
#include "tools.h"
#include "globals.h"

static Fresco* fresco = nil;

TypeObjId TypeIdVar(XfCommand) = 600;
TypeObjId TypeIdVar(XfGroupCmd) = 601;
TypeObjId TypeIdVar(XfUngroupCmd) = 602;
TypeObjId TypeIdVar(XfInstanceCmd) = 603;
TypeObjId TypeIdVar(XfDeleteCmd) = 604;
TypeObjId TypeIdVar(XfCopyCmd) = 605;
TypeObjId TypeIdVar(XfNarrowCmd) = 606;
TypeObjId TypeIdVar(XfSelectCmd) = 607;
TypeObjId TypeIdVar(XfMacroCmd) = 608;
TypeObjId TypeIdVar(XfSelectInfoCmd) = 609;
TypeObjId TypeIdVar(XfNaturalCmd) = 610;

TypeObjId TypeIdVar(XfTool) = 700;
TypeObjId TypeIdVar(XfSelectTool) = 701;
TypeObjId TypeIdVar(XfCreateTool) = 702;
TypeObjId TypeIdVar(XfMoveTool) = 703;
TypeObjId TypeIdVar(XfScaleTool) = 704;
TypeObjId TypeIdVar(XfRotateTool) = 705;
TypeObjId TypeIdVar(XfAlterTool) = 706;
TypeObjId TypeIdVar(XfResizeTool) = 707;

TypeObjId TypeIdVar(XfManipulator) = 800;
TypeObjId TypeIdVar(XfFigureManip) = 801;
TypeObjId TypeIdVar(XfPolyManip) = 802;
TypeObjId TypeIdVar(XfMacroManip) = 803;
TypeObjId TypeIdVar(XfButtonManip) = 804;

TypeObjId TypeIdVar(XfLineManip) = 805;
TypeObjId TypeIdVar(XfRectManip) = 806;
TypeObjId TypeIdVar(XfEllipseManip) = 807;
TypeObjId TypeIdVar(XfVertexManip) = 808;
TypeObjId TypeIdVar(XfOpen_BSplineManip) = 809;
TypeObjId TypeIdVar(XfClosed_BSplineManip) = 810;
TypeObjId TypeIdVar(XfMultiLineManip) = 811;
TypeObjId TypeIdVar(XfPolygonManip) = 812;

TypeObjId TypeIdVar(XfBoxManip) = 813;
TypeObjId TypeIdVar(XfHBoxManip) = 814;
TypeObjId TypeIdVar(XfVBoxManip) = 815;
TypeObjId TypeIdVar(XfLayoutManip) = 817;

TypeObjId TypeIdVar(XfData) = 900;
TypeObjId TypeIdVar(XfTransformer) = 901;

extern TypeObj_Descriptor
    TypeVar(XfCommand), TypeVar(XfGroupCmd), TypeVar(XfUngroupCmd),
    TypeVar(XfInstanceCmd), TypeVar(XfDeleteCmd), TypeVar(XfCopyCmd),
    TypeVar(XfNarrowCmd), TypeVar(XfSelectCmd), TypeVar(XfMacroCmd),
    TypeVar(XfSelectInfoCmd), TypeVar(XfNaturalCmd),

    TypeVar(XfTool), TypeVar(XfSelectTool), TypeVar(XfCreateTool),
    TypeVar(XfMoveTool), TypeVar(XfScaleTool), TypeVar(XfRotateTool),
    TypeVar(XfRotateTool), TypeVar(XfAlterTool), TypeVar(XfResizeTool),

    TypeVar(XfManipulator), TypeVar(XfFigureManip), TypeVar(XfPolyManip),
    TypeVar(XfMacroManip), TypeVar(XfButtonManip), TypeVar(XfLineManip),
    TypeVar(XfRectManip), TypeVar(XfEllipseManip), TypeVar(XfVertexManip),
    TypeVar(XfOpen_BSplineManip), TypeVar(XfClosed_BSplineManip),
    TypeVar(XfMultiLineManip), TypeVar(XfPolygonManip),
    TypeVar(XfBoxManip), TypeVar(XfHBoxManip), TypeVar(XfVBoxManip),
    TypeVar(XfLayoutManip), 

    TypeVar(XfData), TypeVar(XfTransformer);

static TypeObj_Descriptor* fdraw_schema[] = {
    &TypeVar(XfCommand),
    &TypeVar(XfGroupCmd),
    &TypeVar(XfUngroupCmd),
    &TypeVar(XfInstanceCmd),
    &TypeVar(XfDeleteCmd),
    &TypeVar(XfCopyCmd),
    &TypeVar(XfNarrowCmd),
    &TypeVar(XfSelectCmd),
    &TypeVar(XfMacroCmd),
    &TypeVar(XfSelectInfoCmd),
    &TypeVar(XfNaturalCmd),

    &TypeVar(XfTool),
    &TypeVar(XfSelectTool),
    &TypeVar(XfCreateTool),
    &TypeVar(XfMoveTool),
    &TypeVar(XfScaleTool),
    &TypeVar(XfRotateTool),
    &TypeVar(XfAlterTool),
    &TypeVar(XfResizeTool),

    &TypeVar(XfManipulator),
    &TypeVar(XfFigureManip),
    &TypeVar(XfPolyManip),
    &TypeVar(XfMacroManip),
    &TypeVar(XfButtonManip),

    &TypeVar(XfLineManip),
    &TypeVar(XfRectManip),
    &TypeVar(XfEllipseManip),
    &TypeVar(XfVertexManip),
    &TypeVar(XfOpen_BSplineManip),
    &TypeVar(XfClosed_BSplineManip),
    &TypeVar(XfMultiLineManip),
    &TypeVar(XfPolygonManip),

    &TypeVar(XfBoxManip),
    &TypeVar(XfHBoxManip),
    &TypeVar(XfVBoxManip),
    &TypeVar(XfLayoutManip),

    &TypeVar(XfData),
    &TypeVar(XfTransformer),

    nil
};

void global_init(Fresco* f) {
    TypeSchemaImpl::schema()->load_list(fdraw_schema);
    fresco = f;
    Fresco::ref(fresco);
    CopyCmd::glyphmap_ = new GlyphImplMap;
}

Fresco* fresco_instance () {
    return fresco;
}

