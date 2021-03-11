/*
 * figviewer.h
 */

#ifndef _figviewer_h
#define _figviewer_h

#include <X11/Fresco/figures.h>
#include <X11/Fresco/layouts.h>
#include <X11/Fresco/Impls/viewers.h>

class Manipulator;
class Selection;
class Tool;
class TransformImpl;

class FigViewer;
typedef FigViewer* FigViewerRef;

class FigViewer : public ViewerImpl {
public:
    FigViewer(Fresco*, Boolean* editing);
    ~FigViewer();
    
    void allocations(Glyph::AllocationInfoSeq& a); //+ Glyph::allocations
    void traverse(GlyphTraversal_in t); //+ Glyph::traverse
    void request(Glyph::Requisition& r); //+ Glyph::request
    Transform_return transformation(); //+ Glyph::transformation
    void need_resize(); //+ Glyph::need_resize

    Tool* current_tool();
    void current_tool(Tool*);

    Boolean press(GlyphTraversal_in, EventRef);
    Boolean drag(GlyphTraversal_in, EventRef);
    Boolean move(GlyphTraversal_in, EventRef);
    Boolean release(GlyphTraversal_in, EventRef);
    
    Manipulator* root();
    Selection* selection();

protected:
    Boolean* editing_;
    Tool* curtool_;
    Selection* sel_;
    Manipulator* root_;
    Manipulator* active_;
    RegionImpl* allocation_;
    DamageRef damage_;
    TransformImpl* transform_;
};

#endif
