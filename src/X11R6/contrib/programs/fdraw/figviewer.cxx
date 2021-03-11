/*
 * figviewer.cxx
 */

#include "commands.h"
#include "figviewer.h"
#include "globals.h"
#include "manipulators.h"
#include "selection.h"
#include "tools.h"
#include "visitors.h"
#include <X11/Fresco/Impls/glyphs.h>
#include <X11/Fresco/Impls/region.h>
#include <X11/Fresco/Impls/transform.h>
#include <X11/Fresco/OS/memory.h>

class ViewerDamage : public Damage {
public:
    ViewerDamage(Region_in a, Transform_in t, Damage_in d);
    virtual void extend(Region_in r);
protected:
    Damage_var damage_;
    RegionImpl clip_;
};

ViewerDamage::ViewerDamage(Region_in a, Transform_in t, Damage_in d) {
    damage_ = d;
    clip_.copy(a);
    clip_.apply_transform(t);
    float tol = 0.1;
    clip_.lower_.x += tol;
    clip_.lower_.y += tol;
    clip_.upper_.x -= tol;
    clip_.upper_.y -= tol;
}

void ViewerDamage::extend(Region_in r) {
    if (r->intersects(&clip_)) {
	RegionImpl region;
	region.copy(r);
	region.merge_intersect(&clip_);
	damage_->extend(&region);
    }
}

FigViewer::FigViewer(Fresco* f, Boolean* editing) : ViewerImpl(f, true) {
    curtool_ = nil;
    active_ = nil;
    editing_ = editing;

    sel_ = new Selection(10);
    PolyFigure* pf = new PolyFigure;
    root_ = new PolyManip(pf);
    Fresco::unref(pf);

    allocation_ = new RegionImpl;
    damage_ = nil;
    transform_ = new TransformImpl;
    body(root_);
}
 
FigViewer::~FigViewer() {
    delete sel_;
}

//+ FigViewer(Glyph::allocations)
void FigViewer::allocations(Glyph::AllocationInfoSeq& a) {
    if (a._length >= a._maximum) {
	Long n = a._maximum == 0 ? 10 : a._maximum + a._maximum;
	Glyph::AllocationInfo* buffer = new Glyph::AllocationInfo[n];
	if (a._maximum > 0) {
	    Memory::copy(
		a._buffer, buffer, a._maximum * sizeof(Glyph::AllocationInfo)
	    );
	}
	/* should free old buffer? */
	a._buffer = buffer;
	a._maximum = n;
    }
    Glyph::AllocationInfo& i = a._buffer[a._length];
    i.allocation = new RegionImpl;
    i.allocation->copy(allocation_);
    i.transformation = new TransformImpl;
    i.transformation->premultiply(transform_);
    i.damaged = new ViewerDamage(allocation_, transform_, damage_);
    ++a._length;
}

//+ FigViewer(Glyph::traverse)
void FigViewer::traverse(GlyphTraversal_in t) {
    switch (t->op()) {
    case GlyphTraversal::pick_top:
    case GlyphTraversal::pick_all:
    case GlyphTraversal::pick_any:

// This is inefficient for motion events
        ViewerImpl::traverse(t);
        if (is_nil(_tmp(t->picked())) && t->allocation_is_visible()) {
            t->begin_viewer(this);
            t->hit();
            t->end_viewer();
        }
        break;
    case GlyphTraversal::draw:
        {
            allocation_->copy(_tmp(t->allocation()));
            Painter_var p = t->current_painter();
            transform_->load(_tmp(p->current_matrix()));
            damage_ = t->damaged();

	    p->push_clipping();
	    p->clip_rect(
		allocation_->lower_.x, allocation_->lower_.y,
		allocation_->upper_.x, allocation_->upper_.y
	    );
            ViewerImpl::traverse(t);
	    p->pop_clipping();
        }
        break;
    default:
        ViewerImpl::traverse(t);
        break;
    }
}

Manipulator* FigViewer::root () { 
    return root_; 
}

Selection* FigViewer::selection() {
    return sel_;
}

//+ FigViewer(Glyph::request)
void FigViewer::request(Glyph::Requisition& r) {
    LayoutKit_var layouts = fresco_instance()->layout_kit();
    Coord fil = layouts->fil();
    r.x.natural = 0; r.x.maximum = fil, r.x.minimum = 0, r.x.align = 0.5;
    r.y.natural = 0; r.y.maximum = fil, r.y.minimum = 0, r.y.align = 0.5;
}

Tool* FigViewer::current_tool() { return curtool_; }
void FigViewer::current_tool(Tool* t) { curtool_ = t; }

//+ FigViewer(Glyph::transformation)
Transform_return FigViewer::transformation() {
    Fresco::ref(transform_);
    return transform_;
}

//+ FigViewer(Glyph::need_resize)
void FigViewer::need_resize() {
    /*
     * Do nothing because we assume that we've already handled
     * a resize appropriately.
     */
}

Boolean FigViewer::press(GlyphTraversal_in t, EventRef e) {
    if (!*editing_) {
        return false;
    }
    Boolean ok = false;
    if (is_nil(active_)) {
        SelectInfo* si = curtool_->create_manipulator(t, e, this);
        active_ = (si != nil) ? si->m_ : nil;
        if (is_not_nil(active_)) {
            ok = active_->grasp(*curtool_, *this, si, e);
        } else {
            ok = false;
        }
        if (ok) {
            grab(t);
        }
    } else {
        ok = active_->manipulate(e);
    }
    if(!ok && is_not_nil(active_)) {
        Command* command = active_->effect(e);
        if (is_not_nil(command) && command->reversible()) {
            command->log();
        }
        active_ = nil;
    }
    return true;
}

Boolean FigViewer::move(GlyphTraversal_in t, EventRef e) {
    if (!*editing_) {
        return false;
    }
    return drag(t, e);
}

Boolean FigViewer::drag(GlyphTraversal_in, EventRef e) {
    if (!*editing_) {
        return false;
    }
    Boolean ok = false;
    if (is_not_nil(active_)) {
        ok = active_->manipulate(e);
        if (!ok) {
            Command* command = active_->effect(e);
            if (is_not_nil(command) && command->reversible()) {
                command->log();
            }
            active_ = nil;
            ungrab();
        }
    }
    return true;
}

Boolean FigViewer::release(GlyphTraversal_in, EventRef e) {
    if (!*editing_) {
        return false;
    }
    Boolean ok = false;
    if (is_not_nil(active_)) {
        ok = active_->manipulate(e);
        if (!ok) {
            Command* command = active_->effect(e);
            if (is_not_nil(command) && command->reversible()) {
                command->log();
            }
            active_ = nil;
            ungrab();
        }
    }
    return true;
}
