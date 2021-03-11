/*
 * vistitors.cxx
 */

#include "globals.h"
#include "commands.h"
#include "manipulators.h"
#include "visitors.h"
#include <X11/Fresco/fresco.h>
#include <X11/Fresco/viewer.h>
#include <X11/Fresco/Impls/region.h>
#include <X11/Fresco/Impls/transform.h>
#include <stdlib.h> 

GlyphVisitor::GlyphVisitor() { }
GlyphVisitor::~GlyphVisitor() { }

Boolean GlyphVisitor::visit(GlyphRef, GlyphOffsetRef) { return false; }

void GlyphVisitor::visit_children(GlyphRef parent) {
    GlyphOffset_var g = parent->first_child_offset();
    for (; is_not_nil(g); g = g->next_child()) {
	if (!visit(g->child(), g)) {
	    break;
	}
    }
}

void GlyphVisitor::visit_parents(GlyphRef parent) {
    GlyphOffsetRef static_parents[5];
    Glyph::OffsetSeq parents(5, 0, static_parents);
    parent->parent_offsets(parents);
    for (Long i = 0; i < parents._length; i++) {
	GlyphOffsetRef g = parents._buffer[i];
	if (!visit(g->parent(), g)) {
	    break;
	}
    }
    if (parents._buffer == static_parents) {
	/* don't deallocate the static buffer */
	parents._buffer = nil;
    }
}

implementPtrList(AllocationList, RegionImpl);
implementPtrList(TransformList, TransformImpl);

Appender::Appender(GlyphRef glyph, Boolean resize) {
    glyph_ = glyph;
    resize_ = resize;
}

Boolean Appender::visit(GlyphRef parent, GlyphOffsetRef) {
    parent->append(glyph_);
    if (resize_) {
        parent->need_resize();
    }
    return true;
}

Appender::~Appender () {}

Remover::Remover() {}

Remover::~Remover() {
    for (ListItr(GlyphOffsetList) i(list_); i.more(); i.next()) {
	GlyphOffsetRef offset = i.cur();
        Glyph_var parent = offset->parent();
        Glyph_var child = offset->child();
        child->need_redraw();
	offset->notify_observers();
	offset->remove();
        parent->need_resize();
    }
}

Boolean Remover::visit(GlyphRef, GlyphOffsetRef offset) {
    list_.append(offset);
    return true;
}

Counter::Counter() {
    count_ = 0;
}

Boolean Counter::visit(GlyphRef, GlyphOffsetRef) {
    count_++;
    return true;
}

CmdVisitor::CmdVisitor (Command* cmd, Boolean e) {
    cmd_ = cmd;
    execute_ = e;
}

CmdVisitor::~CmdVisitor () {}

Boolean CmdVisitor::visit (GlyphRef g, GlyphOffsetRef) {
    Manipulator* m = Manipulator::_narrow(g);
    if (is_not_nil(m)) {
        if (execute_) {
            m->execute(cmd_);
        } else {
            m->unexecute(cmd_);
        }
    }
    return true;
}
    
ManipCopier::ManipCopier (Boolean s) {
    maniplist_ = new ManipList;
    shallow_ = s;
}

ManipCopier::~ManipCopier () {
    for (Long i = 0; i < maniplist_->count(); i++) {
        Fresco::unref(maniplist_->item(i));
    }
    delete maniplist_;
}

Boolean ManipCopier::visit (GlyphRef g, GlyphOffsetRef) {
    Manipulator* m = Manipulator::_narrow(g);
    if (is_not_nil(m)) {
        if (shallow_) {
            maniplist_->append(m->shallow_copy());
        } else {
            maniplist_->append(m->deep_copy());
        }
    }
    return true;
}

TAManipCopier::TAManipCopier (Region_in a, Boolean s) {
    alist_ = new AllocationList;
    tlist_ = new TransformList;
    a_ = a;
}

TAManipCopier::~TAManipCopier () {
    for (long i = 0; i < alist_->count(); i++) {
        Fresco::unref(alist_->item(i));
    }
    delete alist_;
    for (long j = 0; i < tlist_->count(); j++) {
        Fresco::unref(tlist_->item(j));
    }
    delete tlist_;
}

Boolean TAManipCopier::visit (GlyphRef g, GlyphOffsetRef go) {
    ManipCopier::visit(g, go);
    RegionImpl* r = new RegionImpl;
    TransformImpl* t = new TransformImpl;
    r->copy(a_);

    Glyph::AllocationInfo a;
    a.allocation = r;
    a.transformation = t;
    a.damaged = nil;
    
    go->child_allocate(a);
    
    alist_->append(r);
    tlist_->append(t);

    return true;
}

OffsetVisitor::OffsetVisitor () {
    glist_ = new GlyphOffsetList;
}

OffsetVisitor::~OffsetVisitor () {
    for (long i = 0; i < glist_->count(); i++) {
        Fresco::unref(glist_->item(i));
    }
    delete glist_;
}

Boolean OffsetVisitor::visit (GlyphRef, GlyphOffsetRef go) {
    glist_->append(GlyphOffset::_duplicate(go));
    return true;
}

GlyphOffsetRef OffsetVisitor::offset (long i) {
    GlyphOffsetRef go = nil;
    if (i < glist_->count()) {
        go = glist_->item(i);
    }
    return go;
}

long OffsetVisitor::offset_count () {
    return glist_->count();
}
