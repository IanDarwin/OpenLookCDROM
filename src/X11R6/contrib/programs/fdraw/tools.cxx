/*
 * tools.c
 */

#include "commands.h"
#include "figviewer.h"
#include "manipulators.h"
#include "selection.h"
#include "tools.h"
#include "visitors.h"
#include <X11/Fresco/fresco.h>
#include <X11/Fresco/viewer.h>
#include <X11/Fresco/Impls/region.h>
#include <X11/Fresco/Impls/traversal.h>
#include <X11/Fresco/Ox/typeobjs.h>

SelectInfo::SelectInfo () {
    m_ = nil;
    a_ = nil;
}

SelectInfo::~SelectInfo () {
    Fresco::unref(a_);
}

SelectInfo* SelectInfo::copy () {
    SelectInfo* scopy = new SelectInfo;
    scopy->t_.load(&t_);
    scopy->level_ = level_;
    scopy->m_ = m_;
    if (is_not_nil(a_)) {
        scopy->a_ = new RegionImpl;
        scopy->a_->copy(a_);
    } else {        
        scopy->a_ = nil;
    }
    return scopy;
}

declarePtrList(SelectInfoList, SelectInfo);
implementPtrList(SelectInfoList, SelectInfo);

static void get_manipulators(
    GlyphTraversalRef gt, SelectInfoList* slist, long l
) {
    if (gt->forward()) {
        GlyphOffsetRef go = gt->current_offset();
        Glyph_var g = go->child();
        Manipulator* m = Manipulator::_narrow(g);
        if (is_not_nil(m)) {
            SelectInfo* sinfo = new SelectInfo;
            sinfo->m_ = m; 
            sinfo->t_.load(_tmp(gt->current_transform())); 
            sinfo->level_ = l;
            if (is_not_nil(_tmp(gt->allocation()))) { 
                sinfo->a_ = new RegionImpl;
                sinfo->a_->copy(_tmp(gt->allocation()));
            }
            slist->append(sinfo);
        }
        get_manipulators(gt, slist, ++l);
        gt->backward();
    }
}

static long level (Manipulator* m) {
    SelectInfoCmd scmd;
    m->execute(&scmd);
    SelectInfo* sinfo = scmd.select_info();
    if (sinfo != nil) {
        return sinfo->level_;
    }
    return -1;
}

static Manipulator* create_macro (Selection* sel) {
    if (sel->count() == 0) {
        return nil;
    } else if (sel->count() == 1) {
        return sel->item(0);
    } else {
        MacroManip* mm = new MacroManip;
        for (long i = 0; i < sel->count(); i++) {
            Manipulator* m = sel->item(i);
            mm->add(m);
        }
        return mm;
    }
}

static void unselect_all(Selection* sel) {
    SelectCmd unselect(nil, false);
    for (long i = 0; i < sel->count(); i++) {
        Manipulator* m = sel->item(i);
        m->execute(&unselect);
    }
    sel->remove_all();
}

static void unselect(Selection* sel, Manipulator* m) {
    SelectCmd unselect(nil, false);
    m->execute(&unselect);
    sel->remove(m);
}

//+ Tool(FrescoObject)
extern TypeObj_Descriptor _XfFrescoObject_type;
TypeObj_Descriptor* _XfTool_parents[] = { &_XfFrescoObject_type, nil };
extern TypeObjId _XfTool_tid;
TypeObj_Descriptor _XfTool_type = {
    /* type */ 0,
    /* id */ &_XfTool_tid,
    "Tool",
    _XfTool_parents, /* offsets */ nil, /* excepts */ nil,
    /* methods */ nil, /* params */ nil,
    /* receive */ nil
};

Tool* Tool::_narrow(BaseObjectRef o) {
    return (Tool*)_BaseObject_tnarrow(
        o, _XfTool_tid, 0
    );
}
TypeObjId Tool::_tid() { return _XfTool_tid; }
//+

Tool::Tool () {}

Tool::~Tool () {}

void Tool::anchor (Coord& ax, Coord& ay) {
    ax = ax_;
    ay = ay_;
}

SelectInfo* Tool::create_manipulator (
    GlyphTraversalRef gt, EventRef e, FigViewer* fv
) {
    SelectTool select_tool;
    SelectInfo* si = select_tool.create_manipulator(gt, e, fv);
    select_tool.anchor(ax_, ay_);
    if (si != nil) {
        SelectCmd selectcmd(fv);
        si->m_->execute(&selectcmd);
        si->m_ = create_macro(fv->selection());
    }
    return si;
}

//+ SelectTool(Tool)
extern TypeObj_Descriptor _XfTool_type;
TypeObj_Descriptor* _XfSelectTool_parents[] = { &_XfTool_type, nil };
extern TypeObjId _XfSelectTool_tid;
TypeObj_Descriptor _XfSelectTool_type = {
    /* type */ 0,
    /* id */ &_XfSelectTool_tid,
    "SelectTool",
    _XfSelectTool_parents, /* offsets */ nil, /* excepts */ nil,
    /* methods */ nil, /* params */ nil,
    /* receive */ nil
};

SelectTool* SelectTool::_narrow(BaseObjectRef o) {
    return (SelectTool*)_BaseObject_tnarrow(
        o, _XfSelectTool_tid, 0
    );
}
TypeObjId SelectTool::_tid() { return _XfSelectTool_tid; }
//+

SelectInfo* SelectTool::create_manipulator (
    GlyphTraversalRef gt, EventRef e, FigViewer* fv
) {
    ax_ = e->pointer_x();
    ay_ = e->pointer_y();

    SelectInfoList slist;
    get_manipulators(gt, &slist, 0);
    SelectInfo* target = nil;
    Selection* sel = fv->selection();
    if (slist.count() != 0) {
        if (sel->count() == 0) {
            target = slist.item(1);
            slist.remove(1);
        } else {
            Boolean selected = false;
            for (long i = 1; i < slist.count(); i++) {
                if (sel->selected(slist.item(i)->m_)) {
                    selected = true;
                    break;
                }
            }
            if (selected) {
                if (e->modifier_is_down(Event::control)) {
                    unselect_all(sel);
                } else if (e->modifier_is_down(Event::shift)) {
                    unselect_all(sel);
                    i = i%(slist.count()-1)+1;
                } else {
                    unselect(sel, slist.item(i)->m_);
                }
                target = slist.item(i);
                slist.remove(i);
            } else if (e->modifier_is_down(Event::shift)) {
                target = slist.item(1);
                slist.remove(1);
                Selection* sel_copy = sel->copy();
                for (long i = 0; i < sel_copy->count(); i++) {
                    if (level(sel->item(i)) != target->level_) {
                        unselect(sel, sel->item(i));
                    }
                }
                delete sel_copy;

            } else {
                unselect_all(sel);
                target = slist.item(1);;
                slist.remove(1);
            }
        }
        for (long i = 0; i < slist.count(); i++) {
            delete slist.item(i);
        }
    } else if (!e->modifier_is_down(Event::shift)) {
        unselect_all(sel);
    }
    if (target != nil) {
        Vertex lower, upper;
        Glyph::AllocationInfo a;
        RegionImpl r;

        a.allocation = nil;
        a.transformation = new TransformImpl;
        a.transformation->load(&target->t_);
        a.damaged = nil;
        target->m_->extension(a, &r);
        r.bounds(lower, upper);
        ax_ = (lower.x + upper.x) * 0.5;
        ay_ = (lower.y + upper.y) * 0.5;
        Fresco::unref(a.allocation);
        Fresco::unref(a.transformation);
    }
    return target;
}

SelectTool::SelectTool () {}

SelectTool::~SelectTool () {}

//+ CreateTool(Tool)
extern TypeObj_Descriptor _XfTool_type;
TypeObj_Descriptor* _XfCreateTool_parents[] = { &_XfTool_type, nil };
extern TypeObjId _XfCreateTool_tid;
TypeObj_Descriptor _XfCreateTool_type = {
    /* type */ 0,
    /* id */ &_XfCreateTool_tid,
    "CreateTool",
    _XfCreateTool_parents, /* offsets */ nil, /* excepts */ nil,
    /* methods */ nil, /* params */ nil,
    /* receive */ nil
};

CreateTool* CreateTool::_narrow(BaseObjectRef o) {
    return (CreateTool*)_BaseObject_tnarrow(
        o, _XfCreateTool_tid, 0
    );
}
TypeObjId CreateTool::_tid() { return _XfCreateTool_tid; }
//+

CreateTool::CreateTool (Manipulator* m) {
    manip_ = m;
}

CreateTool::~CreateTool () {
    Fresco::unref(manip_);
}

SelectInfo* CreateTool::create_manipulator(
    GlyphTraversalRef gt, EventRef e, FigViewer* fv
) {
    ax_ = e->pointer_x();
    ay_ = e->pointer_y();

    unselect_all(fv->selection());
    CopyCmd::glyphmap_->clear();
    Manipulator* fm = manip_->deep_copy();
    Manipulator* m = fv->root();
    Glyph_var b = m->body();
    b->append(fm);

    SelectCmd selectcmd(fv);
    fm->execute(&selectcmd);

    SelectInfo* sinfo = new SelectInfo;
    sinfo->level_ = 2; // you just know you start from level 2
    sinfo->m_ = fm;
    sinfo->t_.load(gt->current_transform());

    return sinfo;
}

//+ MoveTool(Tool)
extern TypeObj_Descriptor _XfTool_type;
TypeObj_Descriptor* _XfMoveTool_parents[] = { &_XfTool_type, nil };
extern TypeObjId _XfMoveTool_tid;
TypeObj_Descriptor _XfMoveTool_type = {
    /* type */ 0,
    /* id */ &_XfMoveTool_tid,
    "MoveTool",
    _XfMoveTool_parents, /* offsets */ nil, /* excepts */ nil,
    /* methods */ nil, /* params */ nil,
    /* receive */ nil
};

MoveTool* MoveTool::_narrow(BaseObjectRef o) {
    return (MoveTool*)_BaseObject_tnarrow(
        o, _XfMoveTool_tid, 0
    );
}
TypeObjId MoveTool::_tid() { return _XfMoveTool_tid; }
//+

MoveTool::MoveTool () {}

MoveTool::~MoveTool () {}

//+ ScaleTool(Tool)
extern TypeObj_Descriptor _XfTool_type;
TypeObj_Descriptor* _XfScaleTool_parents[] = { &_XfTool_type, nil };
extern TypeObjId _XfScaleTool_tid;
TypeObj_Descriptor _XfScaleTool_type = {
    /* type */ 0,
    /* id */ &_XfScaleTool_tid,
    "ScaleTool",
    _XfScaleTool_parents, /* offsets */ nil, /* excepts */ nil,
    /* methods */ nil, /* params */ nil,
    /* receive */ nil
};

ScaleTool* ScaleTool::_narrow(BaseObjectRef o) {
    return (ScaleTool*)_BaseObject_tnarrow(
        o, _XfScaleTool_tid, 0
    );
}
TypeObjId ScaleTool::_tid() { return _XfScaleTool_tid; }
//+

ScaleTool::ScaleTool () {}

ScaleTool::~ScaleTool () {}

//+ RotateTool(Tool)
extern TypeObj_Descriptor _XfTool_type;
TypeObj_Descriptor* _XfRotateTool_parents[] = { &_XfTool_type, nil };
extern TypeObjId _XfRotateTool_tid;
TypeObj_Descriptor _XfRotateTool_type = {
    /* type */ 0,
    /* id */ &_XfRotateTool_tid,
    "RotateTool",
    _XfRotateTool_parents, /* offsets */ nil, /* excepts */ nil,
    /* methods */ nil, /* params */ nil,
    /* receive */ nil
};

RotateTool* RotateTool::_narrow(BaseObjectRef o) {
    return (RotateTool*)_BaseObject_tnarrow(
        o, _XfRotateTool_tid, 0
    );
}
TypeObjId RotateTool::_tid() { return _XfRotateTool_tid; }
//+

RotateTool::RotateTool () {}

RotateTool::~RotateTool () {}

//+ AlterTool(Tool)
extern TypeObj_Descriptor _XfTool_type;
TypeObj_Descriptor* _XfAlterTool_parents[] = { &_XfTool_type, nil };
extern TypeObjId _XfAlterTool_tid;
TypeObj_Descriptor _XfAlterTool_type = {
    /* type */ 0,
    /* id */ &_XfAlterTool_tid,
    "AlterTool",
    _XfAlterTool_parents, /* offsets */ nil, /* excepts */ nil,
    /* methods */ nil, /* params */ nil,
    /* receive */ nil
};

AlterTool* AlterTool::_narrow(BaseObjectRef o) {
    return (AlterTool*)_BaseObject_tnarrow(
        o, _XfAlterTool_tid, 0
    );
}
TypeObjId AlterTool::_tid() { return _XfAlterTool_tid; }
//+

AlterTool::AlterTool () {}

AlterTool::~AlterTool () {}

//+ ResizeTool(Tool)
extern TypeObj_Descriptor _XfTool_type;
TypeObj_Descriptor* _XfResizeTool_parents[] = { &_XfTool_type, nil };
extern TypeObjId _XfResizeTool_tid;
TypeObj_Descriptor _XfResizeTool_type = {
    /* type */ 0,
    /* id */ &_XfResizeTool_tid,
    "ResizeTool",
    _XfResizeTool_parents, /* offsets */ nil, /* excepts */ nil,
    /* methods */ nil, /* params */ nil,
    /* receive */ nil
};

ResizeTool* ResizeTool::_narrow(BaseObjectRef o) {
    return (ResizeTool*)_BaseObject_tnarrow(
        o, _XfResizeTool_tid, 0
    );
}
TypeObjId ResizeTool::_tid() { return _XfResizeTool_tid; }
//+

ResizeTool::ResizeTool () {}

ResizeTool::~ResizeTool () {}
