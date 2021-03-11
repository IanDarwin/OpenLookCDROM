/*
 * manipulators.cxx
 */

#include "commands.h"
#include "figviewer.h"
#include "globals.h"
#include "manipulators.h"
#include "selection.h"
#include "tools.h"
#include "visitors.h"
#include <X11/Fresco/drawing.h>
#include <X11/Fresco/figures.h>
#include <X11/Fresco/fresco.h>
#include <X11/Fresco/viewer.h>
#include <X11/Fresco/widgets.h>
#include <X11/Fresco/Impls/action.h>
#include <X11/Fresco/Impls/region.h>
#include <X11/Fresco/Impls/Xdrawing.h>
#include <X11/Fresco/OS/math.h>
#include <X11/Fresco/Ox/typeobjs.h>
#include <math.h>
#include <stdio.h>
#include <string.h>
#if defined(sgi) || (defined(sun) && !defined(SVR4))
#include <values.h>
#endif

static Coord dot = 4.0;
static double tol = 0.000005;
static ColorImpl* black = nil;
static Coord mfil = 10.0e6;

class PrintAction : public ActionImpl {
public:
    PrintAction(const char*);
    virtual ~PrintAction();

    virtual void execute(); 
private:
    char* string_;
};

PrintAction::PrintAction (const char* s) {
    string_ = strdup(s);
}

PrintAction::~PrintAction () {
    delete string_;
}

void PrintAction::execute () {
    printf("%s\n", string_);
}

static void glyph_need_redraw(
    GlyphRef g, const Glyph::AllocationInfoSeq& a, 
    Boolean obj_manip, Boolean immediate
) {
    RegionImpl r;
    Glyph::AllocationInfo clone;
    clone.transformation = new TransformImpl;
    clone.allocation = nil;

    for (Long i = 0; i < a._length; i++) {
	const Glyph::AllocationInfo& info = a._buffer[i];
	if (
            is_not_nil(info.damaged) && (is_nil(info.allocation) || immediate)
        ) {
            r.defined_ = false;
            if (is_not_nil(info.transformation)) {
                clone.transformation->load(info.transformation);
            } else {
                clone.transformation->load_identity();
            }
            if (is_not_nil(info.allocation)) {
                clone.allocation = new RegionImpl;
                clone.allocation->copy(info.allocation);
            }
            clone.damaged = info.damaged;
	    g->extension(clone, &r);
            if (obj_manip) {
                float delta = dot/2+1;
                r.lower_.x -= delta;
                r.lower_.y -= delta;
                r.upper_.x += delta;
                r.upper_.y += delta;
            }
	    info.damaged->extend(&r);
            if (is_not_nil(info.allocation)) {
                Fresco::unref(clone.allocation);
                clone.allocation = nil;
            }
	}
    }
    Fresco::unref(clone.transformation);
}

static void delete_info_list (Glyph::AllocationInfoSeq& alist) {
    for (long i = 0; i < alist._length; i++) {
        Fresco::unref(alist._buffer[i].allocation);
        Fresco::unref(alist._buffer[i].transformation);
        Fresco::unref(alist._buffer[i].damaged);
    }
}

static void _nat_allocation (Glyph::Requisition* r, Region_in region) {
    RegionImpl rr;
    rr.xalign_ = r->x.align;
    rr.lower_.x = -r->x.align * r->x.natural;
    rr.upper_.x = rr.lower_.x + r->x.natural;
    rr.yalign_ = r->y.align;
    rr.lower_.y = -r->y.align * r->y.natural;
    rr.upper_.y = rr.lower_.y + r->y.natural;
    rr.lower_.z = 0;
    rr.upper_.z = 0;
    rr.zalign_ = 0;
    rr.defined_ = true;
    region->copy(&rr);
}

static void adjust_dotted_region (RegionImpl& region) {
    float delta = dot/2+2;
    region.lower_.x -= delta;
    region.lower_.y -= delta;
    region.upper_.x += delta;
    region.upper_.y += delta;
}    

static Float aspect_ratio(Glyph::Requisition* r) {
    Float aspect;
    if (r->x.natural == 0) {
        aspect = mfil;
    } else {
        aspect = r->y.natural/r->x.natural;
    }
    return aspect;
}

static void _translate (TransformRef tr, Vertex& v) {
    tr->translate(v);
}

static void _scale (TransformRef tr, Vertex& center, Vertex& s) {
    Vertex ncenter = center;
    ncenter.x = -ncenter.x;
    ncenter.y = -ncenter.y;
    ncenter.z = -ncenter.z;

    tr->translate(ncenter);
    tr->scale(s);
    tr->translate(center);
}

static void _rotate(TransformRef tr, double angle, Axis a, Vertex& center) {
    Vertex ncenter = center;
    ncenter.x = -ncenter.x;
    ncenter.y = -ncenter.y;
    ncenter.z = -ncenter.z;

    tr->translate(ncenter);
    tr->rotate(angle, a);
    tr->translate(center);
}

static void compose (GlyphRef m) {
    Transform_var tx = m->transformation();
    if (is_not_nil(tx)) {
        Transform::Matrix mat;
        tx->store_matrix(mat);
        mat[2][0] = 0; mat[2][1] = 0;
        tx->load_matrix(mat);
    }
}

static void local_extension(GlyphRef g, RegionImpl* r) {
    Glyph::AllocationInfo a;
    a.transformation = nil;
    a.allocation = nil;
    a.damaged = nil;
    r->defined_ = false;
    g->extension(a, r);
}

static void flexible_transform_requisition(
    Glyph::Requisition& req, TransformRef tx
) {
    RegionImpl nat, maxi, mini;
    
    nat.xalign_ = req.x.align;
    nat.lower_.x = -req.x.align * req.x.natural;
    nat.upper_.x = nat.lower_.x + req.x.natural;
    nat.yalign_ = req.y.align;
    nat.lower_.y = -req.y.align * req.y.natural;
    nat.upper_.y = nat.lower_.y + req.y.natural;
    nat.lower_.z = nat.upper_.z = 0;
    nat.defined_ = true;

    maxi.xalign_ = req.x.align;
    maxi.lower_.x = -req.x.align * req.x.maximum;
    maxi.upper_.x = maxi.lower_.x + req.x.maximum;
    maxi.yalign_ = req.y.align;
    maxi.lower_.y = -req.y.align * req.y.maximum;
    maxi.upper_.y = maxi.lower_.y + req.y.maximum;
    maxi.lower_.z = maxi.upper_.z = 0;
    maxi.defined_ = true;

    mini.xalign_ = req.x.align;
    mini.lower_.x = -req.x.align * req.x.minimum;
    mini.upper_.x = mini.lower_.x + req.x.minimum;
    mini.yalign_ = req.y.align;
    mini.lower_.y = -req.y.align * req.y.minimum;
    mini.upper_.y = mini.lower_.y + req.y.minimum;
    mini.lower_.z = mini.upper_.z = 0;
    mini.defined_ = true;

    if (!tx->identity()) {
        nat.apply_transform(tx);
        maxi.apply_transform(tx);
        mini.apply_transform(tx);
    }
            
    Coord x_nat_lead = -nat.lower_.x;
    Coord x_max_lead = -maxi.lower_.x;
    Coord x_min_lead = -mini.lower_.x;
    Coord x_nat_trail = nat.upper_.x;
    Coord x_max_trail = maxi.upper_.x;
    Coord x_min_trail = mini.upper_.x;

    Coord y_nat_lead = -nat.lower_.y;
    Coord y_max_lead = -maxi.lower_.y;
    Coord y_min_lead = -mini.lower_.y;
    Coord y_nat_trail = nat.upper_.y;
    Coord y_max_trail = maxi.upper_.y;
    Coord y_min_trail = mini.upper_.y;

    GlyphImpl::require_lead_trail(
        req.x, x_nat_lead, x_max_lead, x_min_lead, 
        x_nat_trail, x_max_trail, x_min_trail
    );
    GlyphImpl::require_lead_trail(
        req.y, y_nat_lead, y_max_lead, y_min_lead, 
        y_nat_trail, y_max_trail, y_min_trail
    );
}

static void fixed_transform_requisition(
    Glyph::Requisition& req, TransformRef tx
) {
    RegionImpl nat;
    
    nat.xalign_ = req.x.align;
    nat.lower_.x = -req.x.align * req.x.natural;
    nat.upper_.x = nat.lower_.x + req.x.natural;
    nat.yalign_ = req.y.align;
    nat.lower_.y = -req.y.align * req.y.natural;
    nat.upper_.y = nat.lower_.y + req.y.natural;
    nat.lower_.z = nat.upper_.z = 0;
    nat.defined_ = true;

    if (!tx->identity()) {
        nat.apply_transform(tx);
    }
            
    Coord x_nat_lead = -nat.lower_.x;
    Coord x_nat_trail = nat.upper_.x;

    Coord y_nat_lead = -nat.lower_.y;
    Coord y_nat_trail = nat.upper_.y;

    GlyphImpl::require_lead_trail(
        req.x, x_nat_lead, x_nat_lead, x_nat_lead, 
        x_nat_trail, x_nat_trail, x_nat_trail
    );
    GlyphImpl::require_lead_trail(
        req.y, y_nat_lead, y_nat_lead, y_nat_lead, 
        y_nat_trail, y_nat_trail, y_nat_trail
    );
}

static Float pie = 3.1415927;

static void extract_info (
    TransformRef t, Vertex& scaling, Float& rotation, Vertex& translation
) {
    Transform::Matrix m;
    t->store_matrix(m);
    if (m[1][1] == 0) {
        rotation = pie/2;
    } else {
        rotation = atan(m[0][1]/m[1][1]);
    }
    if (rotation < 0) {
        if (m[0][1] > 0 && m[1][1] < 0) {
            rotation += pie;
        }
    } else {
        if (m[0][1] < 0 && m[1][1] < 0) {
            rotation += pie;
        }
    }
    double cos_rot = cos(rotation);
    double sin_rot = sin(rotation);
    if (cos_rot != 0) {
        scaling.x = m[0][0]/cos_rot;
    } else {
        scaling.x = -m[1][0]/sin_rot;
    }
    if (sin_rot != 0) {
        scaling.y = m[0][1]/sin_rot;
    } else {
        scaling.y = m[1][1]/cos_rot;
    }
    scaling.z = 1.0;
    translation.x = m[2][0];
    translation.y = m[2][1];
    translation.z = 0;
}

static Boolean rotated (TransformRef t) {
    Transform::Matrix m;
    t->store_matrix(m);
    return (m[0][1] != 0 || m[1][0] != 0);
}

static void adjust_allocation (
    Region_in a, TransformRef t, Glyph::Requisition* req
) {
    if (!rotated(t)) {
        TransformImpl tx;
        tx.load(t);
        tx.invert();
        a->apply_transform(&tx);
    } else {
        RegionImpl region;
        Vertex scaling, translation, upper, lower;
        Float rotation, xnat, ynat;
        Float aspect = aspect_ratio(req);
        extract_info(t, scaling, rotation, translation);
        region.copy(a);
        TransformImpl tx;
        tx.scale(scaling);
        tx.invert();
        region.apply_transform(&tx);
        region.bounds(lower, upper);

        double cos_rot = Math::abs(cos(rotation));
        double sin_rot = Math::abs(sin(rotation));
        xnat = Math::min(
            (upper.x-lower.x)/(cos_rot+aspect*sin_rot),
            (upper.y-lower.y)/(sin_rot+aspect*cos_rot)
        );
        ynat = aspect*xnat;

        region.xalign_ = req->x.align;
        region.yalign_ = req->y.align;
        
        region.lower_.x = -region.xalign_*xnat;
        region.lower_.y = -region.yalign_*ynat;
        region.upper_.x = region.lower_.x + xnat;
        region.upper_.y = region.lower_.y + ynat;
        a->copy(&region);
    }
}

static void draw_handle_box(
    PainterRef po, Coord l, Coord b, Coord r, Coord t
) {
    if (is_nil(black)) {
        black = new ColorImpl(0.0, 0.0, 0.0);
    }
    po->current_color(black);
    po->push_matrix();
    po->current_matrix()->load_identity();

    Coord cx, cy;
    cx = l; cy = b;
    po->fill_rect(cx-dot/2, cy-dot/2, cx+dot/2, cy+dot/2); 

    cx = l; cy = (b+t)/2;
    po->fill_rect(cx-dot/2, cy-dot/2, cx+dot/2, cy+dot/2); 

    cx = l; cy = t;
    po->fill_rect(cx-dot/2, cy-dot/2, cx+dot/2, cy+dot/2); 

    cx = (l+r)/2; cy = t;
    po->fill_rect(cx-dot/2, cy-dot/2, cx+dot/2, cy+dot/2); 

    cx = r; cy = t;
    po->fill_rect(cx-dot/2, cy-dot/2, cx+dot/2, cy+dot/2); 

    cx = r; cy = (t+b)/2;
    po->fill_rect(cx-dot/2, cy-dot/2, cx+dot/2, cy+dot/2); 

    cx = r; cy = b;
    po->fill_rect(cx-dot/2, cy-dot/2, cx+dot/2, cy+dot/2); 

    cx = (r+l)/2; cy = b;
    po->fill_rect(cx-dot/2, cy-dot/2, cx+dot/2, cy+dot/2); 

    po->pop_matrix();
}

implementPtrList(ManipList, Manipulator);

ManipInfo::ManipInfo (SelectInfo* sinfo) {
    sinfo_ = sinfo;
    tooltype_ = undefined;
}

ManipInfo::~ManipInfo () {
    delete sinfo_;
}

class Inserter : public GlyphVisitor {
public:
    Inserter(ManipList& m);
    Boolean visit(GlyphRef, GlyphOffsetRef);
private:
    ManipList* m_;
};

Inserter::Inserter (ManipList& m) {
    m_ = &m;
}

Boolean Inserter::visit (GlyphRef, GlyphOffsetRef go) {
    for (long i = 0; i < m_->count(); i++) {
        go->insert(m_->item(i));
    }
    return true;
}

//- Transformer*
//+ Transformer : Allocator
class Transformer : public Allocator {
public:
    ~Transformer();
    TypeObjId _tid();
    static Transformer* _narrow(BaseObjectRef);
//+
public:
    Transformer(Transformer*);
    Transformer();
    void request(Glyph::Requisition& r); //+ Glyph::request
    void extension(const Glyph::AllocationInfo& a, Region_in r); //+ Glyph::extension
    void traverse(GlyphTraversal_in t); //+ Glyph::traverse
    Transform_return transformation(); //+ Glyph::transformation
    void child_allocate(Glyph::AllocationInfo& a);

    Region_return nat_allocation();
    void need_resize();
    void set_allocation(Coord, Coord);
    void unset_allocation();
    Boolean allocation_set();
    
protected:
    TransformImpl tx_;
    Boolean set_;
};

//+ Transformer(GlyphImpl)
extern TypeObj_Descriptor _XfGlyphImpl_type;
TypeObj_Descriptor* _XfTransformer_parents[] = { &_XfGlyphImpl_type, nil };
extern TypeObjId _XfTransformer_tid;
TypeObj_Descriptor _XfTransformer_type = {
    /* type */ 0,
    /* id */ &_XfTransformer_tid,
    "Transformer",
    _XfTransformer_parents, /* offsets */ nil, /* excepts */ nil,
    /* methods */ nil, /* params */ nil,
    /* receive */ nil
};

Transformer* Transformer::_narrow(BaseObjectRef o) {
    return (Transformer*)_BaseObject_tnarrow(
        o, _XfTransformer_tid, 0
    );
}
TypeObjId Transformer::_tid() { return _XfTransformer_tid; }
//+

Transformer::Transformer (Transformer* t) {
    tx_.load(&t->tx_);
    set_ = t->set_;
    if (set_) {
        nat_.copy(_tmp(t->nat_allocation()));
        t->request(req_);
        requested_ = true;
    }
}
    
Transformer::Transformer () { 
    set_ = false;
}

Transformer::~Transformer () {}

void Transformer::need_resize () {
    if (!set_) {
        Allocator::need_resize();
    }
}

Boolean Transformer::allocation_set () {
    return set_;
}

Region_return Transformer::nat_allocation () {
    Fresco::ref(&nat_);
    return &nat_;
}

void Transformer::set_allocation (Coord w, Coord h) {
    set_ = true;
    update_requisition();
    nat_.lower_.x = -w*nat_.xalign_;
    nat_.lower_.y = -h*nat_.yalign_;
    nat_.upper_.x = nat_.lower_.x + w;
    nat_.upper_.y = nat_.lower_.y + h;
}

void Transformer::unset_allocation () {
    set_ = false;
    requested_ = false;
    update_requisition();
}

//+ Transformer(Glyph::extension)
void Transformer::extension(const Glyph::AllocationInfo& a, Region_in r) {
    if (set_) {
        RegionImpl region;
        region.copy(&nat_);
        region.apply_transform(&tx_);
        if (is_not_nil(a.transformation)) {
            region.apply_transform(a.transformation);
        }
        adjust_dotted_region(region);
    } else {
        Allocator::extension(a, r);
    }
}

//+ Transformer(Glyph::request)
void Transformer::request(Glyph::Requisition& r) {
    if (!requested_) {
        Allocator::request(r);
        flexible_transform_requisition(r, &tx_);
        req_ = r;
    } else {
        r = req_;
    }
}

//+ Transformer(Glyph::traverse)
void Transformer::traverse(GlyphTraversal_in t) {
    Painter_var p = t->current_painter();
    p->push_matrix();
    p->premultiply(&tx_);
    if (is_not_nil(t->allocation()) || set_) {
        RegionImpl* rr = new RegionImpl;
        if (set_) {
            rr->copy(&nat_);
            rr->apply_transform(&tx_);
        } else {
            rr->copy(_tmp(t->allocation()));
        }
        Glyph::Requisition r;
        MonoGlyph::request(r);
        adjust_allocation(rr, &tx_, &r);
        t->traverse_child(&offset_, rr);
        Fresco::unref(rr);
    } else {
        Allocator::traverse(t);
    }
    p->pop_matrix();
}

//+ Transformer(Glyph::transformation)
Transform_return Transformer::transformation() {
    Fresco::ref(&tx_);
    return &tx_;
}

void Transformer::child_allocate(Glyph::AllocationInfo& a) {
    if (is_nil(a.transformation)) {
        a.transformation = new TransformImpl;
    }
    a.transformation->premultiply(&tx_);
    if (is_not_nil(a.allocation) || set_) {
        RegionImpl rr;
        if (set_) {
            rr.copy(&nat_);
            rr.apply_transform(&tx_);
        } else {
            rr.copy(a.allocation);
        }
        Glyph::Requisition r;
        MonoGlyph::request(r);
        adjust_allocation(&rr, &tx_, &r);
        if (is_nil(a.allocation)) {
            a.allocation = new RegionImpl;
        }
        a.allocation->copy(&rr);
    } else {
        Allocator::child_allocate(a);
    }
}

//+ Manipulator(GlyphImpl)
extern TypeObj_Descriptor _XfGlyphImpl_type;
TypeObj_Descriptor* _XfManipulator_parents[] = { &_XfGlyphImpl_type, nil };
extern TypeObjId _XfManipulator_tid;
TypeObj_Descriptor _XfManipulator_type = {
    /* type */ 0,
    /* id */ &_XfManipulator_tid,
    "Manipulator",
    _XfManipulator_parents, /* offsets */ nil, /* excepts */ nil,
    /* methods */ nil, /* params */ nil,
    /* receive */ nil
};

Manipulator* Manipulator::_narrow(BaseObjectRef o) {
    return (Manipulator*)_BaseObject_tnarrow(
        o, _XfManipulator_tid, 0
    );
}
TypeObjId Manipulator::_tid() { return _XfManipulator_tid; }
//+

Manipulator::Manipulator (Manipulator* m) {
    selected_ = false;
    info_ = nil;
    if (is_not_nil(m)) {
        tx_.load(&m->tx_);
        selected_ = m->selected_;
        if (m->info_ != nil) {
            info_ = new ManipInfo(m->info_->sinfo_->copy());
        }
    }
}
    
Manipulator::~Manipulator () {
    delete info_;
}
    
void Manipulator::info (Manipulator* m, ManipInfo* i) {
    m->info_ = i;
}

ManipInfo* Manipulator::info (Manipulator* m) {
    return m->info_;
}

Boolean Manipulator::grasp (
    Tool& tool, FigViewer& fv, SelectInfo* si, EventRef e
) {
    Boolean ok = false;
    
    if (info_ == nil) {
        info_ = new ManipInfo(si);
    }
    if (is_not_nil(SelectTool::_narrow(&tool))) {
        info_->tooltype_ = choose;
    } else if (is_not_nil(CreateTool::_narrow(&tool))) {
        info_->tooltype_ = create;
    } else if (is_not_nil(MoveTool::_narrow(&tool))) {
        info_->tooltype_ = move;
    } else if (is_not_nil(ScaleTool::_narrow(&tool))) {
        info_->tooltype_ = scale;
    } else if (is_not_nil(RotateTool::_narrow(&tool))) {
        info_->tooltype_ = rotate;
    } else if (is_not_nil(ResizeTool::_narrow(&tool))) {
        info_->tooltype_ = resize;
    } else {
        info_->tooltype_ = undefined;
    }
    if (info_->tooltype_ != undefined) {
        ok = true;
        Glyph_var g = body();
        info_->obj_manip_ = e->modifier_is_down(Event::control);
        tool.anchor(info_->tool_ax_, info_->tool_ay_);
        info_->t_.load(&si->t_);
        if (info_->obj_manip_) {
            SelectCmd s;
            CmdVisitor cv(&s);
            cv.visit_parents(g);
        }
        if (info_->obj_manip_) {
            info_->t_.premultiply(&tx_);
        } 
        Vertex lower, upper;
        Glyph::AllocationInfo a;
        RegionImpl r;

        if (info_->tooltype_ == create) {
            info_->ax_ = e->pointer_x();
            info_->ay_ = e->pointer_y();
            info_->pt_.x = e->pointer_x();
            info_->pt_.y = e->pointer_y();
            info_->pt_.z = 0;
        } else {
            a.allocation = nil;
            a.transformation = new TransformImpl;
            a.transformation->load(&si->t_);
            a.damaged = nil;

            extension(a, &r);
            r.bounds(lower, upper);
            info_->ax_ = (lower.x+upper.x)/2.0;
            info_->ay_ = (lower.y+upper.y)/2.0;
            info_->pt_.x = e->pointer_x() + info_->ax_ - info_->tool_ax_;
            info_->pt_.y = e->pointer_y() + info_->ay_ - info_->tool_ay_;
            info_->pt_.z = 0;

            Fresco::unref(a.transformation);
        }
        info_->t_.invert();
        info_->t_.transform_vertex(info_->pt_);

        if (is_not_nil(info_->sinfo_->a_)) {
            r.copy(info_->sinfo_->a_);
            if (info_->obj_manip_) {
                TransformImpl t;
                t.load(&tx_);
                t.invert();
                r.apply_transform(&t);
            }
        } else {
            if (info_->obj_manip_) {
                local_extension(_tmp(body()), &r);
            } else {
                Boolean selected = selected_;
                selected_ = false;
                local_extension(this, &r);
                selected_ = selected;
            }
        }
        r.bounds(lower, upper);
        info_->center_.x = (lower.x+upper.x)/2.0;
        info_->center_.y = (lower.y+upper.y)/2.0;
        info_->center_.z = 0.0;

        Glyph::AllocationInfoSeq alist;
        if (info_->obj_manip_) {
            g->allocations(alist);
        } else {
            allocations(alist);
        }
        if (info_->tooltype_ == create) {
            Vertex delta;
 
            delta.x = info_->pt_.x - (lower.x+upper.x)/2.0;
            delta.y = info_->pt_.y - (lower.y+upper.y)/2.0;
            delta.z = 0.0;
 
            if (info_->obj_manip_) {
                glyph_need_redraw(g, alist, true, false);
                _translate(_tmp(g->transformation()), delta);
                glyph_need_redraw(g, alist, true, false);
                g->need_resize();
            } else {
                glyph_need_redraw(this, alist, false, false);
                _translate(&tx_, delta);
                glyph_need_redraw(this, alist, false, false);
                if (is_not_nil(info_->sinfo_->a_)) {
                    compose(this);
                }
                need_resize();
            }
            info_->center_.x += delta.x;
            info_->center_.y += delta.y;
 
        } else if (info_->tooltype_ == choose) {
            SelectCmd selectcmd(&fv);
            execute(&selectcmd);

        } else if (info_->tooltype_ == scale) {
            Vertex scaler;
            scaler.x = Math::abs(
                (info_->pt_.x-info_->center_.x)*2.0/(upper.x-lower.x)
            );
            scaler.y = Math::abs(
                (info_->pt_.y-info_->center_.y)*2.0/(upper.y-lower.y)
            );
            scaler.z = 1.0;

            if (info_->obj_manip_) {
                glyph_need_redraw(g, alist, true, false);
                _scale(_tmp(g->transformation()), info_->center_, scaler);
                glyph_need_redraw(g, alist, true, false);
                g->need_resize();
            } else {
                glyph_need_redraw(this, alist, false, false);
                _scale(&tx_, info_->center_, scaler);
                glyph_need_redraw(this, alist, false, false);
                if (is_not_nil(info_->sinfo_->a_)) {
                    compose(this);
                }
                need_resize();
            }
        }
        delete_info_list(alist);
    }
    return ok;
}

Boolean Manipulator::manipulate (EventRef e) {
    switch (e->type()) {
    case Event::motion:
    case Event::enter:
    case Event::leave:
	break;
    case Event::focus_in:
    case Event::focus_out:
	return true;
    default:
	return false;
    }

    Vertex pt;
    pt.x = e->pointer_x() + info_->ax_ - info_->tool_ax_;
    pt.y = e->pointer_y() + info_->ay_ - info_->tool_ay_;
    pt.z = 0;
    info_->t_.transform_vertex(pt);

    Glyph_var g = body();
    Glyph::AllocationInfoSeq a;
    if (info_->obj_manip_) {
        g->allocations(a);
    } else {
        allocations(a);
    }
    switch(info_->tooltype_) {
    case create:
    case move:
        {
            Vertex orig = pt;

            pt.x -= info_->pt_.x;
            pt.y -= info_->pt_.y;

            if (info_->obj_manip_) {
                glyph_need_redraw(g, a, true, false);
                _translate(_tmp(g->transformation()), pt);
                glyph_need_redraw(g, a, true, false);
                g->need_resize();
            } else {
                glyph_need_redraw(this, a, false, false);
                _translate(&tx_, pt);
                glyph_need_redraw(this, a, false, false);
                if (is_not_nil(info_->sinfo_->a_)) {
                    compose(this);
                }
                need_resize();
            }
            info_->center_.x += pt.x;
            info_->center_.y += pt.y;
            info_->pt_ = orig;
        }
        break;
    case rotate:
        {
            double pi = 3.14159;
            double den = pt.x-info_->center_.x;
            if (Math::abs(den) < tol) {
                return true;
            }
            double cur = atan((pt.y-info_->center_.y)/den)/pi*180.0;
            float last = atan(
                (info_->pt_.y-info_->center_.y)/
                (info_->pt_.x-info_->center_.x)
            )/pi*180.0;
            if ((pt.x-info_->center_.x) < 0.0) {
                cur += 180.0;
            }
            if ((info_->pt_.x-info_->center_.x) < 0.0) {
                last += 180.0;
            }
            if (info_->obj_manip_) { 
                glyph_need_redraw(g, a, true, false);
                _rotate(
                    _tmp(g->transformation()), cur-last, Z_axis, info_->center_
                );
                glyph_need_redraw(g, a, true, false);
                g->need_resize();
            } else {
                glyph_need_redraw(this, a, false, false);
                _rotate(&tx_, cur-last, Z_axis, info_->center_);
                glyph_need_redraw(this, a, false, false);
                if (is_not_nil(info_->sinfo_->a_)) {
                    compose(this);
                }
                need_resize();
            }
            info_->pt_ = pt;
        }
        break;
    case scale:
        {
            Vertex scaler;
            double den_x = info_->pt_.x-info_->center_.x;
            double den_y = info_->pt_.y-info_->center_.y;
            double num_x = pt.x-info_->center_.x;
            double num_y = pt.y-info_->center_.y;
            if (Math::abs(num_x) < tol || Math::abs(num_y) < tol) {
                return true;
            }
            scaler.x = num_x/den_x;
            scaler.y = num_y/den_y;
            scaler.z = 1.00;

            if (info_->obj_manip_) {
                glyph_need_redraw(g, a, true, false);
                _scale(_tmp(g->transformation()), info_->center_, scaler);
                glyph_need_redraw(g, a, true, false);
                g->need_resize();
            } else {
                glyph_need_redraw(this, a, false, false);
                _scale(&tx_, info_->center_, scaler);
                glyph_need_redraw(this, a, false, false);
                if (is_not_nil(info_->sinfo_->a_)) {
                    compose(this);
                }
                need_resize();
            }
            info_->pt_ = pt;
        }
        break;
    case choose:
        break;
    default:
        break;
    }
    delete_info_list(a);
    return true;
}

Command* Manipulator::effect (EventRef) {
    if (info_->obj_manip_) {
        selected_ = false;
        Glyph_var g = body();
        SelectCmd s(nil, false);
        CmdVisitor cv(&s);
        cv.visit_parents(g);
        selected_ = true;
    }
    return nil;  // commands for undo and redo not supported yet
}

void Manipulator::draw_handles (GlyphTraversal_in gt) {
    if (selected_) {
        selected_ = false; // turn this off momentarily since it may affect
                           // bounding box computation
        Painter_var po = gt->current_painter();
        RegionImpl bbox;

        Glyph::AllocationInfo ga;
        ga.transformation = new TransformImpl;
        ga.transformation->load(_tmp(po->current_matrix()));
        if (is_not_nil(_tmp(gt->allocation()))) {
            ga.allocation = new RegionImpl;
            ga.allocation->copy(_tmp(gt->allocation()));
        } else {
            ga.allocation = nil;
        }
        ga.damaged = nil;
        extension(ga, &bbox);
        Fresco::unref(ga.transformation);
        Fresco::unref(ga.allocation);
        draw_handle_box(
            po, bbox.lower_.x, bbox.lower_.y, bbox.upper_.x, bbox.upper_.y
        );
        selected_ = true;
    }
}

void Manipulator::execute (Command* cmd) {
    FigViewer* v = cmd->figviewer();
    SelectCmd* selectcmd = SelectCmd::_narrow(cmd);
    if (is_not_nil(selectcmd)) {
        Boolean s = selectcmd->selected();
        if (s != selected_) {
            Glyph::AllocationInfoSeq a;
            allocations(a);
            selected_ = true;
            glyph_need_redraw(this, a, false, true);
            selected_ = s;
            delete_info_list(a);
            if (!selected_) {
                delete info_;
                info_ = nil;
            }
        }
        if (is_not_nil((GlyphRef)v)) {
            if (selected_) {
                v->selection()->add(this);
            } else {
                v->selection()->remove(this);
            }
        }
        return;
    }
    UngroupCmd* ungroupcmd = UngroupCmd::_narrow(cmd);
    if (is_not_nil(ungroupcmd)) {
        Glyph_var g = body();
        Counter counter;
        counter.visit_children(g);
        if (counter.count() == 0) {
            return;
        }

        TransformImpl t;
        t.load(&tx_);
        t.premultiply(_tmp(g->transformation()));
        ManipCopier mc;
        mc.visit_children(g);

        ManipList* ml = mc.manipulators();
        for (long i = 0; i < ml->count(); i++) {
            Manipulator* man = ml->item(i);
            Transform_var tr = man->transformation();
            tr->postmultiply(&t);
        }
        Inserter inserter(*ml);
        inserter.visit_parents(this);
        SelectCmd selectcmd(v);
        for (i = 0; i < ml->count(); i++) {
            Manipulator* man = ml->item(i);
            man->execute(&selectcmd);
            man->info_ = new ManipInfo(info_->sinfo_->copy());
        }
        SelectCmd unselectcmd(v, false);
        execute(&unselectcmd);
        Remover remover;
        remover.visit_parents(this);
        return;
    }
    DeleteCmd* deletecmd = DeleteCmd::_narrow(cmd);
    if (is_not_nil(deletecmd)) {
        Glyph_var g = body();
        if (info_ != nil && info_->obj_manip_) {
            delete info_;
            info_ = nil;
            CmdVisitor cv(deletecmd);
            cv.visit_parents(g);
        } else {
            SelectCmd unselectcmd(v, false);
            execute(&unselectcmd);
            Remover remover;
            remover.visit_parents(this);
        }
        return;
    }   
    InstanceCmd* instancecmd = InstanceCmd::_narrow(cmd);
    CopyCmd* copycmd = CopyCmd::_narrow(cmd);
    if (is_not_nil(instancecmd) || is_not_nil(copycmd)) {
        Manipulator* man;
        if (is_not_nil(instancecmd)) {
            man = shallow_copy();
        } else {
            CopyCmd::glyphmap_->clear();
            man = deep_copy();
        }
        SelectCmd unselectcmd(v, false);
        execute(&unselectcmd);

        Transform_var tr = man->transformation();

        Vertex delta;
        delta.x = 4.0;
        delta.y = 4.0;
        delta.z = 0.0;

        tr->translate(delta);
        Appender app(man);
        app.visit_parents(this);

        SelectCmd selectcmd(v);
        man->execute(&selectcmd);
	man->need_redraw();
        return;
    }
    NarrowCmd* narrowcmd = NarrowCmd::_narrow(cmd);
    if (is_not_nil(narrowcmd)) {
        /* not implemented */
        return;
    }
    SelectInfoCmd* scmd = SelectInfoCmd::_narrow(cmd);
    if (is_not_nil(scmd)) {
        scmd->select_info(info_->sinfo_);
        return;
    }
    GroupCmd* groupcmd = GroupCmd::_narrow(cmd);
    if (is_not_nil(groupcmd)) {
        Selection* sel = v->selection()->copy();
        long n = sel->count();
        if (n < 2) {
            delete sel;
            return;
        }
        PolyFigure* pf = new PolyFigure;
        Manipulator* pm = new PolyManip(pf);
        Fresco::unref(pf);

        SelectCmd unselectcmd(v, false);
        SelectCmd selectcmd(v);
        pm->execute(&selectcmd);

        Manipulator* m0 = sel->item(0);
        pm->info_ = new ManipInfo(m0->info_->sinfo_->copy());
        OffsetVisitor ov;
        ov.visit_parents(m0);

        Remover* remover = new Remover;
        for (long i = 0; i < sel->count(); i++) {
            Manipulator* m = sel->item(i);
            m->execute(&unselectcmd);
            remover->visit_parents(m);
            pf->append(m);
        }
        delete remover;
        for (long j = 0; j < ov.offset_count(); j++) {
            ov.offset(j)->parent()->append(pm);
        }
        pm->need_resize();
        return;
    }
}

void Manipulator::unexecute (Command*) {}

//+ Manipulator(Glyph::draw)
void Manipulator::draw(GlyphTraversal_in t) {
    draw_handles(t);
}

//+ Manipulator(Glyph::traverse)
void Manipulator::traverse(GlyphTraversal_in t) {
    Painter_var p = t->current_painter();
    if (is_not_nil(p)) {
        if (tx_.identity()) {
            MonoGlyph::traverse(t);
        } else {
            p->push_matrix();
            p->premultiply(&tx_);
            MonoGlyph::traverse(t);
            p->pop_matrix();
        }
    }
    if (t->op() == GlyphTraversal::draw) {
        draw(t);
    }
}

//+ Manipulator(Glyph::extension)
void Manipulator::extension(const Glyph::AllocationInfo& a, Region_in r) {
    MonoGlyph::extension(a, r);
    if (selected_) {
        RegionImpl region;
        region.copy(r);
        adjust_dotted_region(region);
        r->copy(&region);
    }
}

//+ Manipulator(Glyph::request)
void Manipulator::request(Glyph::Requisition& r) {
    MonoGlyph::request(r);
    fixed_transform_requisition(r, &tx_);
}

//+ Manipulator(Glyph::transformation)
Transform_return Manipulator::transformation() {
    Fresco::ref(&tx_);
    return &tx_;
}

void Manipulator::child_allocate(Glyph::AllocationInfo& a) {
    if (is_nil(a.transformation)) {
	a.transformation = new TransformImpl;
    }
    a.transformation->premultiply(&tx_);
}

//+ FigureManip(Manipulator)
extern TypeObj_Descriptor _XfManipulator_type;
TypeObj_Descriptor* _XfFigureManip_parents[] = { &_XfManipulator_type, nil };
extern TypeObjId _XfFigureManip_tid;
TypeObj_Descriptor _XfFigureManip_type = {
    /* type */ 0,
    /* id */ &_XfFigureManip_tid,
    "FigureManip",
    _XfFigureManip_parents, /* offsets */ nil, /* excepts */ nil,
    /* methods */ nil, /* params */ nil,
    /* receive */ nil
};

FigureManip* FigureManip::_narrow(BaseObjectRef o) {
    return (FigureManip*)_BaseObject_tnarrow(
        o, _XfFigureManip_tid, 0
    );
}
TypeObjId FigureManip::_tid() { return _XfFigureManip_tid; }
//+

FigureManip::FigureManip (FigureManip* m) : Manipulator (m) {
    figure_ = nil;
    if (is_not_nil(m) && is_not_nil(m->figure_)) {
        figure_body(m->figure_);
    }
}

void FigureManip::figure_body (Figure* f) {
    Fresco::unref(figure_);
    figure_ = f;
    Fresco::ref(figure_);
    if (is_not_nil(figure_)) {
        body(figure_);
    }
}

FigureManip::~FigureManip () {
    Fresco::unref(figure_);
}

Boolean FigureManip::grasp (
    Tool& tool, FigViewer& fv, SelectInfo* si, EventRef e
) {
    Boolean ok = Manipulator::grasp(tool, fv, si, e);
    if (info_->tooltype_ == create) {
        TransformRef tr = &info_->sinfo_->t_;
        info_->t_.load(tr);
        info_->t_.premultiply(&tx_);
        Glyph_var g = body();
        info_->t_.premultiply(_tmp(g->transformation()));
        info_->t_.invert();

        info_->ax_ = e->pointer_x();
        info_->ay_ = e->pointer_y();
        info_->pt_.x = e->pointer_x();
        info_->pt_.y = e->pointer_y();
        info_->pt_.z = 0;
        info_->t_.transform_vertex(info_->pt_);
    }
    return ok;
}

//+ VertexManip(FigureManip)
extern TypeObj_Descriptor _XfFigureManip_type;
TypeObj_Descriptor* _XfVertexManip_parents[] = { &_XfFigureManip_type, nil };
extern TypeObjId _XfVertexManip_tid;
TypeObj_Descriptor _XfVertexManip_type = {
    /* type */ 0,
    /* id */ &_XfVertexManip_tid,
    "VertexManip",
    _XfVertexManip_parents, /* offsets */ nil, /* excepts */ nil,
    /* methods */ nil, /* params */ nil,
    /* receive */ nil
};

VertexManip* VertexManip::_narrow(BaseObjectRef o) {
    return (VertexManip*)_BaseObject_tnarrow(
        o, _XfVertexManip_tid, 0
    );
}
TypeObjId VertexManip::_tid() { return _XfVertexManip_tid; }
//+

VertexManip::VertexManip (VertexManip* m) : FigureManip (m) {
    vlist_ = new VertexList;
    if (is_not_nil(m)) {
        for (long i = 0; i < m->vlist_->count(); i++) {
            vlist_->append(m->vlist_->item(i));
        }
        vmin_ = m->vmin_;
        vmax_ = m->vmax_;
    }
}

VertexManip::~VertexManip () { delete vlist_; }

//+ VertexManip(Glyph::extension)
void VertexManip::extension(const Glyph::AllocationInfo& a, Region_in r) {
    if (selected_ && vlist_->count() > 0) {
        TransformImpl t;
        RegionImpl region;
        if (is_not_nil(a.transformation)) {
            t.load(a.transformation);
        }
        t.premultiply(&tx_);
        if (is_not_nil(_tmp(figure_->transformation()))) {
            t.premultiply(_tmp(figure_->transformation()));
        }
        region.defined_ = true;
	region.lower_ = vmin_;
	region.upper_ = vmax_;
	region.xalign_ = region.yalign_ = region.zalign_ = 0;
	region.apply_transform(&t);

        adjust_dotted_region(region);
        r->merge_union(&region);
    } else {
        Manipulator::extension(a, r);
    }
}

Boolean VertexManip::grasp (
    Tool& tool, FigViewer& fv, SelectInfo* si, EventRef e
) {
    Boolean ok = FigureManip::grasp(tool, fv, si, e);
    if (info_->tooltype_ == create) {
        vlist_->remove_all();
        vlist_->append(info_->pt_);
        vlist_->append(info_->pt_);
    }
    return ok;
}

Boolean VertexManip::manipulate (EventRef e) {
    if (e->type() == Event::down && e->pointer_button() == 2) {
        return false;
    }
    Boolean ok = true;
    if (info_->tooltype_ == create) {
        Vertex pt;
        Glyph::AllocationInfoSeq a;
        allocations(a);
        glyph_need_redraw(this, a, false, false);

        pt.x = e->pointer_x(); pt.y = e->pointer_y(); pt.z = 0;
        info_->t_.transform_vertex(pt);
        if (e->type() == Event::motion) {
            Vertex& last = vlist_->item_ref(vlist_->count()-1);
            last = pt;
        } else if (e->type() == Event::down) {
            if (e->pointer_button() == 1) {
                vlist_->append(pt);
            } else if (e->pointer_button() == 3) {
                if (vlist_->count() > 2) {
                    vlist_->remove(vlist_->count()-1);
                    Vertex& last = vlist_->item_ref(vlist_->count()-1);
                    last = pt;
                }
            }
        }
        recompute_shape();
        info_->pt_ = pt;

        glyph_need_redraw(this, a, false, false);
        delete_info_list(a);
        if (is_not_nil(info_->sinfo_->a_)) {
            compose(this);
        }
        need_resize();
    } else {
        ok = Manipulator::manipulate(e);
    }
    return ok;
}

void VertexManip::draw_handles (GlyphTraversal_in gt) {
    if (selected_) {
        Painter_var po = gt->current_painter();
        if (is_nil(black)) {
            black = new ColorImpl(0.0, 0.0, 0.0);
        }
        po->current_color(black);
        Transform_var tx = figure_->transformation();
        po->push_matrix();
        po->premultiply(&tx_);
        po->premultiply(tx);
        TransformImpl ti;
        Transform_var cm = po->current_matrix();
        ti.load(cm);
        cm->load_identity();

        for (long i = 0; i < vlist_->count(); i++) {
            Vertex vtex = vlist_->item(i);
            ti.transform_vertex(vtex);
            po->fill_rect(vtex.x-dot/2,vtex.y-dot/2,vtex.x+dot/2,vtex.y+dot/2);
        }
        po->pop_matrix();
    }
}

void VertexManip::recompute_shape () {
    figure_->reset();
    vmin_.x = MAXFLOAT;
    vmin_.y = MAXFLOAT;
    vmax_.x = -MAXFLOAT;
    vmax_.y = -MAXFLOAT;
    for (long i = 0; i < vlist_->count(); i++) {
        figure_->add_point(vlist_->item(i).x, vlist_->item(i).y);
        vmin_.x = Math::min(vmin_.x, vlist_->item(i).x);
        vmax_.x = Math::max(vmax_.x, vlist_->item(i).x);
        vmin_.y = Math::min(vmin_.y, vlist_->item(i).y);
        vmax_.y = Math::max(vmax_.y, vlist_->item(i).y);
    }
}

//+ LineManip(FigureManip)
extern TypeObj_Descriptor _XfFigureManip_type;
TypeObj_Descriptor* _XfLineManip_parents[] = { &_XfFigureManip_type, nil };
extern TypeObjId _XfLineManip_tid;
TypeObj_Descriptor _XfLineManip_type = {
    /* type */ 0,
    /* id */ &_XfLineManip_tid,
    "LineManip",
    _XfLineManip_parents, /* offsets */ nil, /* excepts */ nil,
    /* methods */ nil, /* params */ nil,
    /* receive */ nil
};

LineManip* LineManip::_narrow(BaseObjectRef o) {
    return (LineManip*)_BaseObject_tnarrow(
        o, _XfLineManip_tid, 0
    );
}
TypeObjId LineManip::_tid() { return _XfLineManip_tid; }
//+

LineManip::LineManip (LineManip* m) : VertexManip (m) {
    if (is_nil(m) || is_nil(m->figure_)) {
        FigureKit_var figurekit = fresco_instance()->figure_kit();
        Figure* f = new Figure(
            FigureKit::stroke, _tmp(figurekit->default_style()), false, false, 1
        );
        f->add_point(0, 0);
        figure_body(f);
    }
}

LineManip::~LineManip () {}

Manipulator* LineManip::shallow_copy () {
    LineManip* manip = new LineManip(this);
    return manip;
}

Manipulator* LineManip::deep_copy () {
    Figure* copy = (Figure*)CopyCmd::glyphmap_->find(figure_);
    if (is_nil(copy)) {
        Figure* f = new Figure(figure_);
        CopyCmd::glyphmap_->map(figure_, f);
        LineManip* manip =  new LineManip(this);
        manip->figure_body(f);
        Fresco::unref(f);
        return manip;
    } else {
        LineManip* manip = new LineManip(this);
        manip->figure_body(copy);
        return manip;
    }
}

Boolean LineManip::manipulate (EventRef e) {
    switch (e->type()) {
    case Event::motion:
    case Event::enter:
    case Event::leave:
    case Event::up:
	break;
    case Event::focus_in:
    case Event::focus_out:
	return true;
    default:
	return false;
    }
    Boolean ok = true;
    if (info_->tooltype_ == create) {
        Vertex pt;
        Glyph::AllocationInfoSeq a;
        allocations(a);
        glyph_need_redraw(this, a, false, false);

        pt.x = e->pointer_x(); pt.y = e->pointer_y(); pt.z = 0;
        info_->t_.transform_vertex(pt);
        Vertex& last = vlist_->item_ref(1);
        last = pt;
        if (e->type() == Event::up) {
            ok = false;
        }
        recompute_shape();
        info_->pt_ = pt;

        glyph_need_redraw(this, a, false, false);
        delete_info_list(a);
        if (is_not_nil(info_->sinfo_->a_)) {
            compose(this);
        }
        need_resize();
    } else {
        ok = Manipulator::manipulate(e);
    }
    return ok;
}

//+ RectManip(FigureManip)
extern TypeObj_Descriptor _XfFigureManip_type;
TypeObj_Descriptor* _XfRectManip_parents[] = { &_XfFigureManip_type, nil };
extern TypeObjId _XfRectManip_tid;
TypeObj_Descriptor _XfRectManip_type = {
    /* type */ 0,
    /* id */ &_XfRectManip_tid,
    "RectManip",
    _XfRectManip_parents, /* offsets */ nil, /* excepts */ nil,
    /* methods */ nil, /* params */ nil,
    /* receive */ nil
};

RectManip* RectManip::_narrow(BaseObjectRef o) {
    return (RectManip*)_BaseObject_tnarrow(
        o, _XfRectManip_tid, 0
    );
}
TypeObjId RectManip::_tid() { return _XfRectManip_tid; }
//+

RectManip::RectManip (RectManip* m) : VertexManip (m) {
    if (is_nil(m) || is_nil(m->figure_)) {
        FigureKit_var figurekit = fresco_instance()->figure_kit();
        Figure* f = new Figure(
            FigureKit::stroke, figurekit->default_style(), true, false, 1
        );
        f->add_point(0, 0);
        figure_body(f);
    }
}

RectManip::~RectManip () {}

Manipulator* RectManip::shallow_copy () {
    RectManip* manip = new RectManip(this);
    return manip;
}

Manipulator* RectManip::deep_copy () {
    Figure* copy = (Figure*)CopyCmd::glyphmap_->find(figure_);
    if (is_nil(copy)) {
        Figure* f = new Figure(figure_);
        CopyCmd::glyphmap_->map(figure_, f);
        RectManip* manip = new RectManip(this);
        manip->figure_body(f);
        Fresco::unref(f);
        return manip;
    } else {
        RectManip* manip = new RectManip(this);
        manip->figure_body(copy);
        return manip;
    }
}

Boolean RectManip::manipulate (EventRef e) {
    switch (e->type()) {
    case Event::motion:
    case Event::enter:
    case Event::leave:
    case Event::up:
	break;
    case Event::focus_in:
    case Event::focus_out:
	return true;
    default:
	return false;
    }
    Boolean ok = true;
    if (info_->tooltype_ == create) {
        Vertex pt, orig;
        Glyph::AllocationInfoSeq a;
        allocations(a);
        glyph_need_redraw(this, a, false, false);

        pt.x = e->pointer_x(); pt.y = e->pointer_y(); pt.z = 0;
        info_->t_.transform_vertex(pt);
        orig = vlist_->item(0);
        vlist_->remove_all();
        if (e->modifier_is_down(Event::shift)) {
            if (Math::abs(pt.x-orig.x) > Math::abs(pt.y-orig.y)) {
                if (
                    (pt.y-orig.y) > 0 && (pt.x-orig.x) < 0 ||
                    (pt.y-orig.y) < 0 && (pt.x-orig.x) > 0
                ) {
                    pt.y = -pt.x;
                } else {
                    pt.y = pt.x;
                }
            } else {
                if (
                    (pt.x-orig.x) > 0 && (pt.y-orig.y) < 0 ||
                    (pt.x-orig.x) < 0 && (pt.y-orig.y) > 0
                ) {
                    pt.x = -pt.y;
                } else {
                    pt.x = pt.y;
                }
            }
        }
        Vertex v = orig;
        vlist_->append(v);
        v.y = pt.y;
        vlist_->append(v);
        v.x = pt.x;
        vlist_->append(v);
        v.y = orig.y;
        vlist_->append(v);
        if (e->type() == Event::up) {
            ok = false;
        }
        recompute_shape();
        info_->pt_ = pt;

        glyph_need_redraw(this, a, false, false);
        delete_info_list(a);
        if (is_not_nil(info_->sinfo_->a_)) {
            compose(this);
        }
        need_resize();
    } else {
        ok = Manipulator::manipulate(e);
    }
    return ok;
}

//+ EllipseManip(FigureManip)
extern TypeObj_Descriptor _XfFigureManip_type;
TypeObj_Descriptor* _XfEllipseManip_parents[] = { &_XfFigureManip_type, nil };
extern TypeObjId _XfEllipseManip_tid;
TypeObj_Descriptor _XfEllipseManip_type = {
    /* type */ 0,
    /* id */ &_XfEllipseManip_tid,
    "EllipseManip",
    _XfEllipseManip_parents, /* offsets */ nil, /* excepts */ nil,
    /* methods */ nil, /* params */ nil,
    /* receive */ nil
};

EllipseManip* EllipseManip::_narrow(BaseObjectRef o) {
    return (EllipseManip*)_BaseObject_tnarrow(
        o, _XfEllipseManip_tid, 0
    );
}
TypeObjId EllipseManip::_tid() { return _XfEllipseManip_tid; }
//+

EllipseManip::EllipseManip (EllipseManip* m) : VertexManip (m) {
    if (is_nil(m) || is_nil(m->figure_)) {
        FigureKit_var figurekit = fresco_instance()->figure_kit();
        Figure* f = new Figure(
            FigureKit::stroke, figurekit->default_style(), true, true, 1
        );
        f->add_point(0, 0);
        figure_body(f);
    }
}

EllipseManip::~EllipseManip () {}

//+ EllipseManip(Glyph::extension)
void EllipseManip::extension(const Glyph::AllocationInfo& a, Region_in r) {
    Manipulator::extension(a, r);
}

Manipulator* EllipseManip::shallow_copy () {
    EllipseManip* manip = new EllipseManip(this);
    return manip;
}

Manipulator* EllipseManip::deep_copy () {
    Figure* copy = (Figure*)CopyCmd::glyphmap_->find(figure_);
    if (is_nil(copy)) {
        Figure* f = new Figure(figure_);
        CopyCmd::glyphmap_->map(figure_, f);
        EllipseManip* manip = new EllipseManip(this);
        manip->figure_body(f);
        Fresco::unref(f);
        return manip;
    } else {
        EllipseManip* manip = new EllipseManip(this);
        manip->figure_body(copy);
        return manip;
    }
}


static const float p0 = 1.00000000;
static const float p1 = 0.89657547;   // cos 30 * sqrt(1 + tan 15 * tan 15)
static const float p2 = 0.70710678;   // cos 45
static const float p3 = 0.51763809;   // cos 60 * sqrt(1 + tan 15 * tan 15)
static const float p4 = 0.26794919;   // tan 15

Boolean EllipseManip::manipulate (EventRef e) {
    switch (e->type()) {
    case Event::motion:
    case Event::enter:
    case Event::leave:
    case Event::up:
	break;
    case Event::focus_in:
    case Event::focus_out:
	return true;
    default:
	return false;
    }
    Boolean ok = true;
    if (info_->tooltype_ == create) {
        Vertex pt;
        Glyph::AllocationInfoSeq a;
        allocations(a);
        glyph_need_redraw(this, a, false, false);

        pt.x = e->pointer_x(); pt.y = e->pointer_y(); pt.z = 0;
        info_->t_.transform_vertex(pt);
        Vertex& last = vlist_->item_ref(1);
        Vertex& orig = vlist_->item_ref(0);
        if (e->type() == Event::up) {
            ok = false;
        }
        Coord r1 = Math::abs(pt.x - orig.x);
        Coord r2 = Math::abs(pt.y - orig.y);
        if (e->modifier_is_down(Event::shift)) {
            if (r1 > r2) {
                r2 = r1;
            } else {
                r1 = r2;
            }
            pt.x = r1 + orig.x;
            pt.y = r2 + orig.y;
        }
        last = pt;
        recompute_shape();
        info_->pt_ = pt;

        glyph_need_redraw(this, a, false, false);
        delete_info_list(a);
        if (is_not_nil(info_->sinfo_->a_)) {
            compose(this);
        }
        need_resize();
    } else {
        ok = Manipulator::manipulate(e);
    }
    return ok;
}

void EllipseManip::recompute_shape () {
    figure_->reset();

    Vertex& pt = vlist_->item_ref(1);
    Vertex& orig = vlist_->item_ref(0);

    Coord r1 = pt.x - orig.x;
    Coord r2 = pt.y - orig.y;

    float px0 = p0 * r1, py0 = p0 * r2;
    float px1 = p1 * r1, py1 = p1 * r2;
    float px2 = p2 * r1, py2 = p2 * r2;
    float px3 = p3 * r1, py3 = p3 * r2;
    float px4 = p4 * r1, py4 = p4 * r2;
    
    figure_->add_point(orig.x + r1, orig.y);
    figure_->add_curve(
        orig.x + px2, orig.y + py2, orig.x + px0, 
        orig.y + py4, orig.x + px1, orig.y + py3
    );
    figure_->add_curve(
        orig.x, orig.y + r2, orig.x + px3, 
        orig.y + py1, orig.x + px4, orig.y + py0
    );
    figure_->add_curve(
        orig.x - px2, orig.y + py2, orig.x - px4, 
        orig.y + py0, orig.x - px3, orig.y + py1
    );
    figure_->add_curve(
        orig.x - r1, orig.y, orig.x - px1, 
        orig.y + py3, orig.x - px0, orig.y + py4
    );
    figure_->add_curve(
        orig.x - px2, orig.y - py2, orig.x - px0, 
        orig.y - py4, orig.x - px1, orig.y - py3
    );
    figure_->add_curve(
        orig.x, orig.y - r2, 
        orig.x - px3, orig.y - py1, orig.x - px4, orig.y - py0
    );
    figure_->add_curve(
        orig.x + px2, orig.y - py2, orig.x + px4,
        orig.y - py0, orig.x + px3, orig.y - py1
    );
    figure_->add_curve(
        orig.x + r1, orig.y, orig.x + px1, orig.y - py3, 
        orig.x + px0, orig.y - py4
    );
}

void EllipseManip::draw_handles (GlyphTraversal_in gt) {
    Manipulator::draw_handles(gt);
}

//+ Open_BSplineManip(VertexManip)
extern TypeObj_Descriptor _XfVertexManip_type;
TypeObj_Descriptor* _XfOpen_BSplineManip_parents[] = { &_XfVertexManip_type, nil };
extern TypeObjId _XfOpen_BSplineManip_tid;
TypeObj_Descriptor _XfOpen_BSplineManip_type = {
    /* type */ 0,
    /* id */ &_XfOpen_BSplineManip_tid,
    "Open_BSplineManip",
    _XfOpen_BSplineManip_parents, /* offsets */ nil, /* excepts */ nil,
    /* methods */ nil, /* params */ nil,
    /* receive */ nil
};

Open_BSplineManip* Open_BSplineManip::_narrow(BaseObjectRef o) {
    return (Open_BSplineManip*)_BaseObject_tnarrow(
        o, _XfOpen_BSplineManip_tid, 0
    );
}
TypeObjId Open_BSplineManip::_tid() { return _XfOpen_BSplineManip_tid; }
//+

Open_BSplineManip::Open_BSplineManip (Open_BSplineManip* m) : VertexManip (m) {
    if (is_nil(m) || is_nil(m->figure_)) {
        FigureKit_var figurekit = fresco_instance()->figure_kit();
        Figure* f = new Figure(
            FigureKit::stroke, figurekit->default_style(), false, true, 1
        );
        f->add_point(0, 0);
        figure_body(f);
    }
}

Open_BSplineManip::~Open_BSplineManip () {}

Manipulator* Open_BSplineManip::shallow_copy () {
    Open_BSplineManip* manip = new Open_BSplineManip(this);
    return manip;
}

Manipulator* Open_BSplineManip::deep_copy () {
    Figure* copy = (Figure*)CopyCmd::glyphmap_->find(figure_);
    if (is_nil(copy)) {
        Figure* f = new Figure(figure_);
        CopyCmd::glyphmap_->map(figure_, f);
        Open_BSplineManip* manip = new Open_BSplineManip(this);
        manip->figure_body(f);
        Fresco::unref(f);
        return manip;
    } else {
        Open_BSplineManip* manip = new Open_BSplineManip(this);
        manip->figure_body(copy);
        return manip;
    }
}

void Open_BSplineManip::recompute_shape () {
    long n = vlist_->count();
    figure_->reset();
    figure_->Bspline_move_to(
        vlist_->item(0).x, vlist_->item(0).y, 
        vlist_->item(0).x, vlist_->item(0).y, 
        vlist_->item(0).x, vlist_->item(0).y
    );
    figure_->Bspline_curve_to(
        vlist_->item(0).x, vlist_->item(0).y, 
        vlist_->item(0).x, vlist_->item(0).y, 
        vlist_->item(1).x, vlist_->item(1).y
    );
    for (long i = 1; i < n - 1; ++i) {
        figure_->Bspline_curve_to(
            vlist_->item(i).x, vlist_->item(i).y, 
            vlist_->item(i-1).x, vlist_->item(i-1).y, 
            vlist_->item(i+1).x, vlist_->item(i+1).y
        );
    }
    figure_->Bspline_curve_to(
        vlist_->item(n-1).x, vlist_->item(n-1).y, 
        vlist_->item(n-2).x, vlist_->item(n-2).y, 
        vlist_->item(n-1).x, vlist_->item(n-1).y
    );
    figure_->Bspline_curve_to(
        vlist_->item(n-1).x, vlist_->item(n-1).y, 
        vlist_->item(n-1).x, vlist_->item(n-1).y, 
        vlist_->item(n-1).x, vlist_->item(n-1).y
    );
    vmin_.x = MAXFLOAT;
    vmin_.y = MAXFLOAT;
    vmax_.x = -MAXFLOAT;
    vmax_.y = -MAXFLOAT;
    for (long j = 0; j < vlist_->count(); j++) {
        vmin_.x = Math::min(vmin_.x, vlist_->item(j).x);
        vmax_.x = Math::max(vmax_.x, vlist_->item(j).x);
        vmin_.y = Math::min(vmin_.y, vlist_->item(j).y);
        vmax_.y = Math::max(vmax_.y, vlist_->item(j).y);
    }
}

//+ Closed_BSplineManip(VertexManip)
extern TypeObj_Descriptor _XfVertexManip_type;
TypeObj_Descriptor* _XfClosed_BSplineManip_parents[] = { &_XfVertexManip_type, nil };
extern TypeObjId _XfClosed_BSplineManip_tid;
TypeObj_Descriptor _XfClosed_BSplineManip_type = {
    /* type */ 0,
    /* id */ &_XfClosed_BSplineManip_tid,
    "Closed_BSplineManip",
    _XfClosed_BSplineManip_parents, /* offsets */ nil, /* excepts */ nil,
    /* methods */ nil, /* params */ nil,
    /* receive */ nil
};

Closed_BSplineManip* Closed_BSplineManip::_narrow(BaseObjectRef o) {
    return (Closed_BSplineManip*)_BaseObject_tnarrow(
        o, _XfClosed_BSplineManip_tid, 0
    );
}
TypeObjId Closed_BSplineManip::_tid() { return _XfClosed_BSplineManip_tid; }
//+

Closed_BSplineManip::Closed_BSplineManip (Closed_BSplineManip* m) : VertexManip (m){
    if (is_nil(m) || is_nil(m->figure_)) {
        FigureKit_var figurekit = fresco_instance()->figure_kit();
        Figure* f = new Figure(
            FigureKit::stroke, figurekit->default_style(), true, true, 1
        );
        f->add_point(0, 0);
        figure_body(f);
    }
}

Closed_BSplineManip::~Closed_BSplineManip () {}

Manipulator* Closed_BSplineManip::shallow_copy () {
    Closed_BSplineManip* manip = new Closed_BSplineManip(this);
    return manip;
}

Manipulator* Closed_BSplineManip::deep_copy () {
    Figure* copy = (Figure*)CopyCmd::glyphmap_->find(figure_);
    if (is_nil(copy)) {
        Figure* f = new Figure(figure_);
        CopyCmd::glyphmap_->map(figure_, f);
        Closed_BSplineManip* manip = new Closed_BSplineManip(this);
        manip->figure_body(f);
        Fresco::unref(f);
        return manip;
    } else {
        Closed_BSplineManip* manip = new Closed_BSplineManip(this);
        manip->figure_body(copy);
        return manip;
    }
}

void Closed_BSplineManip::recompute_shape () {
    long n = vlist_->count();
    figure_->reset();
    figure_->Bspline_move_to(
        vlist_->item(0).x, vlist_->item(0).y, 
        vlist_->item(n-1).x, vlist_->item(n-1).y, 
        vlist_->item(1).x, vlist_->item(1).y
    );
    for (long i = 1; i < n - 1; ++i) {
        figure_->Bspline_curve_to(
            vlist_->item(i).x, vlist_->item(i).y, 
            vlist_->item(i-1).x, vlist_->item(i-1).y, 
            vlist_->item(i+1).x, vlist_->item(i+1).y
        );
    }
    figure_->Bspline_curve_to(
        vlist_->item(n-1).x, vlist_->item(n-1).y, 
        vlist_->item(n-2).x, vlist_->item(n-2).y, 
        vlist_->item(0).x, vlist_->item(0).y
    );
    figure_->Bspline_curve_to(
        vlist_->item(0).x, vlist_->item(0).y, 
        vlist_->item(n-1).x, vlist_->item(n-1).y, 
        vlist_->item(1).x, vlist_->item(1).y
    );
    vmin_.x = MAXFLOAT;
    vmin_.y = MAXFLOAT;
    vmax_.x = -MAXFLOAT;
    vmax_.y = -MAXFLOAT;
    for (long j = 0; j < vlist_->count(); j++) {
        vmin_.x = Math::min(vmin_.x, vlist_->item(j).x);
        vmax_.x = Math::max(vmax_.x, vlist_->item(j).x);
        vmin_.y = Math::min(vmin_.y, vlist_->item(j).y);
        vmax_.y = Math::max(vmax_.y, vlist_->item(j).y);
    }
}

//+ MultiLineManip(VertexManip)
extern TypeObj_Descriptor _XfVertexManip_type;
TypeObj_Descriptor* _XfMultiLineManip_parents[] = { &_XfVertexManip_type, nil };
extern TypeObjId _XfMultiLineManip_tid;
TypeObj_Descriptor _XfMultiLineManip_type = {
    /* type */ 0,
    /* id */ &_XfMultiLineManip_tid,
    "MultiLineManip",
    _XfMultiLineManip_parents, /* offsets */ nil, /* excepts */ nil,
    /* methods */ nil, /* params */ nil,
    /* receive */ nil
};

MultiLineManip* MultiLineManip::_narrow(BaseObjectRef o) {
    return (MultiLineManip*)_BaseObject_tnarrow(
        o, _XfMultiLineManip_tid, 0
    );
}
TypeObjId MultiLineManip::_tid() { return _XfMultiLineManip_tid; }
//+

MultiLineManip::MultiLineManip (MultiLineManip* m) : VertexManip (m) {
    if (is_nil(m) || is_nil(m->figure_)) {
        FigureKit_var figurekit = fresco_instance()->figure_kit();
        Figure* f = new Figure(
            FigureKit::stroke, figurekit->default_style(), false, false, 1
        );
        f->add_point(0, 0);
        figure_body(f);
    }
}

MultiLineManip::~MultiLineManip () {}

Manipulator* MultiLineManip::shallow_copy () {
    MultiLineManip* manip = new MultiLineManip(this);
    return manip;
}

Manipulator* MultiLineManip::deep_copy () {
    Figure* copy = (Figure*)CopyCmd::glyphmap_->find(figure_);
    if (is_nil(copy)) {
        Figure* f = new Figure(figure_);
        CopyCmd::glyphmap_->map(figure_, f);
        MultiLineManip* manip = new MultiLineManip(this);
        manip->figure_body(f);
        Fresco::unref(f);
        return manip;
    } else {
        MultiLineManip* manip = new MultiLineManip(this);
        manip->figure_body(copy);
        return manip;
    }
}

//+ PolygonManip(VertexManip)
extern TypeObj_Descriptor _XfVertexManip_type;
TypeObj_Descriptor* _XfPolygonManip_parents[] = { &_XfVertexManip_type, nil };
extern TypeObjId _XfPolygonManip_tid;
TypeObj_Descriptor _XfPolygonManip_type = {
    /* type */ 0,
    /* id */ &_XfPolygonManip_tid,
    "PolygonManip",
    _XfPolygonManip_parents, /* offsets */ nil, /* excepts */ nil,
    /* methods */ nil, /* params */ nil,
    /* receive */ nil
};

PolygonManip* PolygonManip::_narrow(BaseObjectRef o) {
    return (PolygonManip*)_BaseObject_tnarrow(
        o, _XfPolygonManip_tid, 0
    );
}
TypeObjId PolygonManip::_tid() { return _XfPolygonManip_tid; }
//+

PolygonManip::PolygonManip (PolygonManip* m) : VertexManip (m) {
    if (is_nil(m) || is_nil(m->figure_)) {
        FigureKit_var figurekit = fresco_instance()->figure_kit();
        Figure* f = new Figure(
            FigureKit::stroke, figurekit->default_style(), true, false, 1
        );
        f->add_point(0, 0);
        figure_body(f);
    }
}

PolygonManip::~PolygonManip () {}

Manipulator* PolygonManip::shallow_copy () {
    PolygonManip* manip = new PolygonManip(this);
    return manip;
}

Manipulator* PolygonManip::deep_copy () {
    Figure* copy = (Figure*)CopyCmd::glyphmap_->find(figure_);
    if (is_nil(copy)) {
        Figure* f = new Figure(figure_);
        CopyCmd::glyphmap_->map(figure_, f);
        PolygonManip* manip = new PolygonManip(this);
        manip->figure_body(f);
        Fresco::unref(f);
        return manip;
    } else {
        PolygonManip* manip = new PolygonManip(this);
        manip->figure_body(copy);
        return manip;
    }
}

//+ PolyManip(Manipulator)
extern TypeObj_Descriptor _XfManipulator_type;
TypeObj_Descriptor* _XfPolyManip_parents[] = { &_XfManipulator_type, nil };
extern TypeObjId _XfPolyManip_tid;
TypeObj_Descriptor _XfPolyManip_type = {
    /* type */ 0,
    /* id */ &_XfPolyManip_tid,
    "PolyManip",
    _XfPolyManip_parents, /* offsets */ nil, /* excepts */ nil,
    /* methods */ nil, /* params */ nil,
    /* receive */ nil
};

PolyManip* PolyManip::_narrow(BaseObjectRef o) {
    return (PolyManip*)_BaseObject_tnarrow(
        o, _XfPolyManip_tid, 0
    );
}
TypeObjId PolyManip::_tid() { return _XfPolyManip_tid; }
//+

PolyManip::PolyManip (PolyFigure* pf, PolyManip* m) : Manipulator (m) {
    polyfigure_ = pf;
    Fresco::ref(polyfigure_);
    body(polyfigure_);
}

PolyManip::~PolyManip () {
    Fresco::unref(polyfigure_);
}

void PolyManip::execute (Command* cmd) {
    FigViewer* v = cmd->figviewer();
    NaturalCmd* natcmd = NaturalCmd::_narrow(cmd);
    if (is_not_nil(natcmd)) {
        CmdVisitor cv(natcmd);
        cv.visit_children(polyfigure_);
    } else {
        Manipulator::execute(cmd);
    }
}

void PolyManip::unexecute (Command* cmd) {
    FigViewer* v = cmd->figviewer();
    NaturalCmd* natcmd = NaturalCmd::_narrow(cmd);
    if (is_not_nil(natcmd)) {
        CmdVisitor cv(natcmd, false);
        cv.visit_children(polyfigure_);
    } else {
        Manipulator::unexecute(cmd);
    }
}

Manipulator* PolyManip::shallow_copy () {
    return new PolyManip(polyfigure_, this);
}

Manipulator* PolyManip::deep_copy () {
    PolyFigure* copy = (PolyFigure*)CopyCmd::glyphmap_->find(polyfigure_);
    if (is_nil(copy)) {
        PolyFigure* f = new PolyFigure(polyfigure_);
        CopyCmd::glyphmap_->map(polyfigure_, f);

        ManipCopier dcopier(false);
        Glyph_var g = body();
        dcopier.visit_children(g);
        ManipList* ml = dcopier.manipulators();
        for (long i = 0; i < ml->count(); i++) {
            Manipulator* man = ml->item(i);
            f->append(man);
            Fresco::unref(man);
        }
        Manipulator* m = new PolyManip(f, this);
        Fresco::unref(f);
        return m;
    } else {
        return new PolyManip(copy, this);
    }
}

//+ LayoutManip(Manipulator)
extern TypeObj_Descriptor _XfManipulator_type;
TypeObj_Descriptor* _XfLayoutManip_parents[] = { &_XfManipulator_type, nil };
extern TypeObjId _XfLayoutManip_tid;
TypeObj_Descriptor _XfLayoutManip_type = {
    /* type */ 0,
    /* id */ &_XfLayoutManip_tid,
    "LayoutManip",
    _XfLayoutManip_parents, /* offsets */ nil, /* excepts */ nil,
    /* methods */ nil, /* params */ nil,
    /* receive */ nil
};

LayoutManip* LayoutManip::_narrow(BaseObjectRef o) {
    return (LayoutManip*)_BaseObject_tnarrow(
        o, _XfLayoutManip_tid, 0
    );
}
TypeObjId LayoutManip::_tid() { return _XfLayoutManip_tid; }
//+

LayoutManip::LayoutManip (
    Transformer* t, LayoutManip* m
) : Manipulator (m) {
    transformer_ = t;
    resize_info_ = nil;
    requested_ = false;
    if (is_not_nil(transformer_)) {
        Fresco::ref(transformer_);
        body(transformer_);
    }
    if (is_not_nil(m) && m->resize_info_ != nil) {
        resize_info_ = new ResizeInfo;
        *resize_info_ = *m->resize_info_;
    }
}

LayoutManip::~LayoutManip () {
    delete resize_info_;
    Fresco::unref(transformer_);
}

void LayoutManip::child_allocate(Glyph::AllocationInfo& a) {
    update_requisition();
    if (resize_info_ != nil || is_not_nil(a.allocation)) {
        if (is_nil(a.transformation)) {
            a.transformation = new TransformImpl;
        }
        a.transformation->premultiply(&tx_);
        RegionImpl rr;
        if (resize_info_ != nil) {
            if (is_not_nil(a.allocation)) {
                rr.copy(a.allocation);
            } else {
                _nat_allocation(&req_, &rr);
            }
            rr.upper_.x = rr.lower_.x + resize_info_->width;
            rr.upper_.y = rr.lower_.y + resize_info_->height;
        } else {
            rr.copy(a.allocation);
        }
        Glyph::Requisition r;
        MonoGlyph::request(r);
        adjust_allocation(&rr, &tx_, &r);
        a.allocation->copy(&rr);
    } else {
        Manipulator::child_allocate(a);
    }
}

void LayoutManip::update_requisition () {
    if (!requested_) {
        Glyph::Requisition r;
        MonoGlyph::request(r);
        flexible_transform_requisition(r, &tx_);
        req_ = r;
    }
}

//+ LayoutManip(Glyph::request)
void LayoutManip::request(Glyph::Requisition& r) {
    update_requisition();
    r = req_;
}

//+ LayoutManip(Glyph::allocations)
void LayoutManip::allocations(Glyph::AllocationInfoSeq& a) {
    update_requisition();
    Manipulator::allocations(a);
}

void LayoutManip::draw_handles (GlyphTraversal_in gt) {
    Manipulator::draw_handles(gt);
}

inline Coord rectify(Coord a) { return (a > 0) ? a : 0; }

Boolean LayoutManip::grasp (
    Tool& tool, FigViewer& fv, SelectInfo* si, EventRef e
) {
    Boolean ok = Manipulator::grasp(tool, fv, si, e);
    if (info_->tooltype_ == resize) {
        NaturalCmd natcmd(&fv);
        execute(&natcmd);
        RegionImpl r;
        Glyph::AllocationInfoSeq a;
        if (info_->obj_manip_) {
            transformer_->allocations(a);
        } else {
            allocations(a);
        }
        if (info_->obj_manip_) {
            glyph_need_redraw(transformer_, a, true, false);
            local_extension(transformer_, &r);
            transformer_->set_allocation(
                rectify(info_->pt_.x - r.lower_.x), 
                rectify(info_->pt_.y - r.lower_.y)
            );
            glyph_need_redraw(transformer_, a, true, false);
        } else {
            glyph_need_redraw(this, a, false, false);
            local_extension(this, &r);
            resize_info_ = new ResizeInfo;
            resize_info_->width = rectify(info_->pt_.x - r.lower_.x);
            resize_info_->height = rectify(info_->pt_.y - r.lower_.y);
            glyph_need_redraw(this, a, false, false);
        }
        info_->pt_ = r.lower_;
        delete_info_list(a);
        if (is_not_nil(info_->sinfo_->a_)) {
            compose(this);
        }
    }
    return ok;
}

Boolean LayoutManip::manipulate (EventRef e) {
    switch (e->type()) {
    case Event::motion:
    case Event::enter:
    case Event::leave:
	break;
    case Event::focus_in:
    case Event::focus_out:
	return true;
    default:
	return false;
    }
    Boolean ok = true;
    if (info_->tooltype_ == resize) {
        Vertex pt;
        pt.x = e->pointer_x() + info_->ax_ - info_->tool_ax_;
        pt.y = e->pointer_y() + info_->ay_ - info_->tool_ay_;
        pt.z = 0;
        info_->t_.transform_vertex(pt);
        Glyph::AllocationInfoSeq a;
        if (info_->obj_manip_) {
            transformer_->allocations(a);
        } else {
            allocations(a);
        }
        if (info_->obj_manip_) {
            glyph_need_redraw(transformer_, a, true, false);
            transformer_->set_allocation(
                rectify(pt.x - info_->pt_.x),
                rectify(pt.y - info_->pt_.y)
            );
            glyph_need_redraw(transformer_, a, true, false);
        } else {
            glyph_need_redraw(this, a, false, false);
            resize_info_->width = rectify(pt.x - info_->pt_.x);
            resize_info_->height = rectify(pt.y - info_->pt_.y);
            glyph_need_redraw(this, a, false, false);
        }
        ok = true;
        delete_info_list(a);
        if (is_not_nil(info_->sinfo_->a_)) {
            compose(this);
        }

    } else {
        ok = Manipulator::manipulate(e);
    }
    return ok;
}

void LayoutManip::execute (Command* cmd) {
    NaturalCmd* natcmd = NaturalCmd::_narrow(cmd);
    if (is_not_nil(natcmd)) {
        if (resize_info_ != nil || transformer_->allocation_set()) {
            Glyph::AllocationInfoSeq a;
            if (transformer_->allocation_set()) {
                transformer_->allocations(a);
                glyph_need_redraw(transformer_, a, true, false);
                transformer_->unset_allocation();
                glyph_need_redraw(transformer_, a, true, false);
                transformer_->need_redraw();
            } else {
                allocations(a);
                glyph_need_redraw(this, a, false, false);
                delete resize_info_;
                resize_info_ = nil;
                glyph_need_redraw(this, a, false, false);
            }
            delete_info_list(a);
        }
        CmdVisitor cv(natcmd);
        cv.visit_children(_tmp(transformer_->body()));
        return;
    }
    Manipulator::execute(cmd);
}

void LayoutManip::unexecute (Command* cmd) {
    Manipulator::unexecute(cmd);
}
    
//+ LayoutManip(Glyph::extension)
void LayoutManip::extension(const Glyph::AllocationInfo& a, Region_in r) {
    if (resize_info_ != nil) {
        RegionImpl rr;
        if (is_not_nil(a.allocation)) {
            rr.copy(a.allocation);
        } else {
            _nat_allocation(&req_, &rr);
        }
        rr.upper_.x = rr.lower_.x + resize_info_->width;
        rr.upper_.y = rr.lower_.y + resize_info_->height;
        r->copy(&rr);
        if (is_not_nil(a.transformation)) {
            r->apply_transform(a.transformation);
        }
    } else {
        Manipulator::extension(a, r);
    }
}

//+ LayoutManip(Glyph::traverse)
void LayoutManip::traverse(GlyphTraversal_in t) {
    if (resize_info_ != nil || is_not_nil(t->allocation())) {
        Painter_var p = t->current_painter();
        p->push_matrix();
        p->premultiply(&tx_);
        RegionImpl* rr = new RegionImpl;
        if (resize_info_ != nil) {
            if (is_not_nil(_tmp(t->allocation()))) {
                rr->copy(_tmp(t->allocation()));
            } else {
                _nat_allocation(&req_, rr);
            }
            rr->upper_.x = rr->lower_.x + resize_info_->width;
            rr->upper_.y = rr->lower_.y + resize_info_->height;
        } else {
            rr->copy(_tmp(t->allocation()));
        }
        Glyph::Requisition r;
        MonoGlyph::request(r);
        adjust_allocation(rr, &tx_, &r);
        t->traverse_child(&offset_, rr);
        Fresco::unref(rr);
        p->pop_matrix();
        if (t->op() == GlyphTraversal::draw) {
            draw(t);
        }
    } else {
        Manipulator::traverse(t);
    }
}

void LayoutManip::need_resize () {
    requested_ = false;
    OffsetVisitor ov;
    ov.visit_children(_tmp(transformer_->body()));
    for (long i = 0; i < ov.offset_count(); i++) {
        compose(ov.offset(i)->child());
    }
    Manipulator::need_resize();
}

//+ ButtonManip(LayoutManip)
extern TypeObj_Descriptor _XfLayoutManip_type;
TypeObj_Descriptor* _XfButtonManip_parents[] = { &_XfLayoutManip_type, nil };
extern TypeObjId _XfButtonManip_tid;
TypeObj_Descriptor _XfButtonManip_type = {
    /* type */ 0,
    /* id */ &_XfButtonManip_tid,
    "ButtonManip",
    _XfButtonManip_parents, /* offsets */ nil, /* excepts */ nil,
    /* methods */ nil, /* params */ nil,
    /* receive */ nil
};

ButtonManip* ButtonManip::_narrow(BaseObjectRef o) {
    return (ButtonManip*)_BaseObject_tnarrow(
        o, _XfButtonManip_tid, 0
    );
}
TypeObjId ButtonManip::_tid() { return _XfButtonManip_tid; }
//+

ButtonManip::ButtonManip (
    Boolean* e, Transformer* t, ButtonManip* m
) : LayoutManip (t, m) {
    editing_ = e;
    if (is_nil(transformer_)) {
        Fresco* f = fresco_instance();
        WidgetKit_var widgets = f->widget_kit();
        LayoutKit_var layouts = f->layout_kit();
        Button_var br = widgets->palette_button(layouts->vspace(10), nil);
        transformer_ = new Transformer;
        transformer_->body(br);
        body(transformer_);
    }
}

ButtonManip::~ButtonManip () {}

Manipulator* ButtonManip::shallow_copy () {
    return new ButtonManip(editing_, transformer_, this);
}

Manipulator* ButtonManip::deep_copy () {
    Transformer* copy = (Transformer*)CopyCmd::glyphmap_->find(transformer_);
    if (is_nil(copy)) {
        Fresco* f = fresco_instance();
        WidgetKit_var widgets = f->widget_kit();
        LayoutKit_var layouts = f->layout_kit();
        FigureKit_var figures = f->figure_kit();

        Button_var br = widgets->palette_button(
            figures->label(
                _tmp(figures->default_style()), 
                Fresco::tmp_string_ref("Button")
            ), _tmp(new PrintAction("Pushed"))
        );
        Transformer* transformer = new Transformer(transformer_);
        CopyCmd::glyphmap_->map(transformer_, transformer);
        transformer->body(br);
        Manipulator* m = new ButtonManip(editing_, transformer, this);
        Fresco::unref(transformer);
        return m;
    } else {
        return new ButtonManip(editing_, copy, this);
    }
}

//+ ButtonManip(Glyph::traverse)
void ButtonManip::traverse(GlyphTraversal_in t) {
    switch(t->op()) {
    case GlyphTraversal::pick_top:
    case GlyphTraversal::pick_all:
    case GlyphTraversal::pick_any:
        if (*editing_) {
            RegionImpl r;
            local_extension(this, &r);
            if (
                _tmp(t->current_painter())->is_visible(&r)
            ) {
                t->hit();
            }
        } else {
            LayoutManip::traverse(t);
        }
        break;
    default:
        LayoutManip::traverse(t);
        break;
    }
}

void ButtonManip::execute (Command* cmd) {
    UngroupCmd* ungroupcmd = UngroupCmd::_narrow(cmd);
    if (is_nil(ungroupcmd)) {
        LayoutManip::execute(cmd);
    }
}

void ButtonManip::unexecute (Command* cmd) {
    UngroupCmd* ungroupcmd = UngroupCmd::_narrow(cmd);
    if (is_nil(ungroupcmd)) {
        LayoutManip::unexecute(cmd);
    }
}

//+ BoxManip(LayoutManip)
extern TypeObj_Descriptor _XfLayoutManip_type;
TypeObj_Descriptor* _XfBoxManip_parents[] = { &_XfLayoutManip_type, nil };
extern TypeObjId _XfBoxManip_tid;
TypeObj_Descriptor _XfBoxManip_type = {
    /* type */ 0,
    /* id */ &_XfBoxManip_tid,
    "BoxManip",
    _XfBoxManip_parents, /* offsets */ nil, /* excepts */ nil,
    /* methods */ nil, /* params */ nil,
    /* receive */ nil
};

BoxManip* BoxManip::_narrow(BaseObjectRef o) {
    return (BoxManip*)_BaseObject_tnarrow(
        o, _XfBoxManip_tid, 0
    );
}
TypeObjId BoxManip::_tid() { return _XfBoxManip_tid; }
//+

BoxManip::BoxManip (Transformer* tr, BoxManip* m) : LayoutManip(tr, m) {}

BoxManip::~BoxManip () {}

void BoxManip::execute (Command* cmd) {
    FigViewer* v = cmd->figviewer();
    UngroupCmd* ungroupcmd = UngroupCmd::_narrow(cmd);
    if (is_not_nil(ungroupcmd)) {
        Glyph_var box = transformer_->body();

        Region_var na = transformer_->nat_allocation();
        TAManipCopier mc(na);
        mc.visit_children(box);
        ManipList* ml = mc.manipulators();
        TransformList* tl = mc.transforms();

        Transform_var tt = transformer_->transformation();
        for (long i = 0; i < ml->count(); i++) {
            Manipulator* man = ml->item(i);
            Transform_var tr = man->transformation();
            tr->postmultiply(tl->item(i));
            tr->postmultiply(tt);
            tr->postmultiply(&tx_);
        }
        Inserter inserter(*ml);
        inserter.visit_parents(this);
        SelectCmd selectcmd(v);
        for (i = 0; i < ml->count(); i++) {
            Manipulator* man = ml->item(i);
            man->execute(&selectcmd);
            info(man, new ManipInfo(info_->sinfo_->copy()));
        }
        SelectCmd unselectcmd(v, false);
        execute(&unselectcmd);
        Remover remover;
        remover.visit_parents(this);
        Fresco::unref(na);
        return;
    }
    GroupCmd* groupcmd = GroupCmd::_narrow(cmd);
    if (is_not_nil(groupcmd)) {
        Selection* sel = v->selection()->copy();
        long n = sel->count();
        if (n < 2) {
            delete sel;
            return;
        }
        CopyCmd::glyphmap_->clear();
        Manipulator* bm = deep_copy();
        Glyph_var box = _tmp(bm->body())->body();

        SelectCmd unselectcmd(v, false);
        Manipulator* m0 = sel->item(0);
        info(bm, new ManipInfo(info(m0)->sinfo_->copy()));
        OffsetVisitor ov;
        ov.visit_parents(m0);

        Remover* remover = new Remover;

        RegionImpl r;
        Vertex center;
        for (long i = 0; i < sel->count(); i++) {
            Manipulator* m = sel->item(i);
            RegionImpl ri;
            local_extension(m, &ri);
            r.merge_union(&ri);
            m->execute(&unselectcmd);
            remover->visit_parents(m);
            compose(m);
            box->append(m);
        }
        delete remover;
        for (long j = 0; j < ov.offset_count(); j++) {
            ov.offset(j)->parent()->append(bm);
        }

        center.x = (r.upper_.x+r.lower_.x)/2;
        center.y = (r.upper_.y+r.lower_.y)/2;
        NaturalCmd nat(v);
        bm->execute(&nat);
        local_extension(bm, &r);
        center.x -= (r.upper_.x+r.lower_.x)/2;
        center.y -= (r.upper_.y+r.lower_.y)/2;
        bm->transformation()->translate(center);

        bm->need_resize();
        SelectCmd selectcmd(v);
        bm->execute(&selectcmd);

        return;
    }
    LayoutManip::execute(cmd);
}

void BoxManip::unexecute (Command*) {}

//+ HBoxManip(BoxManip)
extern TypeObj_Descriptor _XfBoxManip_type;
TypeObj_Descriptor* _XfHBoxManip_parents[] = { &_XfBoxManip_type, nil };
extern TypeObjId _XfHBoxManip_tid;
TypeObj_Descriptor _XfHBoxManip_type = {
    /* type */ 0,
    /* id */ &_XfHBoxManip_tid,
    "HBoxManip",
    _XfHBoxManip_parents, /* offsets */ nil, /* excepts */ nil,
    /* methods */ nil, /* params */ nil,
    /* receive */ nil
};

HBoxManip* HBoxManip::_narrow(BaseObjectRef o) {
    return (HBoxManip*)_BaseObject_tnarrow(
        o, _XfHBoxManip_tid, 0
    );
}
TypeObjId HBoxManip::_tid() { return _XfHBoxManip_tid; }
//+

HBoxManip::HBoxManip (Transformer* t, HBoxManip* m) : BoxManip (t, m) {
    if (is_nil(t)) {
        Fresco* f = fresco_instance();
        LayoutKit_var layouts = f->layout_kit();
        Transformer* th = new Transformer;
        th->body(_tmp(layouts->hbox()));
        transformer_ = th;
        body(transformer_);
    }
}

HBoxManip::~HBoxManip () {}

Manipulator* HBoxManip::shallow_copy () {
    return new HBoxManip(transformer_, this);
}

Manipulator* HBoxManip::deep_copy () {
    Transformer* copy = (Transformer*)CopyCmd::glyphmap_->find(transformer_);
    if (is_nil(copy)) {
        Fresco* f = fresco_instance();
        LayoutKit_var layouts = f->layout_kit();
        Glyph_var box = layouts->hbox();

        Transformer* transformer = new Transformer(transformer_);
        CopyCmd::glyphmap_->map(transformer_, transformer);
        
        transformer->body(box);
        ManipCopier dcopier(false);
        Glyph_var g = _tmp(body())->body();
        dcopier.visit_children(g);
        ManipList* ml = dcopier.manipulators();
        for (long i = 0; i < ml->count(); i++) {
            Manipulator* man = ml->item(i);
            box->append(man);
            Fresco::unref(man);
        }

        Manipulator* m = new HBoxManip(transformer, this);
        Fresco::unref(transformer);
        return m;
    } else {
        return new HBoxManip(copy, this);
    }
}

//+ VBoxManip(BoxManip)
extern TypeObj_Descriptor _XfBoxManip_type;
TypeObj_Descriptor* _XfVBoxManip_parents[] = { &_XfBoxManip_type, nil };
extern TypeObjId _XfVBoxManip_tid;
TypeObj_Descriptor _XfVBoxManip_type = {
    /* type */ 0,
    /* id */ &_XfVBoxManip_tid,
    "VBoxManip",
    _XfVBoxManip_parents, /* offsets */ nil, /* excepts */ nil,
    /* methods */ nil, /* params */ nil,
    /* receive */ nil
};

VBoxManip* VBoxManip::_narrow(BaseObjectRef o) {
    return (VBoxManip*)_BaseObject_tnarrow(
        o, _XfVBoxManip_tid, 0
    );
}
TypeObjId VBoxManip::_tid() { return _XfVBoxManip_tid; }
//+

VBoxManip::VBoxManip (Transformer* t, VBoxManip* m) : BoxManip (t, m) {
    if (is_nil(t)) {
        Fresco* f = fresco_instance();
        LayoutKit_var layouts = f->layout_kit();
        Transformer* tv = new Transformer;
        tv->body(_tmp(layouts->vbox()));
        transformer_ = tv;
        body(transformer_);
    }
}

VBoxManip::~VBoxManip () {}

Manipulator* VBoxManip::shallow_copy () {
    return new VBoxManip(transformer_, this);
}

Manipulator* VBoxManip::deep_copy () {
    Transformer* copy = (Transformer*)CopyCmd::glyphmap_->find(transformer_);
    if (is_nil(copy)) {
        Fresco* f = fresco_instance();
        LayoutKit_var layouts = f->layout_kit();
        Glyph_var box = layouts->vbox();

        Transformer* transformer = new Transformer(transformer_);
        CopyCmd::glyphmap_->map(transformer_, transformer);
        transformer->body(box);

        ManipCopier dcopier(false);
        Glyph_var g = _tmp(body())->body();
        dcopier.visit_children(g);
        ManipList* ml = dcopier.manipulators();
        for (long i = 0; i < ml->count(); i++) {
            Manipulator* man = ml->item(i);
            box->append(man);
            Fresco::unref(man);
        }

        Manipulator* m = new VBoxManip(transformer, this);
        Fresco::unref(transformer);
        return m;
    } else {
        return new VBoxManip(copy, this);
    }
}

//+ MacroManip(Manipulator)
extern TypeObj_Descriptor _XfManipulator_type;
TypeObj_Descriptor* _XfMacroManip_parents[] = { &_XfManipulator_type, nil };
extern TypeObjId _XfMacroManip_tid;
TypeObj_Descriptor _XfMacroManip_type = {
    /* type */ 0,
    /* id */ &_XfMacroManip_tid,
    "MacroManip",
    _XfMacroManip_parents, /* offsets */ nil, /* excepts */ nil,
    /* methods */ nil, /* params */ nil,
    /* receive */ nil
};

MacroManip* MacroManip::_narrow(BaseObjectRef o) {
    return (MacroManip*)_BaseObject_tnarrow(
        o, _XfMacroManip_tid, 0
    );
}
TypeObjId MacroManip::_tid() { return _XfMacroManip_tid; }
//+

MacroManip::MacroManip (MacroManip* m) : Manipulator(m) {
    maniplist_ = new ManipList;
}

MacroManip::~MacroManip () {
    delete maniplist_;
}

Boolean MacroManip::grasp(
    Tool& t, FigViewer& fv, SelectInfo* si, EventRef e
) {
    Boolean ok = true;
    for (long i = 0; i < maniplist_->count(); i++) {
        Manipulator* m = maniplist_->item(i);
        ok = m->grasp(t, fv, si, e) && ok;
    }
    return ok;
}

Boolean MacroManip::manipulate(EventRef e) {
    Boolean ok = true;
    for (long i = 0; i < maniplist_->count(); i++) {
        Manipulator* m = maniplist_->item(i);
        ok = m->manipulate(e) && ok;
    }
    return ok;
}

Command* MacroManip::effect (EventRef e) {
    MacroCmd* mc = new MacroCmd;
    for (long i = 0; i < maniplist_->count(); i++) {
        Manipulator* m = maniplist_->item(i);
        Command* cmd = m->effect(e);
        if (is_not_nil(cmd)) {
            mc->append(cmd);
        }
    }
    if (mc->count() == 0) {
        delete mc;
        mc = nil;
    }
    return mc;
}

void MacroManip::execute (Command* cmd) {
    for (long i = 0; i < maniplist_->count(); i++) {
        Manipulator* m = maniplist_->item(i);
        m->execute(cmd);
    }
}

void MacroManip::unexecute (Command* cmd) {
    for (long i = 0; i < maniplist_->count(); i++) {
        Manipulator* m = maniplist_->item(i);
        m->unexecute(cmd);
    }
}

Manipulator* MacroManip::shallow_copy () {
    MacroManip* macro = new MacroManip(this);
    for (long i = 0; i < maniplist_->count(); i++) {
        Manipulator* m = maniplist_->item(i);
        macro->append(m);
    }
    return macro;
}
    
Manipulator* MacroManip::deep_copy () {
    MacroManip* macro = new MacroManip(this);
    for (long i = 0; i < maniplist_->count(); i++) {
        Manipulator* m = maniplist_->item(i)->deep_copy();
        macro->append(m);
        Fresco::unref(m);
    }
    return macro;
}
    
void MacroManip::add (Manipulator* manip) {
    maniplist_->append(manip);
}

void MacroManip::insert (long index, Manipulator* manip) {
    maniplist_->insert(index, manip);
}

void MacroManip::remove(long index) {
    maniplist_->remove(index);
}

void MacroManip::remove_all() {
    maniplist_->remove_all();
}

long MacroManip::count () {
    return maniplist_->count();
}

Manipulator* MacroManip::manipulator (long index) {
    return maniplist_->item(index);
}

