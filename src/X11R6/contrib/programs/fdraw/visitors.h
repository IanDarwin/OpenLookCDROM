/*
 * visitors.h
 */

#include <X11/Fresco/Impls/glyphs.h>
#include <X11/Fresco/OS/list.h>

class RegionImpl;
class TransformImpl;
class Command;
class ManipList;

declarePtrList(AllocationList, RegionImpl);
declarePtrList(TransformList, TransformImpl);

class GlyphVisitor {
public:
    GlyphVisitor();
    virtual ~GlyphVisitor();

    virtual Boolean visit(GlyphRef g, GlyphOffsetRef offset);
    virtual void visit_children(GlyphRef g);
    virtual void visit_parents(GlyphRef g);
};

class Appender : public GlyphVisitor {
public:
    Appender(GlyphRef, Boolean resize = true);
    ~Appender();
    Boolean visit(GlyphRef, GlyphOffsetRef);
protected:
    GlyphRef glyph_;
    Boolean resize_;
};

class Remover : public GlyphVisitor {
public:
    Remover();
    ~Remover();
    Boolean visit(GlyphRef, GlyphOffsetRef);
protected:
    GlyphOffsetList list_;
};

class Counter : public GlyphVisitor {
public:
    Counter();
    Boolean visit(GlyphRef, GlyphOffsetRef);
    long count();
protected:
    long count_;
};

inline long Counter::count () { return count_; }

class CmdVisitor : public GlyphVisitor {
public:
    CmdVisitor(Command* cmd, Boolean execute = true);
    ~CmdVisitor();

    Boolean visit(GlyphRef, GlyphOffsetRef);
protected:
    Command* cmd_;
    Boolean execute_;
};

class ManipCopier : public GlyphVisitor {
public:
    ManipCopier(Boolean shallow = true);
    ~ManipCopier();

    Boolean visit(GlyphRef, GlyphOffsetRef);
    ManipList* manipulators();
private:
    ManipList* maniplist_;
    Boolean shallow_;
};

inline ManipList* ManipCopier::manipulators () { return maniplist_; }

class TAManipCopier : public ManipCopier {
public:
    TAManipCopier(Region_in a, Boolean shallow = true);
    ~TAManipCopier();

    Boolean visit(GlyphRef, GlyphOffsetRef);

    AllocationList* allocations();
    TransformList* transforms();
private:
    AllocationList* alist_;
    TransformList* tlist_;
    Region_in a_;
};

inline AllocationList* TAManipCopier::allocations () { return alist_; }
inline TransformList* TAManipCopier::transforms () { return tlist_; }

class OffsetVisitor : public GlyphVisitor {
public:
    OffsetVisitor();
    ~OffsetVisitor();

    Boolean visit(GlyphRef, GlyphOffsetRef);
    GlyphOffsetRef offset(long);
    long offset_count();
protected:
    GlyphOffsetList* glist_;
};

