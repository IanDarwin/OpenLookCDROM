/*
 * commands.cxx
 */
#include "commands.h"
#include "figviewer.h"
#include "manipulators.h"
#include "selection.h"
#include <X11/Fresco/Ox/typeobjs.h>

struct KeyElem {
    GlyphImpl* key;
    GlyphImpl* object;
};

declareList(KeyList, KeyElem);
implementList(KeyList, KeyElem);

GlyphImplMap::GlyphImplMap () {
    keylist_ = new KeyList;
}

GlyphImplMap::~GlyphImplMap () {
    delete keylist_;
}

void GlyphImplMap::clear () {
    keylist_->remove_all();
}

void GlyphImplMap::map (GlyphImpl* key, GlyphImpl* obj) {
    KeyElem keyelem;
    keyelem.key = key;
    keyelem.object = obj;
    keylist_->append(keyelem);
}

GlyphImpl* GlyphImplMap::find (GlyphImpl* key) {
    for (long i = 0; i < keylist_->count(); i++) {
        KeyElem& keyelem = keylist_->item_ref(i);
        if (keyelem.key == key) {
            return keyelem.object;
        }
    }
    return nil;
}

implementPtrList(CmdList, Command);

//+ Data(FrescoObject)
extern TypeObj_Descriptor _XfFrescoObject_type;
TypeObj_Descriptor* _XfData_parents[] = { &_XfFrescoObject_type, nil };
extern TypeObjId _XfData_tid;
TypeObj_Descriptor _XfData_type = {
    /* type */ 0,
    /* id */ &_XfData_tid,
    "Data",
    _XfData_parents, /* offsets */ nil, /* excepts */ nil,
    /* methods */ nil, /* params */ nil,
    /* receive */ nil
};

Data* Data::_narrow(BaseObjectRef o) {
    return (Data*)_BaseObject_tnarrow(
        o, _XfData_tid, 0
    );
}
TypeObjId Data::_tid() { return _XfData_tid; }
//+

Data::Data () {}

Data::~Data () {}

implementTable(DataTable, long, Data*);

//+ Command(Action)
extern TypeObj_Descriptor _XfAction_type;
TypeObj_Descriptor* _XfCommand_parents[] = { &_XfAction_type, nil };
extern TypeObjId _XfCommand_tid;
TypeObj_Descriptor _XfCommand_type = {
    /* type */ 0,
    /* id */ &_XfCommand_tid,
    "Command",
    _XfCommand_parents, /* offsets */ nil, /* excepts */ nil,
    /* methods */ nil, /* params */ nil,
    /* receive */ nil
};

Command* Command::_narrow(BaseObjectRef o) {
    return (Command*)_BaseObject_tnarrow(
        o, _XfCommand_tid, 0
    );
}
TypeObjId Command::_tid() { return _XfCommand_tid; }
//+

Command::Command (FigViewer* figviewer) {
    figviewer_ = figviewer;
    data_ = new DataTable(32);
    selection_ = nil;
}

void Command::store (long key, Data* data) {
    data_->insert(key, data);
}

Data* Command::recall (long key) {
    Data* d = nil;
    data_->find(d, key);
    return d;
}

void Command::execute () {
    Selection* sel = figviewer_->selection()->copy();
    for (long i = 0; i < sel->count(); i++) {
        Manipulator* m = sel->item(i);
        m->execute(this);
    }
    delete sel;
}

Command::~Command () {
    delete selection_;
    TableIterator(DataTable)* ti = new TableIterator(DataTable)(*data_);
    for (; ti->more(); ti->next()) {
        Data*& data = ti->cur_value();
        delete data;
    }
    delete data_;
    delete ti;
}

void Command::log () {
    /* Logging is not supported.  All commands are irreversible */
}

//+ GroupCmd(Command)
extern TypeObj_Descriptor _XfCommand_type;
TypeObj_Descriptor* _XfGroupCmd_parents[] = { &_XfCommand_type, nil };
extern TypeObjId _XfGroupCmd_tid;
TypeObj_Descriptor _XfGroupCmd_type = {
    /* type */ 0,
    /* id */ &_XfGroupCmd_tid,
    "GroupCmd",
    _XfGroupCmd_parents, /* offsets */ nil, /* excepts */ nil,
    /* methods */ nil, /* params */ nil,
    /* receive */ nil
};

GroupCmd* GroupCmd::_narrow(BaseObjectRef o) {
    return (GroupCmd*)_BaseObject_tnarrow(
        o, _XfGroupCmd_tid, 0
    );
}
TypeObjId GroupCmd::_tid() { return _XfGroupCmd_tid; }
//+

GroupCmd::GroupCmd (FigViewer* fv, Manipulator* m) : Command (fv) {
    m_ = m;
}

GroupCmd::~GroupCmd () {
    delete m_;
}

void GroupCmd::execute () {
    if (is_nil(m_)) {
        figviewer_->root()->execute(this);
    } else {
        m_->execute(this);
    }
}

//+ UngroupCmd(Command)
extern TypeObj_Descriptor _XfCommand_type;
TypeObj_Descriptor* _XfUngroupCmd_parents[] = { &_XfCommand_type, nil };
extern TypeObjId _XfUngroupCmd_tid;
TypeObj_Descriptor _XfUngroupCmd_type = {
    /* type */ 0,
    /* id */ &_XfUngroupCmd_tid,
    "UngroupCmd",
    _XfUngroupCmd_parents, /* offsets */ nil, /* excepts */ nil,
    /* methods */ nil, /* params */ nil,
    /* receive */ nil
};

UngroupCmd* UngroupCmd::_narrow(BaseObjectRef o) {
    return (UngroupCmd*)_BaseObject_tnarrow(
        o, _XfUngroupCmd_tid, 0
    );
}
TypeObjId UngroupCmd::_tid() { return _XfUngroupCmd_tid; }
//+

UngroupCmd::UngroupCmd (FigViewer* fv) : Command (fv) {}

UngroupCmd::~UngroupCmd () {}

//+ DeleteCmd(Command)
extern TypeObj_Descriptor _XfCommand_type;
TypeObj_Descriptor* _XfDeleteCmd_parents[] = { &_XfCommand_type, nil };
extern TypeObjId _XfDeleteCmd_tid;
TypeObj_Descriptor _XfDeleteCmd_type = {
    /* type */ 0,
    /* id */ &_XfDeleteCmd_tid,
    "DeleteCmd",
    _XfDeleteCmd_parents, /* offsets */ nil, /* excepts */ nil,
    /* methods */ nil, /* params */ nil,
    /* receive */ nil
};

DeleteCmd* DeleteCmd::_narrow(BaseObjectRef o) {
    return (DeleteCmd*)_BaseObject_tnarrow(
        o, _XfDeleteCmd_tid, 0
    );
}
TypeObjId DeleteCmd::_tid() { return _XfDeleteCmd_tid; }
//+

DeleteCmd::DeleteCmd (FigViewer* fv) : Command (fv) {}

DeleteCmd::~DeleteCmd () {}

//+ InstanceCmd(Command)
extern TypeObj_Descriptor _XfCommand_type;
TypeObj_Descriptor* _XfInstanceCmd_parents[] = { &_XfCommand_type, nil };
extern TypeObjId _XfInstanceCmd_tid;
TypeObj_Descriptor _XfInstanceCmd_type = {
    /* type */ 0,
    /* id */ &_XfInstanceCmd_tid,
    "InstanceCmd",
    _XfInstanceCmd_parents, /* offsets */ nil, /* excepts */ nil,
    /* methods */ nil, /* params */ nil,
    /* receive */ nil
};

InstanceCmd* InstanceCmd::_narrow(BaseObjectRef o) {
    return (InstanceCmd*)_BaseObject_tnarrow(
        o, _XfInstanceCmd_tid, 0
    );
}
TypeObjId InstanceCmd::_tid() { return _XfInstanceCmd_tid; }
//+

InstanceCmd::InstanceCmd (FigViewer* fv) : Command (fv) {}

InstanceCmd::~InstanceCmd () {}

//+ CopyCmd(Command)
extern TypeObj_Descriptor _XfCommand_type;
TypeObj_Descriptor* _XfCopyCmd_parents[] = { &_XfCommand_type, nil };
extern TypeObjId _XfCopyCmd_tid;
TypeObj_Descriptor _XfCopyCmd_type = {
    /* type */ 0,
    /* id */ &_XfCopyCmd_tid,
    "CopyCmd",
    _XfCopyCmd_parents, /* offsets */ nil, /* excepts */ nil,
    /* methods */ nil, /* params */ nil,
    /* receive */ nil
};

CopyCmd* CopyCmd::_narrow(BaseObjectRef o) {
    return (CopyCmd*)_BaseObject_tnarrow(
        o, _XfCopyCmd_tid, 0
    );
}
TypeObjId CopyCmd::_tid() { return _XfCopyCmd_tid; }
//+

CopyCmd::CopyCmd (FigViewer* fv) : Command (fv) {}

CopyCmd::~CopyCmd () {}

GlyphImplMap* CopyCmd::glyphmap_ = nil;

//+ NarrowCmd(Command)
extern TypeObj_Descriptor _XfCommand_type;
TypeObj_Descriptor* _XfNarrowCmd_parents[] = { &_XfCommand_type, nil };
extern TypeObjId _XfNarrowCmd_tid;
TypeObj_Descriptor _XfNarrowCmd_type = {
    /* type */ 0,
    /* id */ &_XfNarrowCmd_tid,
    "NarrowCmd",
    _XfNarrowCmd_parents, /* offsets */ nil, /* excepts */ nil,
    /* methods */ nil, /* params */ nil,
    /* receive */ nil
};

NarrowCmd* NarrowCmd::_narrow(BaseObjectRef o) {
    return (NarrowCmd*)_BaseObject_tnarrow(
        o, _XfNarrowCmd_tid, 0
    );
}
TypeObjId NarrowCmd::_tid() { return _XfNarrowCmd_tid; }
//+

NarrowCmd::NarrowCmd (FigViewer* fv) : Command (fv) {}

NarrowCmd::~NarrowCmd () {}

//+ NaturalCmd(Command)
extern TypeObj_Descriptor _XfCommand_type;
TypeObj_Descriptor* _XfNaturalCmd_parents[] = { &_XfCommand_type, nil };
extern TypeObjId _XfNaturalCmd_tid;
TypeObj_Descriptor _XfNaturalCmd_type = {
    /* type */ 0,
    /* id */ &_XfNaturalCmd_tid,
    "NaturalCmd",
    _XfNaturalCmd_parents, /* offsets */ nil, /* excepts */ nil,
    /* methods */ nil, /* params */ nil,
    /* receive */ nil
};

NaturalCmd* NaturalCmd::_narrow(BaseObjectRef o) {
    return (NaturalCmd*)_BaseObject_tnarrow(
        o, _XfNaturalCmd_tid, 0
    );
}
TypeObjId NaturalCmd::_tid() { return _XfNaturalCmd_tid; }
//+

NaturalCmd::NaturalCmd (FigViewer* fv) : Command (fv) {}

NaturalCmd::~NaturalCmd () {}

//+ SelectCmd(Command)
extern TypeObj_Descriptor _XfCommand_type;
TypeObj_Descriptor* _XfSelectCmd_parents[] = { &_XfCommand_type, nil };
extern TypeObjId _XfSelectCmd_tid;
TypeObj_Descriptor _XfSelectCmd_type = {
    /* type */ 0,
    /* id */ &_XfSelectCmd_tid,
    "SelectCmd",
    _XfSelectCmd_parents, /* offsets */ nil, /* excepts */ nil,
    /* methods */ nil, /* params */ nil,
    /* receive */ nil
};

SelectCmd* SelectCmd::_narrow(BaseObjectRef o) {
    return (SelectCmd*)_BaseObject_tnarrow(
        o, _XfSelectCmd_tid, 0
    );
}
TypeObjId SelectCmd::_tid() { return _XfSelectCmd_tid; }
//+

SelectCmd::SelectCmd (FigViewer* fv, Boolean s) : Command (fv) {
    selected_ = s;
}

SelectCmd::~SelectCmd () {}

//+ MacroCmd(Command)
extern TypeObj_Descriptor _XfCommand_type;
TypeObj_Descriptor* _XfMacroCmd_parents[] = { &_XfCommand_type, nil };
extern TypeObjId _XfMacroCmd_tid;
TypeObj_Descriptor _XfMacroCmd_type = {
    /* type */ 0,
    /* id */ &_XfMacroCmd_tid,
    "MacroCmd",
    _XfMacroCmd_parents, /* offsets */ nil, /* excepts */ nil,
    /* methods */ nil, /* params */ nil,
    /* receive */ nil
};

MacroCmd* MacroCmd::_narrow(BaseObjectRef o) {
    return (MacroCmd*)_BaseObject_tnarrow(
        o, _XfMacroCmd_tid, 0
    );
}
TypeObjId MacroCmd::_tid() { return _XfMacroCmd_tid; }
//+

MacroCmd::MacroCmd (FigViewer* fv) : Command(fv) {
    cmdlist_ = new CmdList;
}

MacroCmd::~MacroCmd () {
    for (long i = 0; i < cmdlist_->count(); i++) {
        Command* cmd = cmdlist_->item(i);
        delete cmd;
    }
    delete cmdlist_;
}

Boolean MacroCmd::reversible () {
    for (long i = 0; i < cmdlist_->count(); i++) {
        if (cmdlist_->item(i)->reversible()) {
            return true;
        }
    }
    return false;
}
 
void MacroCmd::execute () {       
    for (long i = 0; i < cmdlist_->count(); i++) {
        cmdlist_->item(i)->execute();
    }
}

void MacroCmd::unexecute () {       
    for (long i = cmdlist_->count()-1; i >= 0; i--) {
        cmdlist_->item(i)->execute();
    }
}

void MacroCmd::prepend (Command* cmd) {
    cmdlist_->prepend(cmd);
}

void MacroCmd::append (Command* cmd) {
    cmdlist_->append(cmd);
}

void MacroCmd::insert (long index, Command* cmd) {
    cmdlist_->insert(index, cmd);
}

void MacroCmd::remove(long index) {
    cmdlist_->remove(index);
}

void MacroCmd::remove_all() {
    cmdlist_->remove_all();
}

long MacroCmd::count () {
    return cmdlist_->count();
}

Command* MacroCmd::command (long index) {
    return cmdlist_->item(index);
}

//+ SelectInfoCmd(Command)
extern TypeObj_Descriptor _XfCommand_type;
TypeObj_Descriptor* _XfSelectInfoCmd_parents[] = { &_XfCommand_type, nil };
extern TypeObjId _XfSelectInfoCmd_tid;
TypeObj_Descriptor _XfSelectInfoCmd_type = {
    /* type */ 0,
    /* id */ &_XfSelectInfoCmd_tid,
    "SelectInfoCmd",
    _XfSelectInfoCmd_parents, /* offsets */ nil, /* excepts */ nil,
    /* methods */ nil, /* params */ nil,
    /* receive */ nil
};

SelectInfoCmd* SelectInfoCmd::_narrow(BaseObjectRef o) {
    return (SelectInfoCmd*)_BaseObject_tnarrow(
        o, _XfSelectInfoCmd_tid, 0
    );
}
TypeObjId SelectInfoCmd::_tid() { return _XfSelectInfoCmd_tid; }
//+

SelectInfoCmd::SelectInfoCmd (FigViewer* fv) : Command (fv) {
    sinfo_ = nil;
}

SelectInfoCmd::~SelectInfoCmd () {}


