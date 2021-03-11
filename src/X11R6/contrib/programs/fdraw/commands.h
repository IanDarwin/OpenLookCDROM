/*
 * commands.h 
 */

#ifndef _commands_h
#define _commands_h

#include <X11/Fresco/Impls/action.h>
#include <X11/Fresco/Ox/base.h>
#include <X11/Fresco/OS/list.h>
#include <X11/Fresco/OS/table.h>

class Command;
class FigViewer;
class GlyphImpl;
class KeyList;
class Manipulator;
class SelectInfo;
class Selection;

class GlyphImplMap {
public:
    GlyphImplMap();
    virtual ~GlyphImplMap();
    void clear();
    void map(GlyphImpl* key, GlyphImpl* obj);
    GlyphImpl* find(GlyphImpl* key);
private:
    KeyList* keylist_;
};
    
declarePtrList(CmdList, Command);

//- Data*
//+ Data : FrescoObject
class Data : public FrescoObject {
public:
    ~Data();
    TypeObjId _tid();
    static Data* _narrow(BaseObjectRef);
//+
protected:
    Data();
};

declareTable(DataTable, long, Data*);

//- Command*
//+ Command : ActionImpl
class Command : public ActionImpl {
public:
    ~Command();
    TypeObjId _tid();
    static Command* _narrow(BaseObjectRef);
//+
public:
    virtual void store(long key, Data* data);
    virtual Data* recall(long key);
    virtual void log();
    virtual void execute();

    FigViewer* figviewer();
protected:
    Command(FigViewer* = nil);
protected:
    DataTable* data_;
    FigViewer* figviewer_;
    Selection* selection_;
};

inline FigViewer* Command::figviewer () { return figviewer_; }

//- GroupCmd*
//+ GroupCmd : Command
class GroupCmd : public Command {
public:
    ~GroupCmd();
    TypeObjId _tid();
    static GroupCmd* _narrow(BaseObjectRef);
//+
public:
    GroupCmd(FigViewer* = nil, Manipulator* = nil);
    virtual void execute();
protected:
    Manipulator* m_;
};

//- UngroupCmd*
//+ UngroupCmd : Command
class UngroupCmd : public Command {
public:
    ~UngroupCmd();
    TypeObjId _tid();
    static UngroupCmd* _narrow(BaseObjectRef);
//+
public:
    UngroupCmd(FigViewer* = nil);
};

//- DeleteCmd*
//+ DeleteCmd : Command
class DeleteCmd : public Command {
public:
    ~DeleteCmd();
    TypeObjId _tid();
    static DeleteCmd* _narrow(BaseObjectRef);
//+
public:
    DeleteCmd(FigViewer* = nil);
};

//- InstanceCmd*
//+ InstanceCmd : Command
class InstanceCmd : public Command {
public:
    ~InstanceCmd();
    TypeObjId _tid();
    static InstanceCmd* _narrow(BaseObjectRef);
//+
public:
    InstanceCmd(FigViewer* = nil);
};

//- CopyCmd*
//+ CopyCmd : Command
class CopyCmd : public Command {
public:
    ~CopyCmd();
    TypeObjId _tid();
    static CopyCmd* _narrow(BaseObjectRef);
//+
public:
    CopyCmd(FigViewer* = nil);
public:
    static GlyphImplMap* glyphmap_;
};

//- NarrowCmd*
//+ NarrowCmd : Command
class NarrowCmd : public Command {
public:
    ~NarrowCmd();
    TypeObjId _tid();
    static NarrowCmd* _narrow(BaseObjectRef);
//+
    NarrowCmd(FigViewer* = nil);
};

//- NaturalCmd*
//+ NaturalCmd : Command
class NaturalCmd : public Command {
public:
    ~NaturalCmd();
    TypeObjId _tid();
    static NaturalCmd* _narrow(BaseObjectRef);
//+
    NaturalCmd(FigViewer* = nil);
};

//- SelectCmd*
//+ SelectCmd : Command
class SelectCmd : public Command {
public:
    ~SelectCmd();
    TypeObjId _tid();
    static SelectCmd* _narrow(BaseObjectRef);
//+
    SelectCmd(FigViewer* = nil, Boolean selected = true);

    Boolean selected ();
protected:
    Boolean selected_;
};

inline Boolean SelectCmd::selected () { return selected_; }

//- MacroCmd*
//+ MacroCmd : Command
class MacroCmd : public Command {
public:
    ~MacroCmd();
    TypeObjId _tid();
    static MacroCmd* _narrow(BaseObjectRef);
//+
    MacroCmd(FigViewer* = nil);

    virtual Boolean reversible();
    virtual void execute();
    virtual void unexecute();

    virtual void prepend(Command*);
    virtual void append(Command*);
    virtual void insert(long index, Command*);
    virtual void remove(long index);
    virtual void remove_all();
    virtual long count();

    Command* command(long);
protected:
    CmdList* cmdlist_;

};

//- SelectInfoCmd*
//+ SelectInfoCmd : Command
class SelectInfoCmd : public Command {
public:
    ~SelectInfoCmd();
    TypeObjId _tid();
    static SelectInfoCmd* _narrow(BaseObjectRef);
//+
    SelectInfoCmd(FigViewer* = nil);

    void select_info(SelectInfo*);
    SelectInfo* select_info();
protected:
    SelectInfo* sinfo_;
};

inline void SelectInfoCmd::select_info(SelectInfo* s) { sinfo_ = s; }
inline SelectInfo* SelectInfoCmd::select_info() { return sinfo_; }

#endif
