/* $XConsortium: expr-impl.h,v 1.5 94/06/03 21:41:04 matt Exp $ */

/*
 * Copyright (c) 1992-1993 Silicon Graphics, Inc.
 * Copyright (c) 1993 Fujitsu, Ltd.
 *
 * Permission to use, copy, modify, distribute, and sell this software and 
 * its documentation for any purpose is hereby granted without fee, provided
 * that (i) the above copyright notices and this permission notice appear in
 * all copies of the software and related documentation, and (ii) the names of
 * Silicon Graphics and Fujitsu may not be used in any advertising or
 * publicity relating to the software without the specific, prior written
 * permission of Silicon Graphics and Fujitsu.
 *
 * THE SOFTWARE IS PROVIDED "AS-IS" AND WITHOUT WARRANTY OF ANY KIND, 
 * EXPRESS, IMPLIED OR OTHERWISE, INCLUDING WITHOUT LIMITATION, ANY 
 * WARRANTY OF MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE.  
 *
 * IN NO EVENT SHALL SILICON GRAPHICS OR FUJITSU BE LIABLE FOR
 * ANY SPECIAL, INCIDENTAL, INDIRECT OR CONSEQUENTIAL DAMAGES OF ANY KIND,
 * OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
 * WHETHER OR NOT ADVISED OF THE POSSIBILITY OF DAMAGE, AND ON ANY THEORY OF 
 * LIABILITY, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE 
 * OF THIS SOFTWARE.
 */

/*
 * Expr implementations
 */

#ifndef expr_impl_h
#define expr_impl_h

#include "expr.h"
#include "list.h"

class ExprImpl : public Expr {
public:
    ExprImpl(SourcePosition*);
    ~ExprImpl();

    Symbol* symbol();
    Expr* parent();
    Declarator* declarator();
    void resolve(Resolver*);
    Boolean set_source(Generator*);
    Boolean generate(Generator*);
    Boolean generate_inlines(Generator*);
    Boolean generate_name(Generator*);
    Boolean generate_def(Generator*);
    Boolean generate_impl(Generator*);
    Boolean generate_extern_types(Generator*);
    Boolean generate_method(Generator*);
    Boolean generate_params(Generator*);
    Boolean generate_request(Generator*);
    Boolean generate_stub(Generator*);
    Boolean generate_extern_stubs(Generator*);
    Boolean generate_types(Generator*);
    Boolean generate_marshal(Generator*);
    Boolean generate_unmarshal(Generator*);
    Boolean generate_receive(Generator*);
    Boolean varying();
    SourcePosition* position();
    void set_source_position(ErrorHandler*);

    static void resolve_list(ExprList*, Resolver*);
    static Boolean generate_list(
	ExprList*, Boolean (Expr::*func)(Generator*), Generator*,
	const char* sep = nil, const char* trail = nil
    );
    static Boolean put_except_list(ExprList*, Generator*);
protected:
    Symbol* symbol_;
    enum { unknown, fixed, varies } varying_;

    virtual Boolean compute_varying();
private:
    Expr* parent_;
    SourcePosition* position_;
};

class Module;
class InterfaceDef;
class TypeName;
class Declarator;
class Constant;
class Operation;
class AttrOp;
class Parameter;
class StructDecl;
class StructMember;
class UnionDecl;
class UnionMember;
class EnumDecl;
class Enumerator;
class ExceptDecl;
class SequenceDecl;
class StringDecl;

struct Scope {
    String* name;
    Scope* outer;
    long id;
    long except_index;
};

class Symbol {
public:
    enum Tag {
	sym_unknown,
	sym_module, sym_interface, sym_typedef, sym_constant,
	sym_operation, sym_parameter, sym_attribute, sym_exception,
	sym_array, sym_struct, sym_member, sym_union, sym_union_member,
	sym_enum, sym_enum_value, sym_sequence, sym_string
    };

    Symbol(Scope*);
    Symbol(const Symbol&);
    ~Symbol();

    Tag tag() { return tag_; }
    void scope(Scope*);
    Scope* scope() { return scope_; }
    Scope* inner_scope();
    Symbol* actual_type();
    Boolean varying();
    void declared(Boolean b) { declared_ = b; }
    Boolean declared() { return declared_; }
    void declare_stub(long n) { declared_stub_ |= n; }
    Boolean declared_stub(long n) { return (declared_stub_ & n) != 0; }

    void copy_value(Symbol*);

    void module(Module*);
    Module* module();

    void interface(InterfaceDef*);
    InterfaceDef* interface();

    void typename(TypeName*);
    TypeName* typename();

    void constant(Constant*);
    Constant* constant();

    void operation(Operation*);
    Operation* operation();

    void parameter(Parameter*);
    Parameter* parameter();

    void attribute(AttrOp*);
    AttrOp* attribute();

    void array(Declarator*);
    Declarator* array();

    void struct_tag(StructDecl*);
    StructDecl* struct_tag();

    void struct_member(StructMember*);
    StructMember* struct_member();

    void union_tag(UnionDecl*);
    UnionDecl* union_tag();

    void union_member(UnionMember*);
    UnionMember* union_member();

    void enum_tag(EnumDecl*);
    EnumDecl* enum_tag();

    void enum_value_tag(Enumerator*);
    Enumerator* enum_value_tag();

    void except_type(ExceptDecl*);
    ExceptDecl* except_type();

    void sequence_type(SequenceDecl*);
    SequenceDecl* sequence_type();

    void string_type(StringDecl*);
    StringDecl* string_type();
protected:
    Tag tag_;
    Scope* scope_;
    Boolean declared_;
    long declared_stub_;
    union {
	Module* module_;
	InterfaceDef* interface_;
	TypeName* type_;
	Constant* constant_;
	Operation* operation_;
	AttrOp* attribute_;
	Parameter* parameter_;
	Declarator* array_;
	StructDecl* struct_tag_;
	StructMember* struct_member_;
	UnionDecl* union_tag_;
	UnionMember* union_member_;
	EnumDecl* enum_tag_;
	Enumerator* enum_value_tag_;
	ExceptDecl* except_type_;
	SequenceDecl* sequence_type_;
	StringDecl* string_type_;
    } value_;
};

class SymbolMap;

class SymbolTable {
public:
    SymbolTable();
    ~SymbolTable();

    Scope* enter_scope(String* name);
    Scope* scope();
    void leave_scope();

    void bind(IdentString*, Symbol*);
    void bind_in_scope(Scope*, IdentString*, Symbol*);
    Symbol* resolve(IdentString*);
    Symbol* resolve_in_scope(Scope*, IdentString*);

    Symbol* void_type();
    Symbol* oneway_type();
    Symbol* boolean_type();
    Symbol* char_type();
    Symbol* octet_type();
    Symbol* short_type();
    Symbol* ushort_type();
    Symbol* long_type();
    Symbol* ulong_type();
    Symbol* longlong_type();
    Symbol* ulonglong_type();
    Symbol* float_type();
    Symbol* double_type();
    Symbol* string_type();
protected:
    friend class Resolver;

    Scope* scope_;
    SymbolMap* map_;
    Symbol* void_;
    Symbol* oneway_;
    Symbol* boolean_;
    Symbol* char_;
    Symbol* octet_;
    Symbol* short_;
    Symbol* ushort_;
    Symbol* long_;
    Symbol* ulong_;
    Symbol* longlong_;
    Symbol* ulonglong_;
    Symbol* float_;
    Symbol* double_;
    Symbol* string_;
};

inline Symbol* SymbolTable::void_type() { return void_; }
inline Symbol* SymbolTable::oneway_type() { return oneway_; }
inline Symbol* SymbolTable::boolean_type() { return boolean_; }
inline Symbol* SymbolTable::char_type() { return char_; }
inline Symbol* SymbolTable::octet_type() { return octet_; }
inline Symbol* SymbolTable::short_type() { return short_; }
inline Symbol* SymbolTable::ushort_type() { return ushort_; }
inline Symbol* SymbolTable::long_type() { return long_; }
inline Symbol* SymbolTable::ulong_type() { return ulong_; }
inline Symbol* SymbolTable::longlong_type() { return longlong_; }
inline Symbol* SymbolTable::ulonglong_type() { return ulonglong_; }
inline Symbol* SymbolTable::float_type() { return float_; }
inline Symbol* SymbolTable::double_type() { return double_; }
inline Symbol* SymbolTable::string_type() { return string_; }

class IdentifierImpl : public Identifier {
public:
    IdentifierImpl(IdentString*, SourcePosition*);
    ~IdentifierImpl();

    Symbol* symbol();
    Expr* parent();
    Declarator* declarator();
    void resolve(Resolver*);
    Boolean set_source(Generator*);
    Boolean generate(Generator*);
    Boolean generate_inlines(Generator*);
    Boolean generate_name(Generator*);
    Boolean generate_def(Generator*);
    Boolean generate_impl(Generator*);
    Boolean generate_extern_types(Generator*);
    Boolean generate_method(Generator*);
    Boolean generate_params(Generator*);
    Boolean generate_request(Generator*);
    Boolean generate_stub(Generator*);
    Boolean generate_extern_stubs(Generator*);
    Boolean generate_types(Generator*);
    Boolean generate_marshal(Generator*);
    Boolean generate_unmarshal(Generator*);
    Boolean generate_receive(Generator*);
    Boolean varying();
    SourcePosition* position();
    void set_source_position(ErrorHandler*);

    IdentString* string();
protected:
    Expr* parent_;
    IdentString* value_;
    Symbol* symbol_;
    SourcePosition* position_;

    void check(Resolver*, UnionDecl*, Enumerator*);
};

struct GenSepState;

class ExprKitImpl {
private:
    friend class ExprKit;

    ErrorHandler* handler_;

    SourcePosition* pos() { return handler_->position(); }
};

/*
 * It is unfortunate to resort to macros here, but expanding and
 * possibly changing all the expression classes out by hand is a pain.
 * The same is true for the individual operation definitions.
 */

#define Expr_class(ClassName) \
class ClassName : public ExprImpl { \
public: \
    ClassName(SourcePosition*); \
    ~ClassName(); \
\
    void resolve(Resolver*); \
    Boolean generate(Generator*); \
private: \
    friend class ExprKit;

Expr_class(RootExpr)
public:
    Boolean generate_impl(Generator*);

    static Boolean put_list(
	ExprList*, Boolean (*)(Generator*, Expr*), Generator*
    );
    static Boolean put_decls(Generator*, Expr*);
    static Boolean put_inlines(Generator*, Expr*);
    static Boolean put_stubs(Generator*, Expr*);
    static Boolean put_server(Generator*, Expr*);
protected:
    ExprList* defs_;
};

Expr_class(Module)
public:
    Identifier* ident() { return ident_; }
    ExprList* defs() { return defs_; }
    Scope* block() { return block_; }

    Boolean generate_types(Generator*);
protected:
    Identifier* ident_;
    ExprList* defs_;
    Scope* block_;
};

struct InterfaceInfo {
    Scope* block;
    Boolean has_body;
    Boolean generated_decl;
    Boolean generated_body;
    long op_index;
};

struct InterfaceDefState;

Expr_class(InterfaceDef)
public:
    Identifier* ident() { return ident_; }
    Scope* block() { return info_->block; }
    InterfaceInfo* info() { return info_; }

    /*
     * Tag for stubs should be 1+ value for the last builtin type (string).
     */
    long kind() { return 15; }

    Boolean generate_name(Generator*);
    Boolean generate_impl(Generator*);
    Boolean generate_stub(Generator*);
    Boolean generate_extern_stubs(Generator*);
    Boolean generate_types(Generator*);
    Boolean put_members(long count, InterfaceDefState&);
    Boolean put_impl(InterfaceDefState*, Expr*);
    void put_forward_decl(Generator*);
    void put_hdr(Generator*);
    void put_cast_up(Generator*);
    void put_cast_down(Generator*);
    void put_release(Generator*);
    void put_stub_hdr(Generator*);
    void put_managed_hdr(Generator*);
    void put_init(Generator*);
    void put_type(Generator*);
    void put_type_dii(Generator*);
    void put_empty_type(Generator*);
    void put_trans_hdr(Generator*);
    void put_client(Generator*);
    void put_extern_stubs(Generator*);
    void put_op_stubs(Generator*);
    void put_type_stubs(Generator*);
    void put_receive(Generator*);
    void put_skeleton(Generator*, String* impl);

    void put_extern_info(Generator*);
    Boolean put_extern_types(InterfaceDefState*, Expr*);
    Boolean put_method_info(Generator*);
    Boolean put_method(InterfaceDefState*, Expr*);
    Boolean put_params(InterfaceDefState*, Expr*);
protected:
    Identifier* ident_;
    Boolean forward_;
    ExprList* supertypes_;
    ExprList* defs_;
    InterfaceInfo* info_;
    long visit_;
};

Expr_class(Accessor)
public:
    Expr* qualifier() { return qualifier_; }
    IdentString* string() { return string_; }
    Boolean generate_name(Generator*);
protected:
    Expr* qualifier_;
    IdentString* string_;
};

Expr_class(Constant)
public:
    Boolean generate_def(Generator*);
protected:
    Identifier* ident_;
    Expr* type_;
    Expr* value_;
};

Expr_class(Unary)
    Opcode op_;
    Expr* expr_;
};

Expr_class(Binary)
    Opcode op_;
    Expr* left_;
    Expr* right_;
};

Expr_class(TypeName)
public:
    String* str() { return str_; }
    String* mapping() { return mapping_; }
    void builtin(String* s, String* m, long k) {
	str_ = s; mapping_ = m; kind_ = k; type_ = nil; declarators_ = nil;
    }
    long kind() { return kind_; }
    Expr* type() { return type_; }
    ExprList* declarators() { return declarators_; }

    Boolean generate_name(Generator*);
    Boolean generate_def(Generator*);
    Boolean generate_extern_stubs(Generator*);
    Boolean generate_types(Generator*);
protected:
    Boolean compute_varying();
protected:
    String* str_;
    String* mapping_;
    long kind_;
    Expr* type_;
    ExprList* declarators_;
    Boolean seq_;
};

Expr_class(UnsignedType)
public:
    Boolean generate_name(Generator*);
protected:
    Expr* type_;
};

Expr_class(Declarator)
public:
    Identifier* ident() { return ident_; }
    Expr* element_type() { return element_type_; }
    ExprList* subscripts() { return subscripts_; }

    Declarator* declarator();
    Boolean generate_name(Generator*);
    Boolean generate_method(Generator*);
    Boolean generate_params(Generator*);
    Boolean generate_request(Generator*);
    Boolean generate_extern_stubs(Generator*);
    Boolean generate_types(Generator*);
protected:
    Identifier* ident_;
    Expr* element_type_;
    ExprList* subscripts_;
};

Expr_class(StructDecl)
public:
    Identifier* ident() { return ident_; }

    Boolean generate_name(Generator*);
    Boolean generate_def(Generator*);
    Boolean generate_extern_stubs(Generator*);
    Boolean generate_types(Generator*);
protected:
    Boolean compute_varying();
protected:
    Identifier* ident_;
    ExprList* members_;
    Scope* block_;
};

Expr_class(StructMember)
public:
    Expr* type() { return type_; }
    ExprList* declarators() { return declarators_; }

    Boolean generate_def(Generator*);
    Boolean generate_extern_stubs(Generator*);
    Boolean generate_types(Generator*);
    Boolean generate_marshal(Generator*);
    Boolean generate_unmarshal(Generator*);
protected:
    Boolean compute_varying();

    Expr* type_;
    ExprList* declarators_;
};

class CaseTable;

Expr_class(UnionDecl)
public:
    Symbol* switch_type() { return switch_type_; }
    CaseTable* cases() { return case_table_; }
    Boolean default_label() { return default_label_ != nil; }
    void default_label(Expr* e) { default_label_ = e; }

    Boolean generate_name(Generator*);
    Boolean generate_def(Generator*);
    Boolean generate_extern_stubs(Generator*);
    Boolean generate_types(Generator*);
protected:
    Boolean compute_varying();

    Identifier* ident_;
    Expr* type_;
    CaseList* cases_;
    Scope* block_;
    Symbol* switch_type_;
    CaseTable* case_table_;
    Expr* default_label_;

    Symbol* switch_type(Resolver*);
    void check_cases(Resolver*);
    void generate_access_hdr(Generator*, CaseElement*);
    void generate_access_impl(Generator*, CaseElement*);
    void generate_set_tag(Generator*, CaseElement*);
    void generate_free(Generator*, CaseElement*);
    void generate_assign(Generator*, CaseElement*);
};

Expr_class(CaseElement)
public:
    ExprList* labels() { return labels_; }
    UnionMember* element() { return element_; }
    Boolean generate_copy(Boolean (Expr::*func)(Generator*), Generator*);
protected:
    Boolean compute_varying();

    ExprList* labels_;
    UnionMember* element_;
};

Expr_class(DefaultLabel)
public:
    Boolean generate_marshal(Generator*);
    Boolean generate_unmarshal(Generator*);
protected:
    virtual Boolean label(Generator*);
};

class CaseLabel : public DefaultLabel {
public:
    CaseLabel(SourcePosition*);
    ~CaseLabel();

    void resolve(Resolver*);
    Boolean generate(Generator*);
protected:
    friend class ExprKit;

    Expr* value_;

    Boolean label(Generator*);
};

class UnionMember : public StructMember {
public:
    UnionMember(SourcePosition*);
    ~UnionMember();

    Boolean generate(Generator*);
    Boolean generate_marshal(Generator*);
    Boolean generate_unmarshal(Generator*);
};

Expr_class(EnumDecl)
public:
    long assign_value() { return enums_++; }
    long enums() { return enums_; }
protected:
    Identifier* ident_;
    ExprList* members_;
    long enums_;
};

Expr_class(Enumerator)
public:
    long value() { return value_; }
    EnumDecl* decl() { return decl_; }
protected:
    Identifier* ident_;
    EnumDecl* decl_;
    long value_;
};

Expr_class(SequenceDecl)
public:
    void name(Expr* e) { name_ = e; }
    Expr* name() { return name_; }
    Expr* type() { return type_; }

    Boolean generate_name(Generator*);
    Boolean generate_def(Generator*);
    Boolean generate_extern_stubs(Generator*);
    Boolean generate_types(Generator*);
    Boolean generate_marshal(Generator*);
    Boolean generate_unmarshal(Generator*);
protected:
    Boolean compute_varying();

    Expr* name_;
    Expr* type_;
    Expr* length_;
    long id_;
};

Expr_class(StringDecl)
public:
    Boolean generate_name(Generator*);
protected:
    Boolean compute_varying();

    Expr* length_;
};

class ExceptDecl : public StructDecl {
public:
    ExceptDecl(SourcePosition*);
    ~ExceptDecl();

    void resolve(Resolver*);
    Boolean generate(Generator*);
    Boolean generate_def(Generator*);
    Boolean generate_types(Generator*);
protected:
    friend class ExprKit;

    long index_;

    void put_init_list(Generator*, Boolean decl);
};

Expr_class(Operation)
public:
    Boolean generate_name(Generator*);
    Boolean generate_impl(Generator*);
    Boolean generate_extern_types(Generator*);
    Boolean generate_method(Generator*);
    Boolean generate_params(Generator*);
    Boolean generate_request(Generator*);
    Boolean generate_stub(Generator*);
    Boolean generate_extern_stubs(Generator*);
    Boolean generate_types(Generator*);
    Boolean generate_receive(Generator*);
    void put_skeleton(Generator*, String* impl);

    static long type_desc(Generator*, Expr*);
    static Boolean generate_marshal_func(Generator*, Expr*);
    static Boolean has_marshal(Generator*, Expr*);
    static void generate_return_value(Generator*, Expr*);
    static void generate_receive_addr(
	Generator*, long arg, Expr* type, Expr* value
    );
    static void generate_receive_asg(Generator*, Expr* type);
    static void generate_arg(Generator*, long, const char*);
protected:
    Identifier* ident_;
    long index_;
    Expr* type_;
    ExprList* params_;
    ExprList* exceptions_;
    ExprList* attributes_;
    ExprList* context_;
    InterfaceDef* interface_;
    Scope* block_;
    Boolean oneway_;
    Boolean need_indirect_;
    Boolean inline_indirect_;
    Boolean rtn_indirect_;

    void check_oneway(Resolver*);
    void compute_index(Resolver*);
    virtual void compute_indirect();
    Boolean has_marshal_funcs(Generator*);
    void generate_param_desc(Generator*);
    void generate_param_marshal(Generator*);
    void generate_param_value(Generator*);
};

class AttrOp : public Operation {
public:
    AttrOp(SourcePosition*);
    ~AttrOp();

    void resolve(Resolver*);
    Boolean generate_name(Generator*);
    void put_individual_attr(Generator*, char* param);
protected:
    friend class ExprKit;

    Boolean readonly_;

    virtual void compute_indirect();
};

Expr_class(Parameter)
public:
    ParamTag attr() { return attr_; }
    Expr* type() { return type_; }
    Declarator* declarator() { return declarator_; }

    Boolean generate_extern_types(Generator*);
    Boolean generate_params(Generator*);
    Boolean generate_request(Generator*);
    Boolean generate_stub(Generator*);
    Boolean generate_extern_stubs(Generator*);
    Boolean generate_types(Generator*);
protected:
    ParamTag attr_;
    Expr* type_;
    Declarator* declarator_;
};

Expr_class(BooleanLiteral)
    Boolean value_;
};

Expr_class(IntegerLiteral)
    long value_;

    void check(Resolver*, Symbol* switch_type);
};

Expr_class(FloatLiteral)
    double value_;
};

Expr_class(StringLiteral)
    String* value_;
};

Expr_class(CharLiteral)
    long value_;
};

Expr_class(SrcPos)
public:
    Boolean set_source(Generator*);
};

#if !defined(UNIXCPP)
#define __ExprIteratorMemberFunction(T) T##_ExprIteratorMemberFunction
#define ExprIteratorMemberFunction(T) __ExprIteratorMemberFunction(T)
#else
#define __ExprIteratorMemberFunction(T) T/**/_ExprIteratorMemberFunction
#define ExprIteratorMemberFunction(T) __ExprIteratorMemberFunction(T)
#endif

#endif
