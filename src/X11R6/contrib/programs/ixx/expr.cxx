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
 * Expr - node in parse graph
 */

#include "err.h"
#include "expr-impl.h"
#include "list.h"
#include <stdio.h>

implementPtrList(ExprList,Expr)
implementPtrList(CaseList,CaseElement)

/* for parser */
extern ExprKit* yyparse_exprkit;

/* class Expr */

Expr::Expr() { }
Expr::~Expr() { }

/* class Identifier */

Identifier::Identifier() { }
Identifier::~Identifier() { }

/* class ExprKit */

ExprKit::ExprKit(ErrorHandler* h) {
    yyparse_exprkit = this;
    impl_ = new ExprKitImpl;
    impl_->handler_ = h;
}

ExprKit::~ExprKit() {
    yyparse_exprkit = nil;
}

Expr* ExprKit::root(ExprList* defs) {
    RootExpr* e = new RootExpr(impl_->pos());
    e->defs_ = defs;
    return e;
}

Expr* ExprKit::interface(
    Identifier* ident, ExprList* supertypes, ExprList* defs
) {
    InterfaceDef* e = new InterfaceDef(impl_->pos());
    e->forward_ = false;
    e->ident_ = ident;
    e->supertypes_ = supertypes;
    e->defs_ = defs;
    e->info_ = nil;
    e->visit_ = 0;
    return e;
}

Expr* ExprKit::forward_interface(Identifier* ident) {
    InterfaceDef* e = new InterfaceDef(impl_->pos());
    e->forward_ = true;
    e->ident_ = ident;
    e->supertypes_ = nil;
    e->defs_ = nil;
    e->info_ = nil;
    return e;
}

Expr* ExprKit::module(Identifier* ident, ExprList* defs) {
    Module* e = new Module(impl_->pos());
    e->ident_ = ident;
    e->defs_ = defs;
    return e;
}

Expr* ExprKit::scoped(Expr* scope, IdentString* s) {
    Accessor* e = new Accessor(impl_->pos());
    e->qualifier_ = scope;
    e->string_ = s;
    return e;
}

Expr* ExprKit::constant(Identifier* ident, Expr* type, Expr* value) {
    Constant* e = new Constant(impl_->pos());
    e->ident_ = ident;
    e->type_ = type;
    e->value_ = value;
    return e;
}

Expr* ExprKit::unary(Opcode op, Expr* expr) {
    Unary* e = new Unary(impl_->pos());
    e->op_ = op;
    e->expr_ = expr;
    return e;
}

Expr* ExprKit::binary(Opcode op, Expr* left, Expr* right) {
    Binary* e = new Binary(impl_->pos());
    e->op_ = op;
    e->left_ = left;
    e->right_ = right;
    return e;
}

Expr* ExprKit::typename(Expr* type, ExprList* declarator_list) {
    TypeName* e = new TypeName(impl_->pos());
    e->type_ = type;
    e->declarators_ = declarator_list;
    return e;
}

Expr* ExprKit::unsigned_type(Expr* type) {
    UnsignedType* e = new UnsignedType(impl_->pos());
    e->type_ = type;
    return e;
}

Expr* ExprKit::declarator(Identifier* ident, ExprList* subscripts) {
    Declarator* e = new Declarator(impl_->pos());
    e->ident_ = ident;
    e->subscripts_ = subscripts;
    return e;
}

Expr* ExprKit::struct_decl(Identifier* ident, ExprList* members) {
    StructDecl* e = new StructDecl(impl_->pos());
    e->ident_ = ident;
    e->members_ = members;
    return e;
}

Expr* ExprKit::struct_member(Expr* type, ExprList* declarator_list) {
    StructMember* e = new StructMember(impl_->pos());
    e->type_ = type;
    e->declarators_ = declarator_list;
    return e;
}

Expr* ExprKit::union_decl(
    Identifier* ident, Expr* type, CaseList* case_list
) {
    UnionDecl* e = new UnionDecl(impl_->pos());
    e->ident_ = ident;
    e->type_ = type;
    e->cases_ = case_list;
    return e;
}

CaseElement* ExprKit::case_element(
    ExprList* case_label_list, UnionMember* element
) {
    CaseElement* e = new CaseElement(impl_->pos());
    e->labels_ = case_label_list;
    e->element_ = element;
    return e;
}

Expr* ExprKit::case_label(Expr* value) {
    CaseLabel* e = new CaseLabel(impl_->pos());
    e->value_ = value;
    return e;
}

Expr* ExprKit::default_label() {
    return new DefaultLabel(impl_->pos());
}

UnionMember* ExprKit::union_member(Expr* type, Expr* declarator) {
    UnionMember* e = new UnionMember(impl_->pos());
    e->type_ = type;
    e->declarators_ = new ExprList;
    e->declarators_->append(declarator);
    return e;
}

Expr* ExprKit::enum_decl(Identifier* ident, ExprList* members) {
    EnumDecl* e = new EnumDecl(impl_->pos());
    e->ident_ = ident;
    e->members_ = members;
    return e;
}

Expr* ExprKit::enumerator(Identifier* ident) {
    Enumerator* e = new Enumerator(impl_->pos());
    e->ident_ = ident;
    return e;
}

Expr* ExprKit::sequence_decl(Expr* type, Expr* length) {
    SequenceDecl* e = new SequenceDecl(impl_->pos());
    e->name_ = nil;
    e->type_ = type;
    e->length_ = length;
    return e;
}

Expr* ExprKit::string_decl(Expr* length) {
    StringDecl* e = new StringDecl(impl_->pos());
    e->length_ = length;
    return e;
}

ExprList* ExprKit::attr_decl(
    Boolean readonly, Expr* type, ExprList* declarator_list
) {
    SourcePosition* p = impl_->pos();
    ExprList* list = new ExprList;
    for (ListItr(ExprList) e(*declarator_list); e.more(); e.next()) {
	Declarator* d = e.cur()->declarator();
	if (d != nil) {
	    Expr* t = type;
	    if (d->subscripts() != nil) {
		ExprList* dlist = new ExprList;
		dlist->append(declarator(nil, d->subscripts()));
		t = typename(t, dlist);
	    }
	    AttrOp* get = new AttrOp(p);
	    get->ident_ = d->ident();
	    get->type_ = t;
	    get->params_ = nil;
	    get->exceptions_ = nil;
	    get->attributes_ = nil;
	    get->context_ = nil;
	    get->block_ = nil;
	    get->readonly_ = readonly;
	    list->append(get);
	    if (!readonly) {
		AttrOp* set = new AttrOp(p);
		Identifier* param_name = ident(new IdentString("_p"));
		set->ident_ = d->ident();
		set->type_ = ident(new IdentString("void"));
		set->params_ = new ExprList;
		set->params_->append(
		    parameter(in_param, type, param_name, d->subscripts())
		);
		set->exceptions_ = nil;
		set->attributes_ = nil;
		set->context_ = nil;
		set->block_ = nil;
		set->readonly_ = readonly;
		list->append(set);
	    }
	}
    }
    return list;
}

Expr* ExprKit::except_decl(Identifier* i, ExprList* members) {
    ExceptDecl* e = new ExceptDecl(impl_->pos());
    e->ident_ = i;
    e->members_ = members;
    e->index_ = 0;
    return e;
}

Expr* ExprKit::operation(
    Identifier* ident, Expr* type, ExprList* params,
    ExprList* exceptions, ExprList* attributes, ExprList* context
) {
    Operation* e = new Operation(impl_->pos());
    e->ident_ = ident;
    e->type_ = type;
    e->params_ = params;
    e->exceptions_ = exceptions;
    e->attributes_ = attributes;
    e->context_ = context;
    e->block_ = nil;
    return e;
}

Expr* ExprKit::parameter(
    ParamTag attr, Expr* type, Identifier* ident, ExprList* subscripts
) {
    Parameter* e = new Parameter(impl_->pos());
    e->attr_ = attr;
    e->type_ = type;
    Declarator* d;
    if (ident == nil) {
	d = nil;
    } else {
	d = new Declarator(impl_->pos());
	d->ident_ = ident;
	d->subscripts_ = subscripts;
    }
    e->declarator_ = d;
    return e;
}

Identifier* ExprKit::ident(IdentString* s) {
    return new IdentifierImpl(s, impl_->pos());
}

Expr* ExprKit::boolean_literal(Boolean b) {
    BooleanLiteral* e = new BooleanLiteral(impl_->pos());
    e->value_ = b;
    return e;
}

Expr* ExprKit::integer_literal(long n) {
    IntegerLiteral* e = new IntegerLiteral(impl_->pos());
    e->value_ = n;
    return e;
}

Expr* ExprKit::float_literal(double d) {
    FloatLiteral* e = new FloatLiteral(impl_->pos());
    e->value_ = d;
    return e;
}

Expr* ExprKit::string_literal(String* s) {
    StringLiteral* e = new StringLiteral(impl_->pos());
    e->value_ = s;
    return e;
}

Expr* ExprKit::char_literal(long n) {
    CharLiteral* e = new CharLiteral(impl_->pos());
    e->value_ = n;
    return e;
}

ExprList* ExprKit::exprlist() {
    return new ExprList;
}

CaseList* ExprKit::caselist() {
    return new CaseList;
}

Expr* ExprKit::position(SourcePosition* p) {
    return new SrcPos(p);
}

/* class ExprImpl */

ExprImpl::ExprImpl(SourcePosition* p) {
    parent_ = nil;
    symbol_ = nil;
    varying_ = ExprImpl::unknown;
    position_ = p->clone();
}

ExprImpl::~ExprImpl() {
    position_->unref();
}

Expr* ExprImpl::parent() { return parent_; }
Symbol* ExprImpl::symbol() { return symbol_; }

Boolean ExprImpl::varying() {
    if (varying_ == ExprImpl::unknown) {
	varying_ = compute_varying() ? ExprImpl::varies : ExprImpl::fixed;
    }
    return varying_ == ExprImpl::varies;
}

Boolean ExprImpl::compute_varying() { return false; }

SourcePosition* ExprImpl::position() { return position_; }

void ExprImpl::set_source_position(ErrorHandler* h) {
    if (position_ != nil) {
	position_->ref();
	h->position(position_);
    }
}

/* class IdentifierImpl */

IdentifierImpl::IdentifierImpl(IdentString* s, SourcePosition* p) {
    parent_ = nil;
    value_ = s;
    symbol_ = nil;
    position_ = p->clone();
}

IdentifierImpl::~IdentifierImpl() {
    position_->unref();
}

Expr* IdentifierImpl::parent() { return parent_; }
Symbol* IdentifierImpl::symbol() { return symbol_; }
SourcePosition* IdentifierImpl::position() { return position_; }

void IdentifierImpl::set_source_position(ErrorHandler* h) {
    if (position_ != nil) {
	position_->ref();
	h->position(position_);
    }
}

IdentString* IdentifierImpl::string() { return value_; }

/* constructors and destructors for different kinds of expressions */

RootExpr::RootExpr(SourcePosition* p) : ExprImpl(p) { }
RootExpr::~RootExpr() { }

Module::Module(SourcePosition* p) : ExprImpl(p) { }
Module::~Module() { }

InterfaceDef::InterfaceDef(SourcePosition* p) : ExprImpl(p) { }
InterfaceDef::~InterfaceDef() { }

Accessor::Accessor(SourcePosition* p) : ExprImpl(p) { }
Accessor::~Accessor() { }

Constant::Constant(SourcePosition* p) : ExprImpl(p) { }
Constant::~Constant() { }

Unary::Unary(SourcePosition* p) : ExprImpl(p) { }
Unary::~Unary() { }

Binary::Binary(SourcePosition* p) : ExprImpl(p) { }
Binary::~Binary() { }

TypeName::TypeName(SourcePosition* p) : ExprImpl(p) { }
TypeName::~TypeName() { }

UnsignedType::UnsignedType(SourcePosition* p) : ExprImpl(p) { }
UnsignedType::~UnsignedType() { }

Declarator::Declarator(SourcePosition* p) : ExprImpl(p) { }
Declarator::~Declarator() { }

StructDecl::StructDecl(SourcePosition* p) : ExprImpl(p) { }
StructDecl::~StructDecl() { }

StructMember::StructMember(SourcePosition* p) : ExprImpl(p) { }
StructMember::~StructMember() { }

UnionDecl::UnionDecl(SourcePosition* p) : ExprImpl(p) { }
UnionDecl::~UnionDecl() { }

CaseElement::CaseElement(SourcePosition* p) : ExprImpl(p) { }
CaseElement::~CaseElement() { }

CaseLabel::CaseLabel(SourcePosition* p) : DefaultLabel(p) { }
CaseLabel::~CaseLabel() { }

DefaultLabel::DefaultLabel(SourcePosition* p) : ExprImpl(p) { }
DefaultLabel::~DefaultLabel() { }

UnionMember::UnionMember(SourcePosition* p) : StructMember(p) { }
UnionMember::~UnionMember() { }

EnumDecl::EnumDecl(SourcePosition* p) : ExprImpl(p) { }
EnumDecl::~EnumDecl() { }

Enumerator::Enumerator(SourcePosition* p) : ExprImpl(p) { }
Enumerator::~Enumerator() { }

SequenceDecl::SequenceDecl(SourcePosition* p) : ExprImpl(p) { }
SequenceDecl::~SequenceDecl() { }

StringDecl::StringDecl(SourcePosition* p) : ExprImpl(p) { }
StringDecl::~StringDecl() { }

ExceptDecl::ExceptDecl(SourcePosition* p) : StructDecl(p) { }
ExceptDecl::~ExceptDecl() { }

Operation::Operation(SourcePosition* p) : ExprImpl(p) { }
Operation::~Operation() { }

AttrOp::AttrOp(SourcePosition* p) : Operation(p) { }
AttrOp::~AttrOp() { }

Parameter::Parameter(SourcePosition* p) : ExprImpl(p) { }
Parameter::~Parameter() { }

BooleanLiteral::BooleanLiteral(SourcePosition* p) : ExprImpl(p) { }
BooleanLiteral::~BooleanLiteral() { }

IntegerLiteral::IntegerLiteral(SourcePosition* p) : ExprImpl(p) { }
IntegerLiteral::~IntegerLiteral() { }

FloatLiteral::FloatLiteral(SourcePosition* p) : ExprImpl(p) { }
FloatLiteral::~FloatLiteral() { }

StringLiteral::StringLiteral(SourcePosition* p) : ExprImpl(p) { }
StringLiteral::~StringLiteral() { }

CharLiteral::CharLiteral(SourcePosition* p) : ExprImpl(p) { }
CharLiteral::~CharLiteral() { }

SrcPos::SrcPos(SourcePosition* p) : ExprImpl(p) { }
SrcPos::~SrcPos() { }

/*
 * We want to know whether a struct, union, or sequence contains
 * pointers that might need freeing.
 */

Boolean Symbol::varying() {
    Expr* e = nil;
    switch (tag_) {
    case Symbol::sym_typedef:
	e = value_.type_;
	break;
    case Symbol::sym_struct:
	e = value_.struct_tag_;
	break;
    case Symbol::sym_union:
	e = value_.union_tag_;
	break;
    case Symbol::sym_sequence:
    case Symbol::sym_string:
	return true;
    }
    return e != nil && e->varying();
}

Boolean IdentifierImpl::varying() {
    return symbol_ != nil && symbol_->varying();
}

Boolean StructDecl::compute_varying() {
    if (members_ != nil) {
	for (ListItr(ExprList) i(*members_); i.more(); i.next()) {
	    if (i.cur()->varying()) {
		return true;
	    }
	}
    }
    return false;
}

Boolean UnionDecl::compute_varying() {
    for (ListItr(CaseList) i(*cases_); i.more(); i.next()) {
	if (i.cur()->varying()) {
	    return true;
	}
    }
    return false;
}

Boolean TypeName::compute_varying() {
    return type_ != nil && type_->varying();
}

Boolean StructMember::compute_varying() { return type_->varying(); }
Boolean CaseElement::compute_varying() { return element_->varying(); }
Boolean SequenceDecl::compute_varying() { return true; }
Boolean StringDecl::compute_varying() { return true; }

/*
 * Need to access declarators from attribute lists.
 */

Declarator* ExprImpl::declarator() { return nil; }
Declarator* IdentifierImpl::declarator() { return nil; }
Declarator* Declarator::declarator() { return this; }
