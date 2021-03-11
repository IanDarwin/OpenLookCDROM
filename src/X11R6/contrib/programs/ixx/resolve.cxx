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
 * Resolve names and do type-checking
 */

#include "err.h"
#include "expr-impl.h"
#include "list.h"
#include "table.h"
#include "table2.h"
#include <stdio.h>

struct Context {
    Symbol* in_symbol;
    Symbol* out_symbol;
    Expr* type_expr;
};

declareList(ContextList,Context)
implementList(ContextList,Context)

declareTable(CaseTable,long,long)
implementTable(CaseTable,long,long)

class Resolver {
public:
    Resolver(Expr* superclass, SymbolTable*, ErrorHandler*);
    virtual ~Resolver();

    Expr* superclass();
    SymbolTable* symbol_table();
    Scope* enter_scope(Identifier*);
    Scope* scope();
    void leave_scope();
    void push_context(Context*);
    void push_context(Symbol*, Expr* = nil);
    void push_context();
    Context* context();
    void symbol(Symbol*);
    void pop_context();
    void bind(Identifier*, Symbol*);
    Symbol* resolve(Identifier*);
    Symbol* new_symbol(Identifier*);
    ErrorHandler* handler();
    void undefined(Expr*, String*);
    void redefined(Identifier*);
    void symerr(Expr*, String*, const char* message);
    void error(Expr*, const char* message);
private:
    Expr* superclass_;
    SymbolTable* symbols_;
    ErrorHandler* handler_;
    ContextList* contexts_;

    Symbol* true_;
    Symbol* false_;
    Symbol* builtin_type(
	const char* name, const char* mapping, long kind, Boolean enter = true
    );
};

inline Expr* Resolver::superclass() { return superclass_; }

inline unsigned long key_to_hash(String& s) { return s.hash(); }

declareTable2(SymbolMap,Scope*,IdentString,Symbol*)
implementTable2(SymbolMap,Scope*,IdentString,Symbol*)

/* class Resolver */

Resolver::Resolver(Expr* superclass, SymbolTable* s, ErrorHandler* h) {
    superclass_ = superclass;
    symbols_ = s;
    handler_ = h;
    contexts_ = new ContextList(3);
    symbols_->enter_scope(nil);
    s->void_ = builtin_type("void", "void", 1);
    s->oneway_ = builtin_type("oneway", "void", 2, false);
    s->boolean_ = builtin_type("boolean", "Boolean", 3);
    s->char_ = builtin_type("char", "Char", 4);
    s->octet_ = builtin_type("octet", "Octet", 5);
    s->short_ = builtin_type("short", "Short", 6);
    s->ushort_ = builtin_type("unsigned_short", "UShort", 7, false);
    s->long_ = builtin_type("long", "Long", 8);
    s->ulong_ = builtin_type("unsigned_long", "ULong", 9, false);
    s->longlong_ = builtin_type("longlong", "LongLong", 10);
    s->ulonglong_ = builtin_type("unsigned_longlong", "ULongLong", 11, false);
    s->float_ = builtin_type("float", "Float", 12);
    s->double_ = builtin_type("double", "Double", 13);
    s->string_ = builtin_type("string", "string", 14);
    true_ = builtin_type("TRUE", "TRUE", 1);
    false_ = builtin_type("FALSE", "FALSE", 1);
}

Resolver::~Resolver() {
    handler_->destroy();
    delete contexts_;
}

SymbolTable* Resolver::symbol_table() { return symbols_; }

Scope* Resolver::enter_scope(Identifier* i) {
    return symbols_->enter_scope(i->string());
}

Scope* Resolver::scope() { return symbols_->scope(); }
void Resolver::leave_scope() { symbols_->leave_scope(); }

void Resolver::push_context(Context* c) {
    contexts_->prepend(*c);
}

void Resolver::push_context(Symbol* s, Expr* t) {
    Context c;
    c.in_symbol = s;
    c.out_symbol = nil;
    c.type_expr = t;
    contexts_->prepend(c);
}

void Resolver::push_context() {
    Context c;
    c.in_symbol = nil;
    c.out_symbol = nil;
    c.type_expr = nil;
    contexts_->prepend(c);
}

Context* Resolver::context() {
    Context* c;
    if (contexts_->count() == 0) {
	c = nil;
    } else {
	c = &contexts_->item_ref(0);
    }
    return c;
}

void Resolver::symbol(Symbol* s) {
    if (contexts_->count() != 0) {
	contexts_->item_ref(0).out_symbol = s;
    }
}

void Resolver::pop_context() {
    ContextList* list = contexts_;
    if (list->count() != 0) {
	Symbol* s = list->item_ref(0).out_symbol;
	list->remove(0);
	if (list->count() != 0) {
	    list->item_ref(0).out_symbol = s;
	}
    }
}

void Resolver::bind(Identifier* ident, Symbol* s) {
    symbols_->bind(ident->string(), s);
}

Symbol* Resolver::resolve(Identifier* ident) {
    return symbols_->resolve(ident->string());
}

Symbol* Resolver::new_symbol(Identifier* ident) {
    Scope* block = scope();
    Symbol* s = resolve(ident);
    if (s == nil || s->scope() != block) {
	s = new Symbol(block);
	bind(ident, s);
    } else {
	redefined(ident);
    }
    return s;
}

ErrorHandler* Resolver::handler() { return handler_; }

void Resolver::undefined(Expr* e, String* s) {
    symerr(e, s, "undefined");
}

void Resolver::redefined(Identifier* i) {
    symerr(i, i->string(), "redefined");
}

void Resolver::symerr(Expr* e, String* s, const char* message) {
    ErrorHandler* err = handler_;
    e->set_source_position(err);
    err->begin_error();
    err->put_chars("Symbol \"");
    err->put_string(*s);
    err->put_chars("\" ");
    err->put_chars(message);
    err->end();
}

void Resolver::error(Expr* e, const char* message) {
    ErrorHandler* err = handler_;
    e->set_source_position(err);
    err->error(message);
}

Symbol* Resolver::builtin_type(
    const char* name, const char* mapping, long kind, Boolean enter
) {
    IdentString* u = new IdentString(name);
    TypeName* t = new TypeName(handler_->position());
    t->builtin(u, new String(mapping), kind);
    Symbol* s = new Symbol(scope());
    s->typename(t);
    if (enter) {
	symbols_->bind(u, s);
    }
    return s;
}

/*
 * Create symbol table and resolver.
 */

SymbolTable* ExprKit::symbol_table() {
    return new SymbolTable;
}

Resolver* ExprKit::resolver(const ConfigInfo& cf) {
    return new Resolver(
	(cf.superclass == nil) ?
	    nil : forward_interface(ident(new String(cf.superclass))),
	cf.symbols, impl_->handler_
    );
}

/*
 * Resolve operations for different kinds of expressions.
 */

void ExprImpl::resolve(Resolver*) { }

void ExprImpl::resolve_list(ExprList* list, Resolver* r) {
    if (list != nil) {
	for (ListItr(ExprList) i(*list); i.more(); i.next()) {
	    i.cur()->resolve(r);
	}
    }
}

void RootExpr::resolve(Resolver* r) {
    Expr* e = r->superclass();
    if (e != nil) {
	e->resolve(r);
    }
    resolve_list(defs_, r);
}

void Module::resolve(Resolver* r) {
    Symbol* s = r->new_symbol(ident_);
    s->module(this);
    symbol_ = s;
    block_ = r->enter_scope(ident_);
    resolve_list(defs_, r);
    r->leave_scope();
}

void InterfaceDef::resolve(Resolver* r) {
    Boolean new_symbol = false;
    Symbol* s = r->resolve(ident_);
    InterfaceDef* i;
    if (s == nil) {
	new_symbol = true;
	s = new Symbol(r->scope());
	r->bind(ident_, s);
	i = nil;
    } else {
	i = s->interface();
    }
    symbol_ = s;
    if (i == nil) {
	info_ = new InterfaceInfo;
	info_->block = nil;
	info_->has_body = false;
	info_->generated_decl = false;
	info_->generated_body = false;
	info_->op_index = 0;
    } else {
	info_ = i->info_;
    }
    if (forward_) {
	if (new_symbol) {
	    s->interface(this);
	} else if (i == nil) {
	    r->redefined(ident_);
	}
    } else {
	if (!new_symbol) {
	    InterfaceDef* i = s->interface();
	    if (i == nil || !i->forward_) {
		r->redefined(ident_);
	    }
	}
	/*
	 * Still need to check:
	 *    Supertypes are interfaces
	 *    No redundant supertypes
	 */
	if (supertypes_ != nil) {
	    resolve_list(supertypes_, r);
	}
	info_->has_body = true;
	s->interface(this);
	r->push_context(s);
	info_->block = r->enter_scope(ident_);
	if (defs_ != nil) {
	    resolve_list(defs_, r);
	}
	r->leave_scope();
	r->pop_context();
    }
}

void Accessor::resolve(Resolver* r) {
    Symbol* sym;
    r->push_context();
    qualifier_->resolve(r);
    sym = r->context()->out_symbol;
    r->pop_context();
    if (sym != nil) {
	Scope* s = sym->inner_scope();
	if (s == nil) {
	    r->error(this, "Accessor is not a scope");
	} else {
	    symbol_ = r->symbol_table()->resolve_in_scope(s, string_);
	    if (symbol_ == nil) {
		r->undefined(this, string_);
	    } else {
		r->symbol(symbol_);
	    }
	}
    }
}

/*
 * Still need to check here that the constant's value is appropriate
 * for the declared type.
 */

void Constant::resolve(Resolver* r) {
    Symbol* s = r->new_symbol(ident_);
    s->constant(this);
    symbol_ = s;
    type_->resolve(r);
    value_->resolve(r);
}

void Unary::resolve(Resolver* r) {
    expr_->resolve(r);
}

void Binary::resolve(Resolver* r) {
    left_->resolve(r);
    right_->resolve(r);
}

void TypeName::resolve(Resolver* r) {
    Symbol* s = new Symbol(r->scope());
    s->typename(this);
    r->push_context(s);
    type_->resolve(r);
    Context* c = r->context();
    symbol_ = c->out_symbol;
    c->in_symbol = s;
    c->type_expr = type_;
    resolve_list(declarators_, r);
    r->pop_context();
    seq_ = false;
    if (symbol_ != nil && symbol_->tag() == Symbol::sym_sequence &&
	declarators_->count() == 1
    ) {
	Expr* d = declarators_->item(0);
	/* make sure declarator isn't array */
	s = d->symbol();
	if (s != nil && s->tag() == Symbol::sym_typedef) {
	    symbol_->sequence_type()->name(d);
	    seq_ = true;
	}
    }
}

void UnsignedType::resolve(Resolver* r) {
    r->push_context();
    type_->resolve(r);
    Symbol* type = r->context()->out_symbol;
    SymbolTable* t = r->symbol_table();
    if (type == t->short_type()) {
	symbol_ = t->ushort_type();
    } else if (type == t->long_type()) {
	symbol_ = t->ulong_type();
    } else if (type == t->longlong_type()) {
	symbol_ = t->ulonglong_type();
    } else {
	r->error(type_, "Bad type for unsigned");
    }
    r->symbol(symbol_);
    r->pop_context();
}

void Declarator::resolve(Resolver* r) {
    Context* c = r->context();
    if (c == nil) {
	r->handler()->internal("no context for declarator");
    }
    Symbol* s = r->new_symbol(ident_);
    symbol_ = s;
    element_type_ = c->type_expr;
    if (subscripts_ == nil) {
	s->copy_value(c->in_symbol);
    } else {
	s->array(this);
	r->symbol(s);
    }
}

void StructDecl::resolve(Resolver* r) {
    Symbol* s = r->new_symbol(ident_);
    symbol_ = s;
    s->struct_tag(this);
    block_ = r->enter_scope(ident_);
    resolve_list(members_, r);
    r->leave_scope();
    r->symbol(s);
}

void StructMember::resolve(Resolver* r) {
    type_->resolve(r);
    Symbol* s = new Symbol(r->scope());
    s->struct_member(this);
    symbol_ = s;
    r->push_context(s, type_);
    resolve_list(declarators_, r);
    r->pop_context();
}

void UnionDecl::resolve(Resolver* r) {
    Symbol* s = r->new_symbol(ident_);
    symbol_ = s;
    switch_type_ = switch_type(r);
    if (switch_type_ != nil) {
	case_table_ = new CaseTable(20);
	default_label_ = nil;
	s->union_tag(this);
	r->push_context(s);
	/*
	 * Create two scopes because generated C++ has extra scope:
	 *	struct { ... union { ... } }
	 */
	r->enter_scope(ident_);
	block_ = r->symbol_table()->enter_scope(new String("_U"));
	for (ListItr(CaseList) c(*cases_); c.more(); c.next()) {
	    c.cur()->resolve(r);
	}
	r->leave_scope();
	r->leave_scope();
	r->symbol(s);
	r->pop_context();
	check_cases(r);
	delete case_table_;
	case_table_ = nil;
    }
}

Symbol* UnionDecl::switch_type(Resolver* r) {
    r->push_context();
    type_->resolve(r);
    Symbol* type_sym = r->context()->out_symbol;
    r->pop_context();
    if (type_sym == nil) {
	return nil;
    }
    SymbolTable* t = r->symbol_table();
    if (type_sym != t->long_type() && type_sym != t->short_type() &&
	type_sym != t->ulong_type() && type_sym != t->ushort_type() &&
	type_sym != t->char_type() && type_sym != t->boolean_type() &&
	type_sym->enum_tag() == nil
    ) {
	r->error(type_, "Bad switch type for union");
	return nil;
    }
    return type_sym;
}

void UnionDecl::check_cases(Resolver* r) {
    unsigned long count = default_label_ ? 1 : 0;
    for (TableIterator(CaseTable) i(*case_table_); i.more(); i.next()) {
	++count;
    }
    unsigned long max = ~0; // long or unsigned long
    SymbolTable* t = r->symbol_table();
    if (switch_type_ == t->boolean_type()) {
	max = 2;
    } else if (switch_type_ == t->short_type() ||
	switch_type_ == t->ushort_type()
    ) {
	max = 1 << 16;
    } else if (switch_type_ == t->char_type()) {
	max = 1 << 8;
    } else {
	EnumDecl* enum_decl = switch_type_->enum_tag();
	if (enum_decl != nil) {
	    max = enum_decl->enums();
	}
    }
    if (count > max) {
	r->error(type_, "Maximum number of case labels exceeded");
    }
}

void CaseElement::resolve(Resolver* r) {
    resolve_list(labels_, r);
    r->push_context();
    element_->resolve(r);
    symbol_ = r->context()->out_symbol;
    r->pop_context();
}

void CaseLabel::resolve(Resolver* r) {
    value_->resolve(r);
}

void DefaultLabel::resolve(Resolver* r) {
    Context* context = r->context();
    if (context != nil && context->in_symbol != nil) {
	UnionDecl* union_decl = context->in_symbol->union_tag();
	if (union_decl != nil) {
	    if (!union_decl->default_label()) {
		union_decl->default_label(this);
	    } else {
		r->error(this, "Multiple default labels in switch");
	    }
	} else {
	    r->error(this, "Default label is not inside union");
	}
    }
}

void EnumDecl::resolve(Resolver* r) {
    Symbol* s = r->new_symbol(ident_);
    symbol_ = s;
    s->enum_tag(this);
    r->push_context(s);
    enums_ = 0;
    resolve_list(members_, r);
    r->pop_context();
}

void Enumerator::resolve(Resolver* r) {
    Symbol* s = r->new_symbol(ident_);
    symbol_ = s;
    s->enum_value_tag(this);
    EnumDecl* enum_decl = r->context()->in_symbol->enum_tag();
    decl_ = enum_decl;
    value_ = enum_decl->assign_value();
}

void SequenceDecl::resolve(Resolver* r) {
    type_->resolve(r);
    if (length_ != nil) {
	/* should check that the length expr evaluates to a constant */
	length_->resolve(r);
    }
    Scope* block = r->scope();
    Symbol* s = new Symbol(block);
    s->sequence_type(this);
    r->symbol(s);
    symbol_ = s;
    id_ = block->id + 1;
    block->id = id_;
}

void StringDecl::resolve(Resolver* r) {
    Symbol* s;
    if (length_ == nil) {
	s = r->symbol_table()->string_type();
    } else {
	/* should check that the length expr evaluates to a constant */
	length_->resolve(r);
	s = new Symbol(r->scope());
	s->string_type(this);
    }
    r->symbol(s);
    symbol_ = s;
}

void ExceptDecl::resolve(Resolver* r) {
    StructDecl::resolve(r);
    symbol_->except_type(this);
    Scope* s = r->scope();
    s->except_index += 1;
    index_ = s->except_index;
}

void Operation::resolve(Resolver* r) {
    Symbol* s = r->new_symbol(ident_);
    s->operation(this);
    compute_index(r);
    r->push_context();
    type_->resolve(r);
    r->pop_context();
    symbol_ = s;
    if (params_ != nil) {
	block_ = r->enter_scope(ident_);
	resolve_list(params_, r);
	r->leave_scope();
    }
    if (exceptions_ != nil) {
	r->push_context(s);
	resolve_list(exceptions_, r);
	r->pop_context();
    }
    if (attributes_ != nil) {
	/*
	 * Right now, the only way the attributes list can be non-nil
	 * is if oneway was specified.
	 */
	oneway_ = true;
	check_oneway(r);
    }
    compute_indirect();
}

void AttrOp::resolve(Resolver* r) {
    if (params_ == nil) {
	/* get operation */
	Operation::resolve(r);
    } else {
	/* set operation -- don't rebind identifier */
	symbol_ = new Symbol(r->scope());
	compute_index(r);
	r->push_context();
	type_->resolve(r);
	r->pop_context();
	block_ = r->enter_scope(ident_);
	resolve_list(params_, r);
	r->leave_scope();
	compute_indirect();
    }
    symbol_->attribute(this);
}

/*
 * Check to make sure that an operation specified as oneway
 * doesn't try to return anything.
 */

void Operation::check_oneway(Resolver* r) {
    SymbolTable* t = r->symbol_table();
    if (type_->symbol() != t->void_type()) {
	r->error(this, "Return value must be \"void\" for oneway operation");
    } else if (params_ != nil) {
	for (ListItr(ExprList) e(*params_); e.more(); e.next()) {
	    Parameter* p = e.cur()->symbol()->parameter();
	    /* valid params_ => p != nil */
	    if (p->attr() != ExprKit::in_param) {
		ErrorHandler* err = r->handler();
		set_source_position(err);
		err->begin_error();
		err->put_chars("Parameter \"");
		err->put_string(*p->declarator()->ident()->string());
		err->put_chars("\" must be passed \"in\" to oneway operation");
		err->end();
	    }
	}
    }
}

void Operation::compute_index(Resolver* r) {
    interface_ = r->context()->in_symbol->interface();
    InterfaceInfo* i = interface_->info();
    index_ = i->op_index;
    i->op_index += 1;
}

/*
 * Figure out if the operation needs an indirect form and if so,
 * whether it can be inlined.  The issue is whether the operation
 * returns an object reference or has an inout/out parameter
 * that is an object reference.  The indirect operation can be inlined
 * if all the return/inout/out object reference types have been defined.
 */

void Operation::compute_indirect() {
    need_indirect_ = false;
    rtn_indirect_ = false;
    inline_indirect_ = true;
    Symbol* t = type_->symbol();
    if (t != nil) {
	t = t->actual_type();
	rtn_indirect_ = t != nil && t->tag() == Symbol::sym_interface;
	if (rtn_indirect_) {
	    need_indirect_ = true;
	    inline_indirect_ = t->interface()->info()->has_body;
	}
	if (params_ != nil && (!need_indirect_ || inline_indirect_)) {
	    for (ListItr(ExprList) e(*params_); e.more(); e.next()) {
		Parameter* p = e.cur()->symbol()->parameter();
		/* valid params_ => p != nil */
		if (p->attr() != ExprKit::in_param) {
		    Symbol* pt = p->type()->symbol();
		    if (pt != nil) {
			pt = pt->actual_type();
			if (pt != nil && pt->tag() == Symbol::sym_interface) {
			    need_indirect_ = true;
			    if (!pt->interface()->info()->has_body) {
				inline_indirect_ = false;
				break;
			    }
			}
		    }
		}
	    }
	}
    }
}

/*
 * Always generate indirect ops for attributes with object types
 * because of C++ overloading rules.
 */

void AttrOp::compute_indirect() {
    need_indirect_ = false;
    rtn_indirect_ = false;
    inline_indirect_ = true;
    Expr* t = (params_ == nil) ? type_ : params_->item(0);
    Symbol* s = t->symbol();
    if (s != nil) {
	s = s->actual_type();
	if (s != nil) {
	    InterfaceDef* i = s->interface();
	    if (i != nil) {
		need_indirect_ = true;
		rtn_indirect_ = params_ == nil;
		inline_indirect_ = i->info()->has_body;
	    }
	}
    }
}

void Parameter::resolve(Resolver* r) {
    if (attr_ == ExprKit::err_param) {
	r->error(this, "Missing attribute for parameter");
    }
    r->push_context();
    type_->resolve(r);
    Symbol* s = r->context()->out_symbol;
    r->pop_context();
    if (declarator_ == nil) {
	r->error(this, "Missing parameter name");
    } else {
	symbol_ = new Symbol(r->scope());
	symbol_->parameter(this);
	r->push_context(s, type_);
	declarator_->resolve(r);
	r->pop_context();
    }
}

void IdentifierImpl::resolve(Resolver* r) {
    symbol_ = r->resolve(this);
    if (symbol_ == nil) {
	r->undefined(this, value_);
    } else {
	Context* c = r->context();
	if (c != nil) {
	    Symbol* s = c->in_symbol;
	    if (s != nil) {
		Symbol::Tag t = symbol_->tag();
		switch (s->tag()) {
		case Symbol::sym_union:
		    if (t == Symbol::sym_enum_value) {
			check(r, s->union_tag(), symbol_->enum_value_tag());
		    }
		    break;
		case Symbol::sym_operation:
		    /*
		     * Assume we are resolving the raises clause
		     * of an operation.
		     */
		    if (t != Symbol::sym_exception) {
			r->symerr(this, value_, "is not an exception");
		    }
		    break;
		}
	    }
	}
    }
    r->symbol(symbol_);
}

void IdentifierImpl::check(
    Resolver* r, UnionDecl* union_decl, Enumerator* enumerator
) {
    EnumDecl* enum_decl = union_decl->switch_type()->enum_tag();
    if (enum_decl != nil && enum_decl != enumerator->decl()) {
	r->error(this, "Enumerator not declared in enumeration");
	return;
    }
    long value = enumerator->value();
    CaseTable* cases = union_decl->cases();
    long existing;
    if (cases->find(existing, value)) {
	r->error(this, "Case label not unique in union");
    } else {
	cases->insert(value, value);
    }
}

void BooleanLiteral::resolve(Resolver* r) {
    Context* context = r->context();
    if (context != nil && context->in_symbol != nil) {
	UnionDecl* union_decl = context->in_symbol->union_tag();
	if (union_decl != nil) {
	    if (union_decl->switch_type() !=
		r->symbol_table()->boolean_type()
	    ) {
		r->error(this, "Boolean case label not in boolean switch");
	    } else {
		CaseTable* cases = union_decl->cases();
		long existing;
		if (cases->find(existing, value_)) {
		    r->error(this, "Case label not unique in union");
		} else {
		    cases->insert(value_, value_);
		}
	    }
	}
    }
}

void IntegerLiteral::resolve(Resolver* r) {
    Context* context = r->context();
    if (context && context->in_symbol) {
	UnionDecl* union_decl = context->in_symbol->union_tag();
	if (union_decl != nil) {
	    check(r, union_decl->switch_type());
	    CaseTable* cases = union_decl->cases();
	    long existing;
	    if (cases->find(existing, value_)) {
		r->error(this, "Case label not unique in union");
	    } else {
		cases->insert(value_, value_);
	    }
	}
    }
}

void IntegerLiteral::check(Resolver* r, Symbol* switch_type) {
    long max, min;
    Boolean check_range = false;
    SymbolTable* t = r->symbol_table();
    if (switch_type == t->short_type()) {
	min = -(1 << 15);
	max = (1 << 15) - 1;
	check_range = true;
    } else if (switch_type == t->ushort_type()) {
	min = 0;
	max = (1 << 16) - 1;
	check_range = true;
    } else if (switch_type == t->boolean_type()) {
	r->error(this, "Boolean switch cannot take integer cases");
    } else if (switch_type == t->char_type()) {
	r->error(this, "Char switch cannot take integer cases");
    }
    if (check_range && (value_ < min || value_ > max)) {
	r->error(this, "Case label value out of range for switch type");
    }
}

void FloatLiteral::resolve(Resolver* r) {
    Context* context = r->context();
    if (context != nil && context->in_symbol != nil) {
	UnionDecl* union_decl = context->in_symbol->union_tag();
	if (union_decl != nil) {
	    r->error(this, "Float literal as case label");
	}
    }
}

void StringLiteral::resolve(Resolver* r) {
    Context* context = r->context();
    if (context != nil && context->in_symbol != nil) {
	UnionDecl* union_decl = context->in_symbol->union_tag();
	if (union_decl != nil) {
	    r->error(this, "String literal as case label");
	}
    }
}

void CharLiteral::resolve(Resolver* r) {
    Context* context = r->context();
    if (context != nil && context->in_symbol != nil) {
	UnionDecl* union_decl = context->in_symbol->union_tag();
	if (union_decl != nil) {
	    if (union_decl->switch_type() != r->symbol_table()->char_type()) {
		r->error(this, "Character literal not in char switch type");
		return;
	    }
	    CaseTable* cases = union_decl->cases();
	    long existing;
	    if (cases->find(existing, value_)) {
		r->error(this, "Case label not unique in union");
	    } else {
		cases->insert(value_, value_);
	    }
	}
    }
}

void SrcPos::resolve(Resolver*) { }

/* class SymbolTable */

SymbolTable::SymbolTable() {
    map_ = new SymbolMap(500);
    scope_ = nil;
}

SymbolTable::~SymbolTable() {
    for (Table2Iterator(SymbolMap) i(*map_); i.more(); i.next()) {
	Symbol* s = i.cur_value();
	delete s;
    }
    delete map_;
}

Scope* SymbolTable::enter_scope(String* name) {
    Scope* s = new Scope;
    s->name = name;
    s->outer = scope_;
    s->id = 0;
    s->except_index = 0;
    scope_ = s;
    return s;
}

Scope* SymbolTable::scope() { return scope_; }

void SymbolTable::leave_scope() {
    Scope* s = scope_;
    if (s != nil) {
	scope_ = s->outer;
    }
}

void SymbolTable::bind(IdentString* str, Symbol* sym) {
    bind_in_scope(scope_, str, sym);
}

void SymbolTable::bind_in_scope(Scope* s, IdentString* str, Symbol* sym) {
    sym->scope(s);
    map_->insert(s, *str, sym);
}

Symbol* SymbolTable::resolve(IdentString* str) {
    return resolve_in_scope(scope_, str);
}

Symbol* SymbolTable::resolve_in_scope(Scope* s, IdentString* str) {
    Symbol* sym;
    for (Scope* b = s; b != nil; b = b->outer) {
	if (map_->find(sym, b, *str)) {
	    return sym;
	}
    }
    return nil;
}

/* class Symbol */

Symbol::Symbol(Scope* s) {
    tag_ = sym_unknown;
    scope_ = s;
    declared_ = false;
    declared_stub_ = 0;
    value_.interface_ = nil;
}

Symbol::Symbol(const Symbol& s) {
    tag_ = s.tag_;
    scope_ = s.scope_;
    declared_ = false;
    value_.interface_ = s.value_.interface_;
}

Symbol::~Symbol() { }

void Symbol::scope(Scope* s) { scope_ = s; }

Scope* Symbol::inner_scope() {
    switch (tag_) {
    case sym_module:
	return value_.module_->block();
    case sym_interface:
	return value_.interface_->block();
    }
    return nil;
}

Symbol* Symbol::actual_type() {
    Symbol* s = this;
    for (;;) {
	Expr* t;
	switch (s->tag_) {
	case Symbol::sym_typedef:
	    t = s->typename();
	    break;
	case Symbol::sym_member:
	    t = s->struct_member()->type();
	    break;
	case Symbol::sym_union_member:
	    t = s->union_member()->type();
	    break;
	case Symbol::sym_parameter:
	    t = s->parameter()->type();
	    break;
	default:
	    t = nil;
	    break;
	}
	if (t == nil) {
	    break;
	}
	Symbol* ns = t->symbol();
	if (ns == nil) {
	    break;
	}
	s = ns;
    }
    return s;
}

void Symbol::copy_value(Symbol* s) {
    if (s != nil) {
	tag_ = s->tag_;
	scope_ = s->scope_;
	value_ = s->value_;
    }
}

void Symbol::module(Module* m) {
    tag_ = sym_module;
    value_.module_ = m;
}

Module* Symbol::module() {
    return tag_ == sym_module ? value_.module_ : nil;
}

void Symbol::interface(InterfaceDef* i) {
    tag_ = sym_interface;
    value_.interface_ = i;
}

InterfaceDef* Symbol::interface() {
    return tag_ == sym_interface ? value_.interface_ : nil;
}

void Symbol::typename(TypeName* t) {
    tag_ = sym_typedef;
    value_.type_ = t;
}

TypeName* Symbol::typename() {
    return tag_ == sym_typedef ? value_.type_ : nil;
}

void Symbol::constant(Constant* c) {
    tag_ = sym_constant;
    value_.constant_ = c;
}

Constant* Symbol::constant() {
    return tag_ == sym_constant ? value_.constant_ : nil;
}

void Symbol::operation(Operation* o) {
    tag_ = sym_operation;
    value_.operation_ = o;
}

Operation* Symbol::operation() {
    return tag_ == sym_operation ? value_.operation_ : nil;
}

void Symbol::parameter(Parameter* p) {
    tag_ = sym_parameter;
    value_.parameter_ = p;
}

Parameter* Symbol::parameter() {
    return tag_ == sym_parameter ? value_.parameter_ : nil;
}

void Symbol::attribute(AttrOp* a) {
    tag_ = sym_attribute;
    value_.attribute_ = a;
}

AttrOp* Symbol::attribute() {
    return tag_ == sym_attribute ? value_.attribute_ : nil;
}

void Symbol::array(Declarator* d) {
    tag_ = sym_array;
    value_.array_ = d;
}

Declarator* Symbol::array() {
    return tag_ == sym_array ? value_.array_ : nil;
}

void Symbol::struct_tag(StructDecl* s) {
    tag_ = sym_struct;
    value_.struct_tag_ = s;
}

StructDecl* Symbol::struct_tag() {
    return tag_ == sym_struct ? value_.struct_tag_ : nil;
}

void Symbol::struct_member(StructMember* m) {
    tag_ = sym_member;
    value_.struct_member_ = m;
}

StructMember* Symbol::struct_member() {
    return tag_ == sym_member ? value_.struct_member_ : nil;
}

void Symbol::union_tag(UnionDecl* u) {
    tag_ = sym_union;
    value_.union_tag_ = u;
}

UnionDecl* Symbol::union_tag() {
    return tag_ == sym_union ? value_.union_tag_ : nil;
}

void Symbol::union_member(UnionMember* m) {
    tag_ = sym_union_member;
    value_.union_member_ = m;
}

UnionMember* Symbol::union_member() {
    return tag_ == sym_union_member ? value_.union_member_ : nil;
}

void Symbol::enum_tag(EnumDecl* e) {
    tag_ = sym_enum;
    value_.enum_tag_ = e;
}

EnumDecl* Symbol::enum_tag() {
    return tag_ == sym_enum ? value_.enum_tag_ : nil;
}

void Symbol::enum_value_tag(Enumerator* e) {
    tag_ = sym_enum_value;
    value_.enum_value_tag_ = e;
}

Enumerator* Symbol::enum_value_tag() {
    return tag_ == sym_enum_value ? value_.enum_value_tag_ : nil;
}

void Symbol::except_type(ExceptDecl* e) {
    tag_ = sym_exception;
    value_.except_type_ = e;
}

ExceptDecl* Symbol::except_type() {
    return tag_ == sym_exception ? value_.except_type_ : nil;
}

void Symbol::sequence_type(SequenceDecl* s) {
    tag_ = sym_sequence;
    value_.sequence_type_ = s;
}

SequenceDecl* Symbol::sequence_type() {
    return tag_ == sym_sequence ? value_.sequence_type_ : nil;
}

void Symbol::string_type(StringDecl* s) {
    tag_ = sym_string;
    value_.string_type_ = s;
}

StringDecl* Symbol::string_type() {
    return tag_ == sym_string ? value_.string_type_ : nil;
}
