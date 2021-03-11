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
 * Generate code
 */

#include "generator.h"
#include <ctype.h>

/*
 * Filtering
 */

class Filter {
public:
    void run(Generator* g);
private:
    Generator* generator_;
    char buf_[1024];
    char* line_;
    char* slash_;
    long lineno_;
    long counter_;

    ErrorHandler* handler();
    Boolean get_line();
    void scan();
    Boolean parse(char* pos);
    Boolean get_scope(Symbol*& sym, Scope*& scope, char*& name);
    ExprList* get_classes(ExprList* list, char* start);
    Boolean put_sym(String* name, Symbol*, String* impl, char*);
    Boolean put_members(Symbol*, String* impl, char*);
    Boolean put_special(Symbol*, String* impl, char*);
    void put_special_command(InterfaceDef*, char* start, long length);
    Boolean put_skeleton(Symbol*, String* impl, char*);
    void put_new_impl(char* name, char* p);
    void put_op(Operation*, String* impl);
    void put_op_interface(Operation*);
    void put_op_impl(Operation*, String*);
    void put_interface(InterfaceDef*, String*, char*);
    void put_sequence(SequenceDecl*);
    void put_attr_interface(AttrOp*, char* param);
    void put_attr_impl(AttrOp*, char* param, String* impl);
    char* find_after();
    void find_next_annotation();
};

void Filter::run(Generator* g) {
    generator_ = g;
    line_ = buf_;
    lineno_ = 0;
    counter_ = 0;
    while (get_line()) {
	scan();
    }
    g->flush();
}

/*
 * Return the generator's handler, first setting the source position
 * to the current line.
 */

ErrorHandler* Filter::handler() {
    ErrorHandler* h = generator_->handler();
    SourcePosition* p = h->position();
    if (p != nil) {
	p->set(nil, lineno_);
    }
    return h;
}

/*
 * Read a line from standard input.  Return false when EOF is reached.
 * If the given buffer + size is too small, then a larger one
 * will be allocated.  The stored line is guaranteed to be null-terminated.
 */

Boolean Filter::get_line() {
    ++lineno_;
    if (line_ != buf_) {
	delete line_;
	line_ = buf_;
    }
    int c;
    int n = sizeof(buf_);
    char* bufend = &line_[n - 2];
    char* p = line_;
    for (c = getchar(); c != '\n' && c != EOF; c = getchar()) {
	if (p >= bufend) {
	    int new_n = n + n;
	    char* new_line = new char[new_n];
	    Memory::copy(line_, new_line, n);
	    if (line_ != buf_) {
		delete line_;
	    }
	    line_ = new_line;
	    p = &line_[n];
	    n = new_n;
	    bufend = &line_[n - 2];
	}
	*p++ = c;
    }
    if (c == '\n') {
	*p++ = '\n';
    } else if (p == line_) {
	return false;
    }
    *p = '\0';
    return true;
}

void Filter::scan() {
    Generator* g = generator_;
    char* slash = nil;
    char* slash_slash = nil;
    char* p;
    for (p = line_; *p != '\0'; p++) {
	int c = *p;
	if (c == '/') {
	    if (slash == nil) {
		slash = p;
	    } else {
		slash_slash = p;
	    }
	} else {
	    if (c == '+' && slash_slash != nil && parse(slash)) {
		return;
	    }
	    slash = nil;
	    slash_slash = nil;
	}
    }
    g->copy(line_);
}

/*
 * Parse the characters after "//+ ".  The possibilities include:
 *
 *	Interface::Operation - an operation defined in Interface
 *	Interface::Attribute=v - set an attribute to a parameter named "v"
 *	Interface::Attribute? - get an attribute value
 *	Interface - define the default operations for Interface
 *	Interface::= - define all the operations for Interface
 *	Interface::* - define all possible operations (including inherited)
 *	Interface::@ - define marshalling operations for types
 *	Interface::% - define some operations for a type
 *
 * Multiple "Scope::" prefixes are allowed for module nesting.
 * The syntax also allows Implementation(...), where Implementation
 * is the name of an implementation class and the "..." is the same
 * as above.  Additionally, when specifying an implementation, one can
 * have Implementation(Interface::*Impl) to inherit all Interface
 * operations from the Impl class.
 */

Boolean Filter::parse(char* start) {
    Boolean b = false;
    slash_ = start;
    char* p = start + 3;
    for (; isspace(*p); p++);

    String* impl = nil;
    char* name = p;
    for (; isalnum(*p) || *p == '_'; p++);
    if (p > name) {
	if (p[0] == '(') {
	    impl = new CopyString(name, p - name);
	    name = p + 1;
	} else if (p[0] == ' ' && p[1] == ':' && p[2] == ' ') {
	    put_new_impl(name, p);
	    return true;
	}
    }

    Generator* g = generator_;
    ErrorHandler* err = handler();
    SymbolTable* symbols = g->symbol_table();
    Scope* scope = symbols->scope();
    Symbol* sym = nil;
    while (get_scope(sym, scope, name));
    p = name;
    switch (*p) {
    case '=':
    case '*':
	b = put_members(sym, impl, p);
	break;
    case '%':
	b = put_special(sym, impl, p + 1);
	break;
    case '$':
	b = put_skeleton(sym, impl, p + 1);
	break;
    default:
	for (; isalnum(*p) || *p == '_'; p++);
	if (p != name) {
	    IdentString* u = new IdentString(name, p - name);
	    Symbol* sym = symbols->resolve_in_scope(scope, u);
	    if (sym != nil) {
		b = put_sym(u, sym, impl, p);
	    } else if (impl != nil) {
		if (*p != ')' && *p != ',') {
		    err->warning("Ignoring filter annotation");
		} else {
		    put_interface(nil, impl, name - 1);
		    b = true;
		}
	    } else {
		err->warning("Unrecognized filter annotation");
	    }
	    delete u;
	} else {
	    err->warning("Unrecognized filter annotation");
	}
	break;
    }
    return b;
}

/*
 * Read a list of classes, normally supertypes.
 */

ExprList* Filter::get_classes(ExprList* list, char* start) {
    ExprList* supertypes = (list == nil) ? new ExprList : list;
    char* p = start;
    char* base = start;
    for (;;) {
	for (; *p == ' ' || *p == '\t'; p++);
	base = p;
	for (; isalnum(*p) || *p == '_'; p++);
	if (p > base) {
	    supertypes->append(
		new IdentifierImpl(
		    new IdentString(base, p - base), handler()->position()
		)
	    );
	}
	if (*p == '\n' || *p == ')') {
	    break;
	}
	if (*p != ',') {
	    handler()->unrecoverable("Badly formed base class list");
	    /* should not reach here */
	    break;
	}
	++p;
    }
    return supertypes;
}

/*
 * Parse a scope of the form A::B::C::name, determining C's symbol and scope.
 * Return false if the parse is unsuccessful.  In any event, set start
 * to after the successfully-parsed characters.
 */

Boolean Filter::get_scope(Symbol*& sym, Scope*& scope, char*& start) {
    char* name = start;
    char* p = name;
    for (; isalnum(*p) || *p == '_'; p++);
    if (p != name && p[0] == ':' && p[1] == ':') {
	SymbolTable* symbols = generator_->symbol_table();
	IdentString* u = new IdentString(name, p - name);
	sym = symbols->resolve_in_scope(scope, u);
	delete u;
	if (sym != nil) {
	    Scope* new_scope = sym->inner_scope();
	    if (new_scope != nil) {
		scope = new_scope;
		start = p + 2;
		return true;
	    }
	}
    }
    return false;
}

/*
 * Generate filtered information for a symbol, choosing the appropriate
 * function to call based on the symbol's subtype.
 */

Boolean Filter::put_sym(String*, Symbol* sym, String* impl, char* p) {
    Symbol::Tag t = sym->tag();
    if (t == Symbol::sym_attribute) {
	AttrOp* a = sym->attribute();
	long ch = *(unsigned char*)p;
	char* param;
	if (ch == '=') {
	    param = p + 1;
	} else if (ch == '?') {
	    param = nil;
	} else {
	    return false;
	}
	if (impl == nil) {
	    put_attr_interface(a, param);
	} else {
	    put_attr_impl(a, param, impl);
	}
	return true;
    }
    if (*p != '\n' && (impl == nil || (*p != ',' && *p != ')'))) {
	handler()->warning("Ignoring filter annotation");
	return false;
    }

    Expr* e = nil;
    switch (t) {
    case Symbol::sym_operation:
	put_op(sym->operation(), impl);
	break;
    case Symbol::sym_interface:
	put_interface(sym->interface(), impl, p);
	break;
    case Symbol::sym_module:
	e = sym->module();
	break;
    case Symbol::sym_typedef:
	e = sym->typename();
	break;
    case Symbol::sym_struct:
	e = sym->struct_tag();
	break;
    case Symbol::sym_union:
	e = sym->union_tag();
	break;
    case Symbol::sym_sequence:
	e = sym->sequence_type();
	break;
    default:
	handler()->warning("No filtering for symbol");
	return false;
    }
    if (e != nil) {
	Generator* g = generator_;
	g->copy(slash_);
	e->generate_extern_stubs(g);
	e->generate_types(g);
	find_next_annotation();
	g->copy(line_);
    }
    return true;
}

/*
 * Generate filtered information for all the members of an interface.
 */

Boolean Filter::put_members(Symbol* sym, String* impl, char* p) {
    Generator* g = generator_;
    g->copy(line_);
    InterfaceDef* i = sym->interface();
    if (i == nil) {
	handler()->warning("Ignoring filter annotation (not an interface)");
    } else {
	InterfaceDefState s;
	s.generator = g;
	s.impl = impl;
	s.name = nil;
	s.func = &InterfaceDef::put_impl;
	if (impl != nil) {
	    char* start = p + 1;
	    for (char* pp = start; *pp != ')' && *pp != '\n'; pp++);
	    if (pp != start) {
		s.name = new String(start, pp - start);
	    }
	}
	i->put_members(*p == '=' ? 0 : ++counter_, s);
	delete s.name;
    }
    find_next_annotation();
    g->copy(line_);
    return true;
}

/*
 * Put out part of the interface information, as specified
 * by the string after the "%".
 */

Boolean Filter::put_special(Symbol* sym, String* impl, char* p) {
    Generator* g = generator_;
    g->copy(line_);
    InterfaceDef* i = (sym == nil) ? nil : sym->interface();
    if (i == nil) {
	handler()->error("Unknown interface");
    } else if (impl != nil) {
	handler()->error("Unexpected impl name for special annotation");
    } else {
	char* start = p;
	char* q = p;
	for (; *q != '\0' && *q != '\n'; q++) {
	    if (*q == ',') {
		put_special_command(i, start, q - start);
		start = q + 1;
	    }
	}
	put_special_command(i, start, q - start);
    }
    find_next_annotation();
    g->copy(line_);
    return true;
}

struct CommandInfo {
    char* command;
    void (InterfaceDef::*func)(Generator*);
};

static CommandInfo special[] = {
    { "init", &InterfaceDef::put_init },
    { "type", &InterfaceDef::put_type },
    { "type+dii", &InterfaceDef::put_type_dii },
    { "!type", &InterfaceDef::put_empty_type },
    { "extern", &InterfaceDef::put_trans_hdr },
    { "client", &InterfaceDef::put_client },
    { "stub-externs", &InterfaceDef::put_extern_stubs },
    { "stubs", &InterfaceDef::put_op_stubs },
    { "type-stubs", &InterfaceDef::put_type_stubs },
    { "server", &InterfaceDef::put_receive },
    { nil }
};

void Filter::put_special_command(InterfaceDef* i, char* p, long length) {
    String str(p, length);
    Generator* g = generator_;
    ErrorHandler* err = handler();
    if (str.length() == 0) {
	err->error("Missing name for special annotation");
    } else {
	void (InterfaceDef::*func)(Generator*) = nil;
	for (CommandInfo* c = special; c->command != nil; c++) {
	    if (str == c->command) {
		func = c->func;
		break;
	    }
	}
	if (func != nil) {
	    (i->*func)(g);
	} else {
	    err->error("Unrecognized special annotation");
	}
    }
}

/*
 * Expand a skeleton set of operations for an interface.
 * Unlike most filtering operations, this is one-time only.
 */

Boolean Filter::put_skeleton(Symbol* sym, String* impl, char* p) {
    InterfaceDef* i = (sym == nil) ? nil : sym->interface();
    if (i == nil) {
	handler()->error("Unknown interface");
    } else if (impl == nil) {
	handler()->error("Missing implementation for skeleton");
    } else if (*p != ')') {
	handler()->error("Expected right parenthesis after '$'");
    } else {
	i->put_skeleton(generator_, impl);
    }
    return true;
}

/*
 * Define a new implementation type and generate appropriate
 * class definition.
 */

void Filter::put_new_impl(char* name, char* ptr) {
    Generator* g = generator_;
    g->copy(slash_);
    String* s = new String(name, ptr - name);
    g->emit("class %I : public ", s);
    ExprList* supertypes = get_classes(nil, ptr + 2);
    ExprImpl::generate_list(supertypes, &Expr::generate, g, ", public ");
    g->emit(" {\npublic:\n%i");
    g->emit("~%I();\n", s);
    if (g->metaclass() != nil) {
	g->emit("%MId _tid();\n");
    }
    if (g->superclass() != nil) {
	g->emit("static %I* _narrow(%S%p);\n", s);
    }
    g->emit("%u");
    find_next_annotation();
    g->copy(line_);
}

/*
 * Generate information for a class member function.
 */

void Filter::put_op(Operation* op, String* impl) {
    if (impl == nil) {
	put_op_interface(op);
    } else {
	put_op_impl(op, impl);
	delete impl;
    }
}

/*
 * Generate a type signature suitable for a class member function,
 * given the input "//+ Interface::op."  We need to make sure
 * to remove an old signature, if one exists.
 */

void Filter::put_op_interface(Operation* op) {
    Generator* g = generator_;
    g->emit("%i");
    op->generate_impl(g);
    g->emit("; ");
    g->copy(slash_);
    g->emit("%u");
}

/*
 * Generate a function header suitable for an implementation,
 * given the input "//+ Impl(Interface::op)."  We need to make sure
 * to remove an old header, if one exists.
 */

void Filter::put_op_impl(Operation* op, String* name) {
    Generator* g = generator_;
    g->copy(slash_);
    char* after = find_after();
    if (after[0] == ' ' && after[1] == '}') {
	g->op_body(false);
    }
    g->push_prefix(name);
    op->generate_impl(g);
    g->pop_prefix();
    g->op_body(true);
    g->emit(" {");
    if (after != nil) {
	g->copy(after);
    }
}

/*
 * Generate standard operations given the input "//+ name"
 * where name is an interface or implementation class.
 * We must remove the old operations, if they exist.
 * These are defined as all input up to a line that begins
 * with "//+".  This is dangerous, as the user could lose data
 * if the appropriate //+ is missing or deleted.  There is
 * no reasonable way around this, however, short of putting
 * everything on a single line.
 */

void Filter::put_interface(InterfaceDef* i, String* impl, char* p) {
    Generator* g = generator_;
    g->copy(slash_);
    if (impl == nil) {
	i->generate_impl(g);
    } else {
	ExprList* list = new ExprList;
	if (i != nil) {
	    list->append(i->ident());
	}
	g->emit_type_info(
	    impl, "*", get_classes(list, p + 1), false, false, true
	);
	if (g->metaclass() != nil) {
	    g->emit("%MId %I::_tid() { return %T; }\n", impl);
	}
    }
    find_next_annotation();
    g->copy(line_);
}

/*
 * Generate filtered output for a sequence, which is currently
 * just stubs.
 */

void Filter::put_sequence(SequenceDecl* s) {
    Generator* g = generator_;
    g->copy(line_);
    s->generate_stub(g);
    find_next_annotation();
    g->copy(line_);
}

/*
 * Generate filtered output for the interface to an attribute.
 */

void Filter::put_attr_interface(AttrOp* a, char* param) {
    Generator* g = generator_;
    g->emit("%i");
    a->put_individual_attr(g, param);
    g->emit("; ");
    g->copy(slash_);
    g->emit("%u");
}

/*
 * Generate filtered output for the implementation of an attribute
 * access function.
 */

void Filter::put_attr_impl(AttrOp* a, char* param, String* impl) {
    Generator* g = generator_;
    g->copy(slash_);
    g->push_prefix(impl);
    a->put_individual_attr(g, param);
    g->pop_prefix();
    g->emit(" {");
    char* after = find_after();
    if (after != nil) {
	g->copy(after);
    }
}

/*
 * Find the character after the (old) function header, which is
 * presumed to be the opening left brace.
 */

char* Filter::find_after() {
    while (get_line()) {
	for (char* p = line_; *p != '\0'; p++) {
	    if (*p == '{') {
		return p + 1;
	    }
	}
    }
    return nil;
}

/*
 * Consume input up to the next //+.
 */

void Filter::find_next_annotation() {
    char* p;
    for (;;) {
	if (!get_line()) {
	    handler()->unrecoverable("Missing //+");
	    /* shouldn't reach here */
	    break;
	}
	for (p = line_; isspace(*p); p++);
	if (p[0] == '/' && p[1] == '/' && p[2] == '+') {
	    if (p[3] != '\n') {
		handler()->unrecoverable("Missing //+");
		/* shouldn't reach here */
	    }
	    break;
	}
    }
}

/*
 * Generate part of implementation for specific node types.
 */

Boolean ExprImpl::generate_impl(Generator*) { return false; }
Boolean IdentifierImpl::generate_impl(Generator* g) { return generate(g); }

Boolean RootExpr::generate_impl(Generator* g) {
    Filter* f = new Filter;
    f->run(g);
    delete f;
    return true;
}

Boolean InterfaceDef::generate_impl(Generator* g) {
    put_init(g);
    if (g->superclass() != nil && g->metaclass() != nil) {
	g->emit("\n");
	if (g->request() != nil) {
	    put_type_dii(g);
	} else {
	    put_type(g);
	}
    }
    return true;
}

void InterfaceDef::put_init(Generator* g) {
    if (defs_ != nil) {
	g->enter_scope(info_->block);
	if (generate_list(defs_, &Expr::generate_def, g)) {
	    g->emit("\n");
	}
	g->leave_scope();
    }
    String* s = ident_->string();
    g->emit("%:%I::%I() { }\n", s);
    g->emit("%:%I::~%I() { }\n", s);
    g->emit("void* %:%I::_this() { return this; }\n", s);
    g->emit("\n");
}

void InterfaceDef::put_type(Generator* g) {
    if (g->metaclass() == nil) {
	g->handler()->error("Metaclass is undefined");
    } else {
	String* s = ident_->string();
	Boolean has_exceptions = (info_->block->except_index != 0);
	g->emit_type_info(s, "Ref", supertypes_, false, has_exceptions, true);
	g->emit("%MId %:%I::_tid() { return %T; }\n", s);
    }
}

void InterfaceDef::put_type_dii(Generator* g) {
    if (g->request() == nil) {
	g->handler()->error("Request type is undefined");
    } else if (g->metaclass() == nil) {
	g->handler()->error("Metaclass is undefined");
    } else {
	put_extern_info(g);
	put_method_info(g);
	String* s = ident_->string();
	Boolean has_exceptions = (info_->block->except_index != 0);
	g->emit_type_info(s, "Ref", supertypes_, true, has_exceptions, true);
	g->emit("%MId %:%I::_tid() { return %T; }\n", s);
	put_receive(g);
    }
}

void InterfaceDef::put_empty_type(Generator* g) {
    if (g->metaclass() != nil) {
	String* s = ident_->string();
	g->emit("%MId %:%I::_tid() { return 0; }\n", s);
    }
}

Boolean InterfaceDef::put_members(long count, InterfaceDefState& s) {
    Boolean b = false;
    if (count == 0 || count > visit_) {
	visit_ = count;
	Generator* g = s.generator;
	if (count != 0 && supertypes_ != nil) {
	    for (ListItr(ExprList) i(*supertypes_); i.more(); i.next()) {
		Symbol* sym = i.cur()->symbol();
		if (sym != nil) {
		    InterfaceDef* super = sym->interface();
		    if (super != nil) {
			b |= super->put_members(visit_, s);
		    }
		}
	    }
	}
	if (s.impl == nil) {
	    g->emit("%i");
	}
	if (count != 0) {
	    g->emit_tab();
	    g->emit("/* %I */\n", ident_->string());
	}
	if (s.impl != nil) {
	    g->impl_is_from(s.name);
	    g->push_prefix(s.impl);
	}
	if (defs_ != nil) {
	    for (ListItr(ExprList) i(*defs_); i.more(); i.next()) {
		b |= (this->*s.func)(&s, i.cur());
	    }
	}
	if (s.impl != nil) {
	    g->pop_prefix();
	    g->impl_is_from(nil);
	} else {
	    g->emit("%u");
	}
    }
    return b;
}

Boolean InterfaceDef::put_impl(InterfaceDefState* s, Expr* e) {
    Generator* g = s->generator;
    if (e->generate_impl(g) && s->impl == nil) {
	g->emit(";\n");
    }
    return true;
}

/*
 * Generate filtered output for an operation.  If there is a prefix
 * (meaning the filtering is for an implementation), then generate
 * a function header suitable for an implementation.  Otherwise,
 * generate a header for use in a class definition.
 */

Boolean Operation::generate_impl(Generator* g) {
    String* s = ident_->string();
    String* impl = g->prefix();
    String* name = g->impl_is_from();
    Boolean ref = g->interface_is_ref(false);
    g->emit(impl != nil ? "%F" : "%E", nil, type_);
    if (g->actual_type(type_)->tag() == Symbol::sym_interface) {
	g->emit("_return");
    }
    g->interface_is_ref(ref);
    g->emit(" %.%I", s);
    g->emit_param_list(
	params_, impl == nil ?
	    Generator::emit_env_formals : Generator::emit_env_formals_body
    );
    if (impl != nil && name != nil) {
	g->emit(" {\n%i");
	if (!g->void_type(type_)) {
	    g->emit("return ");
	}
	g->emit("%I", name);
	g->emit("%I", s);
	g->emit_param_list(params_, Generator::emit_env_actuals);
	g->emit(";\n%u}\n");
    }
    return true;
}

/*
 * Generate filtered output for a specific attribute.
 */

void AttrOp::put_individual_attr(Generator* g, char* param) {
    if (param != nil && readonly_) {
	g->handler()->error("Can't assign read-only attribute");
	return;
    }
    String* s = ident_->string();
    if (param != nil) {
	g->emit("void %.%I(", s);
	Boolean addr = g->addr_type(type_);
	if (addr) {
	    g->emit("const ");
	}
	if (g->actual_type(type_)->tag() == Symbol::sym_interface) {
	    Boolean ref = g->interface_is_ref(false);
	    g->emit("%X_in", nil, type_);
	    g->interface_is_ref(ref);
	} else {
	    g->emit("%X", nil, type_);
	}
	if (addr) {
	    g->emit("&");
	}
	char* p;
	for (p = param; isalnum(*p) || *p == '_'; p++);
	if (p != param) {
	    g->emit(" ");
	    g->emit_chars_length(param, p - param);
	}
	g->emit("%,%f)");
    } else {
	g->emit("%X %.%I(%f)", s, type_);
    }
}

/*
 * Generate data and routines for constants and types
 * in an interface or module.
 */

Boolean ExprImpl::generate_def(Generator*) { return false; }
Boolean IdentifierImpl::generate_def(Generator*) { return false; }

Boolean Constant::generate_def(Generator* g) {
    g->emit("const %X %:%I = ", ident_->string(), type_);
    g->emit("%X;\n", nil, value_);
    return true;
}

Boolean TypeName::generate_def(Generator* g) {
    return type_ != nil && type_->generate_def(g);
}

Boolean StructDecl::generate_def(Generator* g) {
    g->enter_scope(block_);
    Boolean b = generate_list(members_, &Expr::generate_def, g);
    g->leave_scope();
    return b;
}

Boolean StructMember::generate_def(Generator* g) {
    return type_->generate_def(g);
}

Boolean UnionDecl::generate_def(Generator* g) {
    Boolean b = false;
    g->enter_scope(block_);
    for (ListItr(CaseList) c(*cases_); c.more(); c.next()) {
	c.cur()->element()->generate_def(g);
    }
    /*
     * Need two leave scopes because unions have an extra implicit block.
     */
    g->leave_scope();
    g->leave_scope();
    if (g->varying()) {
	String* s = ident_->string();
	Boolean v = varying();
	g->emit("%:%I::%I() { ", s);
	if (v) {
	    g->emit("_u.__init_ = 0;");
	}
	g->emit("}\n");
	g->emit("void %:%I::_d(%X _nd) {\n%i", s, type_);
	if (v) {
	    g->emit("if (_nd != _d_) {\n%i");
	    g->emit("_free();\n");
	    g->emit("%u}\n");
	}
	g->emit("_d_ = _nd;\n");
	g->emit("%u}\n");

	for (ListItr(CaseList) i(*cases_); i.more(); i.next()) {
	    generate_access_impl(g, i.cur());
	}

	if (v) {
	    g->emit("void %:%I::_free() {\n%i", s);
	    g->emit("switch (_d()) {\n");
	    for (ListItr(CaseList) j(*cases_); j.more(); j.next()) {
		generate_free(g, j.cur());
	    }
	    g->emit("}\n%u}\n");
	}
	g->emit("%:%I& %:%I::operator =(const %I& _this) {\n%i", s);
	g->emit("switch (_this._d()) {\n");
	for (ListItr(CaseList) k(*cases_); k.more(); k.next()) {
	    generate_assign(g, k.cur());
	}
	g->emit("}\nreturn *this;\n%u}\n");
	b = true;
    }
    return b;
}

void UnionDecl::generate_access_impl(Generator* g, CaseElement* c) {
    String* s = ident_->string();
    UnionMember* u = c->element();
    Expr* t = u->type();
    Expr* e = u->declarators()->item(0);
    Boolean v = t->varying();

    /* set */
    g->emit("void %:%I::%E(", s, e);
    Boolean addr = g->addr_type(t);
    if (addr) {
	g->emit("const ");
    }
    g->emit("%F", nil, t);
    if (addr) {
	g->emit("&");
    }
    g->emit(" _v) {\n%i", nil, t);
    generate_set_tag(g, c);
    if (v) {
	g->emit("delete _u.%E;\n", nil, e);
	g->emit("_u.%E = new ", nil, e);
	g->emit("%F(_v);\n", nil, t);
    } else {
	g->emit("_u.%E = _v;\n", nil, e);
    }
    g->emit("%u}\n");

    /* ref */
    g->emit("%F& ", nil, t);
    g->emit("%:%I::%E() {\n%i", s, e);
    generate_set_tag(g, c);
    g->emit("return ");
    if (v) {
	g->emit("*");
    }
    g->emit("_u.%E;\n%u}\n", nil, e);
}

void UnionDecl::generate_set_tag(Generator* g, CaseElement* c) {
    ExprList* list = c->labels();
    if (list->count() == 1) {
	Expr* e = list->item(0);
	if (e != default_label_) {
	    g->emit("_d_ = %E;\n", nil, e);
	}
    }
}

void UnionDecl::generate_free(Generator* g, CaseElement* c) {
    UnionMember* u = c->element();
    Expr* t = u->type();
    Expr* e = u->declarators()->item(0);
    if (t->varying()) {
	for (ListItr(ExprList) i(*c->labels()); i.more(); i.next()) {
	    i.cur()->generate_marshal(g);
	}
	g->emit("%idelete _u.%E;\nbreak;\n%u", nil, e);
    }
}

void UnionDecl::generate_assign(Generator* g, CaseElement* c) {
    ExprList* list = c->labels();
    for (ListItr(ExprList) i(*list); i.more(); i.next()) {
	i.cur()->generate_marshal(g);
    }
    g->emit("%i");
    if (list->count() != 1 || list->item(0) == default_label_) {
	g->emit("_d(_this._d());\n");
    }
    g->emit("%E(_this.%E());\n", nil, c->element()->declarators()->item(0));
    g->emit("break;\n%u");
}

Boolean SequenceDecl::generate_def(Generator* g) {
    if (g->varying()) {
	g->emit("%:%N& %:%N::operator =(const %N& _s) {\n%i", nil, this);
	g->emit("delete [] _buffer;\n");
	g->emit("_maximum = _s._maximum;\n");
	g->emit("_length = _s._length;\n");
	g->emit(
	    "_buffer = _maximum == 0 ? 0 : new %E[_maximum];\n",
	    nil, type_
	);
	g->emit("for (int i = 0; i < _length; i++) {\n%i");
	g->emit("_buffer[i] = _s._buffer[i];\n%u");
	g->emit("}\nreturn *this;\n%u}\n");
	return true;
    }
    return false;
}

Boolean ExceptDecl::generate_def(Generator* g) {
    String* s = ident_->string();
    g->emit("extern %MId _%_tid;\n");
    g->emit("%Q::%I() {\n%i", s);
    g->emit("_major_ = _index;\n_hash_ = _code;\n");
    g->emit("_interface_ = _%_tid;\n%u}\n");
    if (members_ != nil) {
	g->emit("%Q::%I(", s);
	put_init_list(g, true);
	g->emit(") : \n%i");
	put_init_list(g, false);
	g->emit("\n%u{\n%i");
	g->emit("_major_ = _index;\n_hash_ = _code;\n");
	g->emit("_interface_ = _%_tid;\n%u}\n");
    }
    g->emit("%Q* %Q::_cast(const %x* e) {\n%i", s);
    g->emit("if (e->_major() == _index && e->_interface() == _%_tid) {\n%i");
    g->emit("return (%I*)e;\n%u}\n", s);
    g->emit("return 0;\n%u}\n");
    StructDecl::generate_def(g);
    return true;
}

void ExceptDecl::put_init_list(Generator* g, Boolean decl) {
    Boolean need_sep = false;
    for (ListItr(ExprList) i(*members_); i.more(); i.next()) {
	StructMember* s = i.cur()->symbol()->struct_member();
	if (s != nil && s->declarators() != nil) {
	    for (ListItr(ExprList) d(*s->declarators()); d.more(); d.next()) {
		Expr* m = d.cur();
		if (need_sep) {
		    g->emit(", ");
		}
		if (decl) {
		    g->emit("%F _", nil, s->type());
		    m->generate(g);
		} else {
		    g->emit("%N(_%N)", nil, m);
		}
		need_sep = true;
	    }
	}
    }
}

/*
 * Request generation for dynamic invocation.
 */

Boolean ExprImpl::generate_extern_types(Generator*) { return false; }
Boolean ExprImpl::generate_method(Generator*) { return false; }
Boolean ExprImpl::generate_params(Generator*) { return false; }

Boolean IdentifierImpl::generate_extern_types(Generator*) { return false; }
Boolean IdentifierImpl::generate_method(Generator*) { return false; }
Boolean IdentifierImpl::generate_params(Generator*) { return false; }

void InterfaceDef::put_extern_info(Generator* g) {
    Boolean b = g->need_sep(false);
    InterfaceDefState s;
    s.generator = g;
    s.impl = nil;
    s.name = nil;
    Boolean r = g->interface_is_ref(false);
    s.func = &InterfaceDef::put_extern_types;
    g->emit("extern %M_Descriptor ");
    if (!put_members(0, s)) {
	g->emit("_%^void_type");
    }
    g->emit(";\n");
    g->interface_is_ref(r);
    g->need_sep(b);
}

Boolean InterfaceDef::put_extern_types(InterfaceDefState* s, Expr* e) {
    Boolean b = false;
    Generator* g = s->generator;
    Symbol* sym = e->symbol();
    if (sym != nil) {
	switch (sym->tag()) {
	case Symbol::sym_operation:
	case Symbol::sym_attribute:
	    b = e->generate_extern_types(g);
	    break;
	}
    }
    return b;
}

Boolean Operation::generate_extern_types(Generator* g) {
    Boolean b = false;
    if (g->need_extern(type_)) {
	if (g->need_sep(true)) {
	    g->emit(", ");
	}
	g->emit("%b_%Y_type", nil, type_);
	b = true;
    }
    if (params_ != nil) {
	b |= generate_list(params_, &Expr::generate_extern_types, g);
    }
    return b;
}

Boolean Parameter::generate_extern_types(Generator* g) {
    Symbol* s = g->actual_type(declarator_);
    if ((s != nil && s->array() != nil) || g->need_extern(type_)) {
	if (g->need_sep(true)) {
	    g->emit(", ");
	}
	g->emit("%b_%Y", nil, type_);
	declarator_->generate_params(g);
	g->emit("_type");
	return true;
    }
    return false;
}

Boolean InterfaceDef::put_method_info(Generator* g) {
    Boolean b = g->need_sep(false);
    Boolean r = g->interface_is_ref(false);
    String* name = ident_->string();
    InterfaceDefState s;
    s.generator = g;
    s.impl = nil;
    s.name = nil;

    s.func = &InterfaceDef::put_method;
    g->emit("\n%M_OpData _%_%I_methods[] = {\n", name);
    if (put_members(0, s)) {
	g->emit(",\n");
    }
    g->emit("%i{ 0, 0, 0 }\n%u};\n");

    s.func = &InterfaceDef::put_params;
    g->need_sep(false);
    g->emit("%M_ParamData _%_%I_params[] = {\n", name);
    if (!put_members(0, s)) {
	g->emit("%i{ 0, 0, 0 }%u");
    }
    g->emit("\n};\n");

    g->interface_is_ref(r);
    g->need_sep(b);
    return true;
}

Boolean InterfaceDef::put_method(InterfaceDefState* s, Expr* e) {
    Boolean b = false;
    Generator* g = s->generator;
    Symbol* sym = e->symbol();
    if (sym != nil) {
	switch (sym->tag()) {
	case Symbol::sym_operation:
	case Symbol::sym_attribute:
	    b = e->generate_method(g);
	    break;
	}
    }
    return b;
}

Boolean Operation::generate_method(Generator* g) {
    if (g->need_sep(true)) {
	g->emit(",\n");
    }
    g->emit("%b{ \"%N\", ", nil, this);
    g->emit("&_%Y_type, ", nil, type_);
    g->emit_integer(params_ == nil ? 0 : params_->count());
    g->emit(" }");
    return true;
}

Boolean Declarator::generate_method(Generator* g) { return generate_name(g); }

Boolean InterfaceDef::put_params(InterfaceDefState* s, Expr* e) {
    Boolean b = false;
    Generator* g = s->generator;
    Symbol* sym = e->symbol();
    if (sym != nil) {
	switch (sym->tag()) {
	case Symbol::sym_operation:
	case Symbol::sym_attribute:
	    b = e->generate_params(g);
	    break;
	}
    }
    return b;
}

Boolean Operation::generate_params(Generator* g) {
    Boolean b = false;
    if (params_ != nil) {
	if (g->need_sep(false)) {
	    g->emit(",\n");
	}
	g->emit("/* %I */\n%i", ident_->string());
	b = generate_list(params_, &Expr::generate_params, g);
	g->emit("%u");
    }
    return b;
}

Boolean Parameter::generate_params(Generator* g) {
    if (g->need_sep(true)) {
	g->emit(",\n");
    }
    g->emit("%b{ \"");
    declarator_->generate_method(g);
    g->emit("\", ");
    g->emit_integer(attr_ - 1);
    g->emit(", &_%Y", nil, type_);
    declarator_->generate_params(g);
    g->emit("_type }");
    return true;
}

Boolean Declarator::generate_params(Generator* g) {
    if (subscripts_ != nil) {
	g->emit("_array_");
	generate_list(subscripts_, &Expr::generate, g, "_");
	return true;
    }
    return false;
}

Boolean ExprImpl::generate_request(Generator*) { return false; }
Boolean IdentifierImpl::generate_request(Generator*) { return false; }

void InterfaceDef::put_trans_hdr(Generator* g) {
    g->emit("%i");
    g->emit_transcriptions(false);
    g->emit("%u");
}

void InterfaceDef::put_client(Generator* g) {
    if (g->stubclass() == nil) {
	g->handler()->error("Stub type is undefined");
    } else {
	generate_extern_stubs(g);
	generate_stub(g);
	generate_types(g);
    }
}

void InterfaceDef::put_extern_stubs(Generator* g) {
    generate_extern_stubs(g);
}

void InterfaceDef::put_op_stubs(Generator* g) {
    generate_stub(g);
}

void InterfaceDef::put_type_stubs(Generator* g) {
    generate_types(g);
}

Boolean Operation::generate_request(Generator* g) {
    g->emit("case ");
    g->emit_integer(index_);
    g->emit(":\n%i");
    if (!g->void_type(type_)) {
	g->emit("*(%X*)_r->result() = ", nil, type_);
    }
    g->emit("%I(", ident_->string());
    if (params_ != nil) {
	Boolean b = g->need_sep(false);
	generate_list(params_, &Expr::generate_request, g);
	g->need_sep(b);
    }
    g->emit(");\nbreak;\n%u");
    return true;
}

Boolean Parameter::generate_request(Generator* g) {
    if (g->need_sep(true)) {
	g->emit(", ");
    }
    declarator_->generate_request(g);
    g->emit("((%X*)(_r->param(", nil, type_);
    g->emit_integer(g->counter());
    g->emit(")))");
    return true;
}

Boolean Declarator::generate_request(Generator* g) {
    if (subscripts_ == nil) {
	g->emit("*");
	return true;
    }
    return false;
}

/*
 * Generate a skeleton suitable for future filtering.
 */

void InterfaceDef::put_skeleton(Generator* g, String* impl) {
    if (defs_ != nil) {
	g->enter_scope(info_->block);
	for (ListItr(ExprList) i(*defs_); i.more(); i.next()) {
	    Symbol* s = i.cur()->symbol();
	    switch (s->tag()) {
	    case Symbol::sym_operation:
		s->operation()->put_skeleton(g, impl);
		break;
	    case Symbol::sym_attribute:
		s->attribute()->put_skeleton(g, impl);
		break;
	    default:
		/* do nothing */
		break;
	    }
	}
	g->leave_scope();
    }
}

void Operation::put_skeleton(Generator* g, String* impl) {
    String* s = ident_->string();
    g->emit("//+ %I(", impl);
    g->emit("%:%I)\n", s);
    g->emit("%X %I::", impl, type_);
    g->emit("%I", s);
    g->emit_param_list(params_, Generator::emit_env_formals_body);
    g->emit(" {\n}\n");
}
