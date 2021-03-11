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
#include "tokendefs.h"
#include <ctype.h>
#include <string.h>

implementPtrList(StringList,String)

static char tab[] = "    ";
static const long right_margin = 65;

Generator::Generator(ErrorHandler* h, const ConfigInfo& i) {
    handler_ = h;
    out_ = stdout;
    generate_include_ = true;
    symbols_ = i.symbols;
    inclpath_ = i.inclpath;
    inclpath_length_ = length(inclpath_);
    inclext_ = i.inclext;
    inclext_length_ = length(inclext_);
    includes_ = i.includes;
    stub_includes_ = i.stub_includes;
    server_includes_ = i.stub_includes;
    filename_ = i.filename;
    stubfile_ = open_output(i.stubfile);
    serverfile_ = open_output(i.serverfile);
    superclass_ = i.superclass;
    superclass_length_ = length(superclass_);
    metaclass_ = i.metaclass;
    metaclass_length_ = length(metaclass_);
    envclass_ = i.envclass;
    envclass_length_ = length(envclass_);
    envfirst_ = i.envfirst || i.cdecls;
    stubclass_ = i.stubclass;
    stubclass_length_ = length(stubclass_);
    request_ = i.request;
    request_length_ = length(request_);
    buffer_ = i.buffer;
    buffer_length_= length(buffer_);
    exchange_ = i.exchange;
    exchange_length_= length(exchange_);
    except_ = i.except;
    except_length_= length(except_);
    user_except_ = i.user_except;
    user_except_length_= length(user_except_);
    prefix_ = i.prefix;
    prefix_length_ = length(prefix_);
    direct_prefix_ = i.direct;
    direct_length_ = length(direct_prefix_);
    transcriptions_ = i.transcriptions;
    refobjs_ = i.refobjs;
    cdecls_ = i.cdecls;
    cstubs_ = i.cstubs;
    ptr_ = nil;
    source_ = true;
    qualify_ = true;
    concat_ = false;
    varying_ = do_varying();
    ref_ = true;
    need_sep_ = false;
    body_ = true;
    subscripts_ = true;
    formals_ = true;
    counter_ = 0;
    indent_ = 0;
    column_ = 0;
    include_newline_ = false;
    scope_ = nil;
    impl_is_from_ = nil;
    prefixes_ = new StringList;
    files_ = new StringList;
    if (stubfile_ != nil || serverfile_ != nil) {
	if (superclass_ == nil) {
	    superclass_ = "BaseObject";
	    superclass_length_ = length(superclass_);
	}
	if (metaclass_ == nil) {
	    metaclass_ = "TypeObj";
	    metaclass_length_ = length(metaclass_);
	}
	if (stubclass_ == nil) {
	    stubclass_ = "Stub";
	    stubclass_length_ = length(stubclass_);
	}
    }
}

Generator::~Generator() {
    delete prefixes_;
    delete files_;
}

long Generator::length(const char* s) {
    return s == nil ? 0 : strlen(s);
}

FILE* Generator::open_output(const char* name) {
    FILE* f = nil;
    if (name != nil) {
	f = fopen(name, "w");
	if (f == nil) {
	    handler_->begin_unrecoverable();
	    handler_->put_chars("Can't write output file '");
	    handler_->put_chars(name);
	    handler_->put_chars("'");
	    handler_->end();
	}
    }
    return f;
}

long Generator::file_mask() {
    if (out_ == stubfile_) {
	return 1;
    } else if (out_ == serverfile_) {
	return 2;
    }
    return 0;
}

SymbolTable* Generator::symbol_table() { return symbols_; }

Boolean Generator::set_source(SourcePosition* p) {
    if (filename_ != nil) {
	source_ = (p == nil || *p->filename() == filename_);
    }
    return source_;
}

Boolean Generator::is_source() { return source_; }

Boolean Generator::begin_file(FILE* f) {
    if (f != nil) {
	fflush(out_);
	out_ = f;
	save_indent_ = indent_;
	save_column_ = column_;
	indent_ = 0;
	column_ = 0;
	return true;
    }
    return false;
}

void Generator::end_file() {
    fflush(out_);
    out_ = stdout;
    indent_ = save_indent_;
    column_ = save_column_;
}

Boolean Generator::interface_is_ref(Boolean b) {
    Boolean prev = ref_;
    ref_ = b;
    return prev;
}

Boolean Generator::need_sep(Boolean b) {
    Boolean prev = need_sep_;
    need_sep_ = b;
    return prev;
}

Boolean Generator::op_body(Boolean b) {
    Boolean prev = body_;
    body_ = b;
    return prev;
}

Boolean Generator::is_op_body() { return body_; }

Boolean Generator::array_decl(Boolean b) {
    Boolean prev = subscripts_;
    subscripts_ = b;
    return prev;
}

Boolean Generator::is_array_decl() { return subscripts_; }

void Generator::push_prefix(String* s) {
    prefixes_->prepend(s);
}

void Generator::pop_prefix() {
    if (prefixes_->count() > 0) {
	prefixes_->remove(0);
    }
}

String* Generator::prefix() {
    String* s = nil;
    if (prefixes_->count() > 0) {
	s = prefixes_->item(0);
    }
    return s;
}

void Generator::enter_scope(Scope* s) {
    scope_ = s;
}

void Generator::leave_scope() {
    scope_ = scope_->outer;
}

void Generator::impl_is_from(String* s) { impl_is_from_ = s; }
String* Generator::impl_is_from() { return impl_is_from_; }

Symbol* Generator::actual_type(Expr* e) {
    Symbol* s = e->symbol();
    if (s != nil) {
	s = s->actual_type();
    }
    return s;
}

Boolean Generator::addr_type(Expr* e) {
    Symbol* s = actual_type(e);
    Boolean b = false;
    if (s != nil) {
	switch (s->tag()) {
	case Symbol::sym_string:
	case Symbol::sym_struct:
	case Symbol::sym_sequence:
	case Symbol::sym_union:
	    b = true;
	    break;
	}
    }
    return b;
}

Boolean Generator::void_type(Expr* e) {
    return actual_type(e) == symbols_->void_type();
}

Boolean Generator::need_extern(Expr* e) {
    Boolean b = true;
    Symbol* s = e->symbol();
    if (s != nil) {
	if (s->declared()) {
	    b = false;
	} else {
	    s->declared(true);
	}
    }
    return b;
}

void Generator::emit(const char* format, String* s, Expr* e) {
    const char* start = format;
    for (const char* p = start; *p != '\0'; p++) {
	if (*p == '\n') {
	    if (p > start) {
		emit_substr(start, p - start);
	    }
	    putc('\n', out_);
	    column_ = 0;
	    start = p + 1;
	} else if (*p == '%') {
	    if (p > start) {
		emit_substr(start, p - start);
	    }
	    ++p;
	    emit_format(*p, s, e);
	    start = p + 1;
	}
    }
    if (p > start) {
	emit_substr(start, p - start);
    }
}

void Generator::emit_str(const char* s, long length) {
    if (column_ == 0) {
	emit_tab();
    }
    fputs(s, out_);
    column_ += length;
}

void Generator::emit_substr(const char* s, long length) {
    if (column_ == 0) {
	emit_tab();
    }
    fprintf(out_, "%.*s", length, s);
    column_ += length;
}

void Generator::emit_tab() {
    if (include_newline_) {
	putc('\n', out_);
	include_newline_ = false;
    }
    for (long n = 0; n < indent_; n++) {
	fputs(tab, out_);
	column_ += sizeof(tab) - 1;
    }
}

void Generator::emit_flush(const char* p, const char* start) {
    long n = p - start;
    if (n != 0) {
	emit_substr(start, n);
    }
}

/*
 * Interpret a format specification, potentially using
 * a given string and expression for substitution.
 *
 * Formats:
 *
 *    %^ - name prefix if defined
 *    %p - suffix for an object pointer type
 *    %r - suffix for a managed object pointer type
 *    %P - same as %p if interface types should be output as pointers
 *    %* - current pointer suffix
 *    %i - increment tab indentation level
 *    %u - decrement tab indentation level
 *    %b - discretionary line break
 *    %o - white space if in the body of an operation
 *    %v - use vformat for a string
 *    %I - given string
 *    %Q - short-hand for %:%I
 *    %P - short-hand for %I%p
 *    %T - name of type object using given string as the type name
 *    %E - generate given expression
 *    %; - scope of expression using ::
 *    %- - scope of expression using _
 *    %X - fully-qualified expression
 *    %Y - fully-qualified expression using _ instead of ::
 *    %N - generate type expression's name
 *    %F - generate type expression's fully-qualified name
 *    %A - attribute parameter name for given (type) expression
 *    %S - superclass
 *    %M - metaclass
 *    %C - client stub class
 *    %c - client stub class create function (or "0" when no stubs)
 *    %D - dynamic invocation request
 *    %B - marshal buffer type
 *    %O - object exchange type
 *    %x - exception type
 *    %U - user exception type
 *    %, - emit ", " if envclass is defined
 *    %e - environment formal parameter (if envclass is defined)
 *    %f - environment formal parameter for body (if envclass is defined)
 *    %a - environment actual parameter (if envclass is defined)
 *    %~ - name of current scope
 *    %: - current scope with :: as separator and trailer
 *    %_ - current scope with _ as separator and trailer
 *    %? - scope operator (::) or name separator (_)
 *    %. - scope specification (scope::) if prefix is defined
 *    %= - pure virtual if stubclass is not defined
 */

void Generator::emit_format(int ch, String* s, Expr* e) {
    String* str;
    Boolean b;
    switch (ch) {
    case '^':
	if (prefix_ != nil) {
	    emit_str(prefix_, prefix_length_);
	}
	break;
    case 'p':
	emit_str("Ref", 3);
	break;
    case 'P':
	if (ref_) {
	    emit("%p");
	}
	break;
    case 'r':
	emit_str("_var", 4);
	break;
    case '*':
	emit_str(ptr_, strlen(ptr_));
	break;
    case 'i':
	++indent_;
	break;
    case 'u':
	--indent_;
	break;
    case 'b':
	if (column_ > right_margin) {
	    putc('\n', out_);
	    column_ = 0;
	}
	break;
    case 'o':
	if (body_) {
	    putc(' ', out_);
	    column_ += 1;
	}
	break;
    case 'v':
	if (direct_prefix_ != nil) {
	    emit_str(direct_prefix_, direct_length_);
	}
	break;
    case 'I':
	emit_substr(s->string(), s->length());
	break;
    case 'Q':
	emit("%:%I", s);
	break;
    case 'T':
	emit("_%_%I_tid", s);
	break;
    case 'E':
	e->generate(this);
	break;
    case ';':
	emit_expr_scope(e);
	break;
    case '-':
	b = concat_;
	concat_ = true;
	emit_expr_scope(e);
	concat_ = b;
	break;
    case 'X':
	emit_expr_scope(e);
	b = qualify_;
	qualify_ = false;
	e->generate(this);
	qualify_ = b;
	break;
    case 'Y':
	b = concat_;
	concat_ = true;
	emit("%^%F", nil, e);
	concat_ = b;
	break;
    case 'N':
	e->generate_name(this);
	break;
    case 'F':
	emit_expr_scope(e);
	b = qualify_;
	qualify_ = false;
	e->generate_name(this);
	qualify_ = b;
	break;
    case 'A':
	emit_str("p_", 2);
	e->generate(this);
	break;
    case 'S':
	emit_str(superclass_, superclass_length_);
	break;
    case 'M':
	emit_str(metaclass_, metaclass_length_);
	break;
    case 'C':
	emit_str(stubclass_, stubclass_length_);
	break;
    case 'c':
	emit(stubclass_ != nil ? "&_%_%I%C_create" : "0", s);
	break;
    case 'D':
	emit_str(request_, request_length_);
	break;
    case 'B':
	emit_str(buffer_, buffer_length_);
	break;
    case 'O':
	emit_str(exchange_, exchange_length_);
	break;
    case 'x':
	emit_str(except_, except_length_);
	break;
    case 'U':
	emit_str(user_except_, user_except_length_);
	break;
    case ',':
	if (envclass() != nil) {
	    emit(", ");
	}
	break;
    case 'e':
	if (envclass() != nil) {
	    if (impl_is_from() != nil) {
		emit("%f");
	    } else {
		emit_param_decls(nil, emit_env_formals);
	    }
	}
	break;
    case 'f':
	if (envclass() != nil) {
	    emit_param_decls(nil, emit_env_formals_body);
	}
	break;
    case 'a':
	if (envclass() != nil) {
	    emit_param_decls(nil, emit_env_actuals);
	}
	break;
    case '~':
	emit_scope(scope_);
	break;
    case ':':
	if (emit_scope(scope_)) {
	    emit("%?");
	}
	break;
    case '_':
	b = concat_;
	concat_ = true;
	emit("%^%:");
	concat_ = b;
	break;
    case '?':
	if (concat_ || cdecls_) {
	    emit_str("_", 1);
	} else {
	    emit_str("::", 2);
	}
	break;
    case '.':
	str = prefix();
	if (str != nil) {
	    emit_substr(str->string(), str->length());
	    emit_str("::", 2);
	}
	break;
    case '=':
	if (stubclass_ == nil && !cdecls()) {
	    emit(" = 0");
	}
	break;
    case '%':
	emit_str("%", 1);
	break;
    default:
	emit_str("%", 1);
	putc(ch, out_);
	column_ += 1;
	break;
    }
}

void Generator::emit_expr_scope(Expr* e) {
    Symbol* sym = e->symbol();
    if (sym != nil && emit_scope(sym->scope())) {
	emit("%?");
    }
}

void Generator::copy(const char* s) {
    fputs(s, out_);
    column_ = 0;
}

void Generator::emit_char(long c) {
    putc(int(c), out_);
}

void Generator::emit_chars_length(const char* s, long length) {
    emit_substr(s, length);
}

void Generator::emit_integer(long n) {
    char buf[100];
    sprintf(buf, "%d", n);
    emit_str(buf, strlen(buf));
}

void Generator::emit_float(double d) {
    char buf[100];
    sprintf(buf, "%g", d);
    emit_str(buf, strlen(buf));
}

void Generator::emit_declarator_ident(Identifier* i) {
    if (i != nil && body_) {
	emit("%I", i->string());
    }
}

void Generator::emit_op(Opcode op) {
    char single_char_op[2];
    const char* opname;
    switch (op) {
    case LSHIFT:
	opname = "<<";
	break;
    case RSHIFT:
	opname = ">>";
	break;
    default:
	if (op >= ' ' && op <= '~') {
	    single_char_op[0] = char(op);
	    single_char_op[1] = '\0';
	    opname = single_char_op;
	} else {
	    opname = "???";
	}
	break;
    }
    copy(opname);
}

/*
 * Convert /A/B/.../C/D.idl to C_D_h
 */

void Generator::emit_filename(String* s) {
    const char* start = s->string();
    String::Index n = s->length();
    const char* p;
    const char* end = start + n - 1;
    for (p = end; p > start; p--) {
	if (*p == '.') {
	    end = p;
	    break;
	}
    }
    long slash = 0;
    for (; p > start; p--) {
	if (*p == '/') {
	    if (slash == 1) {
		++p;
		break;
	    }
	    ++slash;
	}
    }
    emit_tr_filename(p, end);
    emit_tr_filename(inclext_, inclext_ + inclext_length_);
}

void Generator::emit_tr_filename(const char* start, const char* end) {
    for (const char* p = start; p < end; p++) {
	int ch = *p;
	if (!isalnum(ch)) {
	    ch = '_';
	}
	putc(ch, out_);
    }
}

void Generator::emit_ifndef(String* filename) {
    emit("#ifndef ");
    emit_filename(filename);
    emit("\n#define ");
    emit_filename(filename);
    emit("\n\n");
    files_->prepend(new CopyString(*filename));
}

void Generator::emit_endif(String* filename) {
    if (files_->count() > 0) {
	String* current = files_->item(0);
	if (*current != *filename) {
	    emit("#endif\n\n");
	    delete current;
	    files_->remove(0);
	}
    }
}

void Generator::emit_include(String* name) {
    if (inclpath_ != nil) {
	include_newline_ = false;
	emit("#include \"");
	const char* start = name->string();
	String::Index n = name->length();
	/* assume n > 0 ==> end >= start */
	const char* end = start + n - 1;
	const char* p;
	for (p = start; ; p++) {
	    if (p > end) {
		for (; p > start; p--) {
		    if (*p == '/') {
			start = p + 1;
			break;
		    }
		}
		emit_str(inclpath_, inclpath_length_);
		break;
	    }
	    const char* s1, * s2;
	    for (s1 = p, s2 = inclpath_; *s1 == *s2; s1++, s2++);
	    if (*s2 == '\0') {
		start = p;
		break;
	    }
	}
	for (p = end; ; p--) {
	    if (p == start) {
		emit_str(start, end - start);
		break;
	    }
	    if (*p == '.') {
		emit_substr(start, p - start);
		emit_str(inclext_, inclext_length_);
		break;
	    }
	}
	emit("\"\n");
	include_newline_ = true;
    }
}

void Generator::emit_includes() {
    if (generate_include_) {
	StringList* list = includes_;
	if (list != nil && list->count() > 0) {
	    emit_include_list(list);
	} else {
	    const char* name = "Ox/object";
	    emit_include_substr(true, name, strlen(name));
	}
	generate_include_ = false;
    }
}

void Generator::emit_stub_includes() {
    StringList* list = stub_includes_;
    if (list != nil && list->count() > 0) {
	emit_include_list(list);
    } else {
	emit_include_filename();
	const char* name = "Ox/stub";
	emit_include_substr(true, name, strlen(name));
	include_newline_ = false;
	emit("\n");
    }
}

void Generator::emit_server_includes() {
    StringList* list = server_includes_;
    if (list != nil && list->count() > 0) {
	emit_include_list(list);
    } else {
	emit_include_filename();
	const char* name = "Ox/server";
	emit_include_substr(true, name, strlen(name));
	include_newline_ = false;
	emit("\n");
    }
}

void Generator::emit_include_list(StringList* list) {
    include_newline_ = false;
    for (ListItr(StringList) i(*list); i.more(); i.next()) {
	emit("#include %I\n", i.cur());
    }
    include_newline_ = true;
}

void Generator::emit_include_filename() {
    if (filename_ != nil) {
	const char* p;
	for (p = filename_; *p != '\0'; p++);
	for (const char* q = p - 1; q > filename_; q--) {
	    if (*q == '.') {
		p = q;
		break;
	    }
	}
	emit_include_substr(false, filename_, p - filename_);
    }
}

void Generator::emit_include_substr(Boolean path, const char* s, long length) {
    include_newline_ = false;
    emit("#include \"");
    if (path && inclpath_ != nil) {
	emit_str(inclpath_, inclpath_length_);
    }
    emit_substr(s, length);
    emit_str(inclext_, inclext_length_);
    emit("\"\n");
    include_newline_ = true;
}

void Generator::emit_param_list(ExprList* params, ParamFlags flags) {
    emit("(");
    emit_param_decls(params, flags);
    emit(")");
}

void Generator::emit_param_decls(ExprList* params, ParamFlags flags) {
    Boolean env = envclass_ != nil && (flags & emit_env) != 0;
    Boolean formals = (flags & emit_formals) != 0;
    if (env && envfirst_) {
	emit_env_param(flags);
	if (params != nil) {
	    emit(", ");
	}
    }
    if (params != nil) {
	Boolean b = formals_;
	formals_ = formals;
	ExprImpl::generate_list(params, &Expr::generate, this, ", ");
	formals_ = b;
    }
    if (env && !envfirst_) {
	if (params != nil) {
	    emit(", ");
	}
	emit_env_param(flags);
	if (formals && (flags & emit_body) == 0 && !cdecls()) {
	    emit(" = 0");
	}
    }
}

void Generator::emit_env_param(ParamFlags flags) {
    if ((flags & emit_formals) != 0) {
	emit_chars_length(envclass_, envclass_length_);
	emit("* ");
    }
    emit("_env");
}

void Generator::emit_type_info(
    String* name, char* ptr, ExprList* parents,
    Boolean dii, Boolean excepts, Boolean narrow
) {
    ptr_ = ptr;
    Boolean has_offsets = false;
    if (parents != nil) {
	has_offsets = parents->count() > 1;
	emit_parent_type_info(name, parents);
    }
    emit("extern %MId %T;\n", name);
    const char* stubs = stubclass_;
    if (stubclass_ != nil && *ptr == '*') {
	stubclass_ = nil;
    }
    if (excepts) {
	emit("extern %M_UnmarshalException _%_%I_excepts[];\n", name);
    }
    if (dii) {
	emit("extern void _%_%I_receive(%S%p, ULong, %B&);\n", name);
    }
    emit("%M_Descriptor _%_%I_type = {\n%i", name);
    emit("/* type */ 0,\n/* id */ &%T,\n\"%I\",\n", name);
    emit_opt_info(parents != nil, nil, "parents", name, ", ");
    emit_opt_info(has_offsets, nil, "offsets", name, ", ");
    emit_opt_info(excepts, nil, "excepts", name, ",\n");
    emit_opt_info(dii, nil, "methods", name, ", ");
    emit_opt_info(dii, nil, "params", name, ",\n");
    emit_opt_info(dii, "&", "receive", name, "\n");
    emit("%u};\n");
    if (narrow) {
	emit("\n%Q%* %Q::_narrow(%S%p o) {\n%i", name);
	emit("return (%Q%*)_%S_tnarrow(\n%io, %T, %b%c\n%u);\n%u}\n", name);
    }
    stubclass_ = stubs;
}

void Generator::emit_parent_type_info(String* name, ExprList* parents) {
    Boolean b = interface_is_ref(false);
    emit("extern %M_Descriptor ");
    long p = 0;
    long nparents = parents->count();
    for (;;) {
	emit("_%Y_type", nil, parents->item(p));
	++p;
	if (p == nparents) {
	    break;
	}
	emit(", ");
    }
    emit(";\n");
    emit("%M_Descriptor* _%_%I_parents[] = { ", name);
    for (p = 0; p < nparents; p++) {
	emit("&_%Y_type, ", nil, parents->item(p));
    }
    emit("nil };\n");
    if (nparents > 1) {
	emit("Long _%_%I_offsets[] = {\n%i", name);
	p = 1;
	for (;;) {
	    emit(
		"Long((%F*)(%:%I*)8) - Long((%:%I*)8)", name, parents->item(p)
	    );
	    ++p;
	    if (p == nparents) {
		break;
	    }
	    emit(", ");
	}
	emit("\n%u};\n");
    }
    interface_is_ref(b);
}

void Generator::emit_opt_info(
    Boolean b, const char* start, const char* tag, String* name,
    const char* trail
) {
    if (b) {
	if (start != nil) {
	    emit(start);
	}
	emit("_%_%I_", name);
	emit(tag);
    } else {
	emit("/* ");
	emit(tag);
	emit(" */ nil");
    }
    emit(trail);
}

Boolean Generator::emit_scope(Scope* s) {
    Boolean b = false;
    if (s != nil) {
	if (s->outer != nil && s->outer->name != nil) {
	    emit_scope(s->outer);
	    emit("%?");
	    b = true;
	}
	if (s->name != nil) {
	    emit("%I", s->name);
	    b = true;
	}
    }
    return b;
}

Boolean Generator::emit_extern_stubs(Expr* type) {
    Boolean b = false;
    long f = file_mask();
    Symbol* s = type->symbol();
    switch (s->tag()) {
    case Symbol::sym_array:
	if (!s->declared_stub(f)) {
	    b = s->array()->generate_extern_stubs(this);
	    s->declare_stub(f);
	}
	break;
    case Symbol::sym_typedef:
	if (!s->declared_stub(f)) {
	    b = s->typename()->generate_extern_stubs(this);
	    s->declare_stub(f);
	}
	break;
    case Symbol::sym_enum:
    case Symbol::sym_string:
    case Symbol::sym_interface:
	break;
    default:
	if (!s->declared_stub(f)) {
	    type->generate_extern_stubs(this);
	    emit("extern void _%Y_put(\n%i%B&, %b", nil, type);
	    emit("const %F", nil, type);
	    if (addr_type(type)) {
		emit("&");
	    }
	    emit("\n%u);\n");
	    emit("extern void _%Y_get(\n%i%B&, %b", nil, type);
	    emit("%F", nil, type);
	    if (s->array() == nil) {
		emit("&");
	    }
	    emit("\n%u);\n");
	    s->declare_stub(f);
	    b = true;
	}
	break;
    }
    return b;
}

void Generator::emit_put(Expr* type, char* format, Expr* value) {
    Boolean b;
    Symbol* s = actual_type(value);
    switch (s->tag()) {
    case Symbol::sym_array:
	b = array_decl(false);
	emit("_%Y_put(_b, ", nil, s->array());
	emit(format, nil, value);
	emit(");\n");
	array_decl(b);
	return;
    case Symbol::sym_enum:
	emit("_b.put_long(");
	break;
    case Symbol::sym_interface:
	emit("_b.put_object(");
	break;
    case Symbol::sym_sequence:
	emit("_%Y_put(_b, ", nil, s->sequence_type());
	break;
    case Symbol::sym_string:
	emit("_b.put_string(");
	break;
    case Symbol::sym_typedef:
	/* builtin type */
	emit("_b.put_%I(", s->typename()->str());
	break;
    default:
	emit("_%Y_put(_b, ", nil, type);
	break;
    }
    emit(format, nil, value);
    emit(");\n");
}

void Generator::emit_array_setup(Declarator* d, Expr* t, Boolean is_put) {
    ExprList* subs = d->subscripts();
    emit_array_loop_start(subs);
    if (is_put) {
	emit("const ");
    }
    emit("%F& _tmp = _array", nil, t);
    emit_array_loop_indices(subs->count());
    emit(";\n");
}

void Generator::emit_array_loop_start(ExprList* subscripts) {
    long index = 0;
    for (ListItr(ExprList) e(*subscripts); e.more(); e.next()) {
	emit("for (int _i");
	emit_integer(index);
	emit(" = 0; _i");
	emit_integer(index);
	emit(" < %E; _i", nil, e.cur());
	emit_integer(index);
	emit("++) {\n%i");
	++index;
    }
}

void Generator::emit_array_loop_indices(long nsubscripts) {
    for (long index = 0; index < nsubscripts; index++) {
	emit("[_i");
	emit_integer(index);
	emit("]");
    }
}

void Generator::emit_array_loop_finish(long nsubscripts) {
    for (long index = 0; index < nsubscripts; index++) {
	emit("%u}\n");
    }
}

void Generator::emit_get(Expr* type, char* format, Expr* value) {
    Boolean b;
    Symbol* s = actual_type(value);
    switch (s->tag()) {
    case Symbol::sym_array:
	b = array_decl(false);
	emit("_%Y_get(_b, ", nil, s->array());
	emit(format, nil, value);
	emit(");\n");
	array_decl(b);
	return;
    case Symbol::sym_enum:
	emit(format, nil, value);
	emit(" = %F(_b.get_long());\n", nil, type);
	break;
    case Symbol::sym_interface:
	emit(format, nil, value);
	b = interface_is_ref(false);
	emit(" = (%F%p)_b.get_object(", nil, type);
	emit(stubclass_ == nil ? "0" : "&_%Y%C_create", nil, type);
	emit(");\n");
	interface_is_ref(b);
	break;
    case Symbol::sym_sequence:
	emit("_%Y_get(_b, ", nil, s->sequence_type());
	emit(format, nil, value);
	emit(");\n");
	break;
    case Symbol::sym_string:
	emit(format, nil, value);
	emit(" = _b.get_string();\n");
	break;
    case Symbol::sym_typedef:
	/* builtin type */
	emit(format, nil, value);
	emit(" = _b.get_%I();\n", s->typename()->str());
	break;
    default:
	emit("_%Y_get(_b, ", nil, type);
	emit(format, nil, value);
	emit(");\n");
	break;
    }
}

void Generator::emit_transcriptions(Boolean abstract) {
    for (ListItr(StringList) i(*transcriptions_); i.more(); i.next()) {
	if (abstract) {
	    emit("virtual ");
	}
	emit("void _put(%I&)", i.cur());
	if (abstract) {
	    emit("%=");
	}
	emit(";\n", i.cur());
    }
}

void Generator::emit_edit_warning(String* s) {
    emit("/* DO NOT EDIT -- Automatically generated");
    if (s != nil) {
	emit(" from %I", s);
    }
    emit(" */\n\n");
}

void Generator::flush() {
    long n = files_->count();
    for (long i = 0; i < n; i++) {
	fprintf(out_, "\n#endif\n");
    }
    fflush(out_);
    if (stubfile_ != nil) {
	fclose(stubfile_);
    }
    if (serverfile_ != nil) {
	fclose(serverfile_);
    }
}

/*
 * Create generator.
 */

Generator* ExprKit::generator(const ConfigInfo& info) {
    return new Generator(impl_->handler_, info);
}
