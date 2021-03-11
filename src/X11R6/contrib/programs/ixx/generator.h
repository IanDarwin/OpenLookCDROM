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

#ifndef generator_h
#define generator_h

#include "err.h"
#include "expr-impl.h"
#include "list.h"
#include <stdio.h>

declarePtrList(StringList,String)

struct InterfaceDefState {
    Generator* generator;
    String* impl;
    String* name;
    Boolean (InterfaceDef::*func)(InterfaceDefState*, Expr*);
};

class Generator {
public:
    Generator(ErrorHandler*, const ConfigInfo&);
    virtual ~Generator();

    enum ParamFlags {
	emit_actuals = 0x1, emit_formals = 0x2,
	emit_env = 0x4, emit_body = 0x8,
	emit_env_actuals = 0x5, emit_env_formals = 0x6,
	emit_env_formals_body = 0xe
    };

    SymbolTable* symbol_table();
    ErrorHandler* handler();
    const char* superclass();
    const char* metaclass();
    const char* stubclass();
    const char* envclass();
    const char* request();
    FILE* stubfile();
    FILE* serverfile();
    long file_mask();
    Boolean refobjs();
    Boolean cdecls();
    Boolean cstubs();
    Boolean envfirst();
    Boolean qualify();
    Boolean formals();
    Boolean concat();
    Boolean do_varying();
    Boolean varying();
    void varying(Boolean);
    void counter(long n);
    long counter();
    void count(long delta);

    Boolean set_source(SourcePosition*);
    Boolean is_source();

    Boolean begin_file(FILE*);
    void end_file();

    Boolean interface_is_ref(Boolean);
    Boolean need_sep(Boolean);
    Boolean op_body(Boolean);
    Boolean is_op_body();
    Boolean array_decl(Boolean);
    Boolean is_array_decl();

    void indirect(Boolean);
    Boolean indirect();
    Boolean gendefs();

    void push_prefix(String*);
    void pop_prefix();
    String* prefix();

    void enter_scope(Scope*);
    void leave_scope();

    void impl_is_from(String*);
    String* impl_is_from();

    Symbol* actual_type(Expr*);
    Boolean addr_type(Expr*);
    Boolean void_type(Expr*);
    Boolean need_extern(Expr*);

    void emit(const char* format, String* = nil, Expr* = nil);
    void emit_str(const char*, long length);
    void emit_substr(const char*, long length);
    void emit_tab();
    void emit_flush(const char* p, const char* start);
    void emit_format(int ch, String*, Expr*);
    void emit_expr_scope(Expr* e);
    void copy(const char*);

    void emit_char(long);
    void emit_chars_length(const char*, long length);
    void emit_integer(long);
    void emit_float(double);
    void emit_declarator_ident(Identifier*);
    void emit_op(Opcode);
    void emit_ifndef(String*);
    void emit_endif(String*);
    void emit_filename(String*);
    void emit_include(String*);
    void emit_includes();
    void emit_stub_includes();
    void emit_server_includes();
    void emit_param_list(ExprList* params, ParamFlags flags);
    void emit_param_decls(ExprList* params, ParamFlags flags);
    void emit_env_param(ParamFlags flags);
    void emit_type_info(
	String* name, char* ptr, ExprList* parents,
	Boolean dii, Boolean excepts, Boolean narrow
    );
    void emit_parent_type_info(String* name, ExprList* parents);
    void emit_opt_info(
	Boolean flag, const char* start, const char* tag, String* name,
	const char* trail
    );
    Boolean emit_scope(Scope*);

    Boolean emit_extern_stubs(Expr* type);
    void emit_put(Expr* type, char* format, Expr* value);
    void emit_array_setup(Declarator* d, Expr* t, Boolean is_put);
    void emit_array_loop_start(ExprList* subscripts);
    void emit_array_loop_indices(long nsubscripts);
    void emit_array_loop_finish(long nsubscripts);
    void emit_get(Expr* type, char* format, Expr* value);
    void emit_transcriptions(Boolean abstract);
    void emit_edit_warning(String*);
    void flush();
private:
    ErrorHandler* handler_;
    FILE* out_;
    FILE* stubfile_;
    FILE* serverfile_;
    SymbolTable* symbols_;
    Boolean generate_include_;
    const char* inclpath_;
    long inclpath_length_;
    const char* inclext_;
    long inclext_length_;
    StringList* includes_;
    StringList* stub_includes_;
    StringList* server_includes_;
    const char* filename_;
    const char* superclass_;
    long superclass_length_;
    const char* metaclass_;
    long metaclass_length_;
    const char* envclass_;
    long envclass_length_;
    Boolean envfirst_;
    const char* stubclass_;
    long stubclass_length_;
    const char* request_;
    long request_length_;
    const char* buffer_;
    long buffer_length_;
    const char* exchange_;
    long exchange_length_;
    const char* except_;
    long except_length_;
    const char* user_except_;
    long user_except_length_;
    const char* prefix_;
    long prefix_length_;
    const char* direct_prefix_;
    long direct_length_;
    StringList* transcriptions_;
    Boolean cdecls_;
    Boolean cstubs_;
    const char* ptr_;
    Boolean source_;
    Boolean qualify_;
    Boolean concat_;
    Boolean need_sep_;
    Boolean ref_;
    Boolean body_;
    Boolean subscripts_;
    Boolean formals_;
    Boolean varying_;
    Boolean indirect_;
    Boolean refobjs_;
    long counter_;
    long indent_;
    long save_indent_;
    long column_;
    long save_column_;
    Boolean include_newline_;
    Scope* scope_;
    String* impl_is_from_;
    StringList* prefixes_;
    StringList* files_;

    long length(const char*);
    FILE* open_output(const char*);
    void emit_include_list(StringList*);
    void emit_include_filename();
    void emit_include_substr(Boolean path, const char* str, long length);
    void emit_tr_filename(const char* start, const char* end);
};

inline ErrorHandler* Generator::handler() { return handler_; }
inline const char* Generator::superclass() { return superclass_; }
inline const char* Generator::metaclass() { return metaclass_; }
inline const char* Generator::stubclass() { return stubclass_; }
inline const char* Generator::envclass() { return envclass_; }
inline const char* Generator::request() { return request_; }
inline FILE* Generator::stubfile() { return stubfile_; }
inline FILE* Generator::serverfile() { return serverfile_; }
inline Boolean Generator::refobjs() { return refobjs_; }
inline Boolean Generator::cdecls() { return cdecls_; }
inline Boolean Generator::cstubs() { return cstubs_; }
inline Boolean Generator::envfirst() { return envfirst_; }
inline Boolean Generator::qualify() { return qualify_; }
inline Boolean Generator::formals() { return formals_; }
inline Boolean Generator::concat() { return concat_; }
inline Boolean Generator::do_varying() { return direct_prefix_ != nil; }
inline Boolean Generator::varying() { return varying_; }
inline void Generator::varying(Boolean b) { varying_ = b; }
inline void Generator::counter(long n) { counter_ = n; }
inline long Generator::counter() { return counter_; }
inline void Generator::count(long delta) { counter_ += delta; }
inline void Generator::indirect(Boolean b) { indirect_ = b; }
inline Boolean Generator::indirect() { return indirect_; }
inline Boolean Generator::gendefs() { return indirect_ || !refobjs_; }

#endif
