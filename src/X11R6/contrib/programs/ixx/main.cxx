/* $XConsortium: main.cxx,v 1.6 94/06/03 21:41:05 matt Exp $ */

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
 * Main program for C++ parser.
 */

#include "err.h"
#include "expr.h"
#include "scanner.h"
#include "list.h"
#include <stdio.h>
#include <unistd.h>

#if defined(AIXV3)
extern "C" int yyparse();
#else
extern int yyparse();
#endif
extern Expr* yyparse_root;

#if defined(YYDEBUG)
extern int yydebug;
#endif

declarePtrList(StringList,String)
/* generate.cxx has implementPtrList(StringList,String) */

class App {
public:
    App() { }

    int main(int argc, char** argv);
private:
    Expr* root_;
    ConfigInfo gen_;
    const char* filter_;
    ErrorHandler* errors_;
    Boolean verbose_;
    Boolean timing_;
    Boolean scanning_only_;
    Boolean resolving_;
    Boolean generating_;
    Boolean filtering_;
    Boolean ucase_;
    unsigned long heapstart_;

    unsigned long curheapsize();
    void init();
    void get_args(int, char**);
    const char* get_next_arg(long& i, int argc, char** argv);
    void run();
    void parse();
    void resolve(Resolver*);
    void generate(Generator*);
    void filter(Generator*);
    void finish();
    void stage(const char*);

    void missing(const char* arg);
    void bad_arg(const char* arg);
    void bad_access(const char* filename, Boolean readable);
};

int main(int argc, char** argv) {
    App a;
    return a.main(argc, argv);
}

int App::main(int argc, char** argv) {
    init();
    get_args(argc, argv);
    if (errors_->count() == 0) {
	run();
    }
    finish();
    return errors_->count();
}

void App::init() {
    root_ = nil;
    gen_.filename = nil;
    gen_.stubfile = nil;
    gen_.serverfile = nil;
    gen_.inclpath = nil;
    gen_.inclext = ".h";
    gen_.includes = new StringList;
    gen_.stub_includes = new StringList;
    gen_.server_includes = new StringList;
    gen_.superclass = nil;
    gen_.metaclass = nil;
    gen_.envclass = nil;
    gen_.envfirst = false;
    gen_.buffer = "MarshalBuffer";
    gen_.exchange = "Exchange";
    gen_.except = "Exception";
    gen_.user_except = "UserException";
    gen_.stubclass = nil;
    gen_.request = nil;
    gen_.prefix = nil;
    gen_.direct = "_";
    gen_.transcriptions = new StringList;
    gen_.refobjs = false;
    gen_.cdecls = false;
    gen_.cstubs = false;
    filter_ = nil;
    ErrorHandlerKit* errkit = new ErrorHandlerKit;
    errors_ = errkit->handler();
    delete errkit;

    verbose_ = false;
    timing_ = false;
    scanning_only_ = false;
    resolving_ = true;
    generating_ = true;
    filtering_ = false;
    ucase_ = false;
    heapstart_ = 0;
    heapstart_ = curheapsize();
    String::case_sensitive(true);
    stage("init");
}

void App::get_args(int argc, char** argv) {
    String arg;
    for (long i = 1; i < argc; i++) {
	arg = argv[i];
	if (arg == "-filter") {
	    if (filter_ != nil) {
		errors_->unrecoverable("Only one -filter allowed");
	    } else {
		filter_ = get_next_arg(i, argc, argv);
		if (access(filter_, R_OK + W_OK) < 0) {
		    bad_access(filter_, access(filter_, R_OK) >= 0);
		}
		filtering_ = true;
	    }
	} else if (arg == "-path") {
	    gen_.inclpath = get_next_arg(i, argc, argv);
	} else if (arg == "-inclext") {
	    gen_.inclext = get_next_arg(i, argc, argv);
	} else if (arg == "-i" || arg == "-include") {
	    gen_.includes->append(new String(get_next_arg(i, argc, argv)));
	} else if (arg == "-stubinclude") {
	    gen_.stub_includes->append(
		new String(get_next_arg(i, argc, argv))
	    );
	} else if (arg == "-serverinclude") {
	    gen_.server_includes->append(
		new String(get_next_arg(i, argc, argv))
	    );
	} else if (arg == "-s" || arg == "-superclass") {
	    gen_.superclass = get_next_arg(i, argc, argv);
	} else if (arg == "-m" || arg == "-metaclass") {
	    gen_.metaclass = get_next_arg(i, argc, argv);
	} else if (arg == "-r" || arg == "-request") {
	    gen_.request = get_next_arg(i, argc, argv);
	} else if (arg == "-e" || arg == "-env") {
	    gen_.envclass = get_next_arg(i, argc, argv);
	} else if (arg == "-envfirst") {
	    gen_.envfirst = true;
	} else if (arg == "-mb") {
	    gen_.buffer = get_next_arg(i, argc, argv);
	} else if (arg == "-exch") {
	    gen_.exchange = get_next_arg(i, argc, argv);
	} else if (arg == "-except") {
	    gen_.except = get_next_arg(i, argc, argv);
	} else if (arg == "-uexcept") {
	    gen_.user_except = get_next_arg(i, argc, argv);
	} else if (arg == "-c" || arg == "-stubclass") {
	    gen_.stubclass = get_next_arg(i, argc, argv);
	} else if (arg == "-stubfile") {
	    gen_.stubfile = get_next_arg(i, argc, argv);
	} else if (arg == "-serverfile") {
	    gen_.serverfile = get_next_arg(i, argc, argv);
	} else if (arg == "-f" || arg == "-file") {
	    gen_.filename = get_next_arg(i, argc, argv);
	} else if (arg == "-p" || arg == "-prefix") {
	    gen_.prefix = get_next_arg(i, argc, argv);
	} else if (arg == "-direct") {
	    gen_.direct = nil;
	} else if (arg == "-indirect") {
	    gen_.direct = get_next_arg(i, argc, argv);
	} else if (arg == "-refobjs") {
	    gen_.refobjs = true;
	} else if (arg == "-extern") {
	    gen_.transcriptions->append(
		new String(get_next_arg(i, argc, argv))
	    );
	} else if (arg == "-cdecls") {
	    gen_.cdecls = true;
	    gen_.direct = nil;
	} else if (arg == "-cstubs") {
	    gen_.cstubs = true;
	} else if (arg == "-cs") {
	    ucase_ = true;
	} else if (arg == "-debug") {
	    const char* debugflags = get_next_arg(i, argc, argv);
	    for (const char* p = debugflags; *p != '\0'; p++) {
		switch (*p) {
#if defined(YYDEBUG)
		case 'y':
		    yydebug = 1;
		    break;
#endif
		case 's':
		    scanning_only_ = true;
		    break;
		case 't':
		    timing_ = true;
		    generating_ = false;
		    break;
		case 'v':
		    verbose_ = true;
		    break;
		case 'p':
		    resolving_ = false;
		    generating_ = false;
		    filtering_ = false;
		    break;
		case 'r':
		    resolving_ = true;
		    generating_ = false;
		    filtering_ = false;
		    break;
		case 'c':
		    resolving_ = true;
		    generating_ = true;
		    filtering_ = false;
		default:
		    errors_->begin_unrecoverable();
		    errors_->put_chars("Unknown debug flag '");
		    errors_->put_chars(p);
		    errors_->put_chars("'");
		    errors_->end();
		}
	    }
	} else {
	    bad_arg(argv[i]);
	}
    }
}

const char* App::get_next_arg(long& i, int argc, char** argv) {
    long next_i = i + 1;
    if (next_i >= argc) {
	missing(argv[i]);
	return nil;
    }
    i = next_i;
    return argv[i];
}

void App::run() {
    ScannerKit* scanners = new ScannerKit;
    Scanner* s = scanners->make_scanner(nil, errors_, ucase_);
    if (scanning_only_) {
	if (timing_) {
	    while (s->get_token() != 0);
	} else {
	    for (TokenType t = s->get_token(); t != 0; t = s->get_token()) {
		s->print_token(t);
		printf("\n");
	    }
	}
    } else {
	ExprKit* exprs = new ExprKit(errors_);
	gen_.symbols = exprs->symbol_table();
	Resolver* r = exprs->resolver(gen_);
	Generator* g = exprs->generator(gen_);
	parse();
	resolve(r);
	generate(g);
	filter(g);
	delete exprs;
    }
    s->destroy();
    delete scanners;
}

void App::parse() {
    stage("parsing");
    yyparse();
    root_ = yyparse_root;
}

void App::resolve(Resolver* r) {
    if (errors_->count() == 0 && resolving_) {
	stage("resolving");
	root_->resolve(r);
    }
}

void App::generate(Generator* g) {
    if (errors_->count() == 0 && generating_ && !filtering_) {
	stage("generating");
	root_->generate(g);
    }
}

void App::filter(Generator* g) {
    if (errors_->count() == 0 && filtering_) {
	stage("filtering");
	String s(filter_);
	errors_->position()->set(&s, 0);
	freopen(filter_, "r", stdin);
	root_->generate_impl(g);
	fclose(stdin);
    }
}

void App::finish() {
    stage("finish");
}

#if !defined(AIXV3) && !defined(__DECCXX)
extern "C" {
    void* sbrk(int);
}
#endif

unsigned long App::curheapsize() {
    return (((unsigned long)sbrk(0) + 1023) >> 10) - heapstart_;
}

void App::stage(const char* s) {
    if (verbose_) {
	errors_->begin_comment();
	errors_->put_chars(s);
	errors_->put_chars("(");
	errors_->put_integer(curheapsize());
	errors_->put_chars("k)");
	errors_->end();
    }
}

void App::bad_arg(const char* arg) {
    errors_->begin_unrecoverable();
    errors_->put_chars("Unexpected argument ");
    errors_->put_chars(arg);
    errors_->end();
}

void App::missing(const char* arg) {
    errors_->begin_unrecoverable();
    errors_->put_chars("Expected argument after ");
    errors_->put_chars(arg);
    errors_->end();
}

void App::bad_access(const char* filename, Boolean readable) {
    errors_->begin_unrecoverable();
    if (readable) {
	errors_->put_chars("Can't write \"");
    } else {
	errors_->put_chars("Can't read \"");
    }
    errors_->put_chars(filename);
    errors_->put_chars("\"");
    errors_->end();
}
