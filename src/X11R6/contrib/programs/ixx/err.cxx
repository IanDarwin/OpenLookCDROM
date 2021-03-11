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
 * ErrorHandler - report errors
 */

#include "err.h"
#include "types.h"
#include <stdio.h>
#include <stdlib.h>

/* class SourcePosition */

SourcePosition::SourcePosition() { }
SourcePosition::~SourcePosition() { }

/* class ErrorHandler */

ErrorHandler::ErrorHandler() { }
ErrorHandler::~ErrorHandler() { }

/* class ErrorHandlerKit */

class SourcePositionImpl : public SourcePosition {
public:
    SourcePositionImpl(FileName, LineNumber);
    virtual ~SourcePositionImpl();

    virtual SourcePosition* clone();
    virtual void set(FileName, LineNumber);
    virtual void newline();
    virtual FileName filename();
    virtual LineNumber lineno();

    virtual void ref();
    virtual void unref();
private:
    FileName filename_;
    LineNumber lineno_;
    long refcount_;
};

class ErrorHandlerImpl : public ErrorHandler {
public:
    ErrorHandlerImpl();
    virtual ~ErrorHandlerImpl();

    virtual void prefix(const char*);
    virtual void prefix(const String&);
    virtual void position(SourcePosition*);
    virtual SourcePosition* position();
    virtual void newline();
    virtual void begin_comment();
    virtual void begin_warning();
    virtual void begin_error();
    virtual void begin_unrecoverable();
    virtual void begin_internal();
    virtual void put_chars(const char*);
    virtual void put_string(const String&);
    virtual void put_integer(long);
    virtual void put_float(double);
    virtual void end();
    virtual long count();
    virtual void destroy();

    virtual void comment(const char*);
    virtual void comment(const String&);
    virtual void warning(const char*);
    virtual void warning(const String&);
    virtual void error(const char*);
    virtual void error(const String&);
    virtual void unrecoverable(const char*);
    virtual void unrecoverable(const String&);
    virtual void internal(const char*);
    virtual void internal(const String&);
private:
    enum { harmless, recoverable, quit };
    unsigned long severity_;
    long count_;
    String* prefix_;
    SourcePosition* position_;

    void begin(unsigned long severity, const char*);
};

ErrorHandlerKit::ErrorHandlerKit() { impl_ = nil; }
ErrorHandlerKit::~ErrorHandlerKit() { }

ErrorHandler* ErrorHandlerKit::handler() {
    return new ErrorHandlerImpl;
}

/* class ErrorHandlerImpl */

ErrorHandlerImpl::ErrorHandlerImpl() {
    count_ = 0;
    prefix_ = nil;
    position_ = new SourcePositionImpl(nil, 0);
}

ErrorHandlerImpl::~ErrorHandlerImpl() {
    delete prefix_;
    position_->unref();
}

void ErrorHandlerImpl::prefix(const char* s) {
    delete prefix_;
    prefix_ = new CopyString(s);
}

void ErrorHandlerImpl::prefix(const String& s) {
    delete prefix_;
    prefix_ = new CopyString(s);
}

void ErrorHandlerImpl::position(SourcePosition* p) {
    position_->unref();
    position_ = p;
}

SourcePosition* ErrorHandlerImpl::position() {
    return position_;
}

void ErrorHandlerImpl::newline() {
    if (position_ != nil) {
	position_->newline();
    }
}

void ErrorHandlerImpl::begin_comment() { begin(harmless, nil); }
void ErrorHandlerImpl::begin_warning() { begin(harmless, "warning"); }
void ErrorHandlerImpl::begin_error() { begin(recoverable, nil); }
void ErrorHandlerImpl::begin_unrecoverable() { begin(quit, "error"); }
void ErrorHandlerImpl::begin_internal() { begin(quit, "internal error"); }

void ErrorHandlerImpl::begin(unsigned long severity, const char* heading) {
    severity_ = severity;
    if (severity != harmless) {
	++count_;
    }
    if (prefix_ != nil) {
	fprintf(stderr, "%.*s: ", prefix_->length(), prefix_->string());
    }
    if (position_ != nil) {
	FileName f = position_->filename();
	if (f != nil) {
	    fprintf(
		stderr, "%.*s: %d: ",
		f->length(), f->string(), position_->lineno()
	    );
	}
    }
    if (heading != nil) {
	fprintf(stderr, "%s: ", heading);
    }
}

void ErrorHandlerImpl::end() {
    fprintf(stderr, "\n");
    if (severity_ == quit) {
	exit(1);
    }
}

void ErrorHandlerImpl::put_chars(const char* s) { fprintf(stderr, "%s", s); }

void ErrorHandlerImpl::put_string(const String& s) {
    fprintf(stderr, "%.*s", s.length(), s.string());
}

void ErrorHandlerImpl::put_integer(long n) { fprintf(stderr, "%ld", n); }
void ErrorHandlerImpl::put_float(double d) { fprintf(stderr, "%lg", d); }

long ErrorHandlerImpl::count() {
    return count_;
}

void ErrorHandlerImpl::destroy() {
    delete this;
}

void ErrorHandlerImpl::comment(const char* s) {
    begin_comment();
    put_chars(s);
    end();
}

void ErrorHandlerImpl::comment(const String& s) {
    begin_comment();
    put_string(s);
    end();
}

void ErrorHandlerImpl::warning(const char* s) {
    begin_warning();
    put_chars(s);
    end();
}

void ErrorHandlerImpl::warning(const String& s) {
    begin_warning();
    put_string(s);
    end();
}

void ErrorHandlerImpl::error(const char* s) {
    begin_error();
    put_chars(s);
    end();
}

void ErrorHandlerImpl::error(const String& s) {
    begin_error();
    put_string(s);
    end();
}

void ErrorHandlerImpl::unrecoverable(const char* s) {
    begin_unrecoverable();
    put_chars(s);
    end();
}

void ErrorHandlerImpl::unrecoverable(const String& s) {
    begin_unrecoverable();
    put_string(s);
    end();
}

void ErrorHandlerImpl::internal(const char* s) {
    begin_internal();
    put_chars(s);
    end();
}

void ErrorHandlerImpl::internal(const String& s) {
    begin_internal();
    put_string(s);
    end();
}

/* class SourcePositionImpl */

SourcePositionImpl::SourcePositionImpl(FileName f, LineNumber n) {
    refcount_ = 1;
    if (f == nil) {
	filename_ = f;
    } else {
	filename_ = new CopyString(*f);
    }
    lineno_ = n;
}

SourcePositionImpl::~SourcePositionImpl() {
    delete filename_;
}

SourcePosition* SourcePositionImpl::clone() {
    return new SourcePositionImpl(filename_, lineno_);
}

void SourcePositionImpl::set(FileName f, LineNumber n) {
    if (f != nil) {
	delete filename_;
	filename_ = new CopyString(*f);
    }
    lineno_ = n;
}

void SourcePositionImpl::newline() { ++lineno_; }

FileName SourcePositionImpl::filename() { return filename_; }
LineNumber SourcePositionImpl::lineno() { return lineno_; }

void SourcePositionImpl::ref() { ++refcount_; }

void SourcePositionImpl::unref() {
    --refcount_;
    if (refcount_ <= 0) {
	delete this;
    }
}
