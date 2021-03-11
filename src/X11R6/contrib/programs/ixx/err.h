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

#ifndef err_h
#define err_h

#include "types.h"

typedef String* FileName;
typedef long LineNumber;

class SourcePosition {
protected:
    SourcePosition();
    virtual ~SourcePosition();
public:
    virtual SourcePosition* clone() = 0;
    virtual void set(FileName, LineNumber) = 0;
    virtual void newline() = 0;
    virtual FileName filename() = 0;
    virtual LineNumber lineno() = 0;

    virtual void ref() = 0;
    virtual void unref() = 0;
};

class ErrorHandler {
protected:
    ErrorHandler();
    virtual ~ErrorHandler();
public:
    virtual void prefix(const char*) = 0;
    virtual void prefix(const String&) = 0;
    virtual void position(SourcePosition*) = 0;
    virtual SourcePosition* position() = 0;
    virtual void newline() = 0;
    virtual void begin_comment() = 0;
    virtual void begin_warning() = 0;
    virtual void begin_error() = 0;
    virtual void begin_unrecoverable() = 0;
    virtual void begin_internal() = 0;
    virtual void put_chars(const char*) = 0;
    virtual void put_string(const String&) = 0;
    virtual void put_integer(long) = 0;
    virtual void put_float(double) = 0;
    virtual void end() = 0;
    virtual long count() = 0;
    virtual void destroy() = 0;

    /* short-hand */
    virtual void comment(const char*) = 0;
    virtual void comment(const String&) = 0;
    virtual void warning(const char*) = 0;
    virtual void warning(const String&) = 0;
    virtual void error(const char*) = 0;
    virtual void error(const String&) = 0;
    virtual void unrecoverable(const char*) = 0;
    virtual void unrecoverable(const String&) = 0;
    virtual void internal(const char*) = 0;
    virtual void internal(const String&) = 0;
};

class ErrorHandlerKitImpl;

class ErrorHandlerKit {
public:
    ErrorHandlerKit();
    virtual ~ErrorHandlerKit();

    virtual ErrorHandler* handler();
private:
    ErrorHandlerKitImpl* impl_;
};

#endif
