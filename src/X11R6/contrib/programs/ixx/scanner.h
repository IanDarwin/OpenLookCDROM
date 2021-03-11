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
 * Scanner - scan input
 */

#ifndef scanner_h
#define scanner_h

class ErrorHandler;
class ScannerKitImpl;

typedef int TokenType;

class Scanner {
protected:
    Scanner();
    virtual ~Scanner();
public:
    virtual TokenType get_token() = 0;
    virtual void print_token(TokenType) = 0;
    virtual void error(const char* msg) = 0;
    virtual void destroy() = 0;
};

class ScannerKit {
public:
    ScannerKit();
    virtual ~ScannerKit();

    virtual Scanner* make_scanner(
	const char* filename, ErrorHandler*, Boolean unique_case
    );
private:
    ScannerKitImpl* impl_;
};

#endif
