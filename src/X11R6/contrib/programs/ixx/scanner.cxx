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
 * Scanner -- scan input
 */

#include "err.h"
#include "scanner.h"
#include "table.h"
#include "tokendefs.h"
#include <ctype.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#if defined(sun) && !defined(SVR4)
#include <sysent.h>
#endif

/* global for yylex/yyparse */
extern Scanner* yyparse_scanner;

/* class Scanner */

Scanner::Scanner() { }
Scanner::~Scanner() { }

class IdentTable;
class KeywordTable;

typedef unsigned int CharSet;
typedef unsigned int CharType;

static const int char_maxchar = 0xff;

/*
 * Character types.
 */

static const int char_other = 0;
static const int char_white = 1;
static const int char_newline = 2;
static const int char_lower = 3;
static const int char_upper = 4;
static const int char_otherident = 5;
static const int char_digit = 6;
static const int char_hexdigit = 7;
static const int char_quote = 8;
static const int char_comment = 9;
static const int char_numbersign = 10;
static const int char_dot = 11;
static const int char_plus = 12;
static const int char_minus = 13;
static const int char_lt = 14;
static const int char_gt = 15;
static const int char_eq = 16;
static const int char_excl = 17;
static const int char_amper = 18;
static const int char_vertbar = 19;
static const int char_colon = 20;

/*
 * Character sets.
 */

static const int charset_empty = 0;
static const int charset_white = (1 << char_white) + (1 << char_newline);
static const int charset_upper = (1 << char_upper);
static const int charset_lower = (1 << char_lower);
static const int charset_alpha = charset_lower + charset_upper;
static const int charset_digit = (1 << char_digit);
static const int charset_alphanum = charset_alpha + charset_digit;
static const int charset_ident = charset_alpha + (1 << char_otherident);
static const int charset_identnum = charset_ident + charset_digit;
static const int charset_hexnum = (1 << char_hexdigit) + charset_digit;

/*
 * String buffer for input.
 */

static const long default_buffer_size = 50;

class StringBuffer {
public:
    StringBuffer();
    StringBuffer(const char*);
    StringBuffer(const StringBuffer&);
    ~StringBuffer();

    char* start() const;
    char* end() const;

    char* cur() const;
    void cur(char* ptr);

    String::Index length() const;

    void reset();
    void put(char);
    char* grow();

    void operator =(const char*);
    void operator =(const StringBuffer&);
private:
    String::Index alloc_;
    char* start_;
    char* end_;
    char* cur_;
    char default_data_[default_buffer_size];

    void init();
    void free();
};

inline char* StringBuffer::start() const { return start_; }
inline char* StringBuffer::end() const { return end_; }

inline char* StringBuffer::cur() const { return cur_; }
inline void StringBuffer::cur(char* ptr) { cur_ = ptr; }

inline String::Index StringBuffer::length() const { return cur_ - start_; }

/*
 * File management for input.
 */

static const int input_buffer_size = 4000;

static const int input_ok = 0;
static const int input_eof = 1 << 0;
static const int input_error = 1 << 1;

class InputFile {
public:
    InputFile(int fd = 0);
    ~InputFile();

    char read();
    void unread();
    void unread(char c);

    int status() { return status_; }

    Boolean ok();
    Boolean eof();
    Boolean error();
private:
    int fd_;
    int status_;
    char* ptr_;
    char* end_;
    char buffer_[input_buffer_size];

    void init(int);
    char fill();
};

inline char InputFile::read() { return ptr_ == end_ ? fill() : *ptr_++; }
inline void InputFile::unread() { if (ptr_ > buffer_) --ptr_; }
inline void InputFile::unread(char c) { if (ptr_ > buffer_) *--ptr_ = c; }

inline Boolean InputFile::ok() { return status_ == input_ok; }
inline Boolean InputFile::eof() { return (status_ & input_eof) != 0; }
inline Boolean InputFile::error() { return (status_ & input_error) != 0; }

class ScannerImpl : public Scanner {
public:
    ScannerImpl(
	InputFile*, const char* filename, ErrorHandler*, Boolean ucase
    );
    virtual ~ScannerImpl();

    virtual TokenType get_token();
    virtual void print_token(TokenType);
    virtual void error(const char*);
    virtual void destroy();
private:
    InputFile* in_;
    ErrorHandler* errs_;
    CharType chartype_[char_maxchar + 1];
    CharSet charset_[char_maxchar + 1];
    IdentTable* idents_;
    KeywordTable* keywords_;
    StringBuffer buf_;

    void init(InputFile*, const char* filename, ErrorHandler*, Boolean ucase);
    void init_charmap();
    void map(const char*, CharType);
    void add(const char*, CharType);
    CharType chartype(char);
    Boolean match(char, CharSet);
    void scan(CharSet);
    void enter(const char*, TokenType);
    TokenType lookup(const String&);
    TokenType get_ident(char);
    TokenType get_number(char);
    TokenType get_string(char);
    void get_special(char);
    void get_comment();
    void get_nlcomment();
    void get_position();
    long get_integer();
    char backslash(char);
};

inline CharType ScannerImpl::chartype(char c) { return chartype_[c]; }
inline Boolean ScannerImpl::match(char c, CharSet s) {
    return (charset_[c] & s) != 0;
}

inline unsigned long key_to_hash(String& s) { return s.hash(); }

declareTable(IdentTable,String,String)
implementTable(IdentTable,String,String)

declareTable(KeywordTable,String,TokenType)
implementTable(KeywordTable,String,TokenType)

/* class ScannerKit */

ScannerKit::ScannerKit() {
    yyparse_scanner = nil;
}

ScannerKit::~ScannerKit() { }

Scanner* ScannerKit::make_scanner(
    const char* filename, ErrorHandler* errs, Boolean ucase
) {
    int fd = 0;
    const char* name = filename;
    if (name == nil) {
	name = "<stdin>";
    } else {
	fd = ::open(name, O_RDONLY);
	if (fd < 0) {
	    return nil;
	}
    }

    if (yyparse_scanner != nil) {
	yyparse_scanner->destroy();
    }
    yyparse_scanner = new ScannerImpl(new InputFile(fd), name, errs, ucase);
    return yyparse_scanner;
}

/* class ScannerImpl */

ScannerImpl::ScannerImpl(
    InputFile* in, const char* filename, ErrorHandler* errs, Boolean ucase
) {
    init(in, filename, errs, ucase);
}

ScannerImpl::~ScannerImpl() { }

void ScannerImpl::init(
    InputFile* in, const char* name, ErrorHandler* errs, Boolean ucase
) {
    in_ = in;
    errs_ = errs;
    String fname(name);
    errs_->position()->set(&fname, 1);
    init_charmap();
    idents_ = ucase ? nil : new IdentTable(500);
    keywords_ = new KeywordTable(200);
    enter("attribute", ATTRIBUTE);
    enter("case", CASE);
    enter("const", CONST);
    enter("context", CONTEXT);
    enter("default", DEFAULT);
    enter("enum", ENUM);
    enter("exception", EXCEPTION);
    enter("FALSE", FALSE);
    enter("in", IN);
    enter("inout", INOUT);
    enter("interface", INTERFACE);
    enter("long", LONG);
    enter("module", MODULE);
    enter("oneway", ONEWAY);
    enter("operator", OPERATOR);
    enter("out", OUT);
    enter("raises", RAISES);
    enter("readonly", READONLY);
    enter("sequence", SEQUENCE);
    enter("string", STRING_TOKEN);
    enter("struct", STRUCT);
    enter("switch", SWITCH);
    enter("TRUE", TRUE);
    enter("typedef", TYPEDEF);
    enter("union", UNION);
    enter("unsigned", UNSIGNED);

    enter("asm", ASM);
    enter("auto", AUTO);
    enter("break", BREAK);
    enter("class", CLASS);
    enter("continue", CONTINUE);
    enter("delete", DELETE);
    enter("do", DO);
    enter("else", ELSE);
    enter("extern", EXTERN);
    enter("for", FOR);
    enter("friend", FRIEND);
    enter("goto", GOTO);
    enter("if", IF);
    enter("inline", INLINE);
    enter("new", NEW);
    enter("private", PRIVATE);
    enter("protected", PROTECTED);
    enter("public", PUBLIC);
    enter("register", REGISTER);
    enter("return", RETURN);
    enter("signed", SIGNED);
    enter("sizeof", SIZEOF);
    enter("static", STATIC);
    enter("template", TEMPLATE);
    enter("this", THIS);
    enter("virtual", VIRTUAL);
    enter("volatile", VOLATILE);
    enter("while", WHILE);
}

void ScannerImpl::init_charmap() {
    register int i;
    for (i = 0; i < char_maxchar; i++) {
	chartype_[i] = char_other;
	charset_[i] = charset_empty;
    }
    map(" \t\f", char_white);
    map("\n", char_newline);
    map("abcdefghijklmnopqrstuvwxyz", char_lower);
    map("ABCDEFGHIJKLMNOPQRSTUVWXYZ", char_upper);
    map("_$", char_otherident);
    map("0123456789", char_digit);
    add("abcdefABCDEF", char_hexdigit);
    map("'\"", char_quote);
    map("/", char_comment);
    map("#", char_numbersign);
    map(".", char_dot);
    map("+", char_plus);
    map("-", char_minus);
    map("<", char_lt);
    map(">", char_gt);
    map("=", char_eq);
    map("!", char_excl);
    map("&", char_amper);
    map("|", char_vertbar);
    map(":", char_colon);
}

/*
 * For each characters, we have two pieces of information.
 * One is the characters "primary" type.  The other is a set
 * of valid types.
 */

void ScannerImpl::map(const char* str, CharType t) {
    register const char* a;
    for (a = str; *a != '\0'; a++) {
	chartype_[*a] = t;
	charset_[*a] |= (1 << t);
    }
}

void ScannerImpl::add(const char* str, CharType t) {
    register const char* a;
    for (a = str; *a != '\0'; a++) {
	charset_[*a] |= (1 << t);
    }
}

TokenType ScannerImpl::get_token() {
    register char c, next;
    register InputFile* f = in_;
    for (c = f->read(); f->ok(); c = f->read()) {
	switch (chartype(c)) {
	case char_newline:
	    errs_->newline();
	    continue;
	case char_white:
	    continue;
	case char_digit:
	    return get_number(c);
	case char_lower:
	case char_upper:
	case char_otherident:
	    return get_ident(c);
	case char_quote:
	    return get_string(c);
	case char_comment:
	    c = f->read();
	    if (c == '*') {
		get_comment();
	    } else if (c == '/') {
		get_nlcomment();
	    } else {
		f->unread();
		return '/';
	    }
	    continue;
	case char_numbersign:
	    get_position();
	    return SRCPOS;
	case char_dot:
	    next = f->read();
	    if (match(next, charset_digit)) {
		return get_number('.');
	    } else if (next == '.') {
		next = f->read();
		if (next == '.') {
		    return ELLIPSES;
		}
		error("'..' is an invalid token sequence");
	    }
	    break;
	case char_plus:
	    if (f->read() == '+') {
		return INCR;
	    }
	    break;
	case char_minus:
	    next = f->read();
	    if (next == '-') {
		return DECR;
	    } else if (next == '>') {
		return ARROW;
	    }
	    break;
	case char_lt:
	    next = f->read();
	    if (next == '=') {
		return LE;
	    } else if (next == '<') {
		return LSHIFT;
	    }
	    break;
	case char_gt:
	    next = f->read();
	    if (next == '=') {
		return GE;
	    } else if (next == '>') {
		return RSHIFT;
	    }
	    break;
	case char_eq:
	    if (f->read() == '=') {
		return EQ;
	    }
	    break;
	case char_excl:
	    if (f->read() == '=') {
		return NE;
	    }
	    break;
	case char_amper:
	    if (f->read() == '&') {
		return AND;
	    }
	    break;
	case char_vertbar:
	    if (f->read() == '|') {
		return OR;
	    }
	    break;
	case char_colon:
	    if (f->read() == ':') {
		return SCOPE;
	    }
	    break;
	default:
	    return c;
	}
	/*
	 * We should only reach here if we checked a special character
	 * for a two-character token and did not find a match.
	 * So, we have to unget the most recently read character, and
	 * return the original one as the token.
	 */
	f->unread();
	return c;
    }
    return 0;
}

void ScannerImpl::enter(const char* name, TokenType t) {
    String s(name);
    keywords_->insert(s, t);
}

TokenType ScannerImpl::lookup(const String& name) {
    TokenType t;
    if (!keywords_->find(t, name)) {
	t = IDENT;
	yylval.string_ = new CopyString(name);
	if (idents_ != nil) {
	    String* original = yylval.string_;
	    const char* fold_chars = original->string();
	    long n = original->length();
	    char* new_chars = nil;
	    for (const char* o = fold_chars; *o != '\0'; o++) {
		if (*o != tolower(*o)) {
		    new_chars = new char[n + 1];
		    char* s1 = new_chars;
		    const char* s2 = fold_chars;
		    for (long i = n; i > 0; i--, s1++, s2++) {
			*s1 = tolower(*s2);
		    }
		    *s1 = '\0';
		    fold_chars = new_chars;
		    break;
		}
	    }
	    String key(fold_chars, n);
	    String found;
	    if (!idents_->find(found, key)) {
		idents_->insert(key, *original);
	    } else {
		if (found != *original) {
		    errs_->begin_error();
		    errs_->put_chars("Mixed case identifier \"");
		    errs_->put_string(*original);
		    errs_->put_chars("\"");
		    errs_->end();
		}
		delete new_chars;
	    }
	}
    }
    return t;
}

TokenType ScannerImpl::get_ident(char c) {
    buf_.reset();
    buf_.put(c);
    scan(charset_identnum);
    String s(buf_.start(), buf_.length());
    return lookup(s);
}

TokenType ScannerImpl::get_number(char ch) {
    buf_.reset();
    buf_.put(ch);

    char c;
    InputFile* f = in_;
    Boolean isint = true;
    scan(charset_digit);
    c = f->read();
    if ((c == 'x' || c == 'X') && buf_.length() == 1 && *buf_.start() == '0') {
	buf_.put(c);
	scan(charset_hexnum);
    } else {
	if (c == '.') {
	    isint = false;
	    buf_.put('.');
	    scan(charset_digit);
	    c = f->read();
	}
	if (c == 'e' || c == 'E') {
	    isint = false;
	    buf_.put(c);
	    c = f->read();
	    if (c == '+' || c == '-') {
		buf_.put(c);
	    } else {
		f->unread();
	    }
	    scan(charset_digit);
	    c = f->read();
	}
	if (!isint || !(c == 'l' || c == 'L')) {
	    f->unread();
	}
    }

    TokenType t;
    if (isint) {
	t = INTCON;
	yylval.long_ = get_integer();
    } else {
	buf_.put('\0');
	t = FLOATCON;
	yylval.double_ = atof(buf_.start());
    }
    return t;
}

long ScannerImpl::get_integer() {
    register const char* p = buf_.start();
    register const char* q = p + buf_.length();
    register int n = 0;
    if (p[0] == '0' && buf_.length() > 1) {
	if (p[1] == 'x' || p[1] == 'X') {
	    for (p += 2; p < q; p++) {
		n = (n << 4) + (*p - '0');
		if (match(*p, charset_upper)) {
		    n += (10 + '0' - 'A');
		} else if (match(*p, charset_lower)) {
		    n += (10 + '0' - 'a');
		}
	    }
	} else {
	    for (p += 1; p < q; p++) {
		n = (n << 3) + (*p - '0');
	    }
	}
    } else {
	for (; p < q; p++) {
	    /* 10*x = 2*(4*x+1) == ((x << 2) + x) << 1 */
	    n = (((n << 2) + n) << 1) + (*p - '0');
	}
    }
    return n;
}

TokenType ScannerImpl::get_string(char q) {
    buf_.reset();
    register char c;
    register char* p = buf_.start();
    InputFile* f = in_;
    for (;;) {
	c = f->read();
	if (c == '\0' || c == '\n') {
	    error("Non-terminated string constant");
	    break;
	}
	if (p == buf_.end()) {
	    p = buf_.grow();
	}
	if (c == q) {
	    break;
	} else if (c == '\\') {
	    c = f->read();
	    if (c != '\n') {
		c = backslash(c);
	    }
	}
	*p++ = c;
    }
    *p = '\0';
    buf_.cur(p);

    TokenType t;
    if (q == '\'') {
	if (buf_.length() > 1) {
	    error("Multi-character constant");
	}
	t = CHARCON;
	yylval.long_ = *buf_.start();
    } else {
	t = STRING;
	yylval.string_ = new CopyString(buf_.start(), buf_.length());
    }
    return t;
}

/*
 * This routine handles a backslash within a string literal.
 * Because we plan to output the string as is, we don't do any processing.
 * However, other uses of this scanner might want to change backslashes
 * as shown in the code below (#ifdef'd out).
 */

char ScannerImpl::backslash(char ch) {
    in_->unread(ch);
    return '\\';
#ifdef notdef
    String src = "\\nrt'\"";
    String dst = "\\\n\r\t'\"";

    register char c = ch;
    String::Index search = src.index(c);
    if (search != -1) {
	c = dst[search];
    } else if (match(c, charset_digit)) {
	scan(charset_digit);
	c = (char)get_integer();
    } else {
	in_->unread(c);
	c = '\\';
    }
    return c;
#endif
}

void ScannerImpl::get_comment() {
    InputFile* f = in_;
    register char c = f->read();
    while (f->ok()) {
	if (c == '*') {
	    c = f->read();
	    if (c == '/') {
		break;
	    }
	} else {
	    if (c == '\n') {
		errs_->newline();
	    }
	    c = f->read();
	}
    }
}

void ScannerImpl::get_nlcomment() {
    register int c;
    InputFile* f = in_;
    do {
	c = f->read();
    } while (f->status() == input_ok && c != '\n');
    errs_->newline();
}

void ScannerImpl::get_position() {
    if (get_token() != INTCON) {
	error("Expected line number");
    } else {
	long n = yylval.long_;
	if (get_token() != STRING) {
	    error("Expected filename");
	} else {
	    SourcePosition* p = errs_->position();
	    p->set(yylval.string_, n);
	    yylval.position_ = p->clone();
	}
    }
    register char c;
    InputFile* f = in_;
    do {
	c = f->read();
    } while (c != '\n' && f->ok());
}

/*
 * Scan characters from input matching the given character set and
 * put them in the token's string buffer.
 */

void ScannerImpl::scan(CharSet s) {
    register char c;
    register char* p = buf_.cur();
    register char* end = buf_.end();
    InputFile* f = in_;
    for (c = f->read(); match(c, s); c = f->read()) {
	if (p == end) {
	    p = buf_.grow();
	    end = buf_.end();
	}
	*p++ = c;
    }
    f->unread();
    buf_.cur(p);
}

/*
 * Print the current token.
 */

void ScannerImpl::print_token(TokenType t) {
    static const char* special[] = {
	"::", "++", "--", "->", "<=", "<<", ">=", ">>", "==", "!=",
	"&&", "||", "..."
    };

    switch (t) {
    case INTCON:
	printf("int %d", yylval.long_);
	break;
    case FLOATCON:
	printf("float %g", yylval.double_);
	break;
    case CHARCON:
	if (yylval.long_ < ' ' || yylval.long_ > '~') {
	    printf("char %d", yylval.long_);
	} else {
	    printf("char '%c'", yylval.long_);
	}
	break;
    case STRING:
	printf("string '%.*s'", buf_.length(), buf_.start());
	break;
    case IDENT:
	printf("name %.*s", buf_.length(), buf_.start());
	break;
    case INCR:
    case DECR:
    case ARROW:
    case LE:
    case LSHIFT:
    case GE:
    case RSHIFT:
    case EQ:
    case NE:
    case AND:
    case OR:
	printf("%s", special[t - INCR]);
	break;
    default:
	if (t > 256) {
	    printf("keyword '%.*s'", buf_.length(), buf_.start());
	} else if (t < ' ' || t > '~') {
	    printf("char %d", int(t));
	} else {
	    printf("char '%c'", char(t));
	}
	break;
    }
}

/*
 * Report an error.
 */

void ScannerImpl::error(const char* msg) { errs_->error(msg); }

/*
 * Make destructor-like operation publicly available.
 * Note that (unfortunately) the client must test for nil
 * before calling this function.
 */

void ScannerImpl::destroy() { delete this; }

/* class InputFile */

InputFile::InputFile(int fd) {
    init(fd);
}

void InputFile::init(int fd) {
    fd_ = fd;
    status_ = input_ok;
    ptr_ = buffer_ + input_buffer_size;
    end_ = ptr_;
}

char InputFile::fill() {
    int nread = ::read(fd_, buffer_, sizeof(buffer_));
    if (nread > 0) {
	ptr_ = buffer_;
	end_ = buffer_ + nread;
    } else if (nread == 0) {
	status_ |= input_eof;
    } else {
	status_ |= input_error;
    }
    return *ptr_++;
}

/* class StringBuffer */

inline void StringBuffer::init() {
    start_ = default_data_;
    alloc_ = sizeof(default_data_);
    end_ = start_ + alloc_;
    cur_ = start_;
}

inline void StringBuffer::free() {
    if (start_ != default_data_) {
	delete start_;
    }
}

StringBuffer::StringBuffer() {
    init();
}

StringBuffer::StringBuffer(const char* s) {
    init();
    *this = s;
}

StringBuffer::StringBuffer(const StringBuffer& b) {
    init();
    String::Index len = b.length();
    if (len > alloc_) {
	start_ = new char[len];
	alloc_ = len;
	end_ = start_ + alloc_;
    }
    Memory::copy(b.start_, start_, (int)len);
    cur_ = start_ + len;
}

StringBuffer::~StringBuffer() {
    free();
}

void StringBuffer::reset() {
    free();
    init();
}

/*
 * Put a single character into the buffer (usually at the start
 * of reading more characters).
 */

void StringBuffer::put(char c) {
    if (cur_ == end_) {
	grow();
    }
    *cur_++ = c;
}

/*
 * Grow the buffer and return a pointer to the beginning
 * of the NEW part of the buffer.
 */

char* StringBuffer::grow() {
    String::Index newalloc = 2 * alloc_;
    String::Index oldalloc = alloc_;
    char* tmp = new char[newalloc];
    Memory::copy(start_, tmp, (int)alloc_);
    free();
    start_ = tmp;
    cur_ = start_ + alloc_;
    alloc_ = newalloc;
    end_ = start_ + alloc_;
    return cur_;
}

void StringBuffer::operator =(const char* s) {
    String str(s);
    String::Index len = str.length();
    if (len > alloc_) {
	start_ = new char[len];
	alloc_ = len;
	end_ = start_ + alloc_;
    }
    Memory::copy(s, start_, (int)len);
    cur_ = start_ + len;
}

/*
 * Copy one string buffer to another.
 */

void StringBuffer::operator =(const StringBuffer& s) {
    String::Index len = s.length();
    if (alloc_ >= len) {
	Memory::copy(s.start(), start_, (int)len);
	cur_ = start_ + len;
    } else {
	char* tmp = new char[s.alloc_];
	Memory::copy(s.start(), tmp, (int)len);
	free();
	start_ = tmp;
	cur_ = start_ + len;
	alloc_ = s.alloc_;
	end_ = start_ + alloc_;
    }
}
