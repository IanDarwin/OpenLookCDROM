/*
 * Copyright (c) 1991 Stanford University
 * Copyright (c) 1991-93 Silicon Graphics, Inc.
 * Copyright (c) 1993 Fujitsu, Ltd.
 *
 * Permission to use, copy, modify, distribute, and sell this software and 
 * its documentation for any purpose is hereby granted without fee, provided
 * that (i) the above copyright notices and this permission notice appear in
 * all copies of the software and related documentation, and (ii) the names
 * of Stanford, Silicon Graphics, and Fujitsu may not be used in any
 * advertising or publicity relating to the software without the specific,
 * prior written permission of Stanford, Silicon Graphics, and Fujitsu.
 * 
 * THE SOFTWARE IS PROVIDED "AS-IS" AND WITHOUT WARRANTY OF ANY KIND, 
 * EXPRESS, IMPLIED OR OTHERWISE, INCLUDING WITHOUT LIMITATION, ANY 
 * WARRANTY OF MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE.  
 *
 * IN NO EVENT SHALL STANFORD, SILICON GRAPHICS, OR FUJITSU BE LIABLE FOR
 * ANY SPECIAL, INCIDENTAL, INDIRECT OR CONSEQUENTIAL DAMAGES OF ANY KIND,
 * OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
 * WHETHER OR NOT ADVISED OF THE POSSIBILITY OF DAMAGE, AND ON ANY THEORY OF 
 * LIABILITY, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE 
 * OF THIS SOFTWARE.
 */

/*
 * Ix has a copy of this code to avoid circularities during development
 * (ix depends on library--library uses ix to generate headers).
 */

#include "list.h"
#include "table.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/*
 * template<T> class List<T>
 */

implementList(__AnyPtrList,__AnyPtr)

static long ListImpl_best_new_sizes[] = {
    48, 112, 240, 496, 1008, 2032, 4080, 8176,
    16368, 32752, 65520, 131056, 262128, 524272, 1048560,
    2097136, 4194288, 8388592, 16777200, 33554416, 67108848
};

long ListImpl_best_new_count(long count, unsigned int size) {
    for (int i = 0; i < sizeof(ListImpl_best_new_sizes)/sizeof(long); i++) {
        if (count * size < ListImpl_best_new_sizes[i]) {
            return ListImpl_best_new_sizes[i] / size;
        }
    }
    return count;
}

void ListImpl_range_error(long i) {
    fprintf(stderr, "internal error: list index %d out of range\n", i);
    abort();
}

/* class Memory */

void Memory::copy(const void* from, void* to, unsigned int nbytes) {
#if defined(sun) && !defined(SVR4)
    bcopy(from, to, nbytes);
#else
    memmove(to, from, nbytes);
#endif
}

int Memory::compare(const void* b1, const void* b2, unsigned int nbytes) {
    return memcmp(b1, b2, nbytes) != 0;
}

void Memory::zero(void* b, unsigned int nbytes) {
    memset(b, 0, nbytes);
}

/* class String */

/*
 * Just to be sure ...
 */

extern "C" {
#ifndef tolower
    extern int tolower(int);
#endif
#ifndef toupper
    extern int toupper(int);
#endif
    extern long int strtol(const char*, char**, int);
    extern double strtod(const char*, char**);
}

String::String() {
    data_ = nil;
    length_ = 0;
}

String::String(const char* s) {
    data_ = s;
    length_ = strlen(s);
}

String::String(const char* s, String::Index n) {
    data_ = s;
    length_ = n;
}

String::String(const String& s) {
    data_ = s.data_;
    length_ = s.length_;
}

String::~String() { }

/*
 * Hash values are case insensitive for now.
 */

unsigned long String::hash() const {
    const char* p;
    unsigned long v = 0;
    if (length_ == -1) {
	for (p = data_; *p != '\0'; p++) {
	    v = (v << 1) ^ tolower(*p);
	}
	String* s = (String*)this;
	s->length_ = p - data_;
    } else {
	const char* q = &data_[length_];
	for (p = data_; p < q; p++) {
	    v = (v << 1) ^ tolower(*p);
	}
    }
    unsigned long t = v >> 10;
    t ^= (t >> 10);
    return v ^ t;
}

String& String::operator =(const String& s) {
    data_ = s.data_;
    length_ = s.length_;
    return *this;
}

String& String::operator =(const char* s) {
    data_ = s;
    length_ = strlen(s);
    return *this;
}

Boolean String::case_sensitive_;

void String::case_sensitive(Boolean b) { case_sensitive_ = b; }
Boolean String::case_sensitive() { return case_sensitive_; }

Boolean String::operator ==(const String& s) const {
    if (!case_sensitive_) {
	return case_insensitive_equal(s);
    }
    return (
	length_ == s.length_ &&
	strncmp(data_, s.data_, (unsigned int)length_) == 0
    );
}

Boolean String::operator ==(const char* s) const {
    if (!case_sensitive_) {
	return case_insensitive_equal(s);
    }
    return (
	strncmp(data_, s, (unsigned int)length_) == 0 &&
	s[length_] == '\0'
    );
}

Boolean String::operator !=(const String& s) const {
    return (
	length_ != s.length_ ||
	strncmp(data_, s.data_, (unsigned int)length_) != 0
    );
}

Boolean String::operator !=(const char* s) const {
    return (
	strncmp(data_, s, (unsigned int)length_) != 0 ||
	s[length_] != '\0'
    );
}

Boolean String::operator >(const String& s) const {
    return strncmp(data_, s.data_, (unsigned int)length_) > 0;
}

Boolean String::operator >(const char* s) const {
    return strncmp(data_, s, (unsigned int)length_) > 0;
}

Boolean String::operator >=(const String& s) const {
    return strncmp(data_, s.data_, (unsigned int)length_) >= 0;
}

Boolean String::operator >=(const char* s) const {
    return strncmp(data_, s, (unsigned int)length_) >= 0;
}

Boolean String::operator <(const String& s) const {
    return strncmp(data_, s.data_, (unsigned int)length_) < 0;
}

Boolean String::operator <(const char* s) const {
    return strncmp(data_, s, (unsigned int)length_) < 0;
}

Boolean String::operator <=(const String& s) const {
    return strncmp(data_, s.data_, (unsigned int)length_) <= 0;
}

Boolean String::operator <=(const char* s) const {
    return strncmp(data_, s, (unsigned int)length_) <= 0;
}

Boolean String::case_insensitive_equal(const String& s) const {
    if (length() != s.length()) {
	return false;
    }
    const char* p = string();
    const char* p2 = s.string();
    const char* q = p + length();
    for (; p < q; p++, p2++) {
	int c1 = *p;
	int c2 = *p2;
	if (c1 != c2 && tolower(c1) != tolower(c2)) {
	    return false;
	}
    }
    return true;
}

Boolean String::case_insensitive_equal(const char* s) const {
    return case_insensitive_equal(String(s));
}

/*
 * A negative value for start initializes the position at the end
 * of the string before indexing.  Any negative length makes
 * the substring extend to the end of the string.
 */

String String::substr(String::Index start, String::Index length) const {
    if (start >= length_ || start < -length_) {
	/* should raise exception */
	return *this;
    }
    String::Index pos = (start >= 0) ? start : (length_ + start);
    if (pos + length > length_) {
	/* should raise exception */
	return *this;
    }
    String::Index len = (length >= 0) ? length : (length_ - pos);
    return String(data_ + pos, len);
}

void String::set_to_substr(String::Index start, String::Index length) {
    if (start > length_ || start < -length_) {
	/* should raise exception */
	return;
    }
    String::Index pos = (start >= 0) ? start : (length_ + start);
    if (pos + length > length_) {
	/* should raise exception */
	return;
    }
    String::Index len = (length >= 0) ? length : (length_ - pos);
    data_ += pos;
    length_ = len;
}

Boolean String::null_terminated() const { return false; }

void String::set_value(const char* s) {
    data_ = s;
    length_ = strlen(s);
}

void String::set_value(const char* s, String::Index len) {
    data_ = s;
    length_ = len;
}

/*
 * A negative value for start initializes the position to the end
 * of the string before indexing and searches right-to-left.
 */

String::Index String::search(String::Index start, octet c) const {
    if (start >= length_ || start < -length_) {
	/* should raise exception */
	return -1;
    }
    if (start >= 0) {
	const char* end = data_ + length_;
	for (const char* p = data_ + start; p < end; p++) {
	    if (*p == c) {
		return p - data_;
	    }
	}
    } else {
	for (const char* p = data_ + length_ + start; p >= data_; p--) {
	    if (*p == c) {
		return p - data_;
	    }
	}
    }
    return -1;
}

/*
 * Convert a string to binary value.
 */

Boolean String::convert(int& value) const {
    NullTerminatedString s(*this);
    const char* str = s.string();
    char* ptr;
    value = (int)strtol(str, &ptr, 0);
    return ptr != str;
}

Boolean String::convert(long& value) const {
    NullTerminatedString s(*this);
    const char* str = s.string();
    char* ptr;
    value = strtol(str, &ptr, 0);
    return ptr != str;
}

Boolean String::convert(float& value) const {
    NullTerminatedString s(*this);
    const char* str = s.string();
    char* ptr;
    value = (float)strtod(str, &ptr);
    return ptr != str;
}

Boolean String::convert(double& value) const {
    NullTerminatedString s(*this);
    const char* str = s.string();
    char* ptr;
    value = strtod(str, &ptr);
    return ptr != str;
}

/* class CopyString */

CopyString::CopyString() : String() { }

CopyString::CopyString(const char* s) : String() {
    set_value(s);
}

CopyString::CopyString(const char* s, String::Index length) : String() {
    set_value(s, length);
}

CopyString::CopyString(const String& s) : String() {
    set_value(s.string(), s.length());
}

CopyString::CopyString(const CopyString& s) : String() {
    set_value(s.string(), s.length());
}

CopyString::~CopyString() {
    free();
}

String& CopyString::operator =(const String& s) {
    free();
    set_value(s.string(), s.length());
    return *this;
}

String& CopyString::operator =(const char* s) {
    free();
    set_value(s);
    return *this;
}

Boolean CopyString::null_terminated() const { return true; }

void CopyString::set_value(const char* s) {
    set_value(s, strlen(s));
}

/*
 * Guarantee null-terminated string for compatibility with printf et al.
 */

void CopyString::set_value(const char* s, String::Index len) {
    char* ns = new char[len + 1];
    ns[len] = '\0';
    String::set_value(strncpy(ns, s, (unsigned int)len), len);
}

void CopyString::free() {
    char* s = (char*)(string());
    delete s;
}

/*
 * class NullTerminatedString
 */

NullTerminatedString::NullTerminatedString() : String() {
    allocated_ = false;
}

NullTerminatedString::NullTerminatedString(const String& s) : String() {
    assign(s);
}

NullTerminatedString::NullTerminatedString(
    const NullTerminatedString& s
) : String() {
    allocated_ = false;
    String::set_value(s.string(), s.length());
}

NullTerminatedString::~NullTerminatedString() {
    free();
}

String& NullTerminatedString::operator =(const String& s) {
    free();
    assign(s);
    return *this;
}

String& NullTerminatedString::operator =(const char* s) {
    free();
    allocated_ = false;
    String::set_value(s, strlen(s));
    return *this;
}

Boolean NullTerminatedString::null_terminated() const { return true; }

void NullTerminatedString::assign(const String& s) {
    if (s.null_terminated()) {
	allocated_ = false;
	String::set_value(s.string(), s.length());
    } else {
	allocated_ = true;
	String::Index len = s.length();
	char* ns = new char[len + 1];
	ns[len] = '\0';
	String::set_value(strncpy(ns, s.string(), (unsigned int)len), len);
    }
}

void NullTerminatedString::free() {
    if (allocated_) {
	char* s = (char*)(string());
	delete s;
	allocated_ = false;
    }
}

/* class UniqueString */

/*
 * UniqueString uses a table for matching strings and a string pool
 * for storing the string data.  A pool is presumably more efficient
 * than malloc/new, but individual strings cannot be deallocated.
 */

inline unsigned long key_to_hash(String& s) { return s.hash(); }

declareTable(UniqueStringTable,String,String)
implementTable(UniqueStringTable,String,String)

static const unsigned strpoolsize = 800;

class UniqueStringPool {
public:
    UniqueStringPool(unsigned long poolsize = strpoolsize);
    ~UniqueStringPool();

    char* add(const char*, unsigned long);
private:
    char* data;
    unsigned long size;
    unsigned long cur;
    UniqueStringPool* prev;
};

UniqueStringTable* UniqueString::table_;
UniqueStringPool* UniqueString::pool_;

UniqueString::UniqueString() : String() { }
UniqueString::UniqueString(const char* s) : String() { init(String(s)); }
UniqueString::UniqueString(const char* s, String::Index n) : String() {
    init(String(s, n));
}
UniqueString::UniqueString(const String& s) : String() { init(s); }
UniqueString::UniqueString(const UniqueString& s) : String(s) { }
UniqueString::~UniqueString() { }

void UniqueString::init(const String& s) {
    if (table_ == nil) {
        table_ = new UniqueStringTable(256);
    }
    if (!table_->find(*this, s)) {
        if (pool_ == nil) {
            pool_ = new UniqueStringPool;
        }
        String::Index n = s.length();
        set_value(pool_->add(s.string(), n), n);
        table_->insert(*this, *this);
    }
}

/*
 * UniqueString's have a unique data pointer, so we can just use
 * that for a hash value.
 */

unsigned long UniqueString::hash() const { return key_to_hash(string()); }

Boolean UniqueString::operator ==(const String& s) const {
    return string() == s.string() && length() == s.length();
}

Boolean UniqueString::operator ==(const char* s) const {
    return String::operator ==(s);
}

Boolean UniqueString::null_terminated() const { return false; }

/*
 * UniqueStringPool implementation.
 */

UniqueStringPool::UniqueStringPool(unsigned long poolsize) {
    data = new char[poolsize];
    size = poolsize;
    cur = 0;
    prev = nil;
}

/*
 * Tail-recursive deletion to walk the list back to the head
 * of the pool.
 */

UniqueStringPool::~UniqueStringPool() {
    delete data;
    delete prev;
}

/*
 * Add a string of a given length to the pool.  If it won't fit,
 * create a copy of the current pool and allocate space for a new one.
 *
 * No null-padding is implied, so if you want that you must include
 * the null in the length.
 */

char* UniqueStringPool::add(const char* str, unsigned long len) {
    if (len > strpoolsize) {
	UniqueStringPool* s = new UniqueStringPool(len);
	strncpy(s->data, str, (unsigned int)len);
	s->cur = len;
	s->prev = prev;
	prev = s;
	return s->data;
    }
    unsigned long index = cur;
    unsigned long newcur = index + len;
    if (newcur > size) {
	UniqueStringPool* s = new UniqueStringPool;
	char* newdata = s->data;
	s->data = data;
	s->size = size;
	s->cur = cur;
	s->prev = prev;
	data = newdata;
	prev = s;
	index = 0;
	newcur = len;
    }
    char* r = &data[index];
    strncpy(r, str, (unsigned int)len);
    cur = newcur;
    return r;
}
