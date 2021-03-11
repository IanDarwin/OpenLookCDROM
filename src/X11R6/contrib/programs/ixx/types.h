/*
 * Copyright (c) 1987-91 Stanford University
 * Copyright (c) 1991-93 Silicon Graphics, Inc.
 *
 * Permission to use, copy, modify, distribute, and sell this software and 
 * its documentation for any purpose is hereby granted without fee, provided
 * that (i) the above copyright notices and this permission notice appear in
 * all copies of the software and related documentation, and (ii) the names of
 * Stanford and Silicon Graphics may not be used in any advertising or
 * publicity relating to the software without the specific, prior written
 * permission of Stanford and Silicon Graphics.
 * 
 * THE SOFTWARE IS PROVIDED "AS-IS" AND WITHOUT WARRANTY OF ANY KIND, 
 * EXPRESS, IMPLIED OR OTHERWISE, INCLUDING WITHOUT LIMITATION, ANY 
 * WARRANTY OF MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE.  
 *
 * IN NO EVENT SHALL STANFORD OR SILICON GRAPHICS BE LIABLE FOR
 * ANY SPECIAL, INCIDENTAL, INDIRECT OR CONSEQUENTIAL DAMAGES OF ANY KIND,
 * OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
 * WHETHER OR NOT ADVISED OF THE POSSIBILITY OF DAMAGE, AND ON ANY THEORY OF 
 * LIABILITY, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE 
 * OF THIS SOFTWARE.
 */

#ifndef types_h
#define types_h

#define nil 0

typedef unsigned char Boolean;

static const unsigned char false = 0;
static const unsigned char true = 1;

inline Boolean is_nil(void* v) { return v == 0; }
inline Boolean is_not_nil(void* v) { return v != 0; }

typedef unsigned char octet;

class Memory {
public:
    static void copy(const void* from, void* to, unsigned int nbytes);
    static int compare(const void*, const void*, unsigned int nbytes);
    static void zero(void*, unsigned int nbytes);
};

/*
 * String - simple (non-copying) string class
 */

class String {
public:
    String();
    String(const char*);
    String(const char*, long length);
    String(const String&);
    virtual ~String();

    typedef long Index;
    typedef unsigned long HashValue;

    const char* string() const;
    String::Index length() const;

    virtual String::HashValue hash() const;
    virtual String& operator =(const String&);
    virtual String& operator =(const char*);
    virtual Boolean operator ==(const String&) const;
    virtual Boolean operator ==(const char*) const;
    virtual Boolean operator !=(const String&) const;
    virtual Boolean operator !=(const char*) const;
    virtual Boolean operator >(const String&) const;
    virtual Boolean operator >(const char*) const;
    virtual Boolean operator >=(const String&) const;
    virtual Boolean operator >=(const char*) const;
    virtual Boolean operator <(const String&) const;
    virtual Boolean operator <(const char*) const;
    virtual Boolean operator <=(const String&) const;
    virtual Boolean operator <=(const char*) const;

    virtual Boolean case_insensitive_equal(const String&) const;
    virtual Boolean case_insensitive_equal(const char*) const;

    octet operator [](String::Index index) const;
    virtual String substr(String::Index start, String::Index length) const;
    String left(String::Index length) const;
    String right(String::Index start) const;

    virtual void set_to_substr(String::Index start, String::Index length);
    void set_to_left(String::Index length);
    void set_to_right(String::Index start);

    virtual String::Index search(String::Index start, octet) const;
    String::Index index(octet) const;
    String::Index rindex(octet) const;

    virtual Boolean convert(int&) const;
    virtual Boolean convert(long&) const;
    virtual Boolean convert(float&) const;
    virtual Boolean convert(double&) const;

    virtual Boolean null_terminated() const;

    static void case_sensitive(Boolean);
    static Boolean case_sensitive();
protected:
    virtual void set_value(const char*);
    virtual void set_value(const char*, String::Index);
private:
    const char* data_;
    String::Index length_;
    static Boolean case_sensitive_;
};

class CopyString : public String {
public:
    CopyString();
    CopyString(const char*);
    CopyString(const char*, String::Index length);
    CopyString(const String&);
    CopyString(const CopyString&);
    virtual ~CopyString();

    virtual String& operator =(const String&);
    virtual String& operator =(const char*);

    virtual Boolean null_terminated() const;
protected:
    virtual void set_value(const char*);
    virtual void set_value(const char*, String::Index);
private:
    void free();
};

class NullTerminatedString : public String {
public:
    NullTerminatedString();
    NullTerminatedString(const String&);
    NullTerminatedString(const NullTerminatedString&);
    virtual ~NullTerminatedString();

    virtual String& operator =(const String&);
    virtual String& operator =(const char*);

    virtual Boolean null_terminated() const;
private:
    Boolean allocated_;

    void assign(const String&);
    void free();
};

inline const char* String::string() const { return data_; }
inline String::Index String::length() const { return length_; }
inline octet String::operator [](String::Index index) const {
    return ((const octet*)data_)[index];
}

inline String String::left(String::Index length) const {
    return substr(0, length);
}

inline String String::right(String::Index start) const {
    return substr(start, -1);
}

inline void String::set_to_left(String::Index length) {
    set_to_substr(0, length);
}

inline void String::set_to_right(String::Index start) {
    set_to_substr(start, -1);
}

inline long String::index(octet c) const { return search(0, c); }
inline long String::rindex(octet c) const { return search(-1, c); }

/*
 * UniqueString - unique string using hash table
 */

class UniqueStringPool;
class UniqueStringTable;

class UniqueString : public String {
public:
    UniqueString();
    UniqueString(const char*);
    UniqueString(const char*, String::Index length);
    UniqueString(const String&);
    UniqueString(const UniqueString&);
    virtual ~UniqueString();

    virtual String::HashValue hash() const;
    virtual Boolean operator ==(const String&) const;
    virtual Boolean operator ==(const char*) const;

    virtual Boolean null_terminated() const;
private:
    static UniqueStringTable* table_;
    static UniqueStringPool* pool_;

    void init(const String&);
};

#endif
