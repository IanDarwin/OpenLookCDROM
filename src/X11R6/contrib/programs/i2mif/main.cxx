/*
 * Copyright (c) 1991 Silicon Graphics, Inc.
 * Copyright (c) 1993 Fujitsu, Ltd.
 *
 * Permission to use, copy, modify, distribute, and sell this software and 
 * its documentation for any purpose is hereby granted without fee, provided
 * that (i) the above copyright notices and this permission notice appear in
 * all copies of the software and related documentation, and (ii) the names of
 * Silicon Graphics and Fujitsu may not be used in any advertising
 * or publicity relating to the software without the specific, prior written
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

/* Possible enhancments:
 *   o Allow invocation of "i2mif *.idl". Use OS/file rather that stdio.
 *   o Allow font change and marker to span input lines.
 *   o Allow font change within marker and font change within font change.
 */

#include <stdio.h>
#include <string.h>
#include <stdlib.h>  // qsort
#include <ctype.h>   // ispunct
#include <X11/Fresco/OS/list.h>

#if defined(AIXV3)
extern "C" int strcasecmp(const char *, const char *);
#endif

static const char* beg_heading_tok         = "//-";
static const char* end_interface_tok       = "};";
static const char* beg_index_marker_tok    = "\\marker{";
static const char* end_index_marker_tok    = "}";
static const char* beg_emph_tok            = "\\emphasis{";
static const char* beg_bold_tok            = "\\bold{";
static const char* end_font_change_tok     = "}";
static const char* beg_comment_tok         = "//.";
static const char* beg_code_comment_tok    = "//{";
static const char* end_code_comment_tok    = "//}";
static const char* normal_comment_tok      = "//.";
static const char* ix_tok                  = "//+";
static const char* hard_return_comment_tok = "\\newline";
static const char* white_space             = " \t\n";
static const char  tab_tok_char            = '.';

static const char* descr_heading_para_tag    = "InterfHeading";
static const char* descr_code_para_tag       = "Interface";
static const char* descr_comment_para_tag    = "InterfaceBody";
static const char* descr_comment_para2_tag   = "InterfBody2";

static const char* section_heading_para_tag  = "OpHeading";
static const char* section_code_para_tag     = "Operation";
static const char* section_comment_para_tag  = "OperationBody";
static const char* section_comment_para2_tag = "OpBody2";

static const char* code_desc_comment_para_tag = "Code";
static const char* code_sect_comment_para_tag = "Code2";
static const char* def_emphasis_font_tag      = "Emphasis";
static const char* def_bold_font_tag          = "Bold";

static const char* mif_Para_beg              = " <Para";
static const char* mif_Para_end              = " > # end of Para";
static const char* mif_PgfTag_beg            = "  <PgfTag `";
static const char* mif_PgfTag_end            = "'>";
static const char* mif_PgfNumString          = "  <PgfNumString `'>";
static const char* mif_ParaLine_beg          = "  <ParaLine";
static const char* mif_ParaLine_end          = "  >";
static const char* mif_String_beg            = "   <String `";
static const char* mif_String_end            = "'>";
static const char* mif_Marker_beg            = "   <Marker";
static const char* mif_Marker_end            = "   > # end of Marker";
static const char* mif_MType                 = "    <MType 2>";
static const char* mif_MText_beg             = "    <MText `";
static const char* mif_MText_end             = "'>";
static const char* mif_Font_beg              = "   <Font";
static const char* mif_Font_end              = "   > # end of Font";
static const char* mif_FTag_beg              = "<FTag `";
static const char* mif_FTag_end              = "'>";

static const char* mif_MCurrPage             = "    <MCurrPage 0>";
static const char* mif_HardReturn            = "   <Char HardReturn >";
static const char* mif_Tab                   = "   <Char Tab >";
static const char* mif_endl                  = "\n";
static const char* mif_space                 = " ";
static const char* mif_nil                   = "";
static const char* line_array_term           = "##ends line array##";

static Boolean sort_defs = true;
static Boolean sort_ops = true;
static Boolean print_impls = true;

class Program {
public:
    Program(int argc, char** argv);
    ~Program();
    
    static FILE& input();
    static FILE& output();
    static FILE& error();
    static int ret_val();

    static void error(const char*, unsigned line_no = 0);   
    
private:
    char *prog_name_;
    static FILE& input_;
    static FILE& output_;
    static FILE& error_;
    static int ret_val_;
};
    
class Definition;
declarePtrList(DefReadBuffer,Definition)
implementPtrList(DefReadBuffer,Definition)
    
class Filter {
public:
    Filter();
    ~Filter();
    
    friend Filter& operator>>(FILE& input, Filter& filter) {
	return filter.read(input).sort();
    }
    void operator>>(FILE& output) { output_to(output); }
	/* For example, prog.input() >> filter >> prog.output; */
    
protected:
    Filter& read(FILE&);
    Filter& sort();
    void output_to(FILE&);
private:

    static int compare_defs_(const void*, const void*);
    DefReadBuffer* def_buffer_; // Variable array for reading input.
    Definition** def_;          // Fixed array for sort, process.
    long def_count_;
};

typedef enum { code, normal } MifFont;
class Line {  
public:
    Line();
    Line(const char*);
    ~Line();
    void operator<<(FILE&);
    
    void begin_token();
    char* next_token(const char* delim);
    char* last_token(const char* delim);
    Boolean on_last_token();
	/* IMPT: pointers returned are only valid until next call to
	 * to begin_token, last_token!!
	 * And this includes the below "begins_ and ends_ routines since
	 * they use begin_token.  (Note last_token uses begin_token and
	 * next_token to sequence list.)
	 * It is ok to modify the string returned by next_token and
	 * last_token.
	 * Boolean on_last_token returns true if the most recent
	 * call to next_token has returned the last token in the line.
	 */

    /* Note that the following will reset token traversal. */
    Boolean begins_definition();  // true for beg_heading 
    Boolean ends_definition();    // true for end_interface_tok or end_idl_file
    Boolean ends_heading();       // true for beg_heading
    Boolean ends_entry();         // true for beg_heading or end_idl_file
    Boolean begins_noprint_sect();
    Boolean begins_code_comment();
    Boolean ends_code_comment();
    Boolean ends_line_array();    // true for line_array_term
    Boolean begins_ix_derived_sect();
    Boolean ends_ix_derived_sect();
    Boolean is_to_be_skipped_ix();
    Boolean is_comment();         // true for beg_comment.
    Boolean is_empty();
    Boolean is_new_para_comment();
    Boolean is_tabbed_comment(int& tab_count);
    Boolean is_normal_comment_line();
    Boolean is_open_brace();
    Boolean is_close_brace();

    char* all_but_first_token();
    void strip_first_token();
        /* all_but_first_token returns malloc allocated buffer containing
	 * line without it's first word. (No checking is done to ensure first
	 * word is actually a token.)
	 * strip_first_token uses all_but_first_token so that this line
	 * is stripped of its first token.
	 */

    unsigned str_len() const { return strlen(data_); }
    unsigned number() const  { return line_no_; }
    const char* _data() const;  // Debugging.
    void free_data();           // For end of program heap clean-up.

private:
    char* data_;  // Got by strdup.
    static char* traversal_tok_buf_;
    static char* cur_tok_, *next_tok_;
    static unsigned tot_line_count_;
    unsigned line_no_;
        /* If a single list of lines were used, rather than dividing
	 * them up, then this would not be needed.
	 */
};
declareList(LineReadBuffer,Line)
implementList(LineReadBuffer,Line)

class DefDescription;
class DefSection;
class Definition {
public:
    Definition(const LineReadBuffer&);
    ~Definition();
    const char* heading();
    void operator>>(FILE&);
    void _print();    // debug.

private:
    static int compare_sections_(const void*, const void*);
    DefDescription*  description_;
    DefSection* sections_;
    long section_count_;
};

class Paragraph;
class DefEntry {
public:
    void operator>>(FILE&);
    virtual Paragraph* new_heading_para(Line*, long line_count) = 0;
    virtual Paragraph* new_code_para(Line*, long line_count) = 0;
    virtual Paragraph* new_comment_para(Line*, long line_count) = 0;
    const char* heading();
    void free_data();     // For end of program heap clean-up.
    void _print();        // debug.

protected:
    DefEntry();
    DefEntry(const LineReadBuffer&, long start, long end);
    ~DefEntry();

    char* heading_;
    Line* lines_;
    long line_count_;
};

class DefDescription : public DefEntry {
public:
    DefDescription();
    DefDescription(const LineReadBuffer&, long start, long end);
    virtual Paragraph* new_heading_para(Line*, long line_count);
    virtual Paragraph* new_code_para(Line*, long line_count);    
    virtual Paragraph* new_comment_para(Line*, long line_count);
};

class DefSection : public DefEntry {
public:
    DefSection();
    DefSection(const LineReadBuffer&, long start, long end);
    virtual Paragraph* new_heading_para(Line*, long line_count);
    virtual Paragraph* new_code_para(Line*, long line_count);
    virtual Paragraph* new_comment_para(Line*, long line_count);
};
declareList(SectionBuffer,DefSection)
implementList(SectionBuffer,DefSection)

class Paragraph {
public:
    Paragraph(Line*, long line_count);
    void operator>>(FILE& output);
protected:
    virtual void write_tag(FILE&) { }
    virtual void write_pre_lines_stuff(FILE&) { }
    virtual void write_lines(FILE&) { }

    virtual const char* emphasis_font_tag() { return def_emphasis_font_tag; }
    virtual const char* bold_font_tag()     { return def_bold_font_tag; }
    virtual char* adjust_ending(char* tok)  { return tok; }
	/* Define to adjust ending token as appropriate. For
	 * convenience, return value is same as token.
	 */
    
    void write_emphasis_font(FILE&);
    void write_bold_font(FILE&);
    void write_normal_font(FILE&);
    void write_marker(FILE&, const char* text);
	/* Strips ending punctuation and outputs marker with text. */

    void write_text(FILE&, const char* text);
        /* Fixes up characters such as <,>, and ' which cannot
	 * go unfixed into mif file.
	 */
	  
    Boolean begins(const char* does_this, const char* beg_this) const;
    Boolean ends(const char* does_this, const char* end_this) const;
    char* strip(char* tok, const char* this_from_beg,
		const char* this_from_end) const;
    char* strip_font_change(char*);
        /* Above are used for identifying tokens that are not
	 * separted from text by spaces, e.g., \emphasis{xxx}.
	 */
protected:
    Line* lines_;
    long line_count_;
};
declarePtrList(ParagraphList,Paragraph)
implementPtrList(ParagraphList,Paragraph)

class HeadingPara : public Paragraph {
public:
    HeadingPara(Line*, long line_count);
    virtual void write_lines(FILE& output);
};

class DescHeadingPara : public HeadingPara {
public:
    DescHeadingPara(Line*, long line_count);
    virtual void write_tag(FILE& output);
    virtual void write_pre_lines_stuff(FILE& output);
};

class SectionHeadingPara : public HeadingPara {
public:
    SectionHeadingPara(Line*, long line_count);
    virtual void write_tag(FILE& output);
};

class CodePara : public Paragraph {
public:
    CodePara(Line*, long line_count);
    virtual void write_lines(FILE& output);
};

class DescCodePara : public CodePara {
public:
    DescCodePara(Line*, long line_count);
    virtual void write_tag(FILE& output);
    virtual char* adjust_ending(char* token); 
};

class SectionCodePara : public CodePara {
public:
    SectionCodePara(Line*, long line_count);
    virtual void write_tag(FILE& output);
};

class CommentPara : public Paragraph {
public:
    typedef enum { code, normal } Font;
    CommentPara(Line*, long line_count);
    virtual void write_lines(FILE& output);
    virtual const char* code_font_para_tag() = 0;
    virtual const char* second_para_tag() = 0;
};

class DescCommentPara : public CommentPara {
public:
    DescCommentPara(Line*, long line_count);
    virtual void write_tag(FILE& output);
    virtual const char* code_font_para_tag();
    virtual const char* second_para_tag();
};

class SectionCommentPara : public CommentPara {
public:
    SectionCommentPara(Line*, long line_count);
    virtual void write_tag(FILE& output);
    virtual const char* code_font_para_tag();
    virtual const char* second_para_tag();
};
    
/********************************************************************/
FILE& Program::error_ = *stderr;
FILE& Program::input_ = *stdin;
FILE& Program::output_ = *stdout;
int   Program::ret_val_ = 0;

FILE& Program::input()   { return input_; }
FILE& Program::output()  { return output_; }
FILE& Program::error()   { return error_; }
int   Program::ret_val() { return ret_val_; }

static FILE& operator<< (FILE& output, const char* s) {
    fprintf(&output, "%s", s);
    return output;
}
static FILE& operator<< (FILE& output, const char c) {
    fprintf(&output, "%c", c);
    return output;
}

void Program::error(const char* s1, unsigned lineno) {
    error_ << "\n";
    if (lineno != 0)
	error_ << "Input line " << lineno << ": ";
    else
	error_ << "Error: ";
    error_ <<  s1 << "\n";
    ret_val_ = 1;
    /* abort(); */
}

Program::Program(int argc, char** argv) {
    prog_name_ = argv[0];
    for (long i = 1; i < argc; i++) {
	if (strcmp(argv[i], "-unsorted_defs") == 0) {
	    sort_defs = false;
	} else if (strcmp(argv[i], "-unsorted_ops") == 0) {
	    sort_ops = false;
	} else if (strcmp(argv[i], "-std") == 0) {
	    print_impls = false;
	} else {
	    error_ << "\nusage example: cat *.idl | " << prog_name_ <<
		" [-unsorted_defs] [-unsorted_ops] > fname.mif.\n";
	    exit(1);
	}
    }
}

Program::~Program() { }

Filter::Filter() {
    const int approx_max_defs = 150;
	/* Set approx_max_defs to (slightly higher than) the approximate
	 * number of IDL definitions (interface and typedef) in Fresco.
	 */
    def_buffer_ = new DefReadBuffer(approx_max_defs); 
    def_ = nil;
    def_count_ = 0;
}

Filter& Filter::read(FILE& input) {
    const int approx_max_lines_in_def = 50;
    LineReadBuffer line_buffer(approx_max_lines_in_def);
   
    enum {inside_def, outside_def } state = outside_def;
    while (!feof(&input)) {
	Line line;
	line << input;   
	if (line.is_empty() || line.is_to_be_skipped_ix()) {
	    line.free_data();
	    continue;
	}
	switch(state) {
	case outside_def:
	    if (line.begins_definition()) {
		line_buffer.append(line);		    
		state = inside_def;
	    } else {
		line.free_data();
	    }
	    break;
	case inside_def:
	    if (line.ends_definition()  ||  feof(&input)) {
		def_buffer_->append(new Definition(line_buffer));
		line_buffer.remove_all();
		if (line.begins_definition()) {
		    line_buffer.append(line);
		} else {
		    line.free_data();
		    state = outside_def;
		}
	    } else {
		line_buffer.append(line);
	    }
	    break;
	}
    }
    return *this;
}
	    
Filter& Filter::sort() {   
    /* Get variable array into contiguous array for sorting. */
    def_count_ = def_buffer_->count();
    def_ = new Definition*[def_count_];
    for (long i=0; i<def_count_; i++) {
	def_[i] = def_buffer_->item(i);
    }
    def_buffer_->remove_all();
    if (sort_defs) {
	qsort(def_, (unsigned int)def_count_, sizeof(def_[0]), compare_defs_);
    }
    return *this;
}

void Filter::output_to(FILE& output) {
    for (long i=0; i<def_count_; i++) {
	*def_[i] >> output;
    }
}

int Filter::compare_defs_(const void* a, const void* b) {
    Definition& da = **(Definition**) a;
    Definition& db = **(Definition**) b;
    return (strcasecmp(da.heading(), db.heading()));
}

Filter::~Filter() {
    delete def_buffer_;
    for (long i=0; i<def_count_; i++) {
	delete(def_[i]);
    }
    delete [] def_;
}

void Line::operator<<(FILE& input) {
    const int max_line_size = 500;
    char line_data[max_line_size+1];
    line_data[0] = '\0'; 
    fgets(line_data, max_line_size, &input);
    data_ = strdup(line_data);
    line_no_ = ++tot_line_count_;
}

Boolean Line::begins_definition() {
    begin_token();
    const char *first_tok  = next_token(white_space);
    return (first_tok == nil)? false :
                 strcmp(first_tok, beg_heading_tok) == 0  && 
		 next_token(white_space) != nil;
}

Boolean Line::ends_definition() {
    begin_token();
    const char *first_tok  = next_token(white_space);
    return (first_tok == nil)? false :
        strncmp(data_, end_interface_tok, strlen(end_interface_tok)) == 0 ||
	(strncmp(first_tok, beg_heading_tok, strlen(beg_heading_tok)) == 0  &&
	 next_token(white_space) == nil);
}

Boolean Line::ends_entry() {
    begin_token();
    const char *first_tok = next_token(white_space);
    return (first_tok == nil)? false :
                 (strcmp(first_tok, beg_heading_tok) == 0);
}

Boolean Line::begins_noprint_sect() {
    const char *last_tok = last_token(white_space);
    return (last_tok == nil)? false :
         (!print_impls  &&  last_tok[strlen(last_tok)-1] == '*');
}

Boolean Line::ends_heading() {
    begin_token();
    const char *first_tok = next_token(white_space);
    return (first_tok == nil)? false :
                 !(strcmp(first_tok, beg_heading_tok) == 0);
}

Boolean Line::begins_code_comment() {
    begin_token();
    const char *first_tok = next_token(white_space);
    return (first_tok == nil)? false :
                 (strcmp(first_tok, beg_code_comment_tok) == 0);
}

Boolean Line::ends_code_comment() {
    begin_token();
    const char *first_tok = next_token(white_space);
    return (first_tok == nil)? false :
                 (strcmp(first_tok, end_code_comment_tok) == 0);
}

Boolean Line::is_comment() {
    begin_token();
    const char *first_tok = next_token(white_space);
    return (first_tok == nil)? false :
         strncmp(first_tok, beg_comment_tok, strlen(beg_comment_tok)) == 0 ||
	 strcmp(first_tok, beg_code_comment_tok) == 0  ||
	 strcmp(first_tok, end_code_comment_tok) == 0;
}

Boolean Line::ends_line_array() {
    return strcmp(data_, line_array_term) == 0;
}

Boolean Line::begins_ix_derived_sect() {
    // True for line in form //+ derived : base
    begin_token();
    const char *cur_tok = next_token(white_space);
    if (cur_tok == nil || strcmp(cur_tok, ix_tok) != 0) 
	return false;
    if (next_token(white_space) == nil) 
	return false;
    if ((cur_tok = next_token(white_space)) == nil ||
	           strcmp(cur_tok, ":") != 0)
	return false;
    return true;
}

Boolean Line::ends_ix_derived_sect() {
    begin_token();
    const char *first_tok = next_token(white_space);
    return (first_tok == nil)? false :
         strcmp(first_tok, ix_tok) == 0 && next_token(white_space) == nil; 
}

Boolean Line::is_to_be_skipped_ix() {
    static enum { is_normal, is_ix_derived_sect } state = is_normal;
    static unsigned ix_skip_counter = 0;
    
    if (state == is_normal) {
	if (begins_ix_derived_sect()) {
	    state = is_ix_derived_sect;
	    return true;
	} else {
	    begin_token();
	    const char *first_tok = next_token(white_space);	    
	    return (first_tok == nil)? false :
	            strncmp(first_tok, ix_tok, strlen(ix_tok)) == 0;
	}
    } else {
	if (ends_ix_derived_sect()) {
	    state = is_normal;
	    ix_skip_counter = 0;
	    return true;
	}
	ix_skip_counter++;
	if (ix_skip_counter != 1)  // don't skip second line
	    return true;
	else
	    return false;
    }
}

Boolean Line::is_empty() {
    if (data_ == nil)
	return true;
    else {
	begin_token();
	const char *first_tok = next_token(white_space);
	return first_tok == nil;
    }
}

Boolean Line::is_new_para_comment() {
    begin_token();
    const char *first_tok = next_token(white_space);
    return (first_tok == nil)? false :
                   strcmp(first_tok, beg_comment_tok) == 0 &&
		   next_token(white_space) == nil;
}

Boolean Line::is_tabbed_comment(int& tab_count) {
    tab_count = 0;
    begin_token();
    const char *first_tok = next_token(white_space);
    if (first_tok == nil)
	return false;
    const char* tok_tabs = &first_tok[strlen(beg_comment_tok)];
    if (*tok_tabs  &&  *tok_tabs != tab_tok_char)
	return false;
    
    while (*tok_tabs) {
	if (*tok_tabs == tab_tok_char)
	    tab_count++;
	else
	    Program::error("Invalid char in tab sequence.", line_no_);
	tok_tabs++;
    }
    return tab_count > 0;	
}

Boolean Line::is_normal_comment_line() {
    begin_token();
    const char *first_tok = next_token(white_space);
    return (first_tok == nil)? false :
                      strcmp(first_tok, normal_comment_tok) == 0;
}

Boolean Line::is_open_brace() {
    const char *last_tok = last_token(white_space);
    if (last_tok == nil)
	return false;
    int len = strlen(last_tok);
    if (len == 0)
	return false;
    return last_tok[len-1] == '{'  ||  last_tok[len-1] == '(';
}

Boolean Line::is_close_brace() {
    begin_token();
    const char *first_tok = next_token(white_space);
    return (first_tok == nil)? false :
                               strcmp(first_tok, "};") == 0  ||
		               strcmp(first_tok, ");") == 0;
}
    
char* Line::traversal_tok_buf_ = nil;
char* Line::cur_tok_ = nil;
char* Line::next_tok_ = nil;
unsigned Line::tot_line_count_ = 0;
void Line::begin_token() {
    if (traversal_tok_buf_ != nil)
	free(traversal_tok_buf_);
    traversal_tok_buf_ = nil;
}

char* Line::next_token(const char* delim) {
    if (traversal_tok_buf_ == nil) {
	traversal_tok_buf_ = strdup(data_);
	cur_tok_ = strtok(traversal_tok_buf_, delim);
    } else {
	cur_tok_ = next_tok_;
    }
    next_tok_ = (cur_tok_ == nil)? nil : strtok(nil, delim);
    return cur_tok_;
}

Boolean Line::on_last_token() {
    return next_tok_ == nil;
}

char* Line::last_token(const char* delim) {
    char* token = nil;
    char* next = nil;
    begin_token();
    do {
	if (next != nil) {
	    token = next;
	}
    } while ((next = next_token(delim)) != nil);
    return token;
}

char* Line::all_but_first_token() {
    char* retval;
    char* line = strdup(data_);
    char* next_word = strtok(line, white_space);
    if (next_word == nil) {
	retval =  nil;
    } else {
	next_word = strtok(nil, white_space);
	if (next_word == nil) {
	    retval =  nil;
	} else {
	    retval = strdup(&data_[next_word-line]);
	}
    }
    free(line);
    return retval;
}

void Line::strip_first_token() {
    char *stripped = all_but_first_token();
    free(data_);
    data_ = stripped;
}

const char* Line::_data() const { return data_; }
Line::Line()  { data_ = nil; }
Line::Line(const char* str) {
    data_ = strdup(str);
}

Line::~Line() { }
void Line::free_data() {
    if (data_ != nil)
	free(data_);
}
    
Definition::Definition(const LineReadBuffer& line_buffer) {
    SectionBuffer sections;
    long line_count = line_buffer.count();
    long cur_entry = 0;
    enum { in_description, in_section, in_noprint } state = in_description;
    
    for (long i=0; i<line_count; i++) {
	Line& line = line_buffer.item_ref(i);
	switch(state) {
	case in_description:
	    if (i == line_count-1) {
		description_ = new DefDescription(line_buffer, cur_entry, i);
	    } else if (line.ends_entry()  &&  i != 0)  {
		description_ = new DefDescription(line_buffer, cur_entry, i-1);
		cur_entry = i;
		state = line.begins_noprint_sect()? in_noprint : in_section;
	    }
	    break;
	case in_section:
	    if (i == line_count-1) {
		sections.append(DefSection(line_buffer, cur_entry, i));
	    } else if (line.ends_entry()  &&
		       !line_buffer.item_ref(i-1).ends_entry()) {
		sections.append(DefSection(line_buffer, cur_entry, i-1));
		cur_entry = i;
		state = line.begins_noprint_sect()? in_noprint : in_section;
	    }
	    break;
	case in_noprint:
	    if (line.ends_entry()  &&
		   !line_buffer.item_ref(i-1).ends_entry()) {
		cur_entry = i;
		state = line.begins_noprint_sect()? in_noprint : in_section;
	    }		
	    break;
	}
    }
    /* Copy variable array into fixed array. */
    section_count_ = sections.count();
    sections_ = new DefSection[section_count_];
    for (long j=0; j<section_count_; j++) {
	sections_[j] = sections.item(j);
    }
    /* Sort sections */
    if (sort_ops) {
	qsort(
	    sections_, (unsigned int)section_count_, sizeof(DefSection),
	    compare_sections_
	);
    }
}

int Definition::compare_sections_(const void* a, const void* b) {
    DefSection& da = *(DefSection*) a;
    DefSection& db = *(DefSection*) b;
    return (strcasecmp(da.heading(), db.heading()));
}

void Definition::operator>>(FILE& output) {
    *description_>> output;
    for (long i=0; i<section_count_; i++) {
	sections_[i] >> output;
    }
}

const char* Definition::heading() {
    return description_->heading();
}

void Definition::_print() {
    description_->_print();
    for (long i=0; i<section_count_; i++) {
	sections_[i]._print();
    }
}

Definition::~Definition() {
    description_->free_data();
    for (long i=0; i<section_count_; i++) {
	sections_[i].free_data();
    }
    delete description_;
    delete [] sections_;
}

DefEntry::DefEntry() {
    lines_ = nil;
    line_count_ = nil;
    heading_ = nil;
}

DefEntry::DefEntry(const LineReadBuffer& line_buffer, long start, long end) {
    line_count_ = (end-start)+1;
    lines_ = new Line[line_count_+1];  // space for one terminator.
    for (long i=0; i<line_count_; i++) {
	lines_[i] = line_buffer.item(start+i);
    }
    heading_ = lines_[0].all_but_first_token();
    lines_[line_count_] = Line(line_array_term);
    line_count_ += 1;  // includes terminator.
}

void DefEntry::operator>>(FILE& output) {
    ParagraphList paragraphs;
    enum { in_heading, in_code, in_comment, in_no_print } state = in_heading;
    long para_start = 0;
    long i=0;
    for (i=0; i<line_count_; i++) {
	Line& line = lines_[i];
	switch (state) {
	case in_heading:
	    if (line.ends_heading()) {
		state = line.is_comment()? in_comment : in_code;
	    }
	    if (state != in_heading  ||  line.ends_line_array()) {
		paragraphs.append(new_heading_para(
		    &lines_[para_start], i-para_start));
		para_start = i;
	    }
	    break;
	case in_code:
	    if (line.is_comment()) {
		state = in_comment;
	    }
	    if (state != in_code  ||  line.ends_line_array()) {
		paragraphs.append(new_code_para(
		    &lines_[para_start], i-para_start));
		para_start = i;
	    }
	    break;
	case in_comment:
	    if (!line.is_comment()) {
		state = in_no_print;
	    }
	    if (state != in_comment  ||  line.ends_line_array()) {
		paragraphs.append(new_comment_para(
		    &lines_[para_start], i-para_start));
		para_start = i;
	    }
	    break;
	case in_no_print:
	    if (line.is_comment()) {
		state = in_comment;
	    }
	    if (state != in_no_print) {
		para_start = i;
	    }
	    break;
	}
    }
    for (i=0; i<paragraphs.count(); i++) {
	*paragraphs.item(i) >> output;
	delete paragraphs.item(i);
    }
}

void DefEntry::_print() {
    for (long i=0; i<line_count_; i++) {
	Program::error() << lines_[i]._data() << "\n";
    }
}

DefEntry::~DefEntry() { }
void DefEntry::free_data() {
    if (heading_ != nil)   free(heading_);
    if (lines_ != nil) {
	for (long i=0; i<line_count_; i++) {
	    lines_[i].free_data();
	}
	delete [] lines_;
    }
	
}
const char* DefEntry::heading() {
    return heading_;
}

DefDescription::DefDescription() : DefEntry() {}
DefDescription::DefDescription(const LineReadBuffer& lb, long start, long end)
: DefEntry(lb, start,end) { }
Paragraph* DefDescription::new_heading_para(Line* lines,
					    long line_count) {
    return new DescHeadingPara(lines, line_count);
}
Paragraph* DefDescription::new_code_para(Line* lines,
					 long line_count) {
    return new DescCodePara(lines, line_count);
}
Paragraph* DefDescription::new_comment_para(Line* lines,
					 long line_count) {
    return new DescCommentPara(lines, line_count);
}

DefSection::DefSection() : DefEntry() {}
DefSection::DefSection(const LineReadBuffer& lb, long start, long end)
: DefEntry(lb, start,end) { }
Paragraph* DefSection::new_heading_para(Line* paragraphs,
					long line_count) {
    return new SectionHeadingPara(paragraphs, line_count);
}
Paragraph* DefSection::new_code_para(Line* paragraphs,
				     long line_count) {
    return new SectionCodePara(paragraphs, line_count);
}
Paragraph* DefSection::new_comment_para(Line* paragraphs,
					long line_count) {
    return new SectionCommentPara(paragraphs, line_count);
}

Paragraph::Paragraph(Line* lines, long line_count) {
    lines_ = lines;
    line_count_ = line_count;
}

void Paragraph::operator>>(FILE& output) {
    output << mif_Para_beg << mif_endl;
    write_tag(output);
    write_pre_lines_stuff(output);
    write_lines(output);
    output << mif_Para_end << mif_endl;
}
void  Paragraph::write_marker(FILE& output, const char* text) {
    char* buf = strdup(text);
    unsigned len = strlen(buf);
    if (ispunct(buf[len-2])) {  // -2 if for space.
	buf[len-2] = '\0';
    } else if (ispunct(buf[len-1])) {
	buf[len-1] = '\0';
    }
    output << mif_Marker_beg << mif_endl
	   << mif_MType << mif_endl << mif_MText_beg;
    write_text(output, buf);
    output << mif_MText_end << mif_endl
	   << mif_MCurrPage << mif_endl
	   << mif_Marker_end << mif_endl;
    free(buf);
}
    
void Paragraph::write_emphasis_font(FILE& output) {
    output << mif_Font_beg << mif_endl << mif_FTag_beg 
	   << emphasis_font_tag() <<  mif_FTag_end << mif_endl
	   << mif_Font_end << mif_endl;
}
void Paragraph::write_bold_font(FILE& output) {
    output << mif_Font_beg << mif_endl << mif_FTag_beg 
	   << bold_font_tag() <<  mif_FTag_end << mif_endl
	   << mif_Font_end << mif_endl;
}
void Paragraph::write_normal_font(FILE& output) {
    output << mif_Font_beg << mif_endl << mif_FTag_beg 
	   <<  mif_FTag_end << mif_endl << mif_Font_end << mif_endl;
}

void Paragraph::write_text(FILE& output, const char* text) {
    const char* cur = text;
    static int smart_qt = 0;  // Toggle for smart quotes.
    while (*cur) {
	if (*cur == '\'' || *cur == '>' || *cur == '"')
	    break;
	cur++;
    }
    if (*cur) {  // ie., if one of the above chars is found...
	cur = text;
	while (*cur) {
	    switch (*cur) {
	        case '\'': output << "\\q";    break;
	        case '>':  output << "\\>";    break;
	        case '"':  output << (++smart_qt%2? "\\xd2 ":"\\xd3 "); break;
	        default:   output << *cur;     break;
	    }
	    cur++;
	}
    } else {
	output << text;
    }
}

Boolean Paragraph::begins(const char* does_this, const char* beg_this) const {
    return strncmp(does_this, beg_this,  strlen(beg_this)) == 0;
}

Boolean Paragraph::ends(const char* does_this, const char* end_this) const {
    return strcmp(&does_this[strlen(does_this)-strlen(end_this)],
		  end_this) == 0;
}

char* Paragraph::strip(char* tok, const char* this_from_beg,
		       const char* this_from_end) const {
    char* retval = tok;
    if (ends(tok, this_from_end)) {
	tok[strlen(tok) - strlen(this_from_end)] = '\0';
    }
    if (begins(tok, this_from_beg)) {
	retval =  &tok[strlen(this_from_beg)];
    }
    return retval;
}

char* Paragraph::strip_font_change(char* tok) {
    return (begins(tok, beg_emph_tok))?
	strip(tok, beg_emph_tok, end_font_change_tok) :
        strip(tok, beg_bold_tok, end_font_change_tok);
}

HeadingPara::HeadingPara(Line* lines,long count): Paragraph (lines, count) { }

void HeadingPara::write_lines(FILE& output) {
    for (long i=0; i<line_count_; i++) {
	output << mif_ParaLine_beg << mif_endl;
	const char *tok;
	Line& line = lines_[i];
	line.strip_first_token();
	line.begin_token();
	enum { in_parens, not_in_parens } state = not_in_parens;
	while ((tok = line.next_token(white_space)) != nil) {
	    if (state == not_in_parens) {
		if (begins(tok, "(")) {
		    state = in_parens;
		} else if (isupper(*tok)) {
		    write_marker(output, tok);
		}
	    } else {
		if (ends(tok, ")")) {
		    state = not_in_parens;
		}
	    }
	    output << mif_String_beg;
	    write_text(output, tok);
	    output << (!line.on_last_token()?  mif_space : mif_nil)
		   << mif_String_end << mif_endl;
	    
	    if (line.on_last_token() && i < line_count_-1) {
		output << mif_HardReturn << mif_endl;
	    }
	}
	output << mif_ParaLine_end << mif_endl;
    }
}

SectionHeadingPara::SectionHeadingPara(Line* lines,long count)
: HeadingPara (lines, count) { }
void SectionHeadingPara::write_tag(FILE& output) {
    output << mif_PgfTag_beg << section_heading_para_tag
	   << mif_PgfTag_end << mif_endl;
}
    
DescHeadingPara::DescHeadingPara(Line* lines,long count)
: HeadingPara (lines, count){ }
void DescHeadingPara::write_tag(FILE& output) {
    output << mif_PgfTag_beg << descr_heading_para_tag
	   << mif_PgfTag_end << mif_endl;
}
void DescHeadingPara::write_pre_lines_stuff(FILE& output) {
    output << mif_PgfNumString << mif_endl;
}

CodePara::CodePara(Line* lines,long count)
: Paragraph (lines, count) { }
void CodePara::write_lines(FILE& output) {
    enum { is_indented, is_normal } line_state = is_normal;
    for (long i=0; i<line_count_; i++) {
	Line& line = lines_[i];
	switch (line_state) {
	case is_normal:
	    if (i>0 &&  lines_[i-1].is_open_brace())
		line_state = is_indented;
	    break;
	case is_indented:
	    if (line.is_close_brace())
		line_state = is_normal;
	    break;
	}
	output << mif_ParaLine_beg << mif_endl;
	if (line_state == is_indented)
	    output << mif_Tab << mif_endl;
	output << mif_String_beg;
	line.begin_token();
	char* tok;
	while ((tok = line.next_token(white_space)) != nil) {
	    if (line.on_last_token()) {
		if (line_state == is_indented) {
		    write_text(output, tok);
		} else {
		    write_text(output, adjust_ending(tok));
		}
	    } else {
		write_text(output, tok);
		output << mif_space;
	    }
	}
	output << mif_String_end   << mif_endl
	       << ((i < line_count_-1)? mif_HardReturn : mif_nil) << mif_endl
	       << mif_ParaLine_end << mif_endl;
    }
}

DescCodePara::DescCodePara(Line* lines,long count)
: CodePara (lines, count) { }

void DescCodePara::write_tag(FILE& output) {
    output << mif_PgfTag_beg << descr_code_para_tag
	   << mif_PgfTag_end << mif_endl;
}

char* DescCodePara::adjust_ending(char* token) {
    int len = strlen(token);
    if (len == 0) {
	return token;
    }
    if (token[len-1] == '{'  || token[len-1] == ';') {
	token[len-1] = '\0';
    }
    return token;
}
    
SectionCodePara::SectionCodePara(Line* lines,long count)
: CodePara (lines, count) { }
    
void SectionCodePara::write_tag(FILE& output) {
    output << mif_PgfTag_beg << section_code_para_tag
	   << mif_PgfTag_end << mif_endl;
}

CommentPara::CommentPara(Line* lines,long count) 
: Paragraph (lines, count) { }

void CommentPara::write_lines(FILE& output) {
    int tab_count;
    enum { in_normal_comment, in_code_comment } line_state = in_normal_comment;
    for (long i=0; i<line_count_; i++) {
	Line& line = lines_[i];
	if (line.is_new_para_comment()) {
	    output << mif_Para_end << mif_endl << mif_Para_beg << mif_endl
		   << mif_PgfTag_beg << second_para_tag()
		   << mif_PgfTag_end << mif_endl;
	    continue;
	}
	if (line.is_tabbed_comment(tab_count)) {
	    output << mif_ParaLine_beg << mif_endl;
	    for (int j=0; j<tab_count; j++) {
		output << mif_Tab << mif_endl;
	    }
	    line.strip_first_token();
	} else if (line.begins_code_comment() || line.ends_code_comment()) {
	    if (i == line_count_-1) {
		break;
	    }
	    output << mif_Para_end << mif_endl << mif_Para_beg << mif_endl;
	    switch (line_state) {
	    case in_normal_comment:
		if (line.ends_code_comment()) {
		    Program::error("Ending code comment without begin",
				   line.number());
		}
		if (line.begins_code_comment()) {
		    line_state = in_code_comment;
		    output << mif_PgfTag_beg << code_font_para_tag()
			   << mif_PgfTag_end << mif_endl;
		}
		break;
	    case in_code_comment:
		if (line.begins_code_comment()) {
		    Program::error("Can't nest code comments", line.number());
		}
		if (i == line_count_-1  &&  !line.ends_code_comment()) {
		    Program::error("Non-terminated code comment",
				   line.number());
		}
		if (line.ends_code_comment()) {
		    line_state = in_normal_comment;
		    write_tag(output);
		}
		break;
	    }
	    output << mif_ParaLine_beg << mif_endl;
	    line.strip_first_token();
	} else if (line.is_normal_comment_line()) {
	    output << mif_ParaLine_beg << mif_endl;
	    line.strip_first_token();
	} else {
	    Program::error("Invalid line inside comment.", line.number());
	}	    
	if (line.is_empty()) {
	    output << mif_ParaLine_end << mif_endl;
	    continue;
	}
	char* marker_buffer = nil;
	char* tok;
	Boolean repeat_tok = false;
	Boolean next_ends_code_comment = (i < line_count_-1  && 
				          lines_[i+1].ends_code_comment());
	enum { in_text, in_index_marker, in_font_change } tok_state = in_text;
	output << mif_String_beg;
	line.begin_token();
	while (repeat_tok || (tok = line.next_token(white_space)) != nil) {
	    repeat_tok = false;
	    switch (tok_state) {
	    case in_text:
		if (begins(tok, beg_index_marker_tok)) {
		    marker_buffer = new char[line.str_len()+1];
		    marker_buffer[0] = '\0';
		    repeat_tok = true;
		    tok_state = in_index_marker;
		} else if (begins(tok, beg_emph_tok) ||
			   begins(tok, beg_bold_tok)) {
		    output << mif_String_end << mif_endl;
		    if (begins(tok, beg_emph_tok))
			write_emphasis_font(output);
		    else
			write_bold_font(output);
		    output << mif_String_beg;
		    repeat_tok = true;
		    tok_state = in_font_change;
		} else {
		    if (line.on_last_token()) {
			if ((line_state == in_code_comment  && 
			    !next_ends_code_comment) ||
			       strcmp(tok, hard_return_comment_tok) == 0) {
			    if (line_state == in_code_comment) {
				write_text(output, adjust_ending(tok));
			    }
			    output << mif_String_end << mif_endl
				   << mif_HardReturn << mif_endl;
			} else {
			    write_text(output, adjust_ending(tok));
			    output << mif_space << mif_String_end << mif_endl;
			}
		    } else {
			write_text(output, tok);
			output << mif_space;
		    }
		}
		break;
	    case in_index_marker:
		if (begins(tok, beg_emph_tok) || begins(tok, beg_bold_tok)) {
		    Program::error("Sorry, can't start font change in marker.",
				   line.number());
		}
		if (ends(tok, end_index_marker_tok)) {
		    strcat(marker_buffer, strip(tok, beg_index_marker_tok,
						end_index_marker_tok));
		    strcat(marker_buffer, mif_space);
		    output << mif_String_end << mif_endl;
		    write_marker(output, marker_buffer);
		    output << mif_String_beg;
		    write_text(output, marker_buffer);
		    delete marker_buffer;
		    marker_buffer = nil;
		    if (line.on_last_token()) {
			output << mif_String_end << mif_endl;
		    }
		    tok_state = in_text;
		} else {
		    strcat(marker_buffer, strip(tok, beg_index_marker_tok,
						end_index_marker_tok));
		    strcat(marker_buffer, mif_space);
		}
		break;
	    case in_font_change:
		if (begins(tok, beg_index_marker_tok)) {
		    Program::error("Sorry, can't start marker in font change.",
				   line.number());
		}
		if (ends(tok, end_font_change_tok)) {
		    write_text(output, strip_font_change(tok));
		    output << mif_space << mif_String_end << mif_endl;
		    write_normal_font(output);
		    if (!line.on_last_token()) {
			output << mif_String_beg;
		    }
		    tok_state = in_text;
		} else {
		    write_text(output, strip_font_change(tok));
		    output << mif_space;		    
		}
		break;
	    } // end switch.
	}
	if (tok_state == in_index_marker || tok_state == in_font_change) {
	    Program::error("Sorry, marker/font-change can't span input lines.",
			   line.number());
	}
	output << mif_ParaLine_end << mif_endl;
    }
}    

DescCommentPara::DescCommentPara(Line* lines,long count)
: CommentPara (lines, count){ }

void DescCommentPara::write_tag(FILE& output) {
    output << mif_PgfTag_beg << descr_comment_para_tag
	   << mif_PgfTag_end << mif_endl;
}

const char* DescCommentPara::code_font_para_tag() {
    return code_desc_comment_para_tag;
}

const char* DescCommentPara::second_para_tag() {
    return descr_comment_para2_tag;
}

SectionCommentPara::SectionCommentPara(Line* lines,long count)
: CommentPara (lines, count){ }

void SectionCommentPara::write_tag(FILE& output) {
    output << mif_PgfTag_beg << section_comment_para_tag
	   << mif_PgfTag_end << mif_endl;
}

const char* SectionCommentPara::code_font_para_tag() {
    return code_sect_comment_para_tag;
}

const char* SectionCommentPara::second_para_tag() {
    return section_comment_para2_tag;
}

int main(int argc, char** argv) {
    Program program(argc,argv);
    Filter filter;
   
    program.input() >>  filter  >> program.output();
    return program.ret_val();
}
