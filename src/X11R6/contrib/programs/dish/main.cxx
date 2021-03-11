/* Copyright (c) 1992-1993 Silicon Graphics, Inc.
 * Copyright (c) 1993 Fujitsu, Ltd.
 *
 * Permission to use, copy, modify, distribute, and sell this software and 
 * its documentation for any purpose is hereby granted without fee, provided
 * that (i) the above copyright notices and this permission notice appear in
 * all copies of the software and related documentation, and (ii) the names
 * of Silicon Graphics and Fujitsu may not be used in any advertising or
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

#include <X11/Fresco/Impls/action.h>
#include <X11/Fresco/Impls/charstr.h>
#include <X11/Fresco/Impls/glyphs.h>
#include <X11/Fresco/Impls/viewers.h>
#include <X11/Fresco/OS/threads.h>
#include <X11/Fresco/Ox/object.h>
#include <X11/Fresco/figures.h>
#include <X11/Fresco/fresco.h>
#include <X11/Fresco/layouts.h>
#include <X11/Fresco/widgets.h>
#include <stdlib.h>
#include <string.h>
#include <tcl.h>

#if defined(AIXV3)
extern "C" int strcasecmp(const char*, const char*);
#endif

static char* err_argument_number = "Wrong number of arguments.";
static char* err_argument_type = "Wrong argument type: ";

class Dish {
public:
    Dish(Tcl_Interp*);
    ~Dish();
    
    void add_commands(Tcl_Interp*);
    void add_variables(Tcl_Interp*);
    Fresco* fresco();
    
    static Dish& instance();
    static void cleanup();
private:
    Fresco* fresco_;
    Boolean interactive_;
    char* default_unknown_;
    static Dish* instance_;

    void restore_args(Tcl_Interp*, int& argc, char**& argv);
    void add_var(Tcl_Interp*, char*, BaseObjectRef);

    Boolean string_to_object(char*, BaseObjectRef&);
    char* object_to_string(BaseObjectRef, char* result);
    
    Boolean dispatch(BaseObjectRef, int argc, char* argv[], Tcl_Interp*);
    void set_call_status(
	 BaseObjectRef, int, char* op, Tcl_Interp*, RequestObj::CallStatus 
    );
    Boolean parse_args(
        int argc, char* argv[], RequestObjRef, TypeObj::OpInfo&, Tcl_Interp*
    );
    char* try_expand(char* arg, Tcl_Interp*);
    Boolean add_arg(TypeObjRef, char* arg, RequestObjRef, Tcl_Interp*); 

    /* marshalling arguments */
    Boolean add_short(char* arg, RequestObjRef);
    Boolean add_long(char* arg, RequestObjRef);
    Boolean add_unsigned_short(char* arg, RequestObjRef);
    Boolean add_unsigned_long(char* arg, RequestObjRef);
    Boolean add_float(char* arg, RequestObjRef);
    Boolean add_double(char* arg, RequestObjRef);
    Boolean add_boolean(char* arg, RequestObjRef);
    Boolean add_char(char* arg, RequestObjRef);
    Boolean add_interface(TypeObjRef, char*, RequestObjRef, Tcl_Interp*);
    Boolean add_array(TypeObjRef, char*, RequestObjRef, Tcl_Interp*);
    Boolean add_enum(TypeObjRef, char*, RequestObjRef, Tcl_Interp*);
    Boolean add_struct(TypeObjRef, char*, RequestObjRef, Tcl_Interp*);
    Boolean add_sequence(TypeObjRef, char*, RequestObjRef, Tcl_Interp*);
    Boolean split_list(
	Tcl_Interp*, char* arg, int&, char**&, char* type_name
    );

    /* getting results */
    void get_results(
        int argc, char* argv[], RequestObjRef, TypeObj::OpInfo&, Tcl_Interp*
    ); 
    void get_result(TypeObjRef, RequestObjRef, Tcl_Interp*);
    void get_concrete(TypeObj::KindOf, RequestObjRef, Tcl_Interp*);
    void get_interface(TypeObjRef, RequestObjRef, Tcl_Interp*);
    void get_array(TypeObjRef, RequestObjRef, Tcl_Interp*);
    void get_string(RequestObjRef, Tcl_Interp*);
    void get_enum(TypeObjRef, RequestObjRef, Tcl_Interp*);
    void get_struct(TypeObjRef, RequestObjRef, Tcl_Interp*);
    void get_sequence(TypeObjRef, RequestObjRef, Tcl_Interp*);
    
    /* Dish-specific Tcl commands */
    static int unknown(ClientData, Tcl_Interp*, int argc, char* argv[]);
    static int main(ClientData, Tcl_Interp*, int argc, char* argv[]);
    static int spawn(ClientData, Tcl_Interp*, int argc, char* argv[]);
    static int action(ClientData, Tcl_Interp*, int argc, char* argv[]);
    static int delay(ClientData, Tcl_Interp*, int argc, char* argv[]);
    static int debug(ClientData, Tcl_Interp*, int argc, char* argv[]);
};

class FrescoMain : public ActionImpl {
public:
    FrescoMain(ViewerRef, GlyphRef);
    ~FrescoMain();

    void execute();
private:
    ViewerRef viewer_;
    GlyphRef glyph_;
};

class DishAction : public ActionImpl {
public:
    DishAction(Tcl_Interp*, char* command);
    ~DishAction();

    void execute();
private:
    Tcl_Interp* interp_;
    char* command_;
};
 
/* class Dish */

Dish* Dish::instance_;
Dish& Dish::instance() { return *instance_; }

Dish::Dish(Tcl_Interp* interp) {
    instance_ = this;
    char* interactive = Tcl_GetVar(
	interp, "tcl_interactive", TCL_GLOBAL_ONLY
    );
    interactive_ = (interactive[0] == '1');
    int argc;
    char** argv;
    restore_args(interp, argc, argv);
    fresco_ = Fresco_open("Dish", argc, argv);
    default_unknown_ = nil;
}

Dish::~Dish() {
    Fresco::unref(fresco_);
}

Fresco* Dish::fresco() { return fresco_; }

void Dish::cleanup() { delete instance_; }

void Dish::restore_args(Tcl_Interp* interp, int& argc, char**& argv) {
    int tcl_argc;
    char** tcl_argv;
    Tcl_SplitList(
        interp,
        Tcl_GetVar(interp, "argv", TCL_GLOBAL_ONLY),
        &tcl_argc, &tcl_argv
    );
    argc = tcl_argc + 1;
    argv = new char*[argc + 1];
    char* argv0 = Tcl_GetVar(interp, "argv0", TCL_GLOBAL_ONLY);
    argv[0] = argv0;
    for (int i = 0; i < tcl_argc; i++) {
	const char* arg = tcl_argv[i];
	int length = strlen(arg);
	char* copy = new char[length + 1];
	sprintf(copy, "%s", arg);
	argv[i + 1] = copy;
    }
    argv[argc] = 0;
    delete [] tcl_argv;
}

void Dish::add_commands(Tcl_Interp* interp) {
    if (Tcl_Eval(interp, "rename unknown default_unknown") == TCL_OK) {
	default_unknown_ = "default_unknown";
    }
    Tcl_CreateCommand(interp, "unknown", unknown, NULL, NULL);
    Tcl_CreateCommand(interp, "main", main, NULL, NULL);
    Tcl_CreateCommand(interp, "spawn", spawn, NULL, NULL);
    Tcl_CreateCommand(interp, "action", action, NULL, NULL);
    Tcl_CreateCommand(interp, "delay", delay, NULL, NULL);
    Tcl_CreateCommand(interp, "debug", debug, NULL, NULL);
}

void Dish::add_variables(Tcl_Interp* interp) {
    Dish& d = *instance_;
    Fresco* f = fresco_;
    d.add_var(interp, "drawing_kit", f->drawing_kit());
    d.add_var(interp, "figure_kit", f->figure_kit());
    d.add_var(interp, "layout_kit", f->layout_kit());
    d.add_var(interp, "widget_kit", f->widget_kit());
}

void Dish::add_var(Tcl_Interp* interp, char* name, BaseObjectRef value) {
    char buffer[100];
    Tcl_SetVar(interp, name, object_to_string(value, buffer), 0);
}

/*
 *  The 'unknown' command first determines if the command is Fresco object
 *  or not.  If it is, then Dish performs dii on the object.  Otherwise, the
 *  default 'unknown' command is invoked, if it exists.
 */
int Dish::unknown(ClientData, Tcl_Interp* interp, int argc, char* argv[]) {
    if (argc < 2) {
	interp->result = "\"unknown\" must be followed by a command.";
	return TCL_ERROR;
    }
    Dish& dish = instance();
    BaseObjectRef object;
    if (dish.string_to_object(dish.try_expand(argv[1], interp), object)) {
	if (is_not_nil(object) &&
	    dish.dispatch(object, argc - 1, &argv[1], interp)
	) {
	    return TCL_OK;
	} else {
	    return TCL_ERROR;
	}
    } else {
	char* default_unknown = dish.default_unknown_;
	if (default_unknown != nil) {
	    argv[0] = default_unknown;
	    return Tcl_Eval(interp, Tcl_Merge(argc, argv));
	} else {
	    Tcl_AppendResult(
		 interp, "invalid command name: \"", argv[1], "\"", (char *)0
	    );
	    return TCL_ERROR;
	}
    }
}

Boolean Dish::string_to_object(char* s, BaseObjectRef& obj) {
    Boolean b = true;
    if (strcmp(s, "0") == 0) {
	obj = nil;
    } else {
	b = sscanf(s, "_dish_%p", &obj) == 1;
    }
    return b;
}

char* Dish::object_to_string(BaseObjectRef obj, char* result) {
    if (is_not_nil(obj)) {
	sprintf(result, "_dish_%p", obj);
    } else {
	sprintf(result, "0");
    }
    return result; 
}

Boolean Dish::dispatch(
    BaseObjectRef obj, int argc, char* argv[], Tcl_Interp* interp
) {
    Boolean b = false;
    if (argc > 1) {
	RequestObj_var req = fresco_->create_request(obj);
	char* op = argv[1];
	req->set_operation(op);
	TypeObj::OpInfo op_info;
	RequestObj::CallStatus status = req->op_info(op_info);
	if (status == RequestObj::ok) {
	    if (parse_args(argc - 2, &argv[2], req, op_info, interp)) {
		status = req->invoke();
		if (status == RequestObj::ok) {
		    get_results(argc - 2, &argv[2], req, op_info, interp);
		    b = true;
		} else {
		    set_call_status(obj, argc, op, interp, status);
		}
	    }
	} else {
	    set_call_status(obj, argc, op, interp, status);
	}
    } else {
	Tcl_AppendResult(interp, err_argument_number, 0);
    }
    if (!b) {
	Tcl_AppendResult(
	    interp, ".\nExpected: ObjectRef operation argument-list.", 0
	);
    }	
    return b;
}

void Dish::set_call_status(
    BaseObjectRef obj, int argc, char* op, Tcl_Interp* interp, 
    RequestObj::CallStatus status
) {
    char obj_string[100];
    object_to_string(obj, obj_string);
    char* type_name = obj->_type()->name();
    switch (status) {
    case RequestObj::unknown_operation:
	Tcl_AppendResult(
	    interp, "Error: no operation '", type_name, "::", op,
	    "' for object ", obj_string, 0
	);
	break;
    case RequestObj::bad_argument_count: {
	char count[10];
	sprintf(count, "%d", argc - 2);
	Tcl_AppendResult(
	    interp, "Error: bad argument count ",  count,
            " for '", type_name, "::", op, 0
	); 
	break;
    }
    case RequestObj::ambiguous_operation:
	Tcl_AppendResult(
	    interp, "Error: ambigous operation '", op, "' for object ",
	    obj_string, " of type ", type_name, 0
        ); 
	break;
    }
}

Boolean Dish::parse_args(
    int argc, char* argv[], RequestObjRef req, TypeObj::OpInfo& op_info,
    Tcl_Interp* interp
) {
    Boolean b = false;
    if (op_info.op_params._length == argc) {
	for (int i = 0; i < argc; i++) {
	    TypeObj::ParamInfo& p = op_info.op_params._buffer[i];
	    char* arg = (p.param_mode != TypeObj::param_out) ? argv[i] : nil;
	    if (!add_arg(p.param_type, arg, req, interp)) {
		Tcl_AppendResult(
		    interp, err_argument_type,
		    "\nExpecting: ", p.param_type->name(),
		    ", got `", argv[i], "'", 0
		);
		break;
	    }
	}
	b = i == argc;
    } else {
	Tcl_AppendResult(interp, err_argument_number, 0);
    }
    return b;
}

char* Dish::try_expand(char* arg, Tcl_Interp* interp) {
    char* expanded = Tcl_GetVar(interp, arg, 0);
    if (expanded == nil) {
	expanded = arg;
    }
    return expanded;
}

Boolean Dish::add_arg(
    TypeObjRef type, char* arg, RequestObjRef req, Tcl_Interp* interp
) {
    if (arg != nil) {
	arg = try_expand(arg, interp);
    }
    switch (type->kind()) {
    case TypeObj::interface_type:
	return add_interface(type, arg, req, interp);
    case TypeObj::boolean_type:
	return add_boolean(arg, req);
    case TypeObj::char_type:
	return add_char(arg, req);
    case TypeObj::double_type:
	return add_double(arg, req);
    case TypeObj::float_type:
	return add_float(arg, req);
    case TypeObj::long_type:
	return add_long(arg, req);
    case TypeObj::unsigned_long_type:
	return add_unsigned_long(arg, req);
    case TypeObj::short_type:
	return add_short(arg, req);
    case TypeObj::unsigned_short_type:
	return add_unsigned_short(arg, req);
    case TypeObj::array_type:
	return add_array(type, arg, req, interp);
    case TypeObj::enum_type:
	return add_enum(type, arg, req, interp);
    case TypeObj::struct_type:
	return add_struct(type, arg, req, interp);
    case TypeObj::sequence_type:
	return add_sequence(type, arg, req, interp);
    case TypeObj::typedef_type:
	return add_arg(type->typedef_info(), arg, req, interp);
    default:
	fprintf(
	    stderr,
	    "internal error: unknown type %d in add_arg\n",
	    type->kind()
	);
	abort();
    }
}

Boolean Dish::add_short(char* arg, RequestObjRef req) {
    Boolean b = true;
    if (arg != nil) {
	char* ptr;
	short i = short(strtol(arg, &ptr, 0));
	if (*ptr == '\0') {
	    req->put_short(i);
	} else {
	    b = false;
	}
    } else {
	req->put_short(0);
    }
    return b;
}

Boolean Dish::add_long(char* arg, RequestObjRef req) {
    Boolean b = true;
    if (arg != nil) {
	char* ptr;
	Long i = strtol(arg, &ptr, 0);
	if (*ptr == '\0') {
	    req->put_long(i);
	} else {
	    b = false;
	}
    } else {
	req->put_long(0);
    }
    return b;
}

Boolean Dish::add_unsigned_short(char* arg, RequestObjRef req) {
    Boolean b = true;
    if (arg != nil) {
	char* ptr;
	unsigned short i = (unsigned short) strtol(arg, &ptr, 0);
	if (*ptr == '\0') {
	    req->put_unsigned_short(i);
	} else {
	    b = false;
	}
    } else {
	req->put_unsigned_short(0);
    }
    return b;
}

Boolean Dish::add_unsigned_long(char* arg, RequestObjRef req) {
    Boolean b = true;
    if (arg != nil) {
	char* ptr;
	ULong i = strtol(arg, &ptr, 0);
	if (*ptr == '\0') {
	    req->put_unsigned_long(i);
	} else {
	    b = false;
	}
    } else {
	req->put_unsigned_long(0);
    }
    return b;
}

Boolean Dish::add_float(char* arg, RequestObjRef req) {
    Boolean b = true;
    if (arg != nil) {
	char* ptr;
	float f = float(strtod(arg, &ptr));
	if (*ptr == '\0') {
	    req->put_float(f);
	} else {
	    b = false;
	}
    } else {
	req->put_float(0);
    }
    return b;
}

Boolean Dish::add_double(char* arg, RequestObjRef req) {
    Boolean b = true;
    if (arg != nil) {
	char* ptr;
	double d = strtod(arg, &ptr);
	if (*ptr == '\0') {
	    req->put_double(d);
	} else {
	    b = false;
	}
    } else {
	req->put_double(0);
    }
    return b;
}

Boolean Dish::add_boolean(char* arg, RequestObjRef req) {
    Boolean b = true;
    if (arg != nil) {
	if (strcasecmp(arg, "true") == 0) {
	    req->put_boolean(true);
	} else if (strcasecmp(arg, "false") == 0) {
	    req->put_boolean(false);
	} else {
	    b = false;
	}
    } else {
	req->put_boolean(false);
    }
    return b;
}

Boolean Dish::add_char(char* arg, RequestObjRef req) {
    if (arg != nil) {
	req->put_char(arg[0]);
    } else {
	req->put_char(0);
    }
    return true;
}

Boolean Dish::add_interface(
    TypeObjRef type, char* arg, RequestObjRef req, Tcl_Interp*
) {
    Boolean b = true;
    if (arg != nil) {
	if (strcmp(type->name(), "CharString") == 0) {
	    req->put_object(Fresco::string_copy(arg));
	} else {
	    BaseObjectRef o = nil;
	    if (string_to_object(arg, o)) {
		/* todo: check if (object is_a type) */
		req->put_object(o);
	    } else {
		b = false;
	    }
	}
    } else {
	req->put_object(nil);
    }
    return b;
}

Boolean Dish::add_array(
    TypeObjRef array, char* arg, RequestObjRef req, Tcl_Interp* interp
) {
    Boolean b = false;
    long element_count;
    TypeObj_var element_type;
    array->array_info(element_type._out(), element_count);
    if (arg != nil) {
	int argc;
	char** argv;
	if (split_list(interp, arg, argc, argv, "array")) {
	    if (argc == element_count) {
		req->begin_aggregate();
		for (int i = 0; i < argc; i++) {
		    if (!add_arg(element_type, argv[i], req, interp)) {
			char error[100];
			sprintf(
			    error, "%s: expected '%s', got '%s'",
			    "Array element type mismatch",
			    element_type->name(), argv[i]
			);
			Tcl_AppendResult(interp, error, 0);
			break;
		    }
		}
		req->end_aggregate();
		b = i == argc;
		delete [] argv;
	    } else {
		char error[100];
		sprintf(
		    error, "Array size mismatch: expected '%d', got '%d'\n.",
		    element_count, argc
                ); 
	        Tcl_AppendResult(interp, error, 0); 
	    }
	}
    } else {
	req->begin_aggregate();
	for (int i = 0; i < element_count; i++) {
	    add_arg(element_type, nil, req, interp);
	}
	req->end_aggregate();
	b = true;
    }
    return b;
}

Boolean Dish::add_enum(
    TypeObjRef, char* arg, RequestObjRef req, Tcl_Interp*
) {
    /* todo: allow enum literals */
    /* should check that given value is valid */
    return add_unsigned_long(arg, req);
}

Boolean Dish::add_struct(
    TypeObjRef t, char* arg, RequestObjRef req, Tcl_Interp* interp
) {
    Boolean b = false;
    long field_count = t->members();
    if (arg != nil) {
	int argc;
	char** argv;
	if (split_list(interp, arg, argc, argv, "struct")) {
	    if (argc == field_count) {
		req->begin_aggregate();
		for (int i = 0; i < argc; i++) {
		    TypeObjRef m = t->member_info(i);
		    if (!add_arg(m, argv[i], req, interp)) {
			char error[100];
			sprintf(
			    error,
			    "Field type mismatch: expected '%s', got '%s'",
			    m->name(), argv[i]
			);
			Tcl_AppendResult(interp, error, 0);
			break;
		    }
		}
		req->end_aggregate();
		b = i == argc;
		delete [] argv;
	    } else {
		char error[100];
		sprintf(
		    error,
		    "Struct field count mismatch: expected %d, got %d.",
		    field_count, argc
	        ); 
		Tcl_AppendResult(interp, error, 0);
	    }
	}
    } else {
	req->begin_aggregate();
	for (int i = 0; i < field_count; i++) {
	    add_arg(t->member_info(i), nil, req, interp);
	}
	req->end_aggregate();
	b = true;
    }
    return b;
}

Boolean Dish::add_sequence(
   TypeObjRef sequence, char* arg, RequestObjRef req, Tcl_Interp* interp
) {
    Boolean b = false;
    long length;
    TypeObj_var element_type;
    sequence->sequence_info(element_type._out(), length);
    if (arg != nil) {
	int argc;
	char** argv;
	if (split_list(interp, arg, argc, argv, "sequence")) {
	    req->begin_aggregate();
	    /* put maximum and length */
	    req->put_long(argc);
	    req->put_long(argc);
	    for (int i = 0; i < argc; i++) {
		if (!add_arg(element_type, argv[i], req, interp)) {
		    char error[100];
		    sprintf(
			error, "%s: expected '%s', got '%s'",
			"Sequence element type mismatch",
			element_type->name(), argv[i]
		    );
		    Tcl_AppendResult(interp, error, 0);
		    break;
		}
		req->end_aggregate();
		b = (i == argc);
		delete [] argv;
	    }
	}
    } else {
	req->begin_aggregate();
	req->put_long(0);
	req->put_long(0);
	req->put_long(0);
	req->end_aggregate();
	b = true;
    }
    return b;
}

Boolean Dish::split_list(
    Tcl_Interp* interp, char* arg, int& argc, char**& argv, char* type
) {
    Boolean b = false;
    /* todo: this doesn't handle "t scale scale" when scale isn't set */
    if (Tcl_SplitList(interp, arg, &argc, &argv) == TCL_OK) {
	char** intermediate = argv;
	if (argc == 1 &&
	    Tcl_SplitList(interp, intermediate[0], &argc, &argv) == TCL_OK
	) {
	    b = true;
	    delete [] intermediate;
	}
    }
    if (!b) {
	Tcl_AppendResult(
	     interp,
	     "Expecting ", type, " to be represented as tcl list, got '",
	     arg, "'", 0
        ); 
    }
    return b;
}
	
void Dish::get_results(
    int argc, char* argv[], RequestObjRef req, TypeObj::OpInfo& op, 
    Tcl_Interp* interp
) {
    for (int i = 0; i < argc; i++) {
	TypeObj::ParamInfo& p = op.op_params._buffer[i];
	if (p.param_mode != TypeObj::param_in) {
	    get_result(p.param_type, req, interp);
	    char* arg = argv[i];
	    if (arg != nil) {
		Tcl_SetVar(interp, arg, interp->result, TCL_LIST_ELEMENT);
		Tcl_ResetResult(interp);
	    }
	}
    }
    TypeObjRef return_type = op.op_result;
    if (return_type->kind() != TypeObj::void_type) {
	get_result(return_type, req, interp);
    }
}

void Dish::get_result(
    TypeObjRef type, RequestObjRef req, Tcl_Interp* interp
) {
    TypeObj::KindOf tag = type->kind();
    switch (tag) {
    case TypeObj::interface_type:
	get_interface(type, req, interp);
	break;
    case TypeObj::array_type:
	get_array(type, req, interp);
	break;
    case TypeObj::enum_type:
	get_enum(type, req, interp);
	break;
    case TypeObj::struct_type:
	get_struct(type, req, interp);
	break;
    case TypeObj::sequence_type: 
	get_sequence(type, req, interp);
	break;
    case TypeObj::typedef_type: 
	get_result(type->typedef_info(), req, interp);
	break;
    default:
	get_concrete(tag, req, interp);
	break;
    }
}

void Dish::get_concrete(
    TypeObj::KindOf tag, RequestObjRef req, Tcl_Interp* interp
) {
    char result[200];
    switch(tag) {
    case TypeObj::boolean_type:
	sprintf(result, "%s", req->get_boolean() ? "true" : "false");
	break;
    case TypeObj::char_type:
	sprintf(result, "%c", req->get_char());
	break;
    case TypeObj::double_type:
	sprintf(result, "%g", req->get_double());
	break;
    case TypeObj::float_type:
	sprintf(result, "%g", req->get_float());
	break;
    case TypeObj::long_type:
	sprintf(result, "%ld", req->get_long());
	break;
    case TypeObj::unsigned_long_type:
	sprintf(result, "%ld", req->get_unsigned_long());
	break;
    case TypeObj::short_type:
	sprintf(result, "%d", req->get_short());
	break;
    case TypeObj::unsigned_short_type:
	sprintf(result, "%d", req->get_unsigned_short());
	break;
    default:
	fprintf(stderr, "Dish::get_concrete: unknown type");
	fflush(stderr);
	abort();
    }
    Tcl_AppendResult(interp, result, 0);
}

/*
 * Should check the interface actual type is compatible with the formal.
 */

void Dish::get_interface(
    TypeObjRef, RequestObjRef req, Tcl_Interp* interp
) {
    BaseObjectRef object = req->get_object();
    CharString_var s = CharString::_narrow(object);
    if (is_not_nil(s)) {
	CharStringBuffer cs(s);
	Tcl_AppendResult(interp, cs.string(), 0);
    } else {
	char buffer[100];
	Tcl_AppendResult(interp, object_to_string(object, buffer), 0);
    }
}

void Dish::get_array(
    TypeObjRef array, RequestObjRef req, Tcl_Interp* interp
) {
    Tcl_DString result;
    Tcl_DStringInit(&result);
    long element_count;
    TypeObj_var element_type;
    array->array_info(element_type._out(), element_count);
    for (int i = 0; i < element_count; i++) {
	get_result(element_type, req, interp);
	result.string = Tcl_DStringAppendElement(&result, interp->result);
	Tcl_ResetResult(interp);
    }
    Tcl_DStringResult(interp, &result);
    Tcl_DStringFree(&result);
}

void Dish::get_string(RequestObjRef req, Tcl_Interp* interp) {
    char* result = req->get_string();
    Tcl_AppendResult(interp, result, 0);
    delete result;
}

void Dish::get_enum(
    TypeObjRef, RequestObjRef req, Tcl_Interp* interp
) {
    /* todo: use enum literals */
    char result[10];
    sprintf(result, "%ld", req->get_unsigned_long());
    Tcl_AppendResult(interp, result, 0);
}

void Dish::get_struct(
    TypeObjRef struct_type, RequestObjRef req, Tcl_Interp* interp
) {
    Tcl_DString result;
    Tcl_DStringInit(&result);
    long field_count = struct_type->members();
    for (int i = 0; i < field_count; i++) {
	get_result(struct_type->member_info(i), req, interp);
	result.string = Tcl_DStringAppendElement(&result, interp->result);
	Tcl_ResetResult(interp);
    }
    Tcl_DStringResult(interp, &result);
    Tcl_DStringFree(&result);
}

void Dish::get_sequence(
    TypeObjRef sequence, RequestObjRef req, Tcl_Interp* interp
) {
    Tcl_DString result;
    Tcl_DStringInit(&result);
    long size;
    TypeObj_var element_type;
    sequence->sequence_info(element_type._out(), size);
    size = req->get_long();
    long length = req->get_long();
    for (int i = 0; i < length; i++) {
	get_result(element_type, req, interp);
	result.string = Tcl_DStringAppendElement(&result, interp->result);
	Tcl_ResetResult(interp);
    }
    Tcl_DStringResult(interp, &result);
    Tcl_DStringFree(&result);
}

int Dish::main(ClientData, Tcl_Interp* interp, int argc, char* argv[]) {
    int status = TCL_ERROR;
    Dish& dish = instance();
    if (argc != 3) {
        Tcl_AppendResult(interp, err_argument_number, 0);
    } else {
	BaseObjectRef obj;
	char* argv1 = dish.try_expand(argv[1], interp);
	if (!dish.string_to_object(argv1, obj)) {
	    Tcl_AppendResult(interp, err_argument_type, argv1, 0); 
	} else {
	    ViewerRef viewer = Viewer::_narrow(obj);
	    char* argv2 = dish.try_expand(argv[2], interp);
	    if (!dish.string_to_object(argv2, obj)) {
		Tcl_AppendResult(interp, err_argument_type, argv2, 0); 
	    } else {
		GlyphRef glyph = Glyph::_narrow(obj);
		if (!(is_not_nil(viewer) || is_not_nil(glyph))) {
		    Tcl_AppendResult(interp, "nil viewer or glyph", 0); 
		} else {
		    Fresco* fresco = dish.fresco();
		    if (dish.interactive_) {
			ThreadKit_var threads = fresco->thread_kit();
			ThreadObjRef t = threads->thread(
			    new FrescoMain(viewer, glyph)
			);
			if (is_not_nil(t)) {
			    t->run();
			} else {
			    fresco->run(viewer, glyph);
			}
		    } else {
			fresco->run(viewer, glyph);
		    }
		    status = TCL_OK;
		}
	    }
	}
    }
    if (status == TCL_ERROR) {
	Tcl_AppendResult(interp, ".\nExpected `main ViewerRef GlyphRef'.", 0); 
    }
    return status;
}

int Dish::spawn(ClientData, Tcl_Interp* interp, int argc, char* argv[]) {
    int status = TCL_ERROR;
    if (argc == 2) {
	Dish& dish = instance();
	BaseObjectRef obj;
	if (dish.string_to_object(argv[1], obj)) {
	    ActionRef a = Action::_narrow(obj);
	    if (is_not_nil(a)) {
		Fresco* fresco = dish.fresco_;
		ThreadKit_var threads = fresco->thread_kit();
		ThreadObjRef t = threads->thread(a);
		if (is_not_nil(t)) {
		    t->run();
		    sprintf(interp->result, "%p", ThreadObjRef(t));
		    status = TCL_OK;
		} else {
		    Tcl_AppendResult(interp, "No threads available", 0);
		}
	    } else {
		Tcl_AppendResult(interp, err_argument_type, ": ", argv[1], 0); 
	    }
	} else {
	    Tcl_AppendResult(interp, err_argument_type, ": ", argv[1], 0); 
	}
    } else {
        Tcl_AppendResult(interp, err_argument_number, 0);
    }
    return status;
}

int Dish::action(ClientData, Tcl_Interp* interp, int argc, char* argv[]) {
    int status = TCL_ERROR;
    if (argc > 1) {
	ActionRef a = new DishAction(interp, Tcl_Merge(argc - 1, &argv[1]));
	char buffer[100];
	Dish& d = Dish::instance();
	Tcl_AppendResult(interp, d.object_to_string(a, buffer), 0);
	status = TCL_OK;
    } else {
	Tcl_AppendResult(
            interp, err_argument_number, ".\nExpected: 'action command'.", 0
        );
    }
    return status;
}

int Dish::delay(ClientData, Tcl_Interp* interp, int argc, char* argv[]) {
    int status = TCL_ERROR;
    if (argc == 2) {
	Dish& d = Dish::instance();
	double n;
	if (Tcl_GetDouble(interp, d.try_expand(argv[1], interp), &n) == TCL_OK) {
	    if (Fresco::delay(Float(n))) {
		status = TCL_OK;
	    } else {
		Tcl_AppendResult(interp, "Failed in delay", 0);
	    }
	}
    } else {
	Tcl_AppendResult(interp, err_argument_number, 0);
    }
    if ((status & TCL_ERROR) != 0) {
	Tcl_AppendResult(interp, ".\nExpected: 'delay float-seconds'.", 0);
    }
    return status;
}

int Dish::debug(ClientData, Tcl_Interp* interp, int argc, char* argv[]) {
    int status = TCL_ERROR;
    BaseObjectRef object;
    GlyphRef g;
    Dish& dish = Dish::instance();

    if (argc != 3) {
	Tcl_AppendResult(interp, err_argument_number, 0);
    } else if (
	!dish.string_to_object(dish.try_expand(argv[1], interp), object)
    ) {
	Tcl_AppendResult(
	    interp, "Argument 1 ", argv[1], " is not a dish object.", 0
	);
    } else if (is_nil(g = Glyph::_narrow(object))) {
	Tcl_AppendResult(
	    interp, "Argument 1 ", argv[1], " is not a glyph.", 0
	);
    } else {
	char* l = new char[strlen(argv[2]) + 1];
	sprintf(l, "%s", argv[2]);
	GlyphRef d = new DebugGlyph(g, l, DebugGlyph::trace_request_traverse);
	char buffer[100];
	Tcl_AppendResult(interp, dish.object_to_string(d, buffer), 0);
	status = TCL_OK;
    }

    if (status == TCL_ERROR) {
	Tcl_AppendResult(interp, "  Expected: debug <glyph> label.", 0);
    }
    return status;
}

/* class FrescoMain */

FrescoMain::FrescoMain(ViewerRef viewer, GlyphRef glyph) {
    viewer_ = viewer;
    glyph_ = glyph;
}

FrescoMain::~FrescoMain() {}

void FrescoMain::execute() {
    ViewerRef v = viewer_;
    GlyphRef g = glyph_;
    Fresco* fresco = Dish::instance().fresco();
    Display_var display = fresco->open_default_display();
    Screen_var s = display->default_screen();
    ViewerRef nv;
    if (is_nil(g)) {
	nv = v;
    } else {
	nv = new ViewerImpl(fresco, true);
	nv->body(g);
	if (is_not_nil(v)) {
	    nv->append_viewer(v);
	}
    }
    Window_var w = s->application(nv);
    w->map();
    display->run(true);
    delete this;
}

/* class DishAction */

DishAction::DishAction(Tcl_Interp* interp, char* command) {
    interp_ = interp;
    command_ = command;
}

DishAction::~DishAction() {
    delete command_;
}

void DishAction::execute() {
    if (Tcl_Eval(interp_, command_) != TCL_OK) {
	fprintf(stderr, "DishAction failed: %s.\n", interp_->result);
	fflush(stdout);
    }
}

/*
 *  Called by main() defined in tcl lib.
 */

#if defined(sun) && !defined(SVR4)
extern "C" {
  void _main();
  void on_exit(void (*)(), caddr_t);
}
#endif

int Tcl_AppInit(Tcl_Interp* interp) {
#if defined(sun) && !defined(SVR4)
    _main();
    on_exit(&Dish::cleanup, NULL);
#else
    atexit(&Dish::cleanup);
#endif
    Dish* dish = new Dish(interp);
    dish->add_commands(interp);
    dish->add_variables(interp);
    return TCL_OK;
}
