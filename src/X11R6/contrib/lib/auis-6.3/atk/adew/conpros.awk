
# 		Copyright IBM Corporation 1991
# 
#                       All Rights Reserved
# 
# Permission to use, copy, modify, and distribute this software and its 
# documentation for any purpose and without fee is hereby granted, 
# provided that the above copyright notice appear in all copies and that
# both that copyright notice and this permission notice appear in 
# supporting documentation, and that the name of IBM not be
# used in advertising or publicity pertaining to distribution of the
# software without specific, written prior permission.  
# 
# IBM DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING
# ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL
# IBM BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR
# ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
# WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS # ACTION,
# ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
# SOFTWARE.
# 
# $Disclaimer: 
# Permission to use, copy, modify, and distribute this software and its 
# documentation for any purpose is hereby granted without fee, 
# provided that the above copyright notice appear in all copies and that 
# both that copyright notice, this permission notice, and the following 
# disclaimer appear in supporting documentation, and that the names of 
# IBM, Carnegie Mellon University, and other copyright holders, not be 
# used in advertising or publicity pertaining to distribution of the software 
# without specific, written prior permission.
# 
# IBM, CARNEGIE MELLON UNIVERSITY, AND THE OTHER COPYRIGHT HOLDERS 
# DISCLAIM ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING 
# ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS.  IN NO EVENT 
# SHALL IBM, CARNEGIE MELLON UNIVERSITY, OR ANY OTHER COPYRIGHT HOLDER 
# BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY 
# DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, 
# WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS 
# ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE 
# OF THIS SOFTWARE.
#  $
BEGIN{
	vcount = 0
	class= "! No Control Button Defined"
	classoverride = 0
	lastfunc = ""
}
{
if ($1 == ">OBJ<" && NF ==2){
	typelist[$2] = $2
	 type=$2
}
if ($1 == ">VIEW<" && NF ==2){
	typelist[$2] = $2
	view = $2
}
if ($1 == ">REF<" ) {
	if(NF == 2) rname = $2
	else rname = substr($0,7,length - 6);
	if(rname != "" && rname != " "){
	    if(view == "controlV"){
		cvlist[rname] = rname
	    }
	    else {
		oblist[rname] = rname
		str = ""
		slen = 0
		for(i = 1; i <= length(rname); i++){
			cc = substr(rname,i,1)
			if(cc < "0" || (cc > "9" && cc < "A") || (cc > "Z" && cc < "a") || cc > "z") {
				str = str  "_"
				if(slen == 0) slen = i
			}
			else {
				if(slen > 0 && (cc < "0" || cc > "9")) slen = 0
				str = str  cc
		}
		}
		obfunc[rname] = str
		if(type == "value") {
			num = 0
			if(slen > 0){
				num = substr(str,slen + 1 ,10)
				 str = substr(str,1,slen - 1)
				}
			valuelist[rname] = str
			uval[str] = str
			numlist[rname] = num
			vcount++
			}
		obtype[rname] = type
		obview[rname]=view
	    }
	}
}
if(NR == 1 && $1=="CLASSNAME=" && $3== "FUNCTIONAME=" && NF=4){
	class = $2
	funcs[$4] = $4
	funcdefined++
	classoverride = 1
}
if($2 == "<class>" && $1=="[string]" && NF==3 && view == "controlV"){
	newclass = substr($3,2,(length($3) - 2))
	if(class == "! No Control Button Defined")
		class = newclass
	if(class == newclass){
		if(lastfunc != ""){
			funcs[lastfunc] = lastfunc
			funcdefined++
			lastfunc = ""
		}
	}
	else if(classoverride == 0){
		class = "! More Than one controller classes defined"
		classoverride = 2
		}
	}
if($2 == "<function>" && $1=="[string]" && NF==3 && view == "controlV"){
	lastfunc = substr($3,2,(length($3) - 2))
	}
}
END {
    if(substr(class,1,1) == "!" ) {
	print class
	}
    else {
	printf "%s.ch\n",class
	printf "/* user code begins here for %s */\n","HeaderInfo"
	printf "/* user code ends here for %s */\n","HeaderInfo"
	printf "class %s : observable [observe] { \nclassprocedures :\n        InitializeClass() returns boolean;\n",class,class
	printf "	FinalizeObject(struct %s *self);\n",class
	printf "	InitializeObject(struct %s *self) returns boolean;\n",class
	printf "/* user code begins here for %s */\n","classprocedures"
	printf "/* user code ends here for %s */\n","classprocedures"
	printf "overrides:\n\tObservedChanged( struct observable * observed, long status );\t"
	printf "/* user code begins here for %s */\n","overrides"
	printf "/* user code ends here for %s */\n","overrides"
	printf "data:\n"
	for(i in oblist){
		printf "	struct %s *%s;\n",obtype[i],obfunc[i]
		printf "	struct %s *%sView;\n",obview[i],obfunc[i]
	}
	printf "/* user code begins here for %s */\n","classdata"
	printf "/* user code ends here for %s */\n","classdata"
	printf "	struct view *v;\n\tstruct arbiterview *arbv;\n\tstruct %s *next;\n};\n\n",class
	printf "! THIS IS THE END OF THIS FILE !!!\n%s.c\n",class
	printf "/* user code begins here for %s */\n","HeaderInfo"
	printf "/* user code ends here for %s */\n","HeaderInfo"
	printf "#include <andrewos.h>\n#include <class.h>\n#include <proctbl.ih>\n#include <view.ih>\n#include <arbiterv.ih>\n#include <%s.eh>\n",class
	for(i in typelist){
		printf "#include <%s.ih>\n",typelist[i]
	}
	printf "/* user code begins here for %s */\n","includes"
	printf "/* user code ends here for %s */\n","includes"
	printf "\nstatic struct %s *first%s;\n",class,class
	printf "static struct %s *FindSelf(v)\nstruct view *v;\n{\n	struct %s *self,*last = NULL;\n",class,class
	printf "\tstruct arbiterview *arbv =arbiterview_FindArb(v);\n"
	printf "\tfor(self= first%s; self != NULL; self = self->next){\n\t\tif(self->arbv == arbv) return self;\n\t\tlast = self;\n\t\t}\n",class
	printf "\tself = %s_New();\n\tself->arbv = arbv;\n\tinitself(self,v);\n\tif(last == NULL) first%s = self;\n",class,class
	printf "\telse last->next = self;\n"
	printf "\tarbiterview_AddObserver(self->arbv,self);\n\treturn self;\n}\n"
	if(vcount){
		for(i in uval){
			printf "static void %sCallBack(self,val,r1,r2)\nstruct %s *self;\nstruct value *val;\nlong r1,r2;\n{\n", uval[i],class
			printf "if(r2 == value_OBJECTDESTROYED) {\n"
			for(j in valuelist){
				if(valuelist[j] == uval[i]){
					printf "\tif(self->%s == val) ",obfunc[j]
					printf "self->%s = NULL;\n",obfunc[j]
				}
			}
			printf "}\n",uval[i]
			printf "{\n/* user code begins here for %sCallBack */\n",uval[i]
			printf "/* user code ends here for %sCallBack */\n}\n}\n",uval[i]
			}
		}
	printf "static initself(self,v)\nstruct %s *self;\nstruct view *v;\n{\n",class
	printf "\tself->v = v;\n"
	for(i in oblist){
		printf "\tself->%sView = (struct %s *)arbiterview_GetNamedView(v,\"%s\");\n",obfunc[i],obview[i],oblist[i]
		printf "\tself->%s = (struct %s *)arbiterview_GetNamedObject(v,\"%s\");\n",obfunc[i],obtype[i],oblist[i]
		if(obtype[i] == "value"){
			printf "\tif(self->%s) value_AddCallBackObserver(self->%s, self,%sCallBack,%d);\n",obfunc[i], obfunc[i], valuelist[i],numlist[i]
		}
		else {
			printf "\tif(self->%s) %s_AddObserver(self->%s, self);\n", obfunc[i],obtype[i],obfunc[i]
		}
		printf "\tif(self->%sView) %s_AddObserver(self->%sView,self);\n", obfunc[i],obview[i],obfunc[i]

	}
	print "}"
	for(i in funcs){
		printf "%s_%s(v,dat)\nstruct view *v;\n long dat;\n{\n",class,funcs[i]
		printf "struct %s *self;\nif((self = FindSelf(v)) == NULL) return;\n",class 
		printf "/* user code begins here for %s_%s */\n",class,funcs[i]
		printf "/* user code ends here for %s_%s */\n}\n",class,funcs[i]
	}
	printf "void %s__ObservedChanged(self,observed,status)\n",class
	printf "struct %s *self;\nstruct observable * observed;\nlong status;\n{\n",class
	printf "/* user code begins here for %s */\n","ObservedChanged"
	printf "/* user code ends here for %s */\n","ObservedChanged"
	printf "if(observed == (struct observable *) self->arbv){\n"
	printf "\tif (status == observable_OBJECTDESTROYED) self->arbv = NULL;\n"
	printf "\t else initself(self,self->v);\n}\n"
	printf "if (status == observable_OBJECTDESTROYED) {\n"
	for(i in oblist){
	     if(obtype[i] != "value"){
		printf "\tif (observed == (struct observable *) self->%s) self->%s=NULL;\n", obfunc[i], obfunc[i];
		}
	     printf "\tif (observed == (struct observable *) self->%sView) self->%sView=NULL;\n", obfunc[i], obfunc[i];
	}
	printf "}\n"
	printf "}\n"
	printf "boolean %s__InitializeClass(ClassID)\nstruct classheader *ClassID;\n{\n",class
	printf "struct classinfo *viewtype = class_Load(\"view\");\n"
	printf "first%s = NULL;\n",class
	for(i in funcs){
		printf  "proctable_DefineProc(\"%s-%s\",%s_%s, viewtype,NULL,\"%s %s\");\n", class,funcs[i],class,funcs[i],class,funcs[i]
		}
	printf "/* user code begins here for %s */\n","InitializeClass"
	printf "/* user code ends here for %s */\n","InitializeClass"
	print "return TRUE;\n}"
	printf "void %s__FinalizeObject(ClassID,self)\nstruct classheader *ClassID;\n",class
	printf "struct %s *self;\n{\n",class
	for(i in oblist){
		if(obtype[i] == "value"){
			printf "\tif(self->%s) value_RemoveCallBackObserver(self->%s, self);\n",obfunc[i], obfunc[i]
		}
	}
	printf "/* user code begins here for %s */\n","FinalizeObject"
	printf "/* user code ends here for %s */\n","FinalizeObject"
	print "}"
	printf "boolean %s__InitializeObject(ClassID,self)\nstruct classheader *ClassID;\nstruct %s *self;\n{\n",class,class
	for(i in oblist){
		printf "self->%s = NULL;\n",obfunc[i]
		printf "self->%sView = NULL;\n",obfunc[i]
	}
	printf "self->v = NULL;\nself->next = NULL;\n"
	printf "/* user code begins here for %s */\n","InitializeObject"
	printf "/* user code ends here for %s */\n","InitializeObject"
	printf "return TRUE;}\n"
	printf "/* user code begins here for %s */\n","Other Functions"
	printf "/* user code ends here for %s */\n","Other Functions"
	printf "! THIS IS THE END OF THIS FILE !!!\n"	
    }
}

