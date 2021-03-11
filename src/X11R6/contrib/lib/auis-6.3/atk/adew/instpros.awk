#/* ********************************************************************** *\
# *         Copyright IBM Corporation 1991 - All Rights Reserved      *
# *        For full copyright information see:'andrew/config/COPYRITE'     *
#\* ********************************************************************** */
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
      initing = 1
      acount = 0;	# number of objects
      bcount = 0;
      dclview = "";
      dclclass = "";
      funcdefined = 0;
      obcount = 0;
      childflag = "FALSE";
      ist = "nada";
      dclfunc = "InitApp";
   }
{
    if(initing == 1){
	if($1 == "DECLARE"){
	    if($3 == "long"){
		declarenm[acount] = "long "   $2   ";"
		  readnm[acount] = "adew_ReadLong(" "self->" $2 ",file,count);"
		  writenm[acount] =  "adew_WriteLong(" "self->" $2 ",file);"
		  initnm[acount] =  "adew_InitLong(" "self->" $2 ");"
		  bcount++
		  acount++
	    }
	    if($3 == "string"){
		declarenm[acount] = "char "   $2   "["  $4 "];"
		  readnm[acount] = "adew_ReadString(" "self->" $2 ",file,count);"
		  writenm[acount] =  "adew_WriteString(" "self->" $2 ",file);"
		  initnm[acount] =  "adew_InitString(" "self->" $2 ");"
		  bcount++
		  acount++
	    }
	    if($3 == "float"){
		declarenm[acount] = "float "   $2   ";"
		  readnm[acount] = "adew_ReadFloat(" "self->" $2 ",file,count);"
		  writenm[acount] =  "adew_WriteFloat(" "self->" $2 ",file);"
		  initnm[acount] =  "adew_InitFloat(" "self->" $2 ");"
		  bcount++
		  acount++
	    }
	    if($3 == "longarray"){
		declarenm[acount] = "long "   $2   "["  $4 "];"
		  readnm[acount] = "adew_ReadLongArray(" "self->" $2 ",file,count," $4 ");"
		  writenm[acount] =  "adew_WriteLongArray(" "self->" $2 ",file," $4 ");"
		  initnm[acount] =  "adew_InitLongArray(" "self->" $2 "," $4 ");"
		  bcount += $4
		  acount++
	    }
	    if($3 == "stringarray"){
		declarenm[acount] = "char  "   $2   "["  $4 "][" $5 "];"
		  readnm[acount] = "adew_ReadStringArray(" "self->" $2 ",file,count," $4 ");"
		  writenm[acount] =  "adew_WriteStringArray(" "self->" $2 ",file," $4 ");"
		  initnm[acount] =  "adew_InitStringArray(" "self->" $2 "," $4 ");"
		  bcount += $4
		  acount++
	    }
	    if($3 == "viewname")
		dclview = $2;
	    if($3 == "funcname")
		dclfunc = $2;
	    if($3 == "classname"){
		dclclass = $2 ;
		class = $2;
		classoverride = 1;
	    }
	    if($3 == "writechild")
		childflag = $2
	    continue;
	}
	else  {
	    initing = 2
	}
    }
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
	  obcount++
    }
}
    }
    if($2 == "<class>" && $1=="[string]" && NF==3 && view == "controlV"){
	newclass = substr($3,2,(length($3) - 2))
	  if(class == "! No Control Button Defined")
	      class = newclass
		if(newclass == dclview){
		    if(lastfunc != ""){
			funcs[lastfunc] = lastfunc
			  funcdefined = 1
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
	dclass = class
	  if(dclview == "")
	      class = class "v"
	  else class = dclview;
#write dataobj .ch file
printf "%s.ch\n",dclass
printf "/* user code begins here for HeaderInfo */\n"
printf "/* user code ends here for HeaderInfo */\n"
printf "class %s : arbiter{ \n",dclass
printf "methods:\n"
printf "/* user code begins here for methods*/\n"
printf "/* user code ends here for methods */\n"
printf "overrides:\n"
printf "    ViewName() returns char *;\n"
printf "     ReadSup (FILE *file, long id) returns long;\n"
printf "     WriteSup (FILE *file, long writeid, int level) returns long;\n"
printf "     ReadObjects() ;\n"
printf "/* user code begins here for overrides */\n"
printf "/* user code ends here for overrides */\n"
printf "classprocedures:\n"
printf "    InitializeObject(struct lset *self) returns boolean;\n"
printf "/* user code begins here for classprocedures */\n"
printf "/* user code ends here for classprocedures */\n"
printf "macromethods:\n"
printf "/* user code begins here for macromethods */\n"
printf "/* user code ends here for macromethods */\n"
printf "data:\n"
for(i = 0; i < acount; i++) print "    " declarenm[i];
printf "/* user code begins here for classdata */\n"
printf "/* user code ends here for classdata */\n"
printf "};\n"
printf "! THIS IS THE END OF THIS FILE !!!\n%s.c\n",dclass
#write dataobj .c file
printf "/* user code begins here for HeaderInfo */\n"
printf "/* user code ends here for HeaderInfo */\n"
printf "#include <class.h>\n"
printf "#include <%s.eh>\n",dclass
printf "#include <%s.fh>\n",dclass
printf "#include <dataobj.ih>\n"
printf "#include <adew.h>\n"
printf "/* user code begins here for includes */\n"
printf "/* user code ends here for includes */\n"
printf "char *%s__ViewName(self)\n",dclass
printf "struct %s *self;\n",dclass
printf "{\n"
printf "    return \"%s\";\n",class
printf "}\n"
printf "void %s__ReadObjects(self)\n",dclass
printf "struct %s *self;\n",dclass
printf "{\n"
printf "/* user code begins here for ReadObjects */\n"
printf "/* user code ends here for ReadObjects */\n"
printf "}\n"
printf "boolean %s__InitializeObject(classID, self)\n",dclass
printf "struct classheader *classID;\n"
printf "struct %s *self;\n",dclass
printf "{\n"
printf "%s_SetDefaultStream(self,defaultstr);\n",dclass
printf "%s_SetWriteChild(self,%s);\n",dclass,childflag
# series of generated init calls are here
for(i = 0; i < acount; i++) print "    " initnm[i];
printf "/* user code begins here for InitializeObject */\n"
printf "/* user code ends here for InitializeObject */\n"
printf "return TRUE;\n"
printf "}\n"
printf "long %s__ReadSup(self, file, id)\n",dclass
printf "struct %s *self;\n",dclass
printf "FILE *file;\n"
printf "long id;\n"
printf "{\n"
printf "    long c,count,extradata;\n"
printf "    if((c = getc(file)) != '@'){\n"
printf "	ungetc(c,file);\n"
printf "	return dataobject_NOREADERROR;\n"
printf "    }\n"
# read the number of data elements to be read 
print "    fscanf(file,\"%ld\\n\",&count);"
# read known data elements
for(i = 0; i < acount; i++) print "    " readnm[i];
# read data saved by newer versions that this version doesn't know about
print "    if((extradata = count) > 0)" # may cause compiler warning 
print "        adew_ReadExtraData(file,count);"
printf "/* user code begins here for ReadSup */\n"
printf "/* user code ends here for ReadSup */\n"
printf "    return dataobject_NOREADERROR;\n"
printf "}\n"
printf "long %s__WriteSup(self,file ,writeid,level)\n",dclass
printf "struct %s *self;\n",dclass
printf "FILE *file;\n"
printf "long writeid;\n"
printf "int level;\n"
printf "{\n"
printf "/* user code begins here for Preparing_To_Write */\n"
printf "/* user code ends here for Preparing_To_Write */\n"
printf "    fprintf(file,\"@%d\\n\");/* the number of data elements */\n",bcount
for(i = 0; i < acount; i++) print "    " writenm[i];
printf "/* user code begins here for WriteSup */\n"
printf "/* user code ends here for WriteSup */\n"
printf "    return(TRUE);\n"
printf "}\n"
printf "/* user code begins here for %s */\n","Other Functions"
printf "/* user code ends here for %s */\n","Other Functions"
printf "! THIS IS THE END OF THIS FILE !!!\n"
# write view ch file
printf "%s.ch\n",class
printf "/* user code begins here for %s */\n","HeaderInfo"
printf "/* user code ends here for %s */\n","HeaderInfo"
printf "#include <adew.h>\n"
printf "class %s : arbiterview[arbiterv] { \nclassprocedures :\n",class
printf "	FinalizeObject(struct %s *self);\n",class
printf "	InitializeObject(struct %s *self) returns boolean;\n",class
printf "        InitializeClass() returns boolean;\n"
printf "/* user code begins here for %s */\n","classprocedures"
printf "/* user code ends here for %s */\n","classprocedures"
printf "overrides:\n\tObservedChanged( struct observable * observed, long status );\n"
printf "\tInitCell(struct celview *cv);\n"
printf "\tGetApplicationLayer() returns struct view *;\n"
printf "/* user code begins here for %s */\n","overrides"
printf "/* user code ends here for %s */\n","overrides"
printf "methods:\n\t%s();\n",dclfunc
printf "/* user code begins here for %s */\n","methods"
printf "/* user code ends here for %s */\n","methods"
printf "data:\n"
printf "long InitCount;\n"
for(i in oblist){
    printf "	struct %s *%s;\n",obtype[i],obfunc[i]
    printf "	struct %s *%sView;\n",obview[i],obfunc[i]
}
printf "    struct adew_array AdewArray[%d];\n", obcount + 1
printf "/* user code begins here for %s */\n","classdata"
printf "/* user code ends here for %s */\n","classdata"
printf "	struct view *v;\n\tstruct arbiterview *arbv;\n\tstruct %s *next;\n};\n\n",class
printf "! THIS IS THE END OF THIS FILE !!!\n%s.c\n",class
#write view .c file
printf "/* user code begins here for %s */\n","HeaderInfo"
printf "/* user code ends here for %s */\n","HeaderInfo"
printf "#include <andrewos.h>\n#include <class.h>\n#include <proctbl.ih>\n#include <cel.ih>\n#include <celview.ih>\n"
printf "#include <view.ih>\n#include <arbiter.ih>\n#include <arbiterv.ih>\n#include <%s.ih>\n#include <%s.eh>\n",dclass,class
for(i in typelist){
    printf "#include <%s.ih>\n",typelist[i]
}
printf "#define DataObject(A) (A->header.view.dataobject)\n"
printf "#define Parent(A) (A->header.view.parent)\n"
printf "#define Data(A) ((struct %s *)DataObject(A) )\n",dclass
printf "#define CEL(A) ((struct cel  *)DataObject(A) )\n"
printf "/* user code begins here for %s */\n","includes"
printf "/* user code ends here for %s */\n","includes"
printf "static struct  %s *FindSelf(v)\n",class
printf "struct view *v;\n{\n"
printf " if(class_IsTypeByName(v,\"%s\")) return (struct %s *) v;\n",class,class
printf "else return (struct %s *)arbiterview_FindArb(v);\n}\n",class
# Should probably make sure it is a 'class' type 
printf "struct view *%s__GetApplicationLayer(self)\n",class
printf "    struct %s *self;\n",class
printf "{\n"
printf "    %s_SetAppFlag(self,TRUE);\n",class
printf "    return super_GetApplicationLayer(self);\n"
printf "}\n"
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
#code for initapp
printf "void %s__%s(self)\nstruct %s *self;",class,dclfunc,class
printf "{\n/* user code begins here for %s_%s */\n",class,dclfunc
printf "/* user code ends here for %s_%s */\n}\n",class,dclfunc
#code for initcell
printf "void %s__InitCell(self,cv)\nstruct %s *self;\nstruct celview *cv;\n{\n",class,class
printf "\tsuper_InitCell(self, cv);\n"
printf "\tif(*(self->AdewArray[0].object) == NULL) adew_InitDataObjects(self,self->AdewArray);\n"
printf "\tadew_InitApplicationCel(self,NULL,cv,self->AdewArray,&(self->InitCount));\n"
printf "\tif(self->InitCount == 0){\n\t\tself->InitCount--;\n"
printf "\t\t%s_%s(self);\n\t}\n",class,dclfunc
printf "/* user code begins here for InitCell */\n"
printf "/* user code ends here for InitCell */\n"
printf "}\n"
# add function definitions
if(funcdefined == 1){
    for(i in funcs){
	printf "%s_%s(v,dat)\nstruct view *v;\n long dat;\n{\n",class,funcs[i]
	printf "struct %s *self;\nif((self = FindSelf(v)) == NULL) return;\n",class 
	printf "/* user code begins here for %s_%s */\n",class,funcs[i]
	printf "/* user code ends here for %s_%s */\n}\n",class,funcs[i]
    }
}
printf "void %s__ObservedChanged(self,observed,status)\n",class
printf "struct %s *self;\nstruct observable * observed;\nlong status;\n{\n",class
printf "/* user code begins here for %s */\n","ObservedChanged"
printf "/* user code ends here for %s */\n","ObservedChanged"
printf "if (status == observable_OBJECTDESTROYED) {\n"
printf "adew_NoteDestroyed(self,observed,self->AdewArray);\n";
printf "}\n"
printf "}\n"
printf "boolean %s__InitializeClass(ClassID)\nstruct classheader *ClassID;\n{\n",class
if(funcdefined == 1){
    printf "struct classinfo *viewtype = class_Load(\"view\");\n"
      for(i in funcs){
	  printf  "proctable_DefineProc(\"%s-%s\",%s_%s, viewtype,NULL,\"%s %s\");\n", class,funcs[i],class,funcs[i],class,funcs[i]
      }
}
printf "/* user code begins here for %s */\n","InitializeClass"
printf "/* user code ends here for %s */\n","InitializeClass"
print "return TRUE;\n}"
printf "void %s__FinalizeObject(ClassID,self)\nstruct classheader *ClassID;\n",class
printf "struct %s *self;\n{\n",class
printf "adew_FinalizeApplication(self,self->AdewArray);\n";
printf "/* user code begins here for %s */\n","FinalizeObject"
printf "/* user code ends here for %s */\n","FinalizeObject"
print "}"
printf "boolean %s__InitializeObject(ClassID,self)\nstruct classheader *ClassID;\nstruct %s *self;\n{\n",class,class
j = 0
printf "struct adew_array *aa = self->AdewArray;\n"
for(i in oblist){
    printf "aa->object = (struct dataobject **) &(self->%s);\n",obfunc[i];
    printf "aa->view = (struct view **) &(self->%sView);\n",obfunc[i];
    printf "aa->name = atom_Intern(\"%s\");\n",oblist[i];
    if(obtype[i] == "value"){
	printf "aa->callback = %sCallBack;\n",valuelist[i]
	printf "aa->rock = %d;\n",numlist[i]
    }
    else {
	printf "aa->callback = NULL;\n"
	printf "aa->rock = -1;\n"
    }
    printf "aa++;\n"
}
printf "aa->object = NULL;",j
printf "adew_InitializeApplication(self,self->AdewArray);\n";
printf "self->v = NULL;\nself->next = NULL;\n"
printf "self->InitCount = %d;\n",obcount
printf "/* user code begins here for %s */\n","InitializeObject"
printf "/* user code ends here for %s */\n","InitializeObject"
printf "return TRUE;}\n"
printf "/* user code begins here for %s */\n","Other Functions"
printf "/* user code ends here for %s */\n","Other Functions"
printf "! THIS IS THE END OF THIS FILE !!!\n"	
    }
}

