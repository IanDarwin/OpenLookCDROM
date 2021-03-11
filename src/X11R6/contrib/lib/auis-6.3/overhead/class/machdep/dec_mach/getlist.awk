## ###################################################################### ##
##         Copyright IBM Corporation 1988,1991 - All Rights Reserved      ##
##        For full copyright information see:'andrew/config/COPYRITE'     ##
## ###################################################################### ##
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


# awk script to list all modules and entry points from a library which are
# needed (via transitive closure) for a given list of basic entry points

# the input to this awk script should be generated by:  nm -go /lib/libc.a | tr : " "

# data structures used here:
#
#  definer[entry-point-name] = module-name
#  library[entry-point-name] = library-name
#  epcount = # entry-point-name's seen so far
#  ep[k] = k'th entry-point-name
#  refcount[entry-point-name] = number of references by wanted modules
#  referrer[entry-point-name.k] = k'th module-name with an undefined reference to this entry-point-name
#  want[module-name] = "Y" if this module wanted

# search output of nm and set up definers and referrers

/ [ATDBCEGSR] / {
    if (definer[$5] == "") {
	definer[ep[epcount++] = $5] = $2;
	library[$5] = $1;
    }
}

/ [UV] / {
    referrer[$4 "." refcount[$4]++] = $1 "." $2;
}

# postprocessing

END {


# Specify which entry points we definitely want.  Edit this list to add entry points.

  want[library["class_RoutineStruct"] "." definer["class_RoutineStruct"]] = "Y";
  want[library["class_Error"] "." definer["class_Error"]] = "Y";
  want[library["ProgramName"] "." definer["ProgramName"]] = "Y";
#  want[library["class_Header"] "." definer["class_Header"]] = "Y";
  want[library["class_NewObject"] "." definer["class_NewObject"]] = "Y";
  want[library["class_Load"] "." definer["class_Load"]] = "Y";
  want[library["class_IsLoaded"] "." definer["class_IsLoaded"]] = "Y";
  want[library["class_Lookup"] "." definer["class_Lookup"]] = "Y";
  want[library["class_IsType"] "." definer["class_IsType"]] = "Y";
  want[library["class_IsTypeByName"] "." definer["class_IsTypeByName"]] = "Y";
  want[library["class_EnterInfo"] "." definer["class_EnterInfo"]] = "Y";
  want[library["class_SetClassPath"] "." definer["class_SetClassPath"]] = "Y";
  want[library["class_PrependClassPath"] "." definer["class_PrependClassPath"]] = "Y";
  want[library["class_GetEText"] "." definer["class_GetEText"]] = "Y";
  want[library["environ"] "." definer["environ"]] = "Y";
  want[library["__start"] "." definer["__start"]] = "Y";
  want[library["_exit"] "." definer["_exit"]] = "Y";
  want[library["abort"] "." definer["abort"]] = "Y";
  want[library["alloca"] "." definer["alloca"]] = "Y";
  want[library["blt"] "." definer["blt"]] = "Y";
  want[library["bcopy"] "." definer["bcopy"]] = "Y";
  want[library["brk"] "." definer["brk"]] = "Y";
  want[library["bzero"] "." definer["bzero"]] = "Y";
  want[library["calloc"] "." definer["calloc"]] = "Y";
  want[library["cfree"] "." definer["cfree"]] = "Y";
  want[library["errno"] "." definer["errno"]] = "Y";
  want[library["close"] "." definer["close"]] = "Y";
  want[library["errno"] "." definer["errno"]] = "Y";
  want[library["creat"] "." definer["creat"]] = "Y";
  want[library["_ctype_"] "." definer["_ctype_"]] = "Y";
  want[library["vsprintf"] "." definer["vsprintf"]] = "Y";
  want[library["ecvt"] "." definer["ecvt"]] = "Y";
  want[library["fcvt"] "." definer["fcvt"]] = "Y";
  want[library["sys_errlist"] "." definer["sys_errlist"]] = "Y";
  want[library["sys_nerr"] "." definer["sys_nerr"]] = "Y";
  want[library["exit"] "." definer["exit"]] = "Y";
  want[library["fcntl"] "." definer["fcntl"]] = "Y";
  want[library["_filbuf"] "." definer["_filbuf"]] = "Y";
  want[library["_iob"] "." definer["_iob"]] = "Y";
  want[library["_cleanup"] "." definer["_cleanup"]] = "Y";
  want[library["_flsbuf"] "." definer["_flsbuf"]] = "Y";
  want[library["fclose"] "." definer["fclose"]] = "Y";
  want[library["fflush"] "." definer["fflush"]] = "Y";
  want[library["fopen"] "." definer["fopen"]] = "Y";
  want[library["fprintf"] "." definer["fprintf"]] = "Y";
  want[library["fread"] "." definer["fread"]] = "Y";
  want[library["fstat"] "." definer["fstat"]] = "Y";
  want[library["fwrite"] "." definer["fwrite"]] = "Y";
  want[library["gcvt"] "." definer["gcvt"]] = "Y";
 want[library["getdtablesize"] "." definer["getdtablesize"]] = "Y";
  want[library["getpagesize"] "." definer["getpagesize"]] = "Y";
  want[library["ioctl"] "." definer["ioctl"]] = "Y";
  want[library["isatty"] "." definer["isatty"]] = "Y";
  want[library["lseek"] "." definer["lseek"]] = "Y";
  want[library["malloc"] "." definer["malloc"]] = "Y";
  want[library["modf"] "." definer["modf"]] = "Y";
  want[library["realloc"] "." definer["realloc"]] = "Y";
  want[library["free"] "." definer["free"]] = "Y";
  want[library["open"] "." definer["open"]] = "Y";
  want[library["perror"] "." definer["perror"]] = "Y";
  want[library["printf"] "." definer["printf"]] = "Y";
  want[library["read"] "." definer["read"]] = "Y";
  want[library["sbrk"] "." definer["sbrk"]] = "Y";
  want[library["curbrk"] "." definer["curbrk"]] = "Y";
  want[library["sigvec"] "." definer["sigvec"]] = "Y";
  want[library["sprintf"] "." definer["sprintf"]] = "Y";
  want[library["strlen"] "." definer["strlen"]] = "Y";
  want[library["syscall"] "." definer["syscall"]] = "Y";
  want[library["write"] "." definer["write"]] = "Y";
  want[library["writev"] "." definer["writev"]] = "Y";
  want[library["gethostbyname"] "." definer["gethostbyname"]] = "Y";
  want[library["gethostbyname_local"] "." definer["gethostbyname_local"]] = "Y";
  want[library["getservbyname_local"] "." definer["getservbyname_local"]] = "Y";
  want[library["gethostbyaddr"] "." definer["gethostbyaddr"]] = "Y";
  want[library["sethostent"] "." definer["sethostent"]] = "Y";
  want[library["res_querydomain"] "." definer["res_querydomain"]] = "Y";
  want[library["res_mkquery"] "." definer["res_mkquery"]] = "Y";
  want[library["res_send"] "." definer["res_send"]] = "Y";
  want[library["dn_expand"] "." definer["dn_expand"]] = "Y";
  want[library["res_init"] "." definer["res_init"]] = "Y";
  want[library["_endhtent"] "." definer["_endhtent"]] = "Y";
  want[library["_sethtent"] "." definer["_sethtent"]] = "Y";
  want[library["_gethtbyaddr"] "." definer["_gethtbyaddr"]] = "Y";
  want[library["_gethtbyname"] "." definer["_gethtbyname"]] = "Y";
  want[library["_gethtent"] "." definer["_gethtent"]] = "Y";
  want[library["_getrhbyaddr"] "." definer["_getrhbyaddr"]] = "Y";
  want[library["endhostent"] "." definer["endhostent"]] = "Y";
  want[library["sethostfile"] "." definer["sethostfile"]] = "Y";
  want[library["hostalias"] "." definer["hostalias"]] = "Y";
  want[library["res_query"] "." definer["res_query"]] = "Y";
  want[library["res_search"] "." definer["res_search"]] = "Y";
  want[library["_res_close"] "." definer["_res_close"]] = "Y";
  want[library["fp_query"] "." definer["fp_query"]] = "Y";
  want[library["p_cdname"] "." definer["p_cdname"]] = "Y";
  want[library["p_class"] "." definer["p_class"]] = "Y";
  want[library["p_query"] "." definer["p_query"]] = "Y";
  want[library["p_rr"] "." definer["p_rr"]] = "Y";
  want[library["p_type"] "." definer["p_type"]] = "Y";
  want[library["_getlong"] "." definer["_getlong"]] = "Y";
  want[library["_getshort"] "." definer["_getshort"]] = "Y";
  want[library["dn_comp"] "." definer["dn_comp"]] = "Y";
  want[library["dn_skipname"] "." definer["dn_skipname"]] = "Y";
  want[library["putlong"] "." definer["putlong"]] = "Y";
  want[library["putshort"] "." definer["putshort"]] = "Y";
  want[library["strcasecmp"] "." definer["strcasecmp"]] = "Y";
  want[library["strncasecmp"] "." definer["strncasecmp"]] = "Y";
  want[library["_res_opcodes"] "." definer["_res_opcodes"]] = "Y";
  want[library["_res_resultcodes"] "." definer["_res_resultcodes"]] = "Y";
  want[library["_res"] "." definer["_res"]] = "Y";
  want[library["h_errno"] "." definer["h_errno"]] = "Y";

# now take transitive closure of wanted modules

    for (needmore = "Y"; needmore == "Y"; ) {
	needmore = "N";
	for ( i = 0; i < epcount; i++) {
	    if (want[library[ep[i]] "." definer[ep[i]]] != "Y") {
		want[library[ep[i]] "." definer[ep[i]]] = "N";
		for ( j = 0; j < refcount[ep[i]]; j++ ) {
		    if (want[referrer[ep[i] "." j]] == "Y") {
			want[library[ep[i]] "." definer[ep[i]]] = "Y";
			needmore = "Y";
			break;
		    }
		}
	    }
	}
    }

# write out all wanted module name and entry points
    for ( i = 0 ; i < epcount ; i++)
	if (want[library[ep[i]] "." definer[ep[i]]] == "Y")
		printf "%s %s\n", definer[ep[i]], ep[i];

}
