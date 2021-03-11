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

/ [ATDBC] / {
    if (definer[$5] == "") {
	definer[ep[epcount++] = $5] = $2;
	library[$5] = $1;
    }
}

/ U / {
    referrer[$4 "." refcount[$4]++] = $1 "." $2;
}

# postprocessing

END {


# Specify which entry points we definitely want.  Edit this list to add entry points.

  want[library["_class_RoutineStruct"] "." definer["_class_RoutineStruct"]] = "Y";
  want[library["_class_Error"] "." definer["_class_Error"]] = "Y";
  want[library["_ProgramName"] "." definer["_ProgramName"]] = "Y";
#  want[library["_class_Header"] "." definer["_class_Header"]] = "Y";
  want[library["_class_NewObject"] "." definer["_class_NewObject"]] = "Y";
  want[library["_class_Load"] "." definer["_class_Load"]] = "Y";
  want[library["_class_IsLoaded"] "." definer["_class_IsLoaded"]] = "Y";
  want[library["_class_Lookup"] "." definer["_class_Lookup"]] = "Y";
  want[library["_class_IsType"] "." definer["_class_IsType"]] = "Y";
  want[library["_class_IsTypeByName"] "." definer["_class_IsTypeByName"]] = "Y";
  want[library["_class_EnterInfo"] "." definer["_class_EnterInfo"]] = "Y";
  want[library["_class_SetClassPath"] "." definer["_class_SetClassPath"]] = "Y";
  want[library["_class_PrependClassPath"] "." definer["_class_PrependClassPath"]] = "Y";
  want[library["_class_GetEText"] "." definer["_class_GetEText"]] = "Y";
  want[library["_environ"] "." definer["_environ"]] = "Y";
  want[library["__exit"] "." definer["__exit"]] = "Y";
  want[library["_abort"] "." definer["_abort"]] = "Y";
  want[library["_alloca"] "." definer["_alloca"]] = "Y";
  want[library["_blt"] "." definer["_blt"]] = "Y";
  want[library["_bcopy"] "." definer["_bcopy"]] = "Y";
  want[library["_brk"] "." definer["_brk"]] = "Y";
  want[library["_bzero"] "." definer["_bzero"]] = "Y";
  want[library["_calloc"] "." definer["_calloc"]] = "Y";
  want[library["_cfree"] "." definer["_cfree"]] = "Y";
  want[library["_errno"] "." definer["_errno"]] = "Y";
  want[library["_close"] "." definer["_close"]] = "Y";
  want[library["_errno"] "." definer["_errno"]] = "Y";
  want[library["_creat"] "." definer["_creat"]] = "Y";
  want[library["__ctype_"] "." definer["__ctype_"]] = "Y";
  want[library["__doprnt"] "." definer["__doprnt"]] = "Y";
  want[library["_ecvt"] "." definer["_ecvt"]] = "Y";
  want[library["_fcvt"] "." definer["_fcvt"]] = "Y";
  want[library["_sys_errlist"] "." definer["_sys_errlist"]] = "Y";
  want[library["_sys_nerr"] "." definer["_sys_nerr"]] = "Y";
  want[library["_exit"] "." definer["_exit"]] = "Y";
  want[library["_fcntl"] "." definer["_fcntl"]] = "Y";
  want[library["__filbuf"] "." definer["__filbuf"]] = "Y";
  want[library["__iob"] "." definer["__iob"]] = "Y";
  want[library["__cleanup"] "." definer["__cleanup"]] = "Y";
  want[library["__flsbuf"] "." definer["__flsbuf"]] = "Y";
  want[library["_fclose"] "." definer["_fclose"]] = "Y";
  want[library["_fflush"] "." definer["_fflush"]] = "Y";
  want[library["_fopen"] "." definer["_fopen"]] = "Y";
  want[library["_fprintf"] "." definer["_fprintf"]] = "Y";
  want[library["_fread"] "." definer["_fread"]] = "Y";
  want[library["_fstat"] "." definer["_fstat"]] = "Y";
  want[library["_fwrite"] "." definer["_fwrite"]] = "Y";
  want[library["_gcvt"] "." definer["_gcvt"]] = "Y";
  want[library["_getdtablesize"] "." definer["_getdtablesize"]] = "Y";
  want[library["_getpagesize"] "." definer["_getpagesize"]] = "Y";
  want[library["_ioctl"] "." definer["_ioctl"]] = "Y";
  want[library["_isatty"] "." definer["_isatty"]] = "Y";
  want[library["_lseek"] "." definer["_lseek"]] = "Y";
  want[library["_malloc"] "." definer["_malloc"]] = "Y";
  want[library["_modf"] "." definer["_modf"]] = "Y";
  want[library["_realloc"] "." definer["_realloc"]] = "Y";
  want[library["_free"] "." definer["_free"]] = "Y";
  want[library["_open"] "." definer["_open"]] = "Y";
  want[library["_perror"] "." definer["_perror"]] = "Y";
  want[library["_printf"] "." definer["_printf"]] = "Y";
  want[library["_read"] "." definer["_read"]] = "Y";
  want[library["_sbrk"] "." definer["_sbrk"]] = "Y";
  want[library["curbrk"] "." definer["curbrk"]] = "Y";
  want[library["_sigvec"] "." definer["_sigvec"]] = "Y";
  want[library["_sprintf"] "." definer["_sprintf"]] = "Y";
  want[library["_strlen"] "." definer["_strlen"]] = "Y";
  want[library["_syscall"] "." definer["_syscall"]] = "Y";
  want[library["_write"] "." definer["_write"]] = "Y";
  want[library["_writev"] "." definer["_writev"]] = "Y";
  want[library["_toupper"] "." definer["_toupper"]] = "Y";
  want[library["_tolower"] "." definer["_tolower"]] = "Y";

  want[library["_gethostbyname"] "." definer["_gethostbyname"]] = "Y";
  want[library["_gethostbyaddr"] "." definer["_gethostbyaddr"]] = "Y";
  want[library["_sethostent"] "." definer["_sethostent"]] = "Y";
  if (definer["_gethostbyname"] != "gethostnamadr.o" )
	want[library["_gethostent"] "." definer["_gethostent"]] = "Y";
  want[library["_res_querydomain"] "." definer["_res_querydomain"]] = "Y";
  want[library["_res_mkquery"] "." definer["_res_mkquery"]] = "Y";
  want[library["_res_send"] "." definer["_res_send"]] = "Y";
  want[library["_dn_expand"] "." definer["_dn_expand"]] = "Y";
  want[library["_res_init"] "." definer["_res_init"]] = "Y";
  want[library["__endhtent"] "." definer["__endhtent"]] = "Y";
  want[library["__sethtent"] "." definer["__sethtent"]] = "Y";
  want[library["__gethtbyaddr"] "." definer["__gethtbyaddr"]] = "Y";
  want[library["__gethtbyname"] "." definer["__gethtbyname"]] = "Y";
  want[library["__gethtent"] "." definer["__gethtent"]] = "Y";
  want[library["__getrhbyaddr"] "." definer["__getrhbyaddr"]] = "Y";
  want[library["_endhostent"] "." definer["_endhostent"]] = "Y";
  want[library["_sethostfile"] "." definer["_sethostfile"]] = "Y";
  want[library["_hostalias"] "." definer["_hostalias"]] = "Y";
  want[library["_res_query"] "." definer["_res_query"]] = "Y";
  want[library["_res_search"] "." definer["_res_search"]] = "Y";
  want[library["__res_close"] "." definer["__res_close"]] = "Y";
  want[library["_fp_query"] "." definer["_fp_query"]] = "Y";
  want[library["_p_cdname"] "." definer["_p_cdname"]] = "Y";
  want[library["_p_class"] "." definer["_p_class"]] = "Y";
  want[library["_p_query"] "." definer["_p_query"]] = "Y";
  want[library["_p_rr"] "." definer["_p_rr"]] = "Y";
  want[library["_p_type"] "." definer["_p_type"]] = "Y";
  want[library["__getlong"] "." definer["__getlong"]] = "Y";
  want[library["__getshort"] "." definer["__getshort"]] = "Y";
  want[library["_dn_comp"] "." definer["_dn_comp"]] = "Y";
  want[library["_dn_skipname"] "." definer["_dn_skipname"]] = "Y";
  want[library["_putlong"] "." definer["_putlong"]] = "Y";
  want[library["_putshort"] "." definer["_putshort"]] = "Y";
  want[library["_strcasecmp"] "." definer["_strcasecmp"]] = "Y";
  want[library["_strncasecmp"] "." definer["_strncasecmp"]] = "Y";
  want[library["__res_opcodes"] "." definer["__res_opcodes"]] = "Y";
  want[library["__res_resultcodes"] "." definer["__res_resultcodes"]] = "Y";
  want[library["__res"] "." definer["__res"]] = "Y";
  want[library["_h_errno"] "." definer["_h_errno"]] = "Y";

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
