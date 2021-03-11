/* ********************************************************************** *\
 *         Copyright IBM Corporation 1988,1991 - All Rights Reserved      *
 *        For full copyright information see:'andrew/config/COPYRITE'     *
\* ********************************************************************** */

/*
	$Disclaimer: 
 * Permission to use, copy, modify, and distribute this software and its 
 * documentation for any purpose is hereby granted without fee, 
 * provided that the above copyright notice appear in all copies and that 
 * both that copyright notice, this permission notice, and the following 
 * disclaimer appear in supporting documentation, and that the names of 
 * IBM, Carnegie Mellon University, and other copyright holders, not be 
 * used in advertising or publicity pertaining to distribution of the software 
 * without specific, written prior permission.
 * 
 * IBM, CARNEGIE MELLON UNIVERSITY, AND THE OTHER COPYRIGHT HOLDERS 
 * DISCLAIM ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING 
 * ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS.  IN NO EVENT 
 * SHALL IBM, CARNEGIE MELLON UNIVERSITY, OR ANY OTHER COPYRIGHT HOLDER 
 * BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY 
 * DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, 
 * WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS 
 * ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE 
 * OF THIS SOFTWARE.
 *  $
*/



/* Line number references are correct as of 28-Jul-88, 2:20 pm
 * (with a few exceptions due to a bug in the program that
 * generates this list).
 */

extern EliCons_t *EliAddToList();	/* ecommon.c, line 1166 */
extern int      EliBind();	/* intrface.c, line 185 */
extern void     (*EliCatchFn())();		/* ecommon.c, line 966 */
extern int      EliCatchMask();	/* ecommon.c, line 960 */
extern void     EliClearErr();	/* ecommon.c, line 586 */
extern void     EliCons_BindCar();	/* cons.c, line 129 */
extern void     EliCons_BindCdr();	/* cons.c, line 148 */
extern EliSexp_t *EliCons_GetCar();	/* cons.c, line 201 */
extern EliSexp_t *EliCons_GetCdr();	/* cons.c, line 207 */
extern EliCons_t *EliCons_GetNew();	/* intrface.c, line 69 */
extern void     EliDebug();	/* ecommon.c, line 1294 */
extern int      EliDebugFPrint();	/* ecommon.c, line 1261 */
extern long     EliDebugFPrintSince();	/* ecommon.c, line 1280 */
extern long     EliDebugHistNum();	/* ecommon.c, line 1245 */
extern char    *EliDebugMessage();	/* ecommon.c, line 1217 */
extern int      EliDebugStateNum();	/* ecommon.c, line 1231 */
extern void     EliDisplaySexp();	/* ecommon.c, line 639 */
extern eliErrStuff_t *EliErrNode();	/* ecommon.c, line 954 */
extern char    *EliErrStr();	/* eerror.c, line 59 */
extern EliCons_t *EliErr_Backtrace();	/* errops.c, line 69 */
extern int      EliErr_BacktraceP();	/* errops.c, line 63 */
extern EliSexp_t *EliErr_BadSexp();	/* errops.c, line 45 */
extern int      EliErr_BadSexpP();	/* errops.c, line 51 */
extern int      EliErr_ErrCode();	/* errops.c, line 39 */
extern char    *EliErr_ErrLoc();/* errops.c, line 57 */
extern int      EliErr_ErrP();	/* errops.c, line 33 */
extern int EliErr_UnixErr();
extern void     EliError();	/* eerror.c, line 47 */
extern EliSexp_t *EliEval();	/* ecommon.c, line 1193 */
extern int      EliEvalAndBind();	/* intrface.c, line 192 */
extern EliCons_t *EliEvalListToList();	/* ecommon.c, line 831 */
extern eliEvalStack_t *EliEvalStack();	/* ecommon.c, line 906 */
extern EliSym_t *EliEvalStk_FindSym();	/* intrface.c, line 199 */
extern void     EliEvalStk_PopN();	/* intrface.c, line 206 */
extern EliSexp_t *EliFGetSexp();/* intrface.c, line 50 */
extern EliSym_t *EliFindSym();	/* ecommon.c, line 242 */
extern int      EliFreeAllEvalStackNodes();	/* ecommon.c, line 1338 */
extern int      EliFreeAllTraceStackNodes();	/* ecommon.c, line 1352 */
extern int      EliGetListCars();	/* ecommon.c, line 791 */
extern EliCons_t *EliGetNextCell();	/* ecommon.c, line 814 */
extern EliSexp_t *EliGetSexp();	/* intrface.c, line 57 */
extern int      EliGetStateNum();	/* ecommon.c, line 1207 */
extern void     EliInit();	/* ecommon.c, line 397 */
extern EliSym_t *EliLambdaSym();/* ecommon.c, line 930 */
extern EliSym_t *EliLambdaqSym();	/* ecommon.c, line 936 */
extern EliSym_t *EliLambdavSym();	/* ecommon.c, line 942 */
extern EliSym_t *EliLambdavqSym();	/* ecommon.c, line 948 */
extern EliCons_t *EliLastCell();/* cons.c, line 192 */
extern int      EliLastCellP();	/* ecommon.c, line 308 */
extern EliCons_t *EliListFromCars();	/* ecommon.c, line 1132 */
extern int      EliListLen();	/* ecommon.c, line 256 */
extern int      EliNilP();	/* ecommon.c, line 283 */
extern EliSym_t *EliNilSym();	/* ecommon.c, line 912 */
extern char    *EliParseStr();	/* ecommon.c, line 115 */
extern void     EliPrimDef();	/* prmtives.c, line 140 */
extern int      EliProcessList();	/* intrface.c, line 132 */
extern EliSym_t *EliQuoteSym();	/* ecommon.c, line 924 */
extern void     EliReset();	/* intrface.c, line 31 */
extern EliSexp_t *EliSGetSexp();/* intrface.c, line 43 */
extern char    *EliSPutSexp();	/* ecommon.c, line 1053 */
extern char    *EliSaveString();/* ecommon.c, line 375 */
extern void     EliSetCatchFn();/* ecommon.c, line 578 */
extern void     EliSetCatchMask();	/* ecommon.c, line 571 */
extern int      EliSexpEq();	/* ecommon.c, line 689 */
extern int      EliSexpEqual();	/* ecommon.c, line 721 */
extern EliCons_t *EliSexp_GetCons();	/* node.c, line 194 */
extern EliFn_t *EliSexp_GetFn();/* node.c, line 218 */
extern long     EliSexp_GetInt();	/* node.c, line 212 */
extern EliSexp_t *EliSexp_GetNew();	/* intrface.c, line 63 */
extern EliStr_t *EliSexp_GetStr();	/* node.c, line 206 */
extern EliSym_t *EliSexp_GetSym();	/* node.c, line 200 */
extern eliDataTypes_t EliSexp_GetType();	/* node.c, line 176 */
extern void     EliSexp_SetCons();	/* node.c, line 150 */
extern void     EliSexp_SetFn();/* node.c, line 163 */
extern void     EliSexp_SetInt();	/* node.c, line 114 */
extern void     EliSexp_SetSexp();	/* node.c, line 224 */
extern void     EliSexp_SetStr();	/* node.c, line 137 */
extern void     EliSexp_SetSym();	/* node.c, line 124 */
extern EliStr_t *EliStr_GetNew();	/* intrface.c, line 94 */
extern char    *EliStr_GetString();	/* str.c, line 156 */
extern char    *EliStringOpBuf();	/* ecommon.c, line 872 */
extern eliHashTable_t *EliStringTable();	/* ecommon.c, line 894 */
extern EliStr_t *EliStringTable_Find();	/* intrface.c, line 213 */
extern EliStr_t *EliStringTable_FindOrMake();	/* intrface.c, line 220 */
extern EliStr_t *EliStringTable_Make();	/* intrface.c, line 227 */
extern EliSym_t *EliSymTab_Find();	/* intrface.c, line 234 */
extern EliSym_t *EliSymTab_FindOrMake();	/* intrface.c, line 241 */
extern EliSym_t *EliSymTab_FindOrMakeAndBind();	/* intrface.c, line 248 */
extern EliSym_t *EliSymTab_Make();	/* intrface.c, line 256 */
extern EliSym_t *EliSymTab_MakeAndBind();	/* intrface.c, line 263 */
extern void     EliSym_BindFn();/* sym.c, line 198 */
extern void     EliSym_BindSexp();	/* sym.c, line 131 */
extern EliFn_t *EliSym_GetFn();	/* sym.c, line 216 */
extern EliStr_t *EliSym_GetName();	/* sym.c, line 224 */
extern EliSym_t *EliSym_GetNew_StrNode();	/* intrface.c, line 87 */
extern EliSym_t *EliSym_GetNew_String();	/* intrface.c, line 75 */
extern EliSexp_t *EliSym_GetSexp();	/* sym.c, line 190 */
extern eliHashTable_t *EliSymbolTable();	/* ecommon.c, line 888 */
extern EliSym_t *EliTSym();	/* ecommon.c, line 918 */
extern eliHashTable_t *EliTempSymTable();	/* ecommon.c, line 900 */
extern eliTraceStack_t *EliTraceStk();	/* ecommon.c, line 972 */
extern void     EliTraceStk_Purge();	/* intrface.c, line 271 */
extern char    *EliUnParseStr();/* ecommon.c, line 182 */
extern void     EliUpCaseStr();	/* ecommon.c, line 226 */
extern void     EliVersion();	/* ecommon.c, line 1331 */
extern int      eliBind();	/* stack.c, line 97 */
extern void     eliBucketNode_DecrRefcount();	/* buktnode.c, line 160 */
extern EliStr_t *eliBucketNode_GetKey();	/* buktnode.c, line 136 */
extern eliBucketNode_t *eliBucketNode_GetNew();	/* buktnode.c, line 42 */
extern eliBucketNode_t *eliBucketNode_GetNewBlock();	/* buktnode.c, line 87 */
extern eliBucketNode_t *eliBucketNode_GetNew_trace();	/* errstkop.c, line 141 */
extern eliBucketNode_t *eliBucketNode_GetNext();	/* buktnode.c, line
							 * 128 */
extern eliBucketNode_t *eliBucketNode_GetPrev();	/* buktnode.c, line
							 * 120 */
extern EliSexp_t *eliBucketNode_GetSexp();	/* buktnode.c, line 144 */
extern void     eliBucketNode_IncrRefcount();	/* buktnode.c, line 152 */
extern void     eliBucketNode_SetNext();	/* buktnode.c, line 112 */
extern void     eliBucketNode_SetPrev();	/* buktnode.c, line 104 */
extern void     eliBucket_Delete();	/* bucket.c, line 91 */
extern EliSexp_t *eliBucket_Find();	/* bucket.c, line 78 */
extern eliBucketNode_t *eliBucket_Find_aux();	/* bucket.c, line 60 */
extern void     eliBucket_Init();	/* bucket.c, line 33 */
extern void     eliBucket_Insert();	/* bucket.c, line 41 */
extern int      eliConsStringLen();	/* ecommon.c, line 1021 */
extern int      eliCons_DecrRefcount();	/* cons.c, line 158 */
extern EliCons_t *eliCons_GetNew();	/* cons.c, line 42 */
extern EliCons_t *eliCons_GetNewBlock();	/* cons.c, line 105 */
extern EliCons_t *eliCons_GetNew_trace();	/* errstkop.c, line 90 */
extern void     eliCons_IncrRefcount();	/* cons.c, line 181 */
extern void     eliDecrRefcount_SexpRef();	/* ecommon.c, line 41 */
extern int      eliDecrRefcount_SexpRef_aux();	/* ecommon.c, line 49 */
extern void     eliDisplayCons();	/* ecommon.c, line 678 */
extern EliCons_t *eliErr_GetBacktrace();	/* errnode.c, line 61 */
extern int      eliErr_GetCode();	/* errnode.c, line 49 */
extern char    *eliErr_GetLoc();/* errnode.c, line 67 */
extern EliSexp_t *eliErr_GetNode();	/* errnode.c, line 55 */
extern int eliErr_GetUnixErr();
extern void     eliErr_Init();	/* errnode.c, line 79 */
extern void     eliErr_Set();	/* errnode.c, line 31 */
extern int      eliErr_SexpP();	/* errnode.c, line 73 */
extern void     eliEval();	/* eval.c, line 33 */
extern int      eliEvalAndBind();	/* stack.c, line 42 */
extern void     eliEvalLambda();/* eval.c, line 271 */
extern void     eliEvalLambdaq();	/* eval.c, line 311 */
extern void     eliEvalLambdav();	/* eval.c, line 345 */
extern void     eliEvalLambdavq();	/* eval.c, line 393 */
extern void     eliEvalList();	/* eval.c, line 93 */
extern EliSym_t *eliEvalStk_FindSym();	/* stk.c, line 86 */
extern void     eliEvalStk_Init();	/* stk.c, line 33 */
extern void     eliEvalStk_Pop();	/* stk.c, line 72 */
extern void     eliEvalStk_PopN();	/* stack.c, line 149 */
extern int      eliEvalStk_Push();	/* stk.c, line 43 */
extern EliSym_t *eliEvalStk_Top();	/* stk.c, line 63 */
extern EliSexp_t *eliFGetSexp();/* ecommon.c, line 613 */
extern EliSexp_t *eliFGetSexp_trace();	/* errstkop.c, line 177 */
extern char    *eliFindPrefix();/* prmtives.c, line 156 */
extern int      eliFn_DecrRefcount();	/* fnnode.c, line 82 */
extern void     (*eliFn_GetCompiled())();		/* fnnode.c, line 147 */
extern EliCons_t *eliFn_GetCons();	/* fnnode.c, line 141 */
extern EliFn_t *eliFn_GetNew();	/* fnnode.c, line 31 */
extern EliFn_t *eliFn_GetNewBlock();	/* fnnode.c, line 61 */
extern EliFn_t *eliFn_GetNew_trace();	/* errstkop.c, line 124 */
extern eliFnTypes_t eliFn_GetType();	/* fnnode.c, line 135 */
extern void     eliFn_IncrRefcount();	/* fnnode.c, line 76 */
extern void     eliFn_SetCompiled();	/* fnnode.c, line 123 */
extern void     eliFn_SetCons();/* fnnode.c, line 105 */
extern EliSexp_t *eliGetSexp();	/* ecommon.c, line 602 */
extern EliSexp_t *eliGetSexp_trace();	/* errstkop.c, line 160 */
extern void     eliHT_Delete();	/* ht.c, line 86 */
extern EliSexp_t *eliHT_Find();	/* ht.c, line 77 */
extern int      eliHT_Hash();	/* ht.c, line 33 */
extern void     eliHT_Init();	/* ht.c, line 47 */
extern void     eliHT_Insert();	/* ht.c, line 62 */
extern void     eliHT_SetHashFn();	/* ht.c, line 96 */
extern void     eliIncrRefcount_SexpRef();	/* ecommon.c, line 81 */
extern int      eliNumDigits();	/* ecommon.c, line 1036 */
extern int      eliParseStrLen();	/* ecommon.c, line 316 */
extern void     eliPrimDefCompiled();	/* prmtives.c, line 121 */
extern void     eliPrimInit();	/* prmtives.c, line 109 */
extern EliSexp_t *eliSGetSexp();/* ecommon.c, line 626 */
extern EliSexp_t *eliSGetSexp_trace();	/* errstkop.c, line 195 */
extern void     eliSPutCons_buf();	/* ecommon.c, line 1113 */
extern void     eliSPutSexp_buf();	/* ecommon.c, line 1070 */
extern          eliSetClientLibrary();	/* ecommon.c, line 386 */
extern int      eliSexpStringLen();	/* ecommon.c, line 978 */
extern void     eliSexp_DecrRefcount();	/* node.c, line 82 */
extern EliSexp_t *eliSexp_GetNew();	/* node.c, line 31 */
extern EliSexp_t *eliSexp_GetNewBlock();	/* node.c, line 61 */
extern EliSexp_t *eliSexp_GetNew_trace();	/* errstkop.c, line 107 */
extern void     eliSexp_IncrRefcount();	/* node.c, line 76 */
extern void     eliSexp_SetType();	/* node.c, line 187 */
extern char    *eliStrCat();	/* prmtives.c, line 195 */
extern int      eliStr_DecrRefcount();	/* str.c, line 126 */
extern EliStr_t *eliStr_GetNew();	/* str.c, line 39 */
extern EliStr_t *eliStr_GetNewBlock();	/* str.c, line 111 */
extern EliStr_t *eliStr_GetNew_trace();	/* errstkop.c, line 54 */
extern void     eliStr_IncrRefcount();	/* str.c, line 150 */
extern EliStr_t *eliStringTable_Find();	/* strtab.c, line 80 */
extern EliStr_t *eliStringTable_FindOrMake();	/* strtab.c, line 41 */
extern EliStr_t *eliStringTable_Make();	/* strtab.c, line 61 */
extern EliSym_t *eliSymTab_Find();	/* symtab.c, line 36 */
extern EliSym_t *eliSymTab_FindOrMake();	/* symtab.c, line 52 */
extern EliSym_t *eliSymTab_FindOrMakeAndBind();	/* symtab.c, line 96 */
extern EliSym_t *eliSymTab_Make();	/* symtab.c, line 67 */
extern EliSym_t *eliSymTab_MakeAndBind();	/* symtab.c, line 119 */
extern int      eliSym_DecrRefcount();	/* sym.c, line 154 */
extern EliSym_t *eliSym_GetNew();	/* sym.c, line 42 */
extern EliSym_t *eliSym_GetNewBlock();	/* sym.c, line 109 */
extern EliSym_t *eliSym_GetNew_trace();	/* errstkop.c, line 72 */
extern int      eliSym_GetRefcount();	/* sym.c, line 243 */
extern eliSymScopes_t eliSym_GetScope();	/* sym.c, line 230 */
extern void     eliSym_IncrRefcount();	/* sym.c, line 184 */
extern void     eliSym_SetScope();	/* sym.c, line 236 */
extern void     eliTraceStk_Init();	/* errstk.c, line 31 */
extern void     eliTraceStk_Pop();	/* errstk.c, line 147 */
extern void     eliTraceStk_Purge();	/* errstkop.c, line 31 */
extern int      eliTraceStk_PurgeN();	/* errstkop.c, line 39 */
extern int      eliTraceStk_PushBucketNode();	/* errstk.c, line 129 */
extern int      eliTraceStk_PushCons();	/* errstk.c, line 75 */
extern int      eliTraceStk_PushFn();	/* errstk.c, line 111 */
extern int      eliTraceStk_PushSexp();	/* errstk.c, line 93 */
extern int      eliTraceStk_PushStr();	/* errstk.c, line 39 */
extern int      eliTraceStk_PushSym();	/* errstk.c, line 57 */
extern eliTraceStackNode_t *eliTraceStk_Top();	/* errstk.c, line 175 */
extern eliObjTypes_t eliTraceStk_TopType();	/* errstk.c, line 183 */
extern void     eliyyerror();	/* eerror.c, line 69 */
#ifdef eliyywrap
#undef eliyywrap
#endif
extern int      eliyywrap();	/* eerror.c, line 74 */
