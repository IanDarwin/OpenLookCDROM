/* -*-C-*-
********************************************************************************
*
* File:         xlftab.h
* RCS:          $Header: /users/npm/src/winterp/src-server/xlisp/RCS/xlftab.h,v 2.4 1994/06/06 15:59:16 npm Exp $
* Description:  xlftab.h - xlisp function table
*		A file new to xlisp-2.1c which holds the external declarations
*		for all the xlisp functions. -- for 'xlisp' not 'winterp'
* Author:       David Michael Betz. WINTERP portions by Niels Mayer;
*		XLISP-PLUS by Tom Almy with contributions from Johnny
*		Greenblatt, Neal Holtz, Niels Mayer, Blake McBride, Mikael
*		Pettersson, Luke Tierney, Ken Whedbee, Pete Yadlowsky.
* Created:      
* Modified:     Mon Jun  6 03:04:26 1994 (Niels Mayer) npm@indeed
* Language:     C
* Package:      N/A
* Status:       X11r6 contrib release
*
* Copyright (C) 1994, Enterprise Integration Technologies Corp. and Niels Mayer.
* WINTERP 1.15-1.99, Copyright (c) 1993, Niels P. Mayer.
* WINTERP 1.0-1.14, Copyright (c) 1989-1992 Hewlett-Packard Co. and Niels Mayer.
* XLISP-STAT 2.1 Copyright (c) 1990, by Luke Tierney
* XLISP version 2.1, Copyright (c) 1989, by David Betz.
*
* Permission to use, copy, modify, distribute, and sell this software and its
* documentation for any purpose is hereby granted without fee, provided that
* the above copyright notice appear in all copies and that both that
* copyright notice and this permission notice appear in supporting
* documentation, and that the name of Enterprise Integration Technologies,
* Hewlett-Packard Company, Niels Mayer, Luke Tierney and David Betz not be
* used in advertising or publicity pertaining to distribution of the software
* without specific, written prior permission.  Enterprise Integration
* Technologies, Hewlett-Packard Company, Niels Mayer, Luke Tierney and David
* Betz make no representations about the suitability of this software for any
* purpose. It is provided "as is" without express or implied warranty.
*
* ENTERPRISE INTEGRATION TECHNOLOGIES, HEWLETT-PACKARD COMPANY, NIELS MAYER,
* LUKE TIERNEY AND DAVID BETZ DISCLAIM ALL WARRANTIES WITH REGARD TO THIS
* SOFTWARE, INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS,
* IN NO EVENT SHALL ENTERPRISE INTEGRATION TECHNOLOGIES, HEWLETT-PACKARD
* COMPANY, NIELS MAYER, LUKE TIERNEY NOR DAVID BETZ BE LIABLE FOR ANY SPECIAL,
* INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM
* LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE
* OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
* PERFORMANCE OF THIS SOFTWARE.
*
********************************************************************************
*/

/*
*------------------------------------------------------------------------------
* See ./winterp/COPYRIGHT for information on contacting the authors.
* Please e-mail comments, modifications, questions, improvements and
* bugfixes to the WINTERP mailing list winterp@netcom.com. Please send 
* mailing list subscribe/unsubscribe notices to winterp-request@netcom.com .
* Post XLISP-specific questions/information to the USENET newsgroup
* comp.lang.lisp.x.
*------------------------------------------------------------------------------
*/

#ifdef ANSI
#define V void
#else
#define V /* */
#endif
/* external functions */
extern LVAL
    rmhash(V),rmquote(V),rmdquote(V),rmbquote(V),rmcomma(V),
    clnew(V),clisnew(V),clanswer(V),
    obisnew(V),obclass(V),obshow(V),
    rmlpar(V),rmrpar(V),rmsemi(V),
    xeval(V),xapply(V),xfuncall(V),xquote(V),xfunction(V),xbquote(V),
    xlambda(V),xset(V),xsetq(V),xsetf(V),xdefun(V),xdefmacro(V),
    xgensym(V),xmakesymbol(V),xintern(V),
    xsymname(V),xsymvalue(V),xsymplist(V),
    xget(V),xputprop(V),xremprop(V),
    xhash(V),xmkarray(V),xaref(V),
    xcar(V),xcdr(V),
    xcaar(V),xcadr(V),xcdar(V),xcddr(V),
    xcaaar(V),xcaadr(V),xcadar(V),xcaddr(V),
    xcdaar(V),xcdadr(V),xcddar(V),xcdddr(V),
    xcaaaar(V),xcaaadr(V),xcaadar(V),xcaaddr(V),
    xcadaar(V),xcadadr(V),xcaddar(V),xcadddr(V),
    xcdaaar(V),xcdaadr(V),xcdadar(V),xcdaddr(V),
    xcddaar(V),xcddadr(V),xcdddar(V),xcddddr(V),
    xcons(V),xlist(V),xappend(V),xreverse(V),xlast(V),xnth(V),xnthcdr(V),
    xmember(V),xassoc(V),xsubst(V),xsublis(V),xlength(V),xsort(V),
    xremove(V),xremif(V),xremifnot(V),
    xmapc(V),xmapcar(V),xmapl(V),xmaplist(V),
    xrplca(V),xrplcd(V),xnconc(V),
    xdelete(V),xdelif(V),xdelifnot(V),
    xatom(V),xsymbolp(V),xnumberp(V),xboundp(V),xnull(V),xlistp(V),xendp(V),
    xconsp(V),xeq(V),xeql(V),xequal(V),
    xcond(V),xcase(V),xand(V),xor(V),xlet(V),xletstar(V),xif(V),
    xprog(V),xprogstar(V),xprog1(V),xprog2(V),xprogn(V),xgo(V),xreturn(V),
    xcatch(V),xthrow(V),
    xerror(V),xcerror(V),xbreak(V),
    xcleanup(V),xtoplevel(V),xcontinue(V),xerrset(V),
    xbaktrace(V),xevalhook(V),
    xdo(V),xdostar(V),xdolist(V),xdotimes(V),
    xminusp(V),xzerop(V),xplusp(V),xevenp(V),xoddp(V),
    xfix(V),xfloat(V),
    xgcd(V),xadd(V),xsub(V),xmul(V),xdiv(V),xrem(V),xmin(V),xmax(V),xabs(V),
    xadd1(V),xsub1(V),xlogand(V),xlogior(V),xlogxor(V),xlognot(V),
    xsin(V),xcos(V),xtan(V),xexpt(V),xexp(V),xsqrt(V),xrand(V),
    xlss(V),xleq(V),xequ(V),xneq(V),xgeq(V),xgtr(V),
    xsubseq(V),xstring(V),xchar(V),
    xread(V),xprint(V),xprin1(V),xprinc(V),xterpri(V),
    xflatsize(V),xflatc(V),
    xopen(V),xclose(V),xrdchar(V),xpkchar(V),xwrchar(V),xreadline(V),
    xload(V),xtranscript(V),
    xtype(V),xexit(V),xpeek(V),xpoke(V),xaddrs(V),
    xvector(V),xblock(V),xrtnfrom(V),xtagbody(V),
    xpsetq(V),xflet(V),xlabels(V),xmacrolet(V),xunwindprotect(V),xpp(V),
    xstrlss(V),xstrleq(V),xstreql(V),xstrneq(V),xstrgeq(V),xstrgtr(V),
    xstrilss(V),xstrileq(V),xstrieql(V),xstrineq(V),xstrigeq(V),xstrigtr(V),
    xupcase(V),xdowncase(V),xnupcase(V),xndowncase(V),
    xtrim(V),xlefttrim(V),xrighttrim(V),
    xuppercasep(V),xlowercasep(V),xbothcasep(V),xdigitp(V),xalphanumericp(V),
    xcharcode(V),xcodechar(V),xchupcase(V),xchdowncase(V),xdigitchar(V),
    xchrlss(V),xchrleq(V),xchreql(V),xchrneq(V),xchrgeq(V),xchrgtr(V),
    xchrilss(V),xchrileq(V),xchrieql(V),xchrineq(V),xchrigeq(V),xchrigtr(V),
    xintegerp(V),xfloatp(V),xstringp(V),xarrayp(V),xstreamp(V),xobjectp(V),
    xwhen(V),xunless(V),xloop(V),
    xsymfunction(V),xfboundp(V),xsend(V),xsendsuper(V),
    xprogv(V),xrdbyte(V),xwrbyte(V),xformat(V),
    xcharp(V),xcharint(V),xintchar(V),
    xmkstrinput(V),xmkstroutput(V),xgetstroutput(V),xgetlstoutput(V),
    xgetlambda(V),xmacroexpand(V),x1macroexpand(V),
    xtrace(V),xuntrace(V),
    Prim_COPY_ARRAY(V),Prim_ARRAY_INSERT_POS(V),Prim_ARRAY_DELETE_POS(V); /* NPM */

#ifdef OBJPRNT
extern LVAL
    obprin1(V);
#endif

#ifdef STRUCTS
extern LVAL
    xdefstruct(V),xmkstruct(V),xcpystruct(V),xstrref(V),xstrset(V),
    xstrtypep(V);
#endif

#if defined(STRUCTS) | defined(COMPLX)
extern LVAL
    xasin(V),xacos(V),xatan(V);
#endif

#ifdef ADDEDTAA
extern LVAL
    xgeneric(V);
#endif

#ifdef COMMONLISPF
extern LVAL
    xnreverse(V),xbutlast(V),xcoerce(V),xconcatenate(V),xelt(V),xtypep(V),
    xliststar(V);

#ifdef SRCHFCN
  extern LVAL xsearch(V);
#endif
#ifdef POSFCNS
  extern LVAL xcountif(V), xfindif(V), xpositionif(V);
#endif
#ifdef REMDUPS
  extern LVAL xremove_duplicates(V);
#endif
#ifdef MAPFCNS
  extern LVAL xsome(V), xevery(V), xnotany(V), xnotevery(V), xmap(V);
#endif
#ifdef TIERNEY
  extern LVAL xreduce(V);
#endif

#else
extern LVAL
    xstrcat(V); /* exists in absence of COMMONLISPF */
#endif

#ifdef BETTERIO
extern LVAL
    xfileposition(V), xfilelength(V), xfreshline(V),
    xopenstreamp(V), xinputstreamp(V), xoutputstreamp(V);
#endif

/* functions specific to xldmem.c */
extern LVAL
    xgc(V),xexpand(V),xalloc(V),xmem(V);

#ifdef SAVERESTORE
extern LVAL
    xsave(V),xrestore(V);
#endif

#ifdef APPLYHOOK
extern LVAL
    xapplyhook(V);
#endif

#ifdef SETS
extern LVAL
    xadjoin(V), xunion(V), xintersection(V), xsubsetp(V),
    xset_difference(V);
#endif

#ifdef TIMES
extern LVAL
    xtime(V), xruntime(V), xrealtime(V);
#endif

#ifdef HASHFCNS
extern LVAL
    xgethash(V),xremhash(V),xmakehash(V),xclrhash(V),xmaphash(V),
    xhashcount(V);
#endif

#ifdef COMPLX
extern LVAL
    xcomplexp(V), xcomplex(V), xconjugate(V), xrealpart(V), ximagpart(V),
    xlog(V), xfloor(V), xceil(V), xround(V), xphase(V), xlcm(V);
#endif

#ifdef SPECIALS
extern LVAL
    xdefconstant(V), xconstantp(V), xdefparameter(V),
    xdefvar(V), xmakunbound(V);
#endif

#ifdef RANDOM
extern LVAL xmakerandom(V);
#endif
