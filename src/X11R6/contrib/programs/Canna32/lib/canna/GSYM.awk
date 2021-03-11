# Copyright 1994 NEC Corporation, Tokyo, Japan.
#
# Permission to use, copy, modify, distribute and sell this software
# and its documentation for any purpose is hereby granted without
# fee, provided that the above copyright notice appear in all copies
# and that both that copyright notice and this permission notice
# appear in supporting documentation, and that the name of NEC
# Corporation not be used in advertising or publicity pertaining to
# distribution of the software without specific, written prior
# permission.  NEC Corporation makes no representations about the
# suitability of this software for any purpose.  It is provided "as
# is" without express or implied warranty.
#
# NEC CORPORATION DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE,
# INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN 
# NO EVENT SHALL NEC CORPORATION BE LIABLE FOR ANY SPECIAL, INDIRECT OR
# CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF 
# USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR 
# OTHER TORTUOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR 
# PERFORMANCE OF THIS SOFTWARE. 
#
# $Id: GSYM.awk,v 1.5 1994/01/27 11:10:34 kuma Exp $
#
BEGIN {
  nsym = 0
}
/^[_a-zA-Z]/ {
  nsym++
  printf "#define %-35s G%03d_%s\n", $1, nsym, $1
}
