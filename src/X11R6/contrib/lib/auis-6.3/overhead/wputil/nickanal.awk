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

BEGIN	{{OFS = ""; ORS = "\n";
	TransCount = 0;
	}}
{
	if (HaveMap[$1 ";" $2] == 0) {
		Mapping[$1] = Mapping[$1] " " $2;
		HaveMap[$1 ";" $2] = 1;
	}
	if (HaveNick[$1 ";" $3] == 0) {
		RealNick[$1] = RealNick[$1] "/" $3;
		HaveNick[$1 ";" $3] = 1;
	}
	if (HaveGiven[$2 ";" $4] == 0) {
		RealGiven[$2] = RealGiven[$2] "/" $4;
		HaveGiven[$2 ";" $4] = 1;
	}
}
END	{
	for (z in Mapping) {
		Nick = z " (" substr(RealNick[z], 2, length(RealNick[z]) - 1) ")	";
		GivStr = substr(Mapping[z], 2, length(Mapping[z]) - 1);
		n = split(GivStr, Givs, " ");
		for (i = 1; i <= n; ++i) {
		    Nick = Nick " " Givs[i] " (";
		    Nick = Nick substr(RealGiven[Givs[i]], 2, length(RealGiven[Givs[i]]) - 1) ")";
		}
		print Nick;
	}
	}
