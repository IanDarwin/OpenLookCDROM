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


# This awk program will covert the output from the AIX "nm -ex" command to
# a form which resembles the output from the 4.3 "nm" command.  It is currently
# only used on AIX systems.

$1 == "Symbols" {
	split($3, NameArray,"[");
	split(NameArray[2], FileName, "]");
	ElementName = NameArray[1] ":" FileName[1] FileName[2];
};
!($1 == "" || $1 == "Symbols" || $1 == "Name" || NF == 0) { 
	if ($5 == "|.data") Type = "D";
	else {
		if ($5 == "|.text") Type= "T";
		else {
			if ($5 == "|.bss") Type= "B";
			else {
				if ($5 == "|") Type = "U";
				else {
					Type = "U";
					print "Unknown type:", $5, "."|"cat>& 2";
				}
			}
		}
	}
	Value = substr($2, 4, 8);
	print ElementName Value, Type, $1	;
};

