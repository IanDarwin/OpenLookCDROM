system "nroff -man help.man >help.out";

open(input,"help.out");
open(output,">help");

sub do_blanks {
   foreach $item (@blanks) {
	print output "$SEP$item";
   }
   undef(@blanks);
}
while (<input>) {
    chop;
    if (/^!/) { 
        do do_blanks();	
	print output "$_\n" ; 
	$SEP = "";
    } elsif (/^\*/) { 
        do do_blanks();	
	print output "\n$_\\\n"; 
	$SEP = "";
    } elsif (/^$/) {
	push(@blanks, $_);
    } else {
        do do_blanks();	
	print output "$SEP$_";
	$SEP = "\\n\\\n";
    }
}
print output "\n";
