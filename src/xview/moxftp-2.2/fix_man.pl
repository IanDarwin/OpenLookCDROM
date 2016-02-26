#! /local/bin/perl

do it("Xftp", "xftp", "Athena", "XFTP");
do it("Mftp", "mftp", "Motif" , "MFTP");
do it("Oftp", "oftp", "Open Look", "OFTP");

sub it {
    ($Xxxx, $xxxx, $type, $keep) = @_;

    open(input,"moxftp.man");
    open(output, ">$xxxx.man");
    $s = 0;
    while (<input>) {
	if (/^KEEP/) {
	     if(/$keep/) { next; }
	     if(!$s) {
		$s++;
	     } else {
		$s = 0;
	     }
	     next;
	}
	if ($s) { next; }
	s/xxxx/$xxxx/g;
        s/Xxxx/$Xxxx/g;
        s/TYPE/$type/g;
	print output $_;
    }
}
	     
	

	
	



