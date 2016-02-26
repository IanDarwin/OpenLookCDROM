open(output, ">fb_rs.c");
print output "#include \"defs.h\"\n\n";
do make("MOTIF",        "Mftp.ad");
do make("XAW"  ,        "Xftp.ad");
do make("OPENWINDOW"  , "Oftp.ad");

sub fix {
    local($string) = @_[0];
    if ( $string =~ /\\n\\$/) {
       $string = substr($string, 0, length($string) - 3) . "\\\\n\\";
    }

    if ($string =~ /\\ /) {
        $string =~ s/\\ /\\\\ /g;
    }
    if ($string =~ /\"/) {
        $string =~ s/\"/\\\"/g;
    }
    $string;
}

sub make {
    local($type, $file) = @_;
    print output "#if defined($type)\n";
    open(input, $file);

    print output "String fallback_resources[] = {\n";
    while (<input>) {
	chop;
	if (/^;/) { next; }
	if (/(\S\S*:)(.*)/) {
	    $p1 = $1;
	    undef(@p2);
	    if ($2) {
	        $p2 = do fix($2);
 	    }
	    if ($2) { push(@p2, $p2); }
	    if (/\\$/) {
	    	while(<input>) {
		    chop;
		    $p2 = do fix($_);
		    push(@p2, $p2);
		    if (!/\\$/) { last;}
		}
	    }
	    if (@p2 == 0 ) { next; }
	    print output "\"$p1";
	    $item = shift(@p2);
	    print output "$item";
	    foreach $item (@p2) {
	    	print output "\n$item";
	    }
	    print output "\\n\",\n";
	}
    }
    print output "NULL,\n};\n";
    print output "#endif\n";
}
