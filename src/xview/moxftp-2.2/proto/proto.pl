open(sout,"|sort -o stemp");
if ($ARGV[0] =~ /(.*).h$/) {
   $SOURCE = "$1.c";
}
open(global, ">>global.h");
while (<STDIN>) {
	if (/^#/) { undef($last); next; }
	if (/\s\s*while/) { undef($last); next; }
	if (/XmStringCreateLtoR/) { undef($last); next; }
	#
	# Gross  hack 
	#
	if ($_ =~ /^CreateSimpleDialog/) {
	    chop($x = $_);
	    while (<STDIN>) {
		chop($x = $x . $_);
		if ($_ =~ /[)]$/) { last; }
	    }
	    $_ = $x;
	}
	if (/(\S\S*)\((.*)\)\s*$/) {
	     $name = $1;
	     $t1 = " $2";
	     @arg = split(/\s\s*|\,\s\s*/, $t1);
	     foreach $arg (@arg) {
		$_ =  <STDIN>;
		if ($_ =~ /^(..*)\s\s*(\S\S*);/) {
		     $X = $1;
		     $Y = $2;
		     if ($X =~ /^\s*(.*\S)\s\s*$/) {
			$X = $1;
		     }
		     if ($X =~ /,/) { 
			   print "fix $name\n";
		     }
		     if ($ARG) {
		         $ARG = $ARG . ", " .  $X;
		     } else {
		         $ARG =  $X;
		     }
		     if ($Y =~ /\s*\*\s*\(\*\S\S*\)\(\)/) {
		    	$ARG = $ARG . "*(*)()";;
		     } elsif ($Y =~ /\(\*\S\S*\)\(\)/) {
		    	$ARG = $ARG . "(*)()";;
		     } elsif ($Y =~ /(\*\**)/) {
		    	$ARG = $ARG . $1;
		     }
		}
		
	     }
	     if ($last =~ /^\s*$/) {undef($last); }
	     if ($last =~ /^\s*(.*)\s\s*$/) {
		$last = $1;
	     }
	     if ($last) { $last = $last . " ";}
	     if (!$last) { 
			   print "fix $name\n";
	     }
	     print sout "$name:$last$name P_(($ARG));\n";
	     undef($ARG);
	     undef($last);
	     while (<STDIN>) {
		if ($_ =~ /^}/) { last; }
	     }
	     next;
	}
	chop($last = $_);

}
close(sout);

open(input, "stemp");
$first = 1;
$firsto = 1;
while (<input>) {
	@list = split(/:/);
	if ($list[1] =~ /static/) {
	    if ($firsto) {
                open(output, ">$ARGV[0]");
		$firsto = 0;
	    }
	    print output "$list[1]";
	} else {
	    if ($first) {
		if ($SOURCE) {
	    	printf global "/*\n * Globals from $SOURCE\n */\n";
		}
		$first = 0;
		if ($ARGV[1]) {
		    print global "#if defined($ARGV[1])\n";
		}
	    }
	    print global "$list[1]";
	}
}

if ($ARGV[1]) {
	print global "#endif /* $ARGV[1] */\n";
}

