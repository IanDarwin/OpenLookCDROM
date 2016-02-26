#! /local/bin/perl

open(input, "$ARGV[0]");
open(output, ">temp");
while (<input>) {
	if (/^#/) { print output "$last$_"; undef($last); next; }
	if (/XmStringCreateLtoR/) { print output "$last$_"; undef($last); next; }
	if (/(\S\S*)\((.*)\)\s*$/ || 
	    /^(CreateSimpleDialog)/) {
	     $name = $1;
	     if ($last =~ /^\s*$/) {undef($last); }
	     if ($last =~ /^\s*(.*)\s\s*$/) {
		$last = $1;
	     }
	     if (!$last) { 
			   print "fix $name\n";
	     }
	     $func{$name} = "$last\n$_";
	     while (<input>) {
	          $func{$name} = $func{$name} . "$_";
		  if ($_ =~ /^}/) { last; }
	     }
	     undef($last);
	     $_ = <input>;
	     if ($_ =~ /^\s*$/) { next; }
	}
	if ($last) { print output "$last";}
	$last = $_;
}

@f = sort keys%func;

$first = 1;
foreach $item (@f) {
	if (!$first) {
            print output  "\n";
	} else {
	    $first = 0;
	}
	print output "$func{$item}";
}
close(output);

$XX = $ARGV[0] . ".uns";
system "mv $ARGV[0] $XX";
system "mv temp $ARGV[0]";

