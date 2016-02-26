open(output,">tran_table.c");
print output "#include <stdio.h>\n";
print output "char *tran_table[] = {\n";
open(input, "dotxftp");
while (<input>) {
	chop;
	if (/^translation/) {
	    $tran = 1;
	} 
 	if ($tran) {
	    print output "\"",do fix($_),"\",\n";
	}
        if (/^end/) {
	    $tran = 0;
	}
}

sub fix {
    local($string) = @_[0];
    $string =~ s/\\/\\\\/g;
    $string;
}

print output "NULL,\n};\n";
