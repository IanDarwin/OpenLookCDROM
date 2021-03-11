# AWK script used by hbf-check.sh.
# <<< For HBF draft version 1.0	>>>
# [ Yongguang Zhang, Apr/25/93, Jun/06/93 ]

BEGIN  {
	may_redef["HBF_BYTE_2_RANGE"] = 1;
	may_redef["HBF_CODE_RANGE"] = 1;

	x11prop["FONTNAME_REGISTRY"] = "s";
	x11prop["ADD_STYLE_NAME"] = "s";
	x11prop["AVERAGE_WIDTH"] = "u";
	x11prop["AVG_CAPITAL_WIDTH"] = "i";
	x11prop["AVG_LOWERCASE_WIDTH"] = "i";
	x11prop["CAP_HEIGHT"] = "u";
	x11prop["CHARSET_ENCODING"] = "s";
	x11prop["CHARSET_REGISTRY"] = "s";
	x11prop["COPYRIGHT"] = "s";
	x11prop["DEFAULT_CHAR"] = "u";
	x11prop["DESTINATION"] = "u";
	x11prop["END_SPACE"] = "u";
	x11prop["FACE_NAME"] = "s";
	x11prop["FAMILY_NAME"] = "s";
	x11prop["FIGURE_WIDTH"] = "i";
	x11prop["FONT_ASCENT"] = "i";
	x11prop["FONT_DESCENT"] = "i";
	x11prop["FOUNDRY"] = "s";
	x11prop["FULL_NAME"] = "s";
	x11prop["ITALIC_ANGLE"] = "i";
	x11prop["MAX_SPACE"] = "u";
	x11prop["MIN_SPACE"] = "u";
	x11prop["NORM_SPACE"] = "u";
	x11prop["NOTICE"] = "s";
	x11prop["PIXEL_SIZE"] = "u";
	x11prop["POINT_SIZE"] = "u";
	x11prop["QUAD_WIDTH"] = "i";
	x11prop["RELATIVE_SETWIDTH"] = "u";
	x11prop["RELATIVE_WEIGHT"] = "u";
	x11prop["RESOLUTION"] = "u";
	x11prop["RESOLUTION_X"] = "u";
	x11prop["RESOLUTION_Y"] = "u";
	x11prop["SETWIDTH_NAME"] = "s";
	x11prop["SLANT"] = "s";
	x11prop["SMALL_CAP_SIZE"] = "u";
	x11prop["SPACING"] = "s";
	x11prop["STRIKEOUT_ASCENT"] = "i";
	x11prop["STRIKEOUT_DESCENT"] = "i";
	x11prop["SUBSCRIPT_SIZE"] = "u";
	x11prop["SUBSCRIPT_X"] = "i";
	x11prop["SUBSCRIPT_Y"] = "i";
	x11prop["SUPERSCRIPT_SIZE"] = "u";
	x11prop["SUPERSCRIPT_X"] = "i";
	x11prop["SUPERSCRIPT_Y"] = "i";
	x11prop["UNDERLINE_POSITION"] = "i";
	x11prop["UNDERLINE_THICKNESS"] = "u";
	x11prop["WEIGHT"] = "u";
	x11prop["WEIGHT_NAME"] = "s";
	x11prop["X_HEIGHT"] = "i";

	code_scheme["GB2312-1980"] = 1;
	code_scheme["Big5"] = 1;
	code_scheme["Unicode"] = 1;

	in_prop = 0
	expect  = "HBF_START_FONT"
	expect2 = ""
}

NF == 0	 {
	printf "** Warning: Line %d:  %s\n", NR, $0
	printf "   there should be no empty line, use 'COMMENT' if necessary\n"
	printf "--------\n"
	warning++
	next
}
$1 == "COMMENT"	 {
	next
}

{	
	# check duplicated declaration etc.
	if ((defined[$1] == 1) && (may_redef[$1] != 1)) {
	    printf "** Warning: Line %d:  %s\n", NR, $0
	    printf "   duplicated '%s' statement.\n", $1
	    printf "--------\n"
	    warning++
	}
	defined[$1] = 1
	# check expecting statements.
	if (expect != "") {
	    if (($1 != expect) && ($1 != expect2)) {
		printf "** Error: Line %d:  %s\n", NR, $0
		printf "   expecting '%s' at this line.\n", expect
		if (expect2 != "")
		    printf "   or, expecting '%s' at this line.\n", expect2
		printf "--------\n"
		exit 1
	    }
	}
}

$1 == "HBF_START_FONT"  {
	if (NF != 2) {
	    printf "** Error: Line %d:  %s\n", NR, $0
	    printf "   correct syntax:  'HBF_START_FONT' unquotedString\n"
	    printf "--------\n"
	    exit 1
	}
	expect = "HBF_CODE_SCHEME"
	next
}

$1 == "HBF_CODE_SCHEME"  {
	if (NF != 2 && NF != 4) {
	    printf "** Error: Line %d:  %s\n", NR, $0
	    printf "   correct syntax:  'HBF_CODE_SCHEME' unquotedString"
	    printf " [ unquotedString unquotedString ]\n"
	    printf "--------\n"
	    exit 1
	}
	expect = "FONT"
	next
}

$1 == "FONT"  {
	if (NF != 2) {
	    printf "** Error: Line %d:  %s\n", NR, $0
	    printf "   correct syntax:  'FONT' unquotedString\n"
	    printf "--------\n"
	    exit 1
	}
	expect = "HBF_BITMAP_BOUNDING_BOX"
	expect2 = "SIZE"
	next
}

$1 == "SIZE"  {
	if (NF != 4) {
	    printf "** Error: Line %d:  %s\n", NR, $0
	    printf "   correct syntax:  'SIZE' unsignedInteger"
	    printf " unsignedInteger unsignedInteger \n"
	    printf "--------\n"
	    exit 1
	}
	expect = "HBF_BITMAP_BOUNDING_BOX"
	expect2 = ""
	next
}

$1 == "HBF_BITMAP_BOUNDING_BOX"  {
	if (NF != 5) {
	    printf "** Error: Line %d:  %s\n", NR, $0
	    printf "   correct syntax:  'HBF_BITMAP_BOUNDING_BOX'"
	    printf " unsignedInteger unsignedInteger"
	    printf " signedInteger signedInteger\n"
	    printf "--------\n"
	    exit 1
	}
	expect = "FONTBOUNDINGBOX"
	expect2 = ""
	next
}

$1 == "FONTBOUNDINGBOX"  {
	if (NF != 5) {
	    printf "** Error: Line %d:  %s\n", NR, $0
	    printf "   correct syntax:  'FONTBOUNDINGBOX' unsignedInteger"
	    printf " unsignedInteger signedInteger signedInteger\n"
	    printf "--------\n"
	    exit 1
	}
	expect = "STARTPROPERTIES"
	next
}

$1 == "STARTPROPERTIES"  {
	if (NF != 2) {
	    printf "** Error: Line %d:  %s\n", NR, $0
	    printf "   correct syntax:  'STARTPROPERTIES' unsignedInteger\n"
	    printf "--------\n"
	    exit 1
	}
	expect = ""			# cannot expect any
	num_prop = $2
	num_found = 0
	line_startprop = NR
	in_prop = 1
	prop_err = 0
	next
}

in_prop == 1 && $1 == "ENDPROPERTIES"  {
	if (NF != 1) {
	    printf "** Error: Line %d:  %s\n", NR, $0
	    printf "   correct syntax:  'ENDPROPERTIES'\n"
	    printf "--------\n"
	    exit 1
	}
	if (num_prop > num_found) {
	    printf "** Error:  too fewer property definitions.\n"
	    printf "   %d declared in 'STARTPROPERTIES' statement", num_prop
	    printf " (line %d)\n", line_startprop
	    printf "   but only %d are found\n", num_found
	    printf "--------\n"
	    exit 1
	}
	if (num_prop < num_found) {
	    printf "** Error:  too many property definitions.\n"
	    printf "   %d declared in 'STARTPROPERTIES' statement", num_prop
	    printf " (line %d)\n", line_startprop
	    printf "   but %d are found\n", num_found
	    printf "--------\n"
	    exit 1
	}
	# check compulsory definitions
	if (defined["DEFAULT_CHAR"] != 1) {
	    printf "** Error:  compulsory property 'DEFAULT_CHAR' is missing\n"
	    printf "--------\n"
	    exit 1
	}
	if (prop_err > 0) {
	    printf "** Total %d error(s) have been found", prop_err
	    printf " in the PROPERTIES section.\n"
	    printf "   Although the format of a property is not compulsory,\n"
	    printf "   it is better to follow the convention.\n"
	    printf "--------\n"
	}
	expect = "CHARS"
	in_prop = 0
	next
}

in_prop == 1 && $1 == "DEFAULT_CHAR"  {
	if (NF != 2) {
	    printf "** Error: Line %d:  %s\n", NR, $0
	    printf "   correct syntax:  'DEFAULT_CHAR' unsignedInteger\n"
	    printf "--------\n"
	    exit 1
	}
	num_found++;
	next
}

in_prop == 1  {
	if (x11prop[$1] == "") {
	    printf "** Warning: Line %d:  %s\n", NR, $0
	    printf "   unknown property.\n"
	    printf "--------\n"
	    prop_err++
	} else if (x11prop[$1] == "i") {
	    if (NF != 2) {
		printf "** Error: Line %d:  %s\n", NR, $0
		printf "   correct syntax:  '%s' integer\n", $1
		printf "--------\n"
		prop_err++
	    }
	} else if (x11prop[$1] == "u") {
	    if (NF != 2) {
		printf "** Error: Line %d:  %s\n", NR, $0
		printf "   correct syntax:  '%s' unsignedInteger\n", $1
		printf "--------\n"
		prop_err++
	    }
	} else if (x11prop[$1] == "s") {
	    if (NF < 2) {
		printf "** Error: Line %d:  %s\n", NR, $0
		printf "   correct syntax:  '%s' quotedString\n", $1
		printf "--------\n"
		prop_err++
	    }
	}
	num_found++;
	next
}

$1 == "CHARS"  {
	if (NF != 2) {
	    printf "** Error: Line %d:  %s\n", NR, $0
	    printf "   correct syntax:  'CHARS' unsignedInteger\n"
	    printf "--------\n"
	    exit 1
	}
	expect = "HBF_START_BYTE_2_RANGES"
	next
}

$1 == "HBF_START_BYTE_2_RANGES"  {
	if (NF != 2) {
	    printf "** Error: Line %d:  %s\n", NR, $0
	    printf "   correct syntax:  'HBF_START_BYTE_2_RANGES'"
	    printf " unsignedInteger\n"
	    printf "--------\n"
	    exit 1
	}
	expect = "HBF_BYTE_2_RANGE"
	num_range = $2
	num_found = 0
	next
}

$1 == "HBF_BYTE_2_RANGE"  {
	if ((NF > 4) || ((NF == 4) && ($3 != "-"))) {
	    printf "** Error: Line %d:  %s\n", NR, $0
	    printf "   correct syntax:  'HBF_BYTE_2_RANGE'"
	    printf " unsignedInteger '-' unsignedInteger\n"
	    printf "--------\n"
	    exit 1
	}
	if ((NF == 3) && (substr($3,1,1) != "-") &&	\
		(substr($2,length($2),1) != "-")) {
	    printf "** Error: Line %d:  %s\n", NR, $0
	    printf "   correct syntax:  'HBF_BYTE_2_RANGE'"
	    printf " unsignedInteger '-' unsignedInteger\n"
	    printf "--------\n"
	    exit 1
	}
	    
	num_found++;
	if (num_found == num_range)
		expect = "HBF_END_BYTE_2_RANGES";
	else
		expect = "HBF_BYTE_2_RANGE";
	next
}

$1 == "HBF_END_BYTE_2_RANGES"  {
	if (NF != 1) {
	    printf "** Error: Line %d:  %s\n", NR, $0
	    printf "   correct syntax:  'HBF_END_BYTE_2_RANGES'\n"
	    printf "--------\n"
	    exit 1
	}
	expect = "HBF_START_CODE_RANGES"
	next
}

$1 == "HBF_START_CODE_RANGES"  {
	if (NF != 2) {
	    printf "** Error: Line %d:  %s\n", NR, $0
	    printf "   correct syntax:  'HBF_START_CODE_RANGES'"
	    printf " unsignedInteger\n"
	    printf "--------\n"
	    exit 1
	}
	expect = "HBF_CODE_RANGE"
	num_range = $2
	num_found = 0
	next
}

$1 == "HBF_CODE_RANGE"  {
	if ((NF > 6) || ((NF == 6) && ($3 != "-"))) {
	    printf "** Error: Line %d:  %s\n", NR, $0
	    printf "   correct syntax:  'HBF_CODE_RANGE'"
	    printf " unsignedInteger '-' unsignedInteger\n"
	    printf "--------\n"
	    exit 1
	}
	if ((NF == 5) && (substr($3,1,1) != "-") &&	\
		(substr($2,length($2),1) != "-")) {
	    printf "** Error: Line %d:  %s\n", NR, $0
	    printf "   correct syntax:  'HBF_CODE_RANGE'"
	    printf " unsignedInteger '-' unsignedInteger\n"
	    printf "--------\n"
	    exit 1
	}
	num_found++;
	if (num_found == num_range)
		expect = "HBF_END_CODE_RANGES";
	else
		expect = "HBF_CODE_RANGE";
	next
}

$1 == "HBF_END_CODE_RANGES"  {
	if (NF != 1) {
	    printf "** Error: Line %d:  %s\n", NR, $0
	    printf "   correct syntax:  'HBF_END_CODE_RANGES'\n"
	    printf "--------\n"
	    exit 1
	}
	expect = "HBF_END_FONT"
	next
}

$1 == "HBF_END_FONT"  {
	expect = "end-of-file"
	next
}

{
	printf "** Error: Line %d:  %s\n", NR, $0
	printf "   Unknown statement.\n"
	printf "--------\n"
	exit 1
}

END  {
	if ( expect != "end-of-file" ) {
	    printf "** Unexpected EOF (end of file)\n"
	    if ( expect != "" ) {
		printf "   still expecting '%s'", expect
		if (expect2 != "")  printf " or '%s'", expect2;
		printf "\n"
	    }
	    printf "--------\n"
	    exit 1
	}
}

