BEGIN {
    printf("#include <stdio.h>\n");
    printf("static char *systypes[]={\n");
}

{
    printf("\t%s,\n",$3);
}

END {
    printf("\tNULL\n");
    printf("};\n");
    printf("char **get_syslist()\n{\n\treturn systypes;\n}\n");
}

