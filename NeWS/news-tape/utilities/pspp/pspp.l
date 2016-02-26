
%{

/*

                    Copyright (c) 1989, Josh Siegel

You may copy the pspp kit in whole or in part as long as you don't try to
make money off it, or pretend that you wrote it.

 Version 0.04
    Fixed a fix in 0.03
    Made it callable on files and sets of files.

 Version 0.03
    Fixed run away white space problem...
    Fixed (()) matching
    Fixed it turning (\(\)) into (\\(\\))

 Version 0.02
    Added bracstack as well as fixing problems associated with class.ps

 Version 0.01
    initial release
*/

#define FLAG_BA        0x1
#define FLAG_BB        0x2
#define FLAG_BS        0x4
#define FLAG_CBA    0x8
#define FLAG_CBB    0x10
#define FLAG_CEA    0x20
#define FLAG_CEB    0x40
#define FLAG_DA        0x80
#define FLAG_DB        0x100
#define FLAG_DBA    0x200
#define FLAG_DBB    0x400
#define FLAG_DEA    0x800
#define FLAG_DEB    0x1000
#define FLAG_EA        0x2000
#define FLAG_EB        0x4000
#define FLAG_LCA    0x8000
#define FLAG_LCB    0x10000
#define FLAG_RCA    0x20000
#define FLAG_RCB    0x40000
#define FLAG_PN        0x80000
#define FLAG_IB        0x100000
#define FLAG_IA        0x200000
#define FLAG_LBB    0x400000
#define FLAG_LBA    0x800000
#define FLAG_RBB    0x1000000
#define FLAG_RBA    0x2000000
#define FLAG_GSB    0x4000000
#define FLAG_GSA    0x8000000
#define FLAG_GRB    0x10000000
#define FLAG_GRA    0x20000000
#define FLAG_GI        0x40000000
#define FLAG_RA     0x80000000

#define DEFAULT_FLAG (FLAG_BA|FLAG_BB|FLAG_BS|FLAG_CBA|FLAG_CBB|\
        FLAG_CEA|FLAG_CEB|FLAG_DA|FLAG_DBA|FLAG_DBB|FLAG_DEA|\
        FLAG_DEB|FLAG_EA|FLAG_EB|FLAG_LCA|FLAG_RCB| \
        FLAG_PN | FLAG_IA | FLAG_GI)

#define check_flag(x) ((x) & flags)

#define STACK_LEVELS 32

int neednew,i,level,braclevel,flags,minlevel;
int minstack[STACK_LEVELS],flagstack[STACK_LEVELS],minstackpnt,flagstackpnt;
int bracstack[STACK_LEVELS],bracpnt;
%}
W    [ \t]+
%%
\(        {
            int pcount;

            pcount=1;
            i = 1;

            while(pcount) {
                yytext[i]=input();
                switch(yytext[i]) {
                    case '\\':
                        i++;
                        yytext[i]=input();
                        break;
                    case '(':
                        pcount++;
                        if(check_flag(FLAG_BS)) {
                            yytext[i++]='\\';
                            yytext[i]='(';
                        }
                        break;
                    case ')':
                        pcount--;
                        if(pcount) {
                            if(check_flag(FLAG_BS)) {
                                yytext[i++]='\\';
                                yytext[i]=')';
                            }
                        }
                        break;
                    case '\n':
                        yytext[i++]='\\';
                        yytext[i]='n';
                        break;
                    default: break;
                }
                i++;
            }
            yytext[i]= '\0';
            newline();
            fprintf(yyout,"%s",yytext);
        }

\{[ \t]*\}    { /* Yet another special case */ newline(); fprintf(yyout,yytext); }

"def"    {
                if(check_flag(FLAG_DB))
                    neednew=1;
                newline();
                fprintf(yyout,"%s",yytext);
                if(check_flag(FLAG_DA))
                    neednew=1;
        }

"{"    {
        if(check_flag(FLAG_LCB))
            neednew=1;
        newline();
        level++;
        pushbraclevel();
        fprintf(yyout,"%s",yytext);
        if(check_flag(FLAG_LCA))
            neednew=1;
    }

"}"    {
    if(check_flag(FLAG_RCB))
        neednew=1;
    level--;
    popbraclevel();
    newline();
    fprintf(yyout,"%s",yytext);
    if(check_flag(FLAG_RCA))
        neednew=1;
    }

"begin"|"dictbegin"|"classbegin"|"["|"gsave"    {
                switch(yytext[0]) {
                case 'd':
                    if(check_flag(FLAG_DBB))
                        neednew=1;
                    break;
                case 'b':
                    if(check_flag(FLAG_BB))
                        neednew=1;
                    break;
                case 'c':
                    if(check_flag(FLAG_CBB))
                        neednew=1;
                    break;
                case 'g':
                    if(check_flag(FLAG_GSB))
                        neednew=1;
                    break;
                case '[':
                    if(check_flag(FLAG_LBB))
                        neednew=1;
                    break;
                }
                newline();

                fprintf(yyout,"%s",yytext);
                braclevel++;
                switch(yytext[0]) {
                case 'd':
                    if(check_flag(FLAG_DBA))
                        neednew=1;
                    break;
                case 'b':
                    if(check_flag(FLAG_BA))
                        neednew=1;
                    break;
                case 'c':
                    if(check_flag(FLAG_CBA))
                        neednew=1;
                    break;
                case 'g':
                    if(check_flag(FLAG_GSA))
                        neednew=1;
                    if(!check_flag(FLAG_GI))
                        braclevel--;
                    break;
                case '[':
                    if(check_flag(FLAG_LBA))
                        neednew=1;
                    break;
                }
            }


"end"|"dictend"|"classend"|"]"|"grestore"     {
                braclevel--;
                switch(yytext[0]) {
                case 'd':
                    if(check_flag(FLAG_DEB))
                        neednew=1;
                    break;
                case 'e':
                    if(check_flag(FLAG_EB))
                        neednew=1;
                    break;
                case 'c':
                    if(check_flag(FLAG_CEB))
                        neednew=1;
                    break;
                case 'g':
                    if(check_flag(FLAG_GRB))
                        neednew=1;
                    if(!check_flag(FLAG_GI))
                        braclevel++;
                    break;
                case ']':
                    if(check_flag(FLAG_RBB))
                        neednew=1;
                    break;
                }

                newline();
                fprintf(yyout,"%s",yytext);
                switch(yytext[0]) {
                case 'd':
                    if(check_flag(FLAG_DEA))
                        neednew=1;
                    break;
                case 'e':
                    if(check_flag(FLAG_EA))
                        neednew=1;
                    break;
                case 'c':
                    if(check_flag(FLAG_CEA))
                        neednew=1;
                    break;
                case 'g':
                    if(check_flag(FLAG_GRA))
                        neednew=1;
                    break;
                case ']':
                    if(check_flag(FLAG_RBA))
                        neednew=1;
                    break;
                }
            }

(if|ifelse)            {

                if(check_flag(FLAG_IB))
                    neednew=1;
                newline();
                fprintf(yyout,"%s",yytext);
                if(check_flag(FLAG_IA))
                    neednew=1;
                }
^cdef    {
            level=0;
            minlevel=0;
            newline();
            fprintf(yyout,"%s",yytext);
            level=1; /* Indent one so it looks ncie */
            braclevel=0; /* Reset the bracket level */
            bracpnt=0;  /* Reset the stack */
            minlevel=1;
        }
^\%\%=.*\n    {
                if(neednew)
                    fprintf(yyout,"\n");
                yytext[yyleng-1]='\0';
                fprintf(yyout,"%s",yytext);
                parseflag(&yytext[3]);
                neednew=1;
            }
^#.*\n    |
^\%.*\n    {
            if(neednew)
                fprintf(yyout,"\n");
            yytext[yyleng-1]='\0';
            fprintf(yyout,"%s",yytext);
            neednew=1;
        }
^[ \t]+\%.*\n {
                char *p;

                newline();
                yytext[yyleng-1]='\0';
                p = yytext;
                while(*p!='%') p++;
                fprintf(yyout,"%s",p);
                neednew=1;
                }
\%.*\n    {
            yytext[yyleng-1]='\0';
            fprintf(yyout,"%s",yytext);
            neednew=1;
        }
^{W}       neednew=1;
{W}       fprintf(yyout," ");
^[ \t]*\n    { if(check_flag(FLAG_PN)) fprintf(yyout,"\n");}
\n        neednew=1;
[^ \t\n\[\]\{\}\(]+ |
.        {newline();
            fprintf(yyout,"%s",yytext);
            ; /* Almost everything falls to this */ }
%%



main(argc, argv)
    int             argc;
    char           *argv[];
{

    FILE           *fp, *fopen(), *fpo;
    char            buff[255];
    int do_stdio;

    do_stdio = 1;
    minstackpnt = 0;
    flagstackpnt = 0;
    bracpnt = 0;
    neednew = 0;
    level = 0;
    braclevel = 0;
    minlevel = 0;
    flags = DEFAULT_FLAG;

    sprintf(buff, "%s/.pspp", getenv("HOME"));

    fp = fopen(buff, "r");

    if(fp!=NULL) {
        while(fgets(buff,255,fp)!=NULL)
            parseflag(buff);
        fclose(fp);
    }

    while (--argc) {
        if( argv[argc][0]=='-' || argv[argc][0]=='+' )
            parseflag(argv[argc]);
        else
            {
            do_stdio = 0;

            sprintf(buff,"%s.BAK",argv[argc]);
            unlink(buff);
            if(rename(argv[argc],buff)!=0) {
                perror("rename");
                exit(0);
            }
            fpo = fopen(argv[argc],"w");
            fp = fopen(buff,"r");
            yyin = fp;
            yyout = fpo;
            yylex();
            fprintf(fpo,"\n");
            fclose(fp);
            fclose(fpo);
        }
    }

    if(do_stdio) {
        yylex();
        fprintf(yyout,"\n");
    }
    exit(0);
}
newline()
{
    int             cnt;

    if (!neednew)
        return;

    fprintf(yyout,"\n");

    if (level < minlevel)        /* Save ourselves from errors in the
                                postscript */
        level = minlevel;

    if(bracpnt>0) {
        if(braclevel < bracstack[bracpnt-1])
                braclevel = bracstack[bracpnt-1];
    }  else {
        if(braclevel<0)
            braclevel = 0;
    }

    cnt = level + braclevel;

    while (cnt--)
        fprintf(yyout,"    ");

    neednew = 0;
}
parseflag(str)
    char           *str;
{
    char           *p;
    int             effect, the_flag;


    p = str;


    while (*p) {
        while (*p == ' ' || *p == '\t')
            p++;

        effect = 1;    /* Set flag (default) */
        the_flag = 0;

        switch (*p) {
        case '+':
            p++;
            break;
        case '-':
            effect = 0;
            p++;
            break;
        default:
            break;
        }

        /*
         * I make no defense of the code below... later I will make a
         * proper hash table (yes.. yes.. I know there are lots of
         * incorrect sets )
         */

        if(effect<2)
        switch (p[0]) {
        case 'b':
            switch (p[1]) {
            case 'a':
                the_flag = FLAG_BA;
                break;
            case 'b':
                the_flag = FLAG_BB;
                break;
            case 's':
                the_flag = FLAG_BS;
                break;
            default:
                break;
            }
            break;
        case 'c':
            if (p[1] == 'b')
                if (p[2] == 'a')
                    the_flag = FLAG_CBA;
                else
                    the_flag = FLAG_CBB;
            else if (p[2] == 'a')
                the_flag = FLAG_CEA;
            else
                the_flag = FLAG_CEB;
            break;
        case 'd':
            switch (p[1]) {
            case 'a':
                the_flag = FLAG_DA;
                break;
            case 'b':
                switch (p[2]) {
                case 'a':
                    the_flag = FLAG_DBA;
                    break;
                case 'b':
                    the_flag = FLAG_DBB;
                    break;
                default:
                    the_flag = FLAG_DB;
                    break;
                }
                break;
            case 'e':
                if (p[2] == 'a')
                    the_flag = FLAG_DEA;
                else
                    the_flag = FLAG_DEB;
                break;
            default:
                break;
            }
            break;
        case 'i':
            if (p[1] == 'a')
                the_flag = FLAG_IA;
            else
                the_flag = FLAG_IB;
            break;
        case 'e':
            if (p[1] == 'a')
                the_flag = FLAG_EA;
            else
                the_flag = FLAG_EB;
            break;
        case 'l':
            if (p[1] == 'c')
                if (p[2] == 'a')
                    the_flag = FLAG_LCA;
                else
                    the_flag = FLAG_LCB;
            else
                if (p[2] == 'a')
                    the_flag = FLAG_LBA;
                else
                    the_flag = FLAG_LBB;
            break;
        case 'g':
            switch(p[1]) {
                case 'i':
                    the_flag = FLAG_GI;
                    break;
                case 's':
                    if(p[2]=='b')
                        the_flag = FLAG_GSB;
                    else
                        the_flag = FLAG_GSA;
                    break;
                case 'r':
                    if(p[2]=='b')
                        the_flag = FLAG_GRB;
                    else
                        the_flag = FLAG_GRA;
                    break;
            }
            break;
        case 'r':
            switch(p[1]) {
                case 'c':
                    if (p[2] == 'a')
                        the_flag = FLAG_RCA;
                    else
                        the_flag = FLAG_RCB;
                    break;
                case 'b':
                    if (p[2] == 'a')
                        the_flag = FLAG_RBA;
                    else
                        the_flag = FLAG_RBB;
                    break;
                case 'a':
                    the_flag = FLAG_RA;
                    break;
                default:
                    break;
            }
            break;
        case 'p':
            the_flag = FLAG_PN;
            break;
        default:
            break;
        }

        if (the_flag) {
            if (effect)
                flags |= the_flag;
            else
                flags &= ~the_flag;
        }
        p++;

        while (*p >= 'a' && *p <= 'z')
            p++;
    }
}
pushflag()
{
    flagstack[flagstackpnt]=flags;

    if(flagstackpnt<STACK_LEVELS)
        flagstackpnt++;
}
popflag()
{
    if(flagstackpnt>0)
        flagstackpnt--;
    else
        return;

    flags=flagstack[flagstackpnt];
}
pushmin()
{
    minstack[minstackpnt]=minlevel;

    if(minstackpnt<STACK_LEVELS)
        minstackpnt++;
}
popmin()
{
    if(minstackpnt>0)
        minstackpnt--;
    else
        return;

    minlevel=minstack[minstackpnt];
}
pushbraclevel()
{
    bracstack[bracpnt]=braclevel;

    if(bracpnt<STACK_LEVELS)
        bracpnt++;
}
popbraclevel()
{
    if(bracpnt>0)
        bracpnt--;
    else
        return;

    braclevel=bracstack[bracpnt];
}
