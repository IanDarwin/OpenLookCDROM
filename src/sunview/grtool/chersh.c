/*
	hershey fonts

	$Header: chersh.c,v 1.5 89/09/02 10:08:20 pturner Locked $
*/

#include <stdio.h>
#include <math.h>
#include <ctype.h>
#include "hersh.h"		/* character defs */
#include "special.h"		/* character defs */
#include "symdef.h"		/* symbols */

#define TRUE 1
#define FALSE 0

static struct {
    unsigned char *h_tables;
    int *h_indices;
} hershey_fonts[12] = {

    Romanc_table, Romanc_indices,
    Romand_table, Romand_indices,
    Romans_table, Romans_indices,
    Romant_table, Romant_indices,
    Italicc_table, Italicc_indices,
    Italict_table, Italict_indices,
    Scriptc_table, Scriptc_indices,
    Scripts_table, Scripts_indices,
    Greekc_table, Greekc_indices,
    Greeks_table, Greeks_indices,
    Special_table, Special_indices,
    0, 0
};

static unsigned char *chartable = Romanc_table;
static int *indices = Romanc_indices;

static int curfont;

/*
	select a font to use below
*/
hselectfont(f)
    int f;
{
    chartable = hershey_fonts[f].h_tables;
    indices = hershey_fonts[f].h_indices;
    curfont = f;
}

/*
	write s at xpos, ypos in device coordinates
	of size scale, direction dir, color color, using vector
*/
puthersh(xpos, ypos, scale, dir, color, vector, s)
    int xpos, ypos, color;
    int dir;
    double scale;
    char *s;
    int (*vector) ();		/* device draw line */

{
    int i, j, len = 0, ind, it1, it2, sfont = curfont, slen = strlen(s);
    int sscript = 0, subscript = 0, underline = 0;
    unsigned char charx, chary;
    double charw, x, y, xtmp, ytmp, saves = scale, slastx = 0.0, slasty = 0.0;
    double si = sin(M_PI / 180.0 * dir);
    double co = cos(M_PI / 180.0 * dir);

    for (i = 0; i < slen; i++) {
	if (s[i] == '\\' && isdigit(s[i + 1])) {
	    hselectfont(s[i + 1] - '0');
	    i++;
	    goto branch;
	} else if (s[i] == '\\' && (isalpha(s[i + 1])||s[i+1]=='+'||s[i+1]=='-')) {
	    switch (s[i + 1]) {
	    case 'x':
		hselectfont(10);
		i++;
		break;
	    case 's':
		scale = 0.6 * saves;
		sscript += 20;
		i++;
		break;
	    case 'S':
		scale = 0.6 * saves;
		sscript += -20;
		i++;
		break;
	    case 'N':
		scale = saves;
		sscript = 0;
		i++;
		break;
	    case 'b':
		xpos = xpos - slastx;
		ypos = ypos - slasty;
		i++;
	    case 'u':
		underline = 1;
		i++;
		break;
	    case 'U':
		underline = 0;
		i++;
		break;
	    case '-':
		scale -= 0.2;
		i++;
		break;
	    case '+':
		scale += 0.2;
		i++;
		break;
	    }
	    goto branch;
	}
	ind = s[i] - ' ';
	len = indices[ind + 1] - indices[ind];

	it1 = chartable[2 * indices[ind]];
	it2 = chartable[2 * indices[ind] + 1];
	x = (it1 - 'R');
	y = (it2 - 'R');
	charw = y - x;
	for (j = 1; j < len; j++) {
	    charx = chartable[2 * indices[ind] + 2 * j];
	    chary = chartable[2 * indices[ind] + 2 * j + 1] + sscript;
	    if (charx & 128) {
		charx &= 127;
		it1 = charx;
		it2 = chary;
		xtmp = scale * (it1 - 'R' - x);
		ytmp = (-scale * (it2 - 'R'));
		(*vector) ((int) (xpos + xtmp * co - ytmp * si), (int) (ypos + xtmp * si + ytmp * co), 0);
	    } else {
		it1 = charx;
		it2 = chary;
		xtmp = scale * (it1 - 'R' - x);
		ytmp = (-scale * (it2 - 'R'));
		(*vector) ((int) (xpos + xtmp * co - ytmp * si), (int) (ypos + xtmp * si + ytmp * co), color);
	    }
	}
	if (underline) {
	    (*vector) ((int) xpos, (int) (ypos - scale * 12.0), 0);
	}
	xpos = xpos + (slastx = scale * charw * co);
	ypos = ypos + (slasty = scale * charw * si);
	if (underline) {
	    (*vector) ((int) xpos, (int) (ypos - scale * 12.0), color);
	}
branch:;
    }
    hselectfont(sfont);
}

/*
	write symbol symno at xpos, ypos in device coordinates
	of size scale, color color, using vector
*/
hwritesym(symno, xpos, ypos, scale, color, vector)
    int symno;
    int xpos, ypos, color;
    double scale;
    int (*vector) ();		/* device draw line */

{
    int j, len = 0, ind, it1, it2;
    unsigned char charx, chary;

    if (symno == 1) {
	(*vector) (xpos, ypos, 0);
	(*vector) (xpos, ypos, color);
	return;
    }
    ind = symno - 1;
    len = symindices[ind + 1] - symindices[ind];
    for (j = 1; j < len; j++) {
	charx = symtable[2 * symindices[ind] + 2 * j];
	chary = symtable[2 * symindices[ind] + 2 * j + 1];
	if (charx & 128) {
	    charx &= 127;
	    it1 = charx;
	    it2 = chary;
	    it1 = (scale * (it1 - 'R') + xpos);
	    it2 = (-scale * (it2 - 'R') + ypos);
	    (*vector) (it1, it2, 0);
	} else {
	    it1 = charx;
	    it2 = chary;
	    it1 = (scale * (it1 - 'R') + xpos);
	    it2 = (-scale * (it2 - 'R') + ypos);
	    (*vector) (it1, it2, color);
	}
    }
}

/*
	get the x extent of the string in hershey coordinates given size
*/
int stringextentx(scale, s)
    double scale;
    char *s;
{
    int i, ind, xpos = 0, it1, it2, sfont = curfont, slen = strlen(s);
    double charw, x, y, saves = scale, slastx = 0.0;

    for (i = 0; i < slen; i++) {
	if (s[i] == '\\' && isdigit(s[i + 1])) {
	    hselectfont(s[i + 1] - '0');
	    i++;
	    goto branch;
	} else if (s[i] == '\\' && (isalpha(s[i + 1])||s[i+1]=='+'||s[i+1]=='-')) {
	    switch (s[i + 1]) {
	    case 'x':
		hselectfont(10);
		i++;
		goto branch;
		break;
	    case 's':
		scale = 0.6 * saves;
		i++;
		break;
	    case 'S':
		scale = 0.6 * saves;
		i++;
		break;
	    case 'N':
		scale = saves;
		i++;
		break;
	    case 'b':
		xpos = xpos - slastx;
		i++;
	    case '-':
		scale -= 0.2;
		i++;
		break;
	    case '+':
		scale += 0.2;
		i++;
		break;
	    }
	    goto branch;
	}
	ind = s[i] - ' ';

	it1 = chartable[2 * indices[ind]];
	it2 = chartable[2 * indices[ind] + 1];
	x = it1 - 'R';
	y = it2 - 'R';
	charw = y - x;
	xpos = xpos + (slastx = scale * charw);
branch:;
    }
    hselectfont(sfont);
    return xpos;
}

/*
	get the y extent of the string in hershey coordinates given size
*/
int stringextenty(scale, s)
    double scale;
    char *s;
{
    int i, j, len = 0, ind, it2;
    char charx, chary;
    double ytmp, ymin = 0, ymax = 0;

    for (i = 0; i < strlen(s); i++) {
	ind = s[i] - ' ';
	len = indices[ind + 1] - indices[ind];

	for (j = 1; j < len; j++) {
	    charx = chartable[2 * indices[ind] + 2 * j];
	    chary = chartable[2 * indices[ind] + 2 * j + 1];
	    if (charx & 128) {
		charx &= 127;
		it2 = chary;
		ytmp = (-scale * (it2 - 'R'));
	    } else {
		it2 = chary;
		ytmp = (-scale * (it2 - 'R'));
	    }
	    if (ymax < ytmp)
		ymax = ytmp;
	    if (ymin > ytmp)
		ymin = ytmp;
	}
    }
    return ymax - ymin;
}
