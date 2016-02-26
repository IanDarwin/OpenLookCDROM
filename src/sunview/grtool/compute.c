/*
	compute.c - perform math between sets

        $Header: compute.c,v 1.5 89/08/27 11:56:14 pturner Locked $
*/

static double *xtmp, *ytmp;
double scanner(), *getx(), *gety();
extern double *ax, *bx, *cx, *dx;	/* scratch arrays */

loadset(selset, toval, startno, stepno)
    int selset, toval;
    double startno, stepno;
{
    int i, lenset;
    double *ltmp;

    if ((lenset = getsetlength(selset)) <= 0) {
	char stmp[30];
	sprintf(stmp, "length of set %d = 0", selset);
	errwin(stmp);
	return;
    }
    xtmp = getx(selset);
    ytmp = gety(selset);
    switch (toval) {
    case 1:
	ltmp = xtmp;
	break;
    case 2:
	ltmp = ytmp;
	break;
    case 3:
	ltmp = ax;
	break;
    case 4:
	ltmp = bx;
	break;
    case 5:
	ltmp = cx;
	break;
    case 6:
	ltmp = dx;
	break;
    default:
	return;
    }
    for (i = 0; i < lenset; i++) {
	*ltmp++ = startno + i * stepno;
    }
    updatesetminmax(selset);
    update_status(selset);
}

formula(selset, sscanstr)
    int selset;
    char sscanstr[];

{
    char stmp[64];
    int i, errpos, lenset;

    if ((lenset = getsetlength(selset)) <= 0) {
	sprintf(stmp, "length of set %d = 0", selset);
	errwin(stmp);
	return;
    }
    xtmp = getx(selset);
    ytmp = gety(selset);
    sscanstr[strlen(sscanstr) + 1] = 0;
    sscanstr[strlen(sscanstr)] = '\n';
    lowtoupper(sscanstr);
    for (i = 0; i < lenset; i++) {
	scanner(sscanstr,&xtmp[i], &ytmp[i], &ax[i], &bx[i], &cx[i], &dx[i], i, &errpos);
	if (errpos) {
	    return;
	}
    }
    updatesetminmax(selset);
    update_status(selset);
}
