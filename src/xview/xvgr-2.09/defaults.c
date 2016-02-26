/* $Id: defaults.c,v 1.24 92/08/23 10:41:11 pturner Exp Locker: pturner $
 *
 * set defaults - changes to the types in defines.h
 * will require changes in here also
 *
 */

#include <stdio.h>
#include "globals.h"

void set_program_defaults();
void set_region_defaults();
void set_default_defaults();
void set_default_framep();
void set_default_world();
void set_default_view();
void set_default_string();
void set_default_line();
void set_default_box();
void set_default_legend();
void set_default_plotarr();
void set_default_graph();
void set_default_annotation();
void set_default_ticks();

static plotarr d_p = {OFF,	/* active */
    XY,				/* dataset type */
    0,				/* deactivate flag */
    DATASET_MISSING,		/* value for missing data */
    NULL, NULL, NULL, NULL, NULL, NULL,	/* data array */
    NULL,			/* string pointer */
    4,				/* string font */
    DECIMAL,			/* val format */
    1,				/* val prec */
    0.0,			/* xmin */
    0.0,			/* xmax */
    0.0,			/* ymin */
    0.0,			/* ymax */
    0,				/* length */
    0,				/* symbol */
    0,				/* symchar */
    0,				/* symskip */
    0,				/* symfill */
    0,				/* symdot */
    1.0,			/* symsize */
    1,				/* line style */
    1,				/* line width */
    1,				/* color */
    0,				/* fill type */
    COLOR,			/* fill using color or pattern */
    0,				/* fill color */
    0,				/* fill pattern */
    -1,				/* is errbar */
    BOTH,			/* errbar type */
    1,				/* errbar linew */
    1,				/* errbar lines */
    ON,				/* errbar riser */
    1,				/* errbar riser linew */
    1,				/* errbar riser lines */
    1.0,			/* length of error bar */
    1.0,			/* length of hi low ticks */
    0,				/* flag for density plots if type is XYZ */
    0.0,			/* zmin for density plots */
    0.0,			/* zmax for density plots */
    0				/* default comment */
};

static linetype d_l = {OFF,	/* active */
    VIEW,			/* location type */
    -1,				/* if loctype == WORLD then graph number */
    0.0,			/* x1 */
    0.0,			/* y1 */
    0.0,			/* x2 */
    0.0,			/* y2 */
    1,				/* line style */
    1,				/* line width */
    1,				/* color */
    0,				/* arrow type */
    0,				/* arrow head type */
    1.0				/* arrow size */
};

static boxtype d_b = {OFF,	/* active */
    VIEW,			/* location type */
    -1,				/* if loctype == WORLD then graph number */
    0.0,			/* x1 */
    0.0,			/* y1 */
    0.0,			/* x2 */
    0.0,			/* y2 */
    1,				/* line style */
    1,				/* line width */
    1,				/* color */
    OFF,			/* fill */
    1,				/* fill color */
    1				/* fill pattern */
};

static plotstr d_s = {OFF,	/* active */
    VIEW,			/* location type */
    -1,				/* if loctype == WORLD then graph number */
    0.0,			/* x */
    0.0,			/* y */
    1,				/* line style */
    1,				/* line width */
    1,				/* color */
    0,				/* rotation (degrees) */
    4,				/* font */
    0,				/* justification */
    1.0,			/* character size */
    0				/* string */
};

static framep d_f = {ON,	/* active */
    0,				/* type */
    1,				/* color */
    1,				/* line style */
    1,				/* line width */
    OFF,			/* fill background */
    0				/* background color */
};

static region d_r = {OFF,	/* active */
    0,				/* type */
    1,				/* color */
    1,				/* line style */
    1,				/* line width */
    0,				/* link to */
    0,				/* number of points */
    NULL, NULL,			/* x and y if a polygon */
    0.0, 0.0,			/* x1 and y1 if a line */
    0.0, 0.0,			/* x2 and y2 if a line */
};

static world d_w = {0.0, 1.0, 0.0, 1.0};

static view d_v = {0.15, 0.85, 0.15, 0.85};

static defaults d_d = {1, 1, 1, 1.0, 2, 0, 1.0};

void set_program_defaults()
{
    int i;
    g = (graph *) calloc(maxgraph, sizeof(graph));
    for (i = 0; i < maxgraph; i++) {
	g[i].p = (plotarr *) calloc(maxplot, sizeof(plotarr));
	set_default_graph(i);
    }
    for (i = 0; i < MAXREGION; i++) {
	set_region_defaults(i);
    }
    set_default_annotation();
    set_default_string(&timestamp);
    timestamp.x = 0.03;
    timestamp.y = 0.03;
    init_scratch_arrays(maxarr);
}

void set_region_defaults(i)
    int i;
{
    memcpy(&rg[i], &d_r, sizeof(region));
}

void set_default_defaults(d)
    defaults *d;
{
    memcpy(d, &d_d, sizeof(defaults));
}

void set_default_framep(f)
    framep *f;
{
    memcpy(f, &d_f, sizeof(framep));
}

void set_default_world(w)
    world *w;
{
    memcpy(w, &d_w, sizeof(world));
}

void set_default_view(v)
    view *v;
{
    memcpy(v, &d_v, sizeof(view));
}

void set_default_string(s)
    plotstr *s;
{
    memcpy(s, &d_s, sizeof(plotstr));
}

void set_default_line(l)
    linetype *l;
{
    memcpy(l, &d_l, sizeof(linetype));
}

void set_default_box(b)
    boxtype *b;
{
    memcpy(b, &d_b, sizeof(boxtype));
}

void set_default_legend(l)
    legend *l;
{
    int i;

    l->active = OFF;
    l->loctype = VIEW;
    l->layout = 0;
    l->vgap = 2;
    l->hgap = 1;
    l->len = 4;
    l->legx = 0.8;
    l->legy = 0.8;
    l->font = 4;
    l->charsize = 1.0;
    l->color = 1;
    l->linew = 1;
    l->lines = 1;
    l->box = OFF;
    l->boxfill = OFF;
    l->boxfillusing = COLOR;
    l->boxfillcolor = 0;
    l->boxfillpat = 1;
    l->boxlcolor = 1;
    l->boxlinew = 1;
    l->boxlines = 1;
    for (i = 0; i < MAXPLOT; i++) {
	set_default_string(&(l->str[i]));
    }
}

void set_default_plotarr(p)
    plotarr *p;
{
    int i;

    for (i = 0; i < MAX_SET_COLS; i++) {
	if (p->ex[i] != NULL) {
	    cfree(p->ex[i]);
	}
	p->ex[i] = NULL;
    }
    memcpy(p, &d_p, sizeof(plotarr));
}

void set_default_graph(gno)
    int gno;
{
    int i;
    char buf[256];

    g[gno].active = OFF;
    g[gno].hidden = FALSE;
    g[gno].label = OFF;
    g[gno].type = XY;
    g[gno].auto_type = AUTO;
    g[gno].revx = FALSE;
    g[gno].revy = FALSE;
    g[gno].ws_top = 0;
    g[gno].maxplot = maxplot;
    g[gno].dsx = g[gno].dsy = 0.0;	/* locator props */
    g[gno].pointset = FALSE;
    g[gno].pt_type = 0;
    g[gno].fx = GENERAL;
    g[gno].fy = GENERAL;
    g[gno].px = 6;
    g[gno].py = 6;
    set_default_defaults(&g[gno].d);
    set_default_ticks(&g[gno].t[0], X_AXIS);
    set_default_ticks(&g[gno].t[1], Y_AXIS);
    set_default_ticks(&g[gno].t[2], ZX_AXIS);
    set_default_ticks(&g[gno].t[3], ZY_AXIS);
    set_default_ticks(&g[gno].t[4], XA_AXIS);
    set_default_ticks(&g[gno].t[5], YA_AXIS);
    set_default_framep(&g[gno].f);
    set_default_world(&g[gno].w);
    set_default_view(&g[gno].v);
    set_default_legend(&g[gno].l);
    set_default_string(&g[gno].labs.title);
    g[gno].labs.title.charsize = 1.5;
    set_default_string(&g[gno].labs.stitle);
    g[gno].labs.stitle.charsize = 1.0;
    for (i = 0; i < maxplot; i++) {
	set_default_plotarr(&g[gno].p[i]);
    }
}

void realloc_plots(maxplot)
int maxplot;
{
    int i, j;
    for (i = 0; i < maxgraph; i++) {
	g[i].p = (plotarr *) realloc(g[i].p, maxplot * sizeof(plotarr));
	for (j = g[i].maxplot; j < maxplot; j++) {
	    set_default_plotarr(&g[i].p[j]);
	}
	g[i].maxplot = maxplot;
    }
}

void realloc_graph_plots(gno, maxplot)
    int gno, maxplot;
{
    int j;
    g[gno].p = (plotarr *) realloc(g[gno].p, maxplot * sizeof(plotarr));
    for (j = g[gno].maxplot; j < maxplot; j++) {
	set_default_plotarr(&g[gno].p[j]);
    }
    g[gno].maxplot = maxplot;
}

void realloc_graphs()
{
    int i, j;

    g = (graph *) realloc(g, maxgraph * sizeof(graph));
    for (j = MAXGRAPH; j < maxgraph; j++) {
	g[j].p = (plotarr *) calloc(maxplot, sizeof(plotarr));
	set_default_graph(j);
    }
}

void set_default_annotation()
{
    int i;

    for (i = 0; i < MAXBOXES; i++) {
	set_default_box(&boxes[i]);
    }
    for (i = 0; i < MAXLINES; i++) {
	set_default_line(&lines[i]);
    }
    for (i = 0; i < MAXSTR; i++) {
	set_default_string(&pstr[i]);
    }
}

void set_default_ticks(t, a)
    tickmarks *t;
    int a;
{
    int i;

    t->axis = a;
    switch (a) {
    case X_AXIS:
    case Y_AXIS:
	t->active = ON;
	t->alt = OFF;
	t->tl_flag = ON;
	t->t_flag = ON;
	break;
    case XA_AXIS:
    case YA_AXIS:
	t->active = ON;
	t->alt = OFF;
	t->tl_flag = OFF;
	t->t_flag = OFF;
	break;
    case ZX_AXIS:
    case ZY_AXIS:
	t->active = ON;
	t->alt = OFF;
	t->tl_flag = OFF;
	t->t_flag = OFF;
	break;
    }
    set_default_string(&t->label);
    t->tmin = 0.0;
    t->tmax = 1.0;
    t->tmajor = 0.5;
    t->tminor = 0.25;
    t->offsx = 0.0;
    t->offsy = 0.0;
    t->label_layout = PARA;
    t->label_place = AUTO;
    t->tl_type = AUTO;
    t->tl_layout = HORIZONTAL;
    t->tl_sign = NORMAL;
    t->tl_prec = 1;
    t->tl_format = DECIMAL;
    t->tl_angle = 0;
    t->tl_just = (a % 2) ? RIGHT : CENTER;
    t->tl_skip = 0;
    t->tl_staggered = 0;
    t->tl_starttype = AUTO;
    t->tl_stoptype = AUTO;
    t->tl_start = 0.0;
    t->tl_stop = 0.0;
    t->tl_op = (a % 2) ? LEFT : BOTTOM;
    t->tl_vgap = 1.0;
    t->tl_hgap = 1.0;
    t->tl_font = 4;
    t->tl_charsize = 1.0;
    t->tl_color = 1;
    t->tl_linew = 1;
    t->tl_appstr[0] = 0;
    t->tl_prestr[0] = 0;
    t->t_color = 1;
    t->t_linew = 1;
    t->t_type = AUTO;
    t->t_mflag = ON;
    t->t_integer = OFF;
    t->t_num = 6;
    t->t_inout = IN;
    t->t_log = OFF;
    t->t_op = BOTH;
    t->t_size = 1.0;
    t->t_msize = 0.5;
    t->t_drawbar = OFF;
    t->t_drawbarcolor = 1;
    t->t_drawbarlines = 1;
    t->t_drawbarlinew = 1;
    t->t_gridflag = OFF;
    t->t_mgridflag = OFF;
    t->t_color = 1;
    t->t_lines = 1;
    t->t_linew = 1;
    t->t_mcolor = 1;
    t->t_mlines = 1;
    t->t_mlinew = 1;
    t->t_spec = 0;
    for (i = 0; i < MAX_TICK_LABELS; i++) {
	t->t_specloc[i] = 0.0;
	t->t_speclab[i].s[0] = '\0';
    }
}
