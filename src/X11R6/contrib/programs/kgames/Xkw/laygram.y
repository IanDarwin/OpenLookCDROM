%{
#include    <X11/Xlib.h>
#include    <X11/Xresource.h>
#include    <stdio.h>
#include    <X11/IntrinsicP.h>
#include    <X11/cursorfont.h>
#include    <X11/StringDefs.h>

#include    <X11/Xmu/Misc.h>
#include    <X11/Xmu/Converters.h>
#include    "LayoutP.h"

static LayoutPtr    *dest;

%}

%union {
    int		    ival;
    XrmQuark	    qval;
    BoxPtr	    bval;
    BoxParamsPtr    pval;
    GlueRec	    gval;
    LayoutDirection lval;
    ExprPtr	    eval;
    Operator	    oval;
}

%type	<bval>	    box boxes compositebox
%type	<pval>	    bothparams oneparams
%type	<gval>	    glue opStretch opShrink
%type	<lval>	    orientation
%type	<eval>	    signedExpr simpleExpr expr exprList

%token		    OC CC OA CA OP CP
%token	<qval>	    NAME
%token	<ival>	    NUMBER
%token	<ival>	    INFINITY
%token		    VERTICAL HORIZONTAL

%token		    EQUAL DOLLAR COMMA
%nonassoc <oval>    MIN MAX

%left	<oval>	    PLUS MINUS
%left	<oval>	    TIMES DIVIDE PERCENTOF
%right	<oval>	    PERCENT
%nonassoc	    WIDTH HEIGHT
%right	<oval>	    UMINUS UPLUS

%%
layout		:   compositebox
		    { *dest = $1; }
		;
box		:   NAME bothparams
		    {
			BoxPtr	box = New(BoxRec);
			box->nextSibling = 0;
			box->type = WidgetBox;
			box->params = *$2;
			Dispose ($2);
			box->u.widget.quark = $1;
			box->u.widget.widget = 0;
			$$ = box;
		    }
		|   signedExpr oneparams
		    {
			BoxPtr	box = New(BoxRec);
			box->nextSibling = 0;
			box->type = GlueBox;
			box->params = *$2;
			Dispose ($2);
			box->u.glue.expr[0] = $1;
			box->u.glue.expr[1] = 0;
			$$ = box;
		    }
		|   signedExpr COMMA signedExpr bothparams
		    {
			BoxPtr	box = New(BoxRec);
			box->nextSibling = 0;
			box->type = GlueBox;
			box->params = *$4;
			Dispose ($4);
			box->u.glue.expr[0] = $1;
			box->u.glue.expr[1] = $3;
			$$ = box;
		    }
		|   NAME EQUAL signedExpr
		    {
			BoxPtr	box = New(BoxRec);
			box->nextSibling = 0;
			box->type = VariableBox;
			box->u.variable.quark = $1;
			box->u.variable.expr = $3;
			ZeroGlue(box->params.stretch[LayoutHorizontal]);
			ZeroGlue(box->params.stretch[LayoutVertical]);
			ZeroGlue(box->params.shrink[LayoutHorizontal]);
			ZeroGlue(box->params.shrink[LayoutVertical]);
			$$ = box;
		    }
		|   compositebox
		    {
			$$ = $1;
		    }
		;
compositebox	:   orientation OC boxes CC
		    {
			BoxPtr	box = New(BoxRec);
			BoxPtr	child;

			box->nextSibling = 0;
			box->parent = 0;
			box->type = BoxBox;
			box->u.box.dir = $1;
			box->u.box.firstChild = $3;
			for (child = $3; child; child = child->nextSibling) 
			{
			    if (child->type == GlueBox && !child->u.glue.expr[1]) 
			    {
				child->params.stretch[!$1].expr = 0;
				child->params.shrink[!$1].expr = 0;
				child->params.stretch[!$1].order = 100000;
				child->params.shrink[!$1].order = 100000;
				child->params.stretch[!$1].value = 1;
				child->params.shrink[!$1].value = 1;
			    }
			    child->parent = box;
			}
			$$ = box;
		    }
		;
boxes		:   box boxes
		    { 
			$1->nextSibling = $2;
			$$ = $1;
		    }
		|   box
		    {	$$ = $1; }
		;
bothparams	:   OA opStretch opShrink TIMES opStretch opShrink CA
		    {	
			BoxParamsPtr	p = New(BoxParamsRec);
			
			p->stretch[LayoutHorizontal] = $2;
			p->shrink[LayoutHorizontal] = $3;
			p->stretch[LayoutVertical] = $5;
			p->shrink[LayoutVertical] = $6;
			$$ = p;
		    }
		|
		    {	
			BoxParamsPtr	p = New(BoxParamsRec);
			
			ZeroGlue (p->stretch[LayoutHorizontal]);
			ZeroGlue (p->shrink[LayoutHorizontal]);
			ZeroGlue (p->stretch[LayoutVertical]);
			ZeroGlue (p->shrink[LayoutVertical]);
			$$ = p;
		    }
		;
oneparams 	:   OA opStretch opShrink CA
		    {	
			BoxParamsPtr	p = New(BoxParamsRec);
			
			p->stretch[LayoutHorizontal] = $2;
			p->shrink[LayoutHorizontal] = $3;
			p->stretch[LayoutVertical] = $2;
			p->shrink[LayoutVertical] = $3;
			$$ = p;
		    }
		|
		    {	
			BoxParamsPtr	p = New(BoxParamsRec);
			
			ZeroGlue (p->stretch[LayoutHorizontal]);
			ZeroGlue (p->shrink[LayoutHorizontal]);
			ZeroGlue (p->stretch[LayoutVertical]);
			ZeroGlue (p->shrink[LayoutVertical]);
			$$ = p;
		    }
		;
opStretch	:   PLUS glue
		    { $$ = $2; }
		|
		    { ZeroGlue ($$); }
		;
opShrink	:   MINUS glue
		    { $$ = $2; }
		|
		    { ZeroGlue ($$); }
		;
glue		:   simpleExpr INFINITY
		    { $$.order = $2; $$.expr = $1; }
		|   simpleExpr
		    { $$.order = 0; $$.expr = $1; }
		|   INFINITY
		    { $$.order = $1; $$.expr = 0; $$.value = 1; }
		;
signedExpr	:   MINUS simpleExpr	    %prec UMINUS
		    {
			$$ = New(ExprRec);
			$$->type = Unary;
			$$->u.unary.op = $1;
			$$->u.unary.down = $2;
		    }
		|   PLUS simpleExpr	    %prec UPLUS
		    { $$ = $2; }
		|   simpleExpr
		;
simpleExpr    	:   WIDTH NAME
		    {	$$ = New(ExprRec);
			$$->type = Width;
			$$->u.width = $2;
		    }
		|   HEIGHT NAME
		    {	$$ = New(ExprRec);
			$$->type = Height;
			$$->u.height = $2;
		    }
		|   OP expr CP
		    { $$ = $2; }
		|   MIN OP exprList CP
		    {
			ExprPtr	e;

			e = $3;
			$$ = $3;
			while (e->type == Binary) {
			    e->u.binary.op = $1;
			    e = e->u.binary.right;
			}
			e->u.unary.op = $1;
		    }
		|   MAX OP exprList CP
		    {
			ExprPtr	e;

			e = $3;
			$$ = $3;
			while (e->type == Binary) {
			    e->u.binary.op = $1;
			    e = e->u.binary.right;
			}
			e->u.unary.op = $1;
		    }
		|   simpleExpr PERCENT
		    {
			$$ = New(ExprRec);
			$$->type = Unary;
			$$->u.unary.op = $2;
			$$->u.unary.down = $1;
		    }
		|   NUMBER
		    {	$$ = New(ExprRec);
			$$->type = Constant;
			$$->u.constant = $1;
		    }
		|   DOLLAR NAME
		    {	$$ = New(ExprRec);
			$$->type = Variable;
			$$->u.variable = $2;
		    }
		;
exprList	:   expr
		    {
			$$ = New(ExprRec);
			$$->type = Unary;
			$$->u.unary.down = $1;
		    }
		|   expr COMMA exprList
		    {
			$$ = New(ExprRec);
			$$->type = Binary;
			$$->u.binary.left = $1;
			$$->u.binary.right = $3;
		    }
expr		:   expr PLUS expr
		    { binary: ;
			$$ = New(ExprRec);
			$$->type = Binary;
			$$->u.binary.op = $2;
			$$->u.binary.left = $1;
			$$->u.binary.right = $3;
		    }
		|   expr MINUS expr
		    { goto binary; }
		|   expr TIMES expr
		    { goto binary; }
		|   expr DIVIDE expr
		    { goto binary; }
		|   expr PERCENTOF expr
		    { goto binary; }
		|   MINUS expr		    %prec UMINUS
		    { unary: ;
			$$ = New(ExprRec);
			$$->type = Unary;
			$$->u.unary.op = $1;
			$$->u.unary.down = $2;
		    }
		|   PLUS expr		    %prec UPLUS
		    { $$ = $2; }
		|   simpleExpr
		;
orientation	:   VERTICAL
		{   $$ = LayoutVertical; }
		|   HORIZONTAL
		{   $$ = LayoutHorizontal; }
		;
%%

yywrap ()
{
    return 1;
}

yysetdest (c)
    LayoutPtr	*c;
{
    dest = c;
}
