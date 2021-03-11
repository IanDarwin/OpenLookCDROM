/* ********************************************************************** *\
 *         Copyright IBM Corporation 1988,1991 - All Rights Reserved      *
 *        For full copyright information see:'andrew/config/COPYRITE'     *
\* ********************************************************************** */

/*
	$Disclaimer: 
 * Permission to use, copy, modify, and distribute this software and its 
 * documentation for any purpose is hereby granted without fee, 
 * provided that the above copyright notice appear in all copies and that 
 * both that copyright notice, this permission notice, and the following 
 * disclaimer appear in supporting documentation, and that the names of 
 * IBM, Carnegie Mellon University, and other copyright holders, not be 
 * used in advertising or publicity pertaining to distribution of the software 
 * without specific, written prior permission.
 * 
 * IBM, CARNEGIE MELLON UNIVERSITY, AND THE OTHER COPYRIGHT HOLDERS 
 * DISCLAIM ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING 
 * ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS.  IN NO EVENT 
 * SHALL IBM, CARNEGIE MELLON UNIVERSITY, OR ANY OTHER COPYRIGHT HOLDER 
 * BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY 
 * DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, 
 * WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS 
 * ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE 
 * OF THIS SOFTWARE.
 *  $
*/

#ifndef NORCSID
#define NORCSID
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/parsec/RCS/support.c,v 1.3 1992/12/15 21:56:26 rr2b R6tape $";
#endif


#include <parsec.h>

/* PC_PreWalkTree takes a parse-tree node, an int-returning function
 * of five arguments and a "rootVal".  It does a preorder walk of the tree rooted
 * at the given node, applying func to the node and then recursively
 * descending node's children.
 *
 * Func, as mentioned, takes five arguments: node, depth, descend, parentVal and whichChild.
 * Node is the current tree node, and depth is its depth in the
 * tree relative to the the root node (which is at depth 0).
 * Descend is a pointer to an integer which by default is 1.
 * If it is set to zero, then node's children are not recursively
 * descended.  ParentVal is the value returned by the user's
 * function on the parent of the current node.  For the root node,
 * parentVal is rootVal.  WhichChild is an index indicating which
 * child of the node's parent he is, and ranges from 0 to
 * 3.  For the root node, whichChild is -1.
 *
 * The value returned by PC_PreWalkTree is the value returned
 * by the user's function applied to the root node.
 */
static int      PreWalkTree(node, func, depth, parentVal, whichChild)
PC_ParseNode_t *node;
int             (*func) ();
int             depth, parentVal, whichChild;

{
    int             descend = 1, result, i;

    result = (*func) (node, depth, &descend, parentVal);
    if (descend && PC_IsProductionNode(node))
        for (i = 0; i < PC_NumChildren(node); ++i)
            (void) PreWalkTree(PC_Child(node, i), func, depth + 1, result, i);
    return (result);
}

int             PC_PreWalkTree(node, func, rootVal)
PC_ParseNode_t *node;
int             (*func) ();
int rootVal;
{
    return (PreWalkTree(node, func, 0, rootVal, -1));
}


/* PC_PostWalkTree takes a parse-tree node, an int-returning function
 * of four arguments, a "combiner" flag and a "leafVal".
 * It does a postorder walk of the tree rooted
 * at the given node, recursively descending a node and then
 * applying func to the node.
 *
 * Func, as mentioned, takes four arguments: node, depth, childrenVal and whichChild.
 * Node is the current tree node, and depth is its depth in the
 * tree relative to the the root node (which is at depth 0).
 * ChildrenVal is the combination of the values returned by the user's
 * function on the children of the current node.  The way the children's
 * values are to be combined is specified with the combiner parameter.
 * The values of this parameter are PC_COMBINE_ADD, PC_COMBINE_LOR,
 * PC_COMBINE_BITOR, PC_COMBINE_LXOR, PC_COMBINE_BITXOR,
 * PC_COMBINE_LAND, PC_COMBINE_BITAND, and PC_COMBINE_MULTIPLY.
 * For leaf nodes, childrenVal is leafVal.  WhichChildis an index indicating which child
 * of a node's parent he is, and ranges from 0 to 3.  For the root node, whichChild is -1.
 *
 * The value returned by PC_WalkTree is the value returned
 * by the user's function applied to the root node.
 */
static int      PostWalkTree(node, func, depth, combine, whichChild, leafVal)
PC_ParseNode_t *node;
int             (*func) ();
int             depth, combine, whichChild, leafVal;
{
    int             result = leafVal, thisResult, i;

    if (PC_IsProductionNode(node)) {
        for (i = 0; i < PC_NumChildren(node); ++i) {
            thisResult = PostWalkTree(PC_Child(node, i), func,
                                       depth + 1, combine, i, leafVal);
            if (i == 0)
                result = thisResult;
            else {
                switch (combine) {
                    case PC_COMBINE_LOR:
                        result = result || thisResult;
                        break;
                    case PC_COMBINE_BITOR:
                        result |= thisResult;
                        break;
                    case PC_COMBINE_LXOR:
                        result = result ? (thisResult ? 0 : 1) : (thisResult ? 1 : 0);
                        break;
                    case PC_COMBINE_BITXOR:
                        result ^= thisResult;
                        break;
                    case PC_COMBINE_LAND:
                        result = result && thisResult;
                        break;
                    case PC_COMBINE_BITAND:
                        result &= thisResult;
                        break;
                    case PC_COMBINE_MULTIPLY:
                        result *= thisResult;
                        break;
                    default:
                        result += thisResult;
                        break;
                }
            }
        }
    }
    return ((*func) (node, depth, result, whichChild));
}

int             PC_PostWalkTree(node, func, combine, leafVal)
PC_ParseNode_t *node;
int             (*func) ();
int             combine, leafVal;
{
    return (PostWalkTree(node, func, 0, combine, -1, leafVal));
}


void     PC_SetCharToken(tok, c)
PC_Token_t     *tok;
int             c;
{
    tok->isChar = 1;
    tok->hasText = 0;
    tok->val = c;
}

void     PC_SetTextToken(tok, val, str)
PC_Token_t     *tok;
int             val;
char           *str;
{
    tok->isChar = 0;
    tok->hasText = 1;
    tok->val = val;
    tok->text = str;
}

void     PC_SetToken(tok, val)
PC_Token_t     *tok;
int             val;
{
    tok->isChar = 0;
    tok->hasText = 0;
    tok->val = val;
}

/* Returns the number of tokens required to
 * recreate the source code associated with
 * node
 */
int             PC_CountTokens(node)
PC_ParseNode_t *node;
{
    if (PC_IsTokenNode(node))
        return (1);
    switch (PC_NodeType(node)) {
        case pnt_module:
            switch (PC_SubType(node)) {
                case 1:
                    return (PC_CountTokens(PC_Child(node, 0)));
                case 2:
                    return (PC_CountTokens(PC_Child(node, 0)) +
                            PC_CountTokens(PC_Child(node, 1)));
            }
        case pnt_external_declaration:
            return (PC_CountTokens(PC_Child(node, 0)));
        case pnt_function_definition:
            switch (PC_SubType(node)) {
                case 1:
                    return (PC_CountTokens(PC_Child(node, 0)) +
                            PC_CountTokens(PC_Child(node, 1)));
                case 2:
                    return (PC_CountTokens(PC_Child(node, 0)) +
                            PC_CountTokens(PC_Child(node, 1)) +
                            PC_CountTokens(PC_Child(node, 2)));
            }
        case pnt_function_body:
            switch (PC_SubType(node)) {
                case 1:
                    return (PC_CountTokens(PC_Child(node, 0)));
                case 2:
                    return (PC_CountTokens(PC_Child(node, 0)) +
                            PC_CountTokens(PC_Child(node, 1)));
            }
        case pnt_declaration:
            switch (PC_SubType(node)) {
                case 1:
                    return (1 + PC_CountTokens(PC_Child(node, 0)));
                case 2:
                    return (1 + PC_CountTokens(PC_Child(node, 0)) +
                            PC_CountTokens(PC_Child(node, 1)));
            }
        case pnt_declaration_list:
            switch (PC_SubType(node)) {
                case 1:
                    return (PC_CountTokens(PC_Child(node, 0)));
                case 2:
                    return (PC_CountTokens(PC_Child(node, 0)) +
                            PC_CountTokens(PC_Child(node, 1)));
            }
        case pnt_declaration_specifiers:
            switch (PC_SubType(node)) {
                case 1:
                case 3:
                case 5:
                    return (PC_CountTokens(PC_Child(node, 0)));
                case 2:
                case 4:
                case 6:
                    return (PC_CountTokens(PC_Child(node, 0)) +
                            PC_CountTokens(PC_Child(node, 1)));
            }
        case pnt_storage_class_specifier:       /* Always a token node */
        case pnt_type_specifier:
            return (PC_CountTokens(PC_Child(node, 0))); /* Presumes it's
                                                         * production 10 or 11
                                                         * (otherwise it's a
                                                         * token node) */
        case pnt_type_qualifier:      /* Always a token node */
        case pnt_struct_or_union_specifier:
            switch (PC_SubType(node)) {
                case 1:
                    return (2 + PC_CountTokens(PC_Child(node, 0)) +
                            PC_CountTokens(PC_Child(node, 1)));
                case 2:
                    return (2 + PC_CountTokens(PC_Child(node, 0)) +
                            PC_CountTokens(PC_Child(node, 1)) +
                             PC_CountTokens(PC_Child(node, 2)));
                case 3:
                    return (PC_CountTokens(PC_Child(node, 0)) +
                            PC_CountTokens(PC_Child(node, 1)));
            }
        case pnt_struct_or_union:     /* Always a token node */
        case pnt_struct_declaration_list:
            switch (PC_SubType(node)) {
                case 1:
                    return (PC_CountTokens(PC_Child(node, 0)));
                case 2:
                    return (PC_CountTokens(PC_Child(node, 0)) +
                            PC_CountTokens(PC_Child(node, 1)));
            }
        case pnt_init_declarator_list:
            switch (PC_SubType(node)) {
                case 1:
                    return (PC_CountTokens(PC_Child(node, 0)));
                case 2:
                    return (1 + PC_CountTokens(PC_Child(node, 0)) +
                            PC_CountTokens(PC_Child(node, 1)));
            }
        case pnt_init_declarator:
            switch (PC_SubType(node)) {
                case 1:
                    return (PC_CountTokens(PC_Child(node, 0)));
                case 2:
                    return (1 + PC_CountTokens(PC_Child(node, 0)) +
                            PC_CountTokens(PC_Child(node, 1)));
            }
        case pnt_struct_declaration:
            return (1 + PC_CountTokens(PC_Child(node, 0)) +
                    PC_CountTokens(PC_Child(node, 1)));
        case pnt_specifier_qualifier_list:
            switch (PC_SubType(node)) {
                case 1:
                case 3:
                    return (PC_CountTokens(PC_Child(node, 0)));
                case 2:
                case 4:
                    return (PC_CountTokens(PC_Child(node, 0)) +
                            PC_CountTokens(PC_Child(node, 1)));
            }
        case pnt_struct_declarator_list:
            switch (PC_SubType(node)) {
                case 1:
                    return (PC_CountTokens(PC_Child(node, 0)));
                case 2:
                    return (1 + PC_CountTokens(PC_Child(node, 0)) +
                            PC_CountTokens(PC_Child(node, 1)));
            }
        case pnt_struct_declarator:
            switch (PC_SubType(node)) {
                case 1:
                    return (PC_CountTokens(PC_Child(node, 0)));
                case 2:
                    return (1 + PC_CountTokens(PC_Child(node, 0)));
                case 3:
                    return (1 + PC_CountTokens(PC_Child(node, 0)) +
                            PC_CountTokens(PC_Child(node, 1)));
            }
        case pnt_enum_specifier:
            switch (PC_SubType(node)) {
                case 1:
                    return (3 + PC_CountTokens(PC_Child(node, 0)));
                case 2:
                    return (3 + PC_CountTokens(PC_Child(node, 0)) +
                            PC_CountTokens(PC_Child(node, 1)));
                case 3:
                    return (1 + PC_CountTokens(PC_Child(node, 0)));
            }
        case pnt_enumerator_list:
            switch (PC_SubType(node)) {
                case 1:
                    return (PC_CountTokens(PC_Child(node, 0)));
                case 2:
                    return (1 + PC_CountTokens(PC_Child(node, 0)) +
                            PC_CountTokens(PC_Child(node, 1)));
            }
        case pnt_enumerator:
            switch (PC_SubType(node)) {
                case 1:
                    return (PC_CountTokens(PC_Child(node, 0)));
                case 2:
                    return (1 + PC_CountTokens(PC_Child(node, 0)) +
                            PC_CountTokens(PC_Child(node, 1)));
            }
        case pnt_declarator:
            switch (PC_SubType(node)) {
                case 1:
                    return (PC_CountTokens(PC_Child(node, 0)));
                case 2:
                    return (PC_CountTokens(PC_Child(node, 0)) +
                            PC_CountTokens(PC_Child(node, 1)));
            }
        case pnt_direct_declarator:
            switch (PC_SubType(node)) {
                case 1:
                    return (PC_CountTokens(PC_Child(node, 0)));
                case 2:
                case 3:
                case 6:
                    return (2 + PC_CountTokens(PC_Child(node, 0)));
                case 4:
                case 5:
                case 7:
                    return (2 + PC_CountTokens(PC_Child(node, 0)) +
                            PC_CountTokens(PC_Child(node, 1)));
            }
        case pnt_pointer:
            switch (PC_SubType(node)) {
                case 1:
                    return (1);
                case 2:
                case 3:
                case 4:
                    return (1 + PC_CountTokens(PC_Child(node, 0)));
            }
        case pnt_type_qualifier_list:
            switch (PC_SubType(node)) {
                case 1:
                    return (PC_CountTokens(PC_Child(node, 0)));
                case 2:
                    return (PC_CountTokens(PC_Child(node, 0)) +
                            PC_CountTokens(PC_Child(node, 1)));
            }
        case pnt_parameter_type_list:
            switch (PC_SubType(node)) {
                case 1:
                    return (PC_CountTokens(PC_Child(node, 0)));
                case 2:
                    return (2 + PC_CountTokens(PC_Child(node, 0)));
            }
        case pnt_parameter_list:
            switch (PC_SubType(node)) {
                case 1:
                    return (PC_CountTokens(PC_Child(node, 0)));
                case 2:
                    return (1 + PC_CountTokens(PC_Child(node, 0)) +
                            PC_CountTokens(PC_Child(node, 1)));
            }
        case pnt_parameter_declaration:
            switch (PC_SubType(node)) {
                case 1:
                case 3:
                    return (PC_CountTokens(PC_Child(node, 0)) +
                            PC_CountTokens(PC_Child(node, 1)));
                case 2:
                    return (PC_CountTokens(PC_Child(node, 0)));
            }
        case pnt_identifier_list:
            switch (PC_SubType(node)) {
                case 1:
                    return (PC_CountTokens(PC_Child(node, 0)));
                case 2:
                    return (1 + PC_CountTokens(PC_Child(node, 0)) +
                            PC_CountTokens(PC_Child(node, 1)));
            }
        case pnt_initializer:
            switch (PC_SubType(node)) {
                case 1:
                    return (PC_CountTokens(PC_Child(node, 0)));
                case 2:
                    return (2 + PC_CountTokens(PC_Child(node, 0)));
                case 3:
                    return (3 + PC_CountTokens(PC_Child(node, 0)));
            }
        case pnt_initializer_list:
            switch (PC_SubType(node)) {
                case 1:
                    return (PC_CountTokens(PC_Child(node, 0)));
                case 2:
                    return (1 + PC_CountTokens(PC_Child(node, 0)) +
                            PC_CountTokens(PC_Child(node, 1)));
            }
        case pnt_type_name:
            switch (PC_SubType(node)) {
                case 1:
                    return (PC_CountTokens(PC_Child(node, 0)));
                case 2:
                    return (PC_CountTokens(PC_Child(node, 0)) +
                            PC_CountTokens(PC_Child(node, 1)));
            }
        case pnt_abstract_declarator:
            switch (PC_SubType(node)) {
                case 1:
                case 2:
                    return (PC_CountTokens(PC_Child(node, 0)));
                case 3:
                    return (PC_CountTokens(PC_Child(node, 0)) +
                            PC_CountTokens(PC_Child(node, 1)));
            }
        case pnt_direct_abstract_declarator:
            switch (PC_SubType(node)) {
                case 1:
                case 3:
                case 4:
                case 7:
                case 8:
                    return (2 + PC_CountTokens(PC_Child(node, 0)));
                case 2:
                case 6:
                    return (2);
                case 5:
                case 9:
                    return (2 + PC_CountTokens(PC_Child(node, 0)) +
                            PC_CountTokens(PC_Child(node, 1)));
            }
        case pnt_statement:
            return (PC_CountTokens(PC_Child(node, 0)));
        case pnt_labeled_statement:
            switch (PC_SubType(node)) {
                case 1:
                    return (1 + PC_CountTokens(PC_Child(node, 0)) +
                            PC_CountTokens(PC_Child(node, 1)));
                case 2:
                    return (2 + PC_CountTokens(PC_Child(node, 0)) +
                            PC_CountTokens(PC_Child(node, 1)));
                case 3:
                    return (2 + PC_CountTokens(PC_Child(node, 0)));
            }
        case pnt_expression_statement:
            switch (PC_SubType(node)) {
                case 1:
                    return (1);
                case 2:
                    return (1 + PC_CountTokens(PC_Child(node, 0)));
            }
        case pnt_compound_statement:
            switch (PC_SubType(node)) {
                case 1:
                    return (2);
                case 2:
                case 3:
                    return (2 + PC_CountTokens(PC_Child(node, 0)));
                case 4:
                    return (2 + PC_CountTokens(PC_Child(node, 0)) +
                            PC_CountTokens(PC_Child(node, 1)));
            }
        case pnt_statement_list:
            switch (PC_SubType(node)) {
                case 1:
                    return (PC_CountTokens(PC_Child(node, 0)));
                case 2:
                    return (PC_CountTokens(PC_Child(node, 0)) +
                            PC_CountTokens(PC_Child(node, 1)));
            }
        case pnt_selection_statement:
            switch (PC_SubType(node)) {
                case 1:
                case 3:
                    return (3 + PC_CountTokens(PC_Child(node, 0)) +
                            PC_CountTokens(PC_Child(node, 1)));
                case 2:
                    return (4 + PC_CountTokens(PC_Child(node, 0)) +
                            PC_CountTokens(PC_Child(node, 1)) +
                            PC_CountTokens(PC_Child(node, 2)));
            }
        case pnt_iteration_statement:
            switch (PC_SubType(node)) {
                case 1:
                    return (3 + PC_CountTokens(PC_Child(node, 0)) +
                            PC_CountTokens(PC_Child(node, 1)));
                case 2:
                case 4:
                case 5:
                case 6:
                    return (5 + PC_CountTokens(PC_Child(node, 0)) +
                            PC_CountTokens(PC_Child(node, 1)));
                case 3:
                    return (5 + PC_CountTokens(PC_Child(node, 0)));
                case 7:
                case 8:
                case 9:
                    return (5 + PC_CountTokens(PC_Child(node, 0)) +
                            PC_CountTokens(PC_Child(node, 1)) +
                            PC_CountTokens(PC_Child(node, 2)));
                case 10:
                    return (5 + PC_CountTokens(PC_Child(node, 0)) +
                            PC_CountTokens(PC_Child(node, 1)) +
                            PC_CountTokens(PC_Child(node, 2)) +
                            PC_CountTokens(PC_Child(node, 3)));
            }
        case pnt_jump_statement:
            switch (PC_SubType(node)) {
                case 1:
                case 5:
                    return (2 + PC_CountTokens(PC_Child(node, 0)));
                case 2:
                case 3:
                case 4:
                    return (2);
            }
        case pnt_expression:
            switch (PC_SubType(node)) {
                case 1:
                    return (PC_CountTokens(PC_Child(node, 0)));
                case 2:
                    return (1 + PC_CountTokens(PC_Child(node, 0)) +
                            PC_CountTokens(PC_Child(node, 1)));
            }
        case pnt_assignment_expression:
            switch (PC_SubType(node)) {
                case 1:
                    return (PC_CountTokens(PC_Child(node, 0)));
                case 2:
                    return (PC_CountTokens(PC_Child(node, 0)) +
                            PC_CountTokens(PC_Child(node, 1)) +
                            PC_CountTokens(PC_Child(node, 2)));
            }
        case pnt_assignment_operator: /* Always a token node */
        case pnt_conditional_expression:
            switch (PC_SubType(node)) {
                case 1:
                    return (PC_CountTokens(PC_Child(node, 0)));
                case 2:
                    return (2 + PC_CountTokens(PC_Child(node, 0)) +
                            PC_CountTokens(PC_Child(node, 1)) +
                            PC_CountTokens(PC_Child(node, 2)));
            }
        case pnt_constant_expression:
            return (PC_CountTokens(PC_Child(node, 0)));
        case pnt_logical_or_expression:
            switch (PC_SubType(node)) {
                case 1:
                    return (PC_CountTokens(PC_Child(node, 0)));
                case 2:
                    return (1 + PC_CountTokens(PC_Child(node, 0)) +
                            PC_CountTokens(PC_Child(node, 1)));
            }
        case pnt_logical_and_expression:
            switch (PC_SubType(node)) {
                case 1:
                    return (PC_CountTokens(PC_Child(node, 0)));
                case 2:
                    return (1 + PC_CountTokens(PC_Child(node, 0)) +
                            PC_CountTokens(PC_Child(node, 1)));
            }
        case pnt_inclusive_or_expression:
            switch (PC_SubType(node)) {
                case 1:
                    return (PC_CountTokens(PC_Child(node, 0)));
                case 2:
                    return (1 + PC_CountTokens(PC_Child(node, 0)) +
                            PC_CountTokens(PC_Child(node, 1)));
            }
        case pnt_exclusive_or_expression:
            switch (PC_SubType(node)) {
                case 1:
                    return (PC_CountTokens(PC_Child(node, 0)));
                case 2:
                    return (1 + PC_CountTokens(PC_Child(node, 0)) +
                            PC_CountTokens(PC_Child(node, 1)));
            }
        case pnt_and_expression:
            switch (PC_SubType(node)) {
                case 1:
                    return (PC_CountTokens(PC_Child(node, 0)));
                case 2:
                    return (1 + PC_CountTokens(PC_Child(node, 0)) +
                            PC_CountTokens(PC_Child(node, 1)));
            }
        case pnt_equality_expression:
            switch (PC_SubType(node)) {
                case 1:
                    return (PC_CountTokens(PC_Child(node, 0)));
                case 2:
                case 3:
                    return (1 + PC_CountTokens(PC_Child(node, 0)) +
                            PC_CountTokens(PC_Child(node, 1)));
            }
        case pnt_relational_expression:
            switch (PC_SubType(node)) {
                case 1:
                    return (PC_CountTokens(PC_Child(node, 0)));
                case 2:
                case 3:
                case 4:
                case 5:
                    return (1 + PC_CountTokens(PC_Child(node, 0)) +
                            PC_CountTokens(PC_Child(node, 1)));
            }
        case pnt_shift_expression:
            switch (PC_SubType(node)) {
                case 1:
                    return (PC_CountTokens(PC_Child(node, 0)));
                case 2:
                case 3:
                    return (1 + PC_CountTokens(PC_Child(node, 0)) +
                            PC_CountTokens(PC_Child(node, 1)));
            }
        case pnt_additive_expression:
            switch (PC_SubType(node)) {
                case 1:
                    return (PC_CountTokens(PC_Child(node, 0)));
                case 2:
                case 3:
                    return (1 + PC_CountTokens(PC_Child(node, 0)) +
                            PC_CountTokens(PC_Child(node, 1)));
            }
        case pnt_multiplicative_expression:
            switch (PC_SubType(node)) {
                case 1:
                    return (PC_CountTokens(PC_Child(node, 0)));
                case 2:
                case 3:
                case 4:
                    return (1 + PC_CountTokens(PC_Child(node, 0)) +
                            PC_CountTokens(PC_Child(node, 1)));
            }
        case pnt_cast_expression:
            switch (PC_SubType(node)) {
                case 1:
                    return (PC_CountTokens(PC_Child(node, 0)));
                case 2:
                    return (2 + PC_CountTokens(PC_Child(node, 0)) +
                            PC_CountTokens(PC_Child(node, 1)));
            }
        case pnt_unary_expression:
            switch (PC_SubType(node)) {
                case 1:
                    return (PC_CountTokens(PC_Child(node, 0)));
                case 2:
                case 3:
                case 5:
                    return (1 + PC_CountTokens(PC_Child(node, 0)));
                case 4:
                    return (PC_CountTokens(PC_Child(node, 0)) +
                            PC_CountTokens(PC_Child(node, 1)));
                case 6:
                    return (3 + PC_CountTokens(PC_Child(node, 0)));
            }
        case pnt_unary_operator:      /* Always a token node */
        case pnt_postfix_expression:
            switch (PC_SubType(node)) {
                case 1:
                    return (PC_CountTokens(PC_Child(node, 0)));
                case 2:
                case 4:
                    return (2 + PC_CountTokens(PC_Child(node, 0)) +
                            PC_CountTokens(PC_Child(node, 1)));
                case 3:
                    return (2 + PC_CountTokens(PC_Child(node, 0)));
                case 5:
                case 6:
                    return (1 + PC_CountTokens(PC_Child(node, 0)) +
                            PC_CountTokens(PC_Child(node, 1)));
                case 7:
                case 8:
                    return (1 + PC_CountTokens(PC_Child(node, 0)));
            }
        case pnt_primary_expression:
            switch (PC_SubType(node)) {
                case 1:
                case 2:
                    return (PC_CountTokens(PC_Child(node, 0)));
                case 3:
                    return (1);
                case 4:
                    return (2 + PC_CountTokens(PC_Child(node, 0)));
            }
        case pnt_argument_expression_list:
            switch (PC_SubType(node)) {
                case 1:
                    return (PC_CountTokens(PC_Child(node, 0)));
                case 2:
                    return (1 + PC_CountTokens(PC_Child(node, 0)) +
                            PC_CountTokens(PC_Child(node, 1)));
            }
    }
}

char           *PC_TokenChars(tok)
PC_Token_t     *tok;
{
    static char     buffer[2];

    if (PC_IsCharToken(tok)) {
        buffer[0] = (char) PC_TokenChar(tok);
        buffer[1] = '\0';
        return (buffer);
    }
    if (PC_IsTextToken(tok)) {
        return (PC_TokenText(tok));
    }
    switch (PC_TokenVal(tok)) {
        case PC_ADDASSIGN:
            return ("+=");
        case PC_ANDASSIGN:
            return ("&=");
        case PC_AUTO:
            return ("auto");
        case PC_BREAK:
            return ("break");
        case PC_CASE:
            return ("case");
        case PC_CHAR:
            return ("char");
        case PC_CHARACTER_CONSTANT:   /* Always has text flag set */
        case PC_CONST:
            return ("const");
        case PC_CONTINUE:
            return ("continue");
        case PC_DECR:
            return ("--");
        case PC_DEFAULT:
            return ("default");
        case PC_DEREF:
            return ("->");
        case PC_DIVASSIGN:
            return ("/=");
        case PC_DO:
            return ("do");
        case PC_DOUBLE:
            return ("double");
        case PC_ELLIPSIS:
            return ("...");
        case PC_ELSE:
            return ("else");
        case PC_ENUM:
            return ("enum");
        case PC_ENUMERATION_CONSTANT: /* Always has text flag set */
        case PC_EQUAL:
            return ("==");
        case PC_EXTERN:
            return ("extern");
        case PC_FLOAT:
            return ("float");
        case PC_FLOATING_CONSTANT:    /* Always has text flag set */
        case PC_FOR:
            return ("for");
        case PC_GE:
            return (">=");
        case PC_GOTO:
            return ("goto");
        case PC_IDENTIFIER:            /* Always has text flag set */
        case PC_IF:
            return ("if");
        case PC_INCR:
            return ("++");
        case PC_INT:
            return ("int");
        case PC_INTEGER_CONSTANT:     /* Always has text flag set */
        case PC_LE:
            return ("<=");
        case PC_LEFT:
            return ("<<");
        case PC_LEFTASSIGN:
            return ("<<=");
        case PC_LOGICAL_AND:
            return ("&&");
        case PC_LOGICAL_OR:
            return ("||");
        case PC_LONG:
            return ("long");
        case PC_MODASSIGN:
            return ("%=");
        case PC_MULASSIGN:
            return ("*=");
        case PC_NOT_EQUAL:
            return ("!=");
        case PC_ORASSIGN:
            return ("|=");
        case PC_REGISTER:
            return ("register");
        case PC_RETURN:
            return ("return");
        case PC_RIGHT:
            return (">>");
        case PC_RIGHTASSIGN:
            return (">>=");
        case PC_SHORT:
            return ("short");
        case PC_SIGNED:
            return ("signed");
        case PC_SIZEOF:
            return ("sizeof");
        case PC_STATIC:
            return ("static");
        case PC_STRING_CONSTANT:      /* Always has text flag set */
        case PC_STRUCT:
            return ("struct");
        case PC_SUBASSIGN:
            return ("-=");
        case PC_SWITCH:
            return ("switch");
        case PC_TYPEDEF:
            return ("typedef");
        case PC_TYPEDEF_NAME:          /* Always has text flag set */
        case PC_UNION:
            return ("union");
        case PC_UNSIGNED:
            return ("unsigned");
        case PC_VOID:
            return ("void");
        case PC_VOLATILE:
            return ("volatile");
        case PC_WHILE:
            return ("while");
        case PC_XORASSIGN:
            return ("^=");
    }
}

int             PC_DumpTokens(node, tokvec)
PC_ParseNode_t *node;
PC_Token_t     *tokvec;
{
    int             result;

    if (PC_IsTokenNode(node)) {
        if (PC_IsCharToken(PC_NodeToken(node))) {
            PC_SetCharToken(tokvec, PC_TokenChar(PC_NodeToken(node)));
            return (1);
        }
        if (PC_IsTextToken(PC_NodeToken(node))) {
            PC_SetTextToken(tokvec, PC_TokenVal(PC_NodeToken(node)),
                         PC_TokenText(PC_NodeToken(node)));
            return (1);
        }
        PC_SetToken(tokvec, PC_TokenVal(PC_NodeToken(node)));
        return (1);
    }
    switch (PC_NodeType(node)) {
        case pnt_module:
            switch (PC_SubType(node)) {
                case 1:
                    return (PC_DumpTokens(PC_Child(node, 0), tokvec));
                case 2:
                    result = PC_DumpTokens(PC_Child(node, 0), tokvec);
                    return (result + PC_DumpTokens(PC_Child(node, 1), tokvec + result));
            }
        case pnt_external_declaration:
            return (PC_DumpTokens(PC_Child(node, 0), tokvec));
        case pnt_function_definition:
            switch (PC_SubType(node)) {
                case 1:
                    result = PC_DumpTokens(PC_Child(node, 0), tokvec);
                    return (result + PC_DumpTokens(PC_Child(node, 1), tokvec + result));
                case 2:
                    result = PC_DumpTokens(PC_Child(node, 0), tokvec);
                    result += PC_DumpTokens(PC_Child(node, 1), tokvec + result);
                    return (result + PC_DumpTokens(PC_Child(node, 2), tokvec + result));
            }
        case pnt_function_body:
            switch (PC_SubType(node)) {
                case 1:
                    return (PC_DumpTokens(PC_Child(node, 0), tokvec));
                case 2:
                    result = PC_DumpTokens(PC_Child(node, 0), tokvec);
                    return (result + PC_DumpTokens(PC_Child(node, 1), tokvec + result));
            }
        case pnt_declaration:
            switch (PC_SubType(node)) {
                case 1:
                    result = PC_DumpTokens(PC_Child(node, 0), tokvec);
                    PC_SetCharToken(tokvec + result, ';');
                    return (1 + result);
                case 2:
                    result = PC_DumpTokens(PC_Child(node, 0), tokvec);
                    result += PC_DumpTokens(PC_Child(node, 1), tokvec + result);
                    PC_SetCharToken(tokvec + result, ';');
                    return (1 + result);
            }
        case pnt_declaration_list:
            switch (PC_SubType(node)) {
                case 1:
                    return (PC_DumpTokens(PC_Child(node, 0), tokvec));
                case 2:
                    result = PC_DumpTokens(PC_Child(node, 0), tokvec);
                    return (result + PC_DumpTokens(PC_Child(node, 1), tokvec + result));
            }
        case pnt_declaration_specifiers:
            switch (PC_SubType(node)) {
                case 1:
                case 3:
                case 5:
                    return (PC_DumpTokens(PC_Child(node, 0), tokvec));
                case 2:
                case 4:
                case 6:
                    result = PC_DumpTokens(PC_Child(node, 0), tokvec);
                    return (result + PC_DumpTokens(PC_Child(node, 1), tokvec + result));
            }
        case pnt_storage_class_specifier:       /* Always a token node */
        case pnt_type_specifier:
            switch (PC_SubType(node)) {
                case 10:
                case 11:
                    return (PC_DumpTokens(PC_Child(node, 0), tokvec));
            }
        case pnt_type_qualifier:      /* Always a token node */
        case pnt_struct_or_union_specifier:
            switch (PC_SubType(node)) {
                case 1:
                    result = PC_DumpTokens(PC_Child(node, 0), tokvec);
                    PC_SetCharToken(tokvec + result, '{');
                    result += PC_DumpTokens(PC_Child(node, 1), tokvec + result + 1);
                    PC_SetCharToken(tokvec + result + 1, '}');
                    return (result + 2);
                case 2:
                    result = PC_DumpTokens(PC_Child(node, 0), tokvec);
                    result += PC_DumpTokens(PC_Child(node, 1), tokvec + result);
                    PC_SetCharToken(tokvec + result, '{');
                    result += PC_DumpTokens(PC_Child(node, 2), tokvec + result + 1);
                    PC_SetCharToken(tokvec + result + 1, '}');
                    return (result + 2);
                case 3:
                    result = PC_DumpTokens(PC_Child(node, 0), tokvec);
                    return (result + PC_DumpTokens(PC_Child(node, 1), tokvec + result));
            }
        case pnt_struct_or_union:     /* Always a token node */
        case pnt_struct_declaration_list:
            switch (PC_SubType(node)) {
                case 1:
                    return (PC_DumpTokens(PC_Child(node, 0), tokvec));
                case 2:
                    result = PC_DumpTokens(PC_Child(node, 0), tokvec);
                    return (result + PC_DumpTokens(PC_Child(node, 1), tokvec + result));
            }
        case pnt_init_declarator_list:
            switch (PC_SubType(node)) {
                case 1:
                    return (PC_DumpTokens(PC_Child(node, 0), tokvec));
                case 2:
                    result = PC_DumpTokens(PC_Child(node, 0), tokvec);
                    PC_SetCharToken(tokvec + result, ',');
                    return (1 + result + PC_DumpTokens(PC_Child(node, 1),
                                                       tokvec + result + 1));
            }
        case pnt_init_declarator:
            switch (PC_SubType(node)) {
                case 1:
                    return (PC_DumpTokens(PC_Child(node, 0), tokvec));
                case 2:
                    result = PC_DumpTokens(PC_Child(node, 0), tokvec);
                    PC_SetCharToken(tokvec + result, '=');
                    return (1 + result + PC_DumpTokens(PC_Child(node, 1),
                                                        tokvec + result + 1));
            }
        case pnt_struct_declaration:
            result = PC_DumpTokens(PC_Child(node, 0), tokvec);
            result += PC_DumpTokens(PC_Child(node, 1), tokvec + result);
            PC_SetCharToken(tokvec + result, ';');
            return (1 + result);
        case pnt_specifier_qualifier_list:
            switch (PC_SubType(node)) {
                case 1:
                case 3:
                    return (PC_DumpTokens(PC_Child(node, 0), tokvec));
                case 2:
                case 4:
                    result = PC_DumpTokens(PC_Child(node, 0), tokvec);
                    return (result + PC_DumpTokens(PC_Child(node, 1), tokvec + result));
            }
        case pnt_struct_declarator_list:
            switch (PC_SubType(node)) {
                case 1:
                    return (PC_DumpTokens(PC_Child(node, 0), tokvec));
                case 2:
                    result = PC_DumpTokens(PC_Child(node, 0), tokvec);
                    PC_SetCharToken(tokvec + result, ',');
                    return (1 + result + PC_DumpTokens(PC_Child(node, 1),
                                                       tokvec + result + 1));
            }
        case pnt_struct_declarator:
            switch (PC_SubType(node)) {
                case 1:
                    return (PC_DumpTokens(PC_Child(node, 0), tokvec));
                case 2:
                    PC_SetCharToken(tokvec, ':');
                    return (1 + PC_DumpTokens(PC_Child(node, 0), tokvec + 1));
                case 3:
                    result = PC_DumpTokens(PC_Child(node, 0), tokvec);
                    PC_SetCharToken(tokvec + result, ':');
                    return (1 + result + PC_DumpTokens(PC_Child(node, 1),
                                                       tokvec + result + 1));
            }
        case pnt_enum_specifier:
            switch (PC_SubType(node)) {
                case 1:
                    PC_SetToken(tokvec, PC_ENUM);
                    PC_SetCharToken(tokvec + 1, '{');
                    result = PC_DumpTokens(PC_Child(node, 0), tokvec + 2);
                    PC_SetCharToken(tokvec + result + 2, '}');
                    return (3 + result);
                case 2:
                    PC_SetToken(tokvec, PC_ENUM);
                    result = PC_DumpTokens(PC_Child(node, 0), tokvec + 1);
                    PC_SetCharToken(tokvec + result + 1, '{');
                    result += PC_DumpTokens(PC_Child(node, 1), tokvec + result + 2);
                    PC_SetCharToken(tokvec + result + 2, '}');
                    return (3 + result);
                case 3:
                    PC_SetToken(tokvec, PC_ENUM);
                    return (1 + PC_DumpTokens(PC_Child(node, 0), tokvec + 1));
            }
        case pnt_enumerator_list:
            switch (PC_SubType(node)) {
                case 1:
                    return (PC_DumpTokens(PC_Child(node, 0), tokvec));
                case 2:
                    result = PC_DumpTokens(PC_Child(node, 0), tokvec);
                    PC_SetCharToken(tokvec + result, ',');
                    return (1 + result + PC_DumpTokens(PC_Child(node, 1),
                                                       tokvec + result + 1));
            }
        case pnt_enumerator:
            switch (PC_SubType(node)) {
                case 1:
                    return (PC_DumpTokens(PC_Child(node, 0), tokvec));
                case 2:
                    result = PC_DumpTokens(PC_Child(node, 0), tokvec);
                    PC_SetCharToken(tokvec + result, '=');
                    return (1 + result + PC_DumpTokens(PC_Child(node, 1),
                                                       tokvec + result + 1));
            }
        case pnt_declarator:
            switch (PC_SubType(node)) {
                case 1:
                    return (PC_DumpTokens(PC_Child(node, 0), tokvec));
                case 2:
                    result = PC_DumpTokens(PC_Child(node, 0), tokvec);
                    return (result + PC_DumpTokens(PC_Child(node, 1), tokvec + result));
            }
        case pnt_direct_declarator:
            switch (PC_SubType(node)) {
                case 1:
                    return (PC_DumpTokens(PC_Child(node, 0), tokvec));
                case 2:
                    PC_SetCharToken(tokvec, '(');
                    result = PC_DumpTokens(PC_Child(node, 0), tokvec + 1);
                    PC_SetCharToken(tokvec + result + 1, ')');
                    return (2 + result);
                case 3:
                    result = PC_DumpTokens(PC_Child(node, 0), tokvec);
                    PC_SetCharToken(tokvec + result, '[');
                    PC_SetCharToken(tokvec + result + 1, ']');
                    return (2 + result);
                case 4:
                    result = PC_DumpTokens(PC_Child(node, 0), tokvec);
                    PC_SetCharToken(tokvec + result, '[');
                    result += PC_DumpTokens(PC_Child(node, 1), tokvec + result + 1);
                    PC_SetCharToken(tokvec + result + 1, ']');
                    return (2 + result);
                case 5:
                case 7:
                    result = PC_DumpTokens(PC_Child(node, 0), tokvec);
                    PC_SetCharToken(tokvec + result, '(');
                    result += PC_DumpTokens(PC_Child(node, 1), tokvec + result + 1);
                    PC_SetCharToken(tokvec + result + 1, ')');
                    return (2 + result);
                case 6:
                    result = PC_DumpTokens(PC_Child(node, 0), tokvec);
                    PC_SetCharToken(tokvec + result, '(');
                    PC_SetCharToken(tokvec + result + 1, ')');
                    return (2 + result);
            }
        case pnt_pointer:
            switch (PC_SubType(node)) {
                case 1:
                    PC_SetCharToken(tokvec, '*');
                    return (1);
                case 2:
                case 3:
                    PC_SetCharToken(tokvec, '*');
                    return (1 + PC_DumpTokens(PC_Child(node, 0), tokvec + 1));
                case 4:
                    PC_SetCharToken(tokvec, '*');
                    result = PC_DumpTokens(PC_Child(node, 0), tokvec + 1);
                    return (1 + result + PC_DumpTokens(PC_Child(node, 1),
                                                       tokvec + result + 1));
            }
        case pnt_type_qualifier_list:
            switch (PC_SubType(node)) {
                case 1:
                    return (PC_DumpTokens(PC_Child(node, 0), tokvec));
                case 2:
                    result = PC_DumpTokens(PC_Child(node, 0), tokvec);
                    return (result + PC_DumpTokens(PC_Child(node, 1), tokvec + result));
            }
        case pnt_parameter_type_list:
            switch (PC_SubType(node)) {
                case 1:
                    return (PC_DumpTokens(PC_Child(node, 0), tokvec));
                case 2:
                    result = PC_DumpTokens(PC_Child(node, 0), tokvec);
                    PC_SetCharToken(tokvec + result, ',');
                    PC_SetToken(tokvec + result + 1, PC_ELLIPSIS);
                    return (2 + result);
            }
        case pnt_parameter_list:
            switch (PC_SubType(node)) {
                case 1:
                    return (PC_DumpTokens(PC_Child(node, 0), tokvec));
                case 2:
                    result = PC_DumpTokens(PC_Child(node, 0), tokvec);
                    PC_SetCharToken(tokvec + result, ',');
                    return (1 + result + PC_DumpTokens(PC_Child(node, 1),
                                                       tokvec + result + 1));
            }
        case pnt_parameter_declaration:
            switch (PC_SubType(node)) {
                case 1:
                case 3:
                    result = PC_DumpTokens(PC_Child(node, 0), tokvec);
                    return (result + PC_DumpTokens(PC_Child(node, 1), tokvec + result));
                case 2:
                    return (PC_DumpTokens(PC_Child(node, 0), tokvec));
            }
        case pnt_identifier_list:
            switch (PC_SubType(node)) {
                case 1:
                    return (PC_DumpTokens(PC_Child(node, 0), tokvec));
                case 2:
                    result = PC_DumpTokens(PC_Child(node, 0), tokvec);
                    PC_SetCharToken(tokvec + result, ',');
                    return (1 + result + PC_DumpTokens(PC_Child(node, 1),
                                                       tokvec + result + 1));
            }
        case pnt_initializer:
            switch (PC_SubType(node)) {
                case 1:
                    return (PC_DumpTokens(PC_Child(node, 0), tokvec));
                case 2:
                    PC_SetCharToken(tokvec, '{');
                    result = PC_DumpTokens(PC_Child(node, 0), tokvec + 1);
                    PC_SetCharToken(tokvec + result + 1, '}');
                    return (2 + result);
                case 3:
                    PC_SetCharToken(tokvec, '{');
                    result = PC_DumpTokens(PC_Child(node, 0), tokvec + 1);
                    PC_SetCharToken(tokvec + result + 1, ',');
                    PC_SetCharToken(tokvec + result + 2, '}');
                    return (3 + result);
            }
        case pnt_initializer_list:
            switch (PC_SubType(node)) {
                case 1:
                    return (PC_DumpTokens(PC_Child(node, 0), tokvec));
                case 2:
                    result = PC_DumpTokens(PC_Child(node, 0), tokvec);
                    PC_SetCharToken(tokvec + result, ',');
                    return (1 + result + PC_DumpTokens(PC_Child(node, 1),
                                                       tokvec + result + 1));
            }
        case pnt_type_name:
            switch (PC_SubType(node)) {
                case 1:
                    return (PC_DumpTokens(PC_Child(node, 0), tokvec));
                case 2:
                    result = PC_DumpTokens(PC_Child(node, 0), tokvec);
                    return (result + PC_DumpTokens(PC_Child(node, 1), tokvec + result));
            }
        case pnt_abstract_declarator:
            switch (PC_SubType(node)) {
                case 1:
                case 2:
                    return (PC_DumpTokens(PC_Child(node, 0), tokvec));
                case 3:
                    result = PC_DumpTokens(PC_Child(node, 0), tokvec);
                    return (result + PC_DumpTokens(PC_Child(node, 1), tokvec + result));
            }
        case pnt_direct_abstract_declarator:
            switch (PC_SubType(node)) {
                case 1:
                    PC_SetCharToken(tokvec, '(');
                    result = PC_DumpTokens(PC_Child(node, 0), tokvec + 1);
                    PC_SetCharToken(tokvec + result + 1, ')');
                    return (2 + result);
                case 2:
                    PC_SetCharToken(tokvec, '[');
                    PC_SetCharToken(tokvec + 1, ']');
                    return (2);
                case 3:
                    PC_SetCharToken(tokvec, '[');
                    result = PC_DumpTokens(PC_Child(node, 0), tokvec + 1);
                    PC_SetCharToken(tokvec + result + 1, ']');
                    return (2 + result);
                case 4:
                    result = PC_DumpTokens(PC_Child(node, 0), tokvec);
                    PC_SetCharToken(tokvec + result, '[');
                    PC_SetCharToken(tokvec + result + 1, ']');
                    return (2 + result);
                case 5:
                    result = PC_DumpTokens(PC_Child(node, 0), tokvec);
                    PC_SetCharToken(tokvec + result, '[');
                    result += PC_DumpTokens(PC_Child(node, 1), tokvec + result + 1);
                    PC_SetCharToken(tokvec + result + 1, ']');
                    return (2 + result);
                case 6:
                    PC_SetCharToken(tokvec, '(');
                    PC_SetCharToken(tokvec + 1, ')');
                    return (2);
                case 7:
                    PC_SetCharToken(tokvec, '(');
                    result = PC_DumpTokens(PC_Child(node, 0), tokvec + 1);
                    PC_SetCharToken(tokvec + result + 1, ')');
                    return (2 + result);
                case 8:
                    result = PC_DumpTokens(PC_Child(node, 0), tokvec);
                    PC_SetCharToken(tokvec + result, '(');
                    PC_SetCharToken(tokvec + result + 1, ')');
                    return (2 + result);
                case 9:
                    result = PC_DumpTokens(PC_Child(node, 0), tokvec);
                    PC_SetCharToken(tokvec + result, '(');
                    result += PC_DumpTokens(PC_Child(node, 1), tokvec + result + 1);
                    PC_SetCharToken(tokvec + result + 1, ')');
                    return (2 + result);
            }
        case pnt_statement:
            return (PC_DumpTokens(PC_Child(node, 0), tokvec));
        case pnt_labeled_statement:
            switch (PC_SubType(node)) {
                case 1:
                    result = PC_DumpTokens(PC_Child(node, 0), tokvec);
                    PC_SetCharToken(tokvec + result, ':');
                    return (1 + result + PC_DumpTokens(PC_Child(node, 1),
                                                       tokvec + result + 1));
                case 2:
                    PC_SetToken(tokvec, PC_CASE);
                    result = PC_DumpTokens(PC_Child(node, 0), tokvec + 1);
                    PC_SetCharToken(tokvec + result + 1, ':');
                    return (2 + result + PC_DumpTokens(PC_Child(node, 1),
                                                       tokvec + result + 2));
                case 3:
                    PC_SetToken(tokvec, PC_DEFAULT);
                    PC_SetCharToken(tokvec + 1, ':');
                    return (2 + PC_DumpTokens(PC_Child(node, 0), tokvec + 2));
            }
        case pnt_expression_statement:
            switch (PC_SubType(node)) {
                case 1:
                    PC_SetCharToken(tokvec, ';');
                    return (1);
                case 2:
                    result = PC_DumpTokens(PC_Child(node, 0), tokvec);
                    PC_SetCharToken(tokvec + result, ';');
                    return (1 + result);
            }
        case pnt_compound_statement:
            switch (PC_SubType(node)) {
                case 1:
                    PC_SetCharToken(tokvec, '{');
                    PC_SetCharToken(tokvec + 1, '}');
                    return (2);
                case 2:
                case 3:
                    PC_SetCharToken(tokvec, '{');
                    result = PC_DumpTokens(PC_Child(node, 0), tokvec + 1);
                    PC_SetCharToken(tokvec + result + 1, '}');
                    return (2 + result);
                case 4:
                    PC_SetCharToken(tokvec, '{');
                    result = PC_DumpTokens(PC_Child(node, 0), tokvec + 1);
                    result += PC_DumpTokens(PC_Child(node, 1), tokvec + result + 1);
                    PC_SetCharToken(tokvec + result + 1, '}');
                    return (2 + result);
            }
        case pnt_statement_list:
            switch (PC_SubType(node)) {
                case 1:
                    return (PC_DumpTokens(PC_Child(node, 0), tokvec));
                case 2:
                    result = PC_DumpTokens(PC_Child(node, 0), tokvec);
                    return (result + PC_DumpTokens(PC_Child(node, 1), tokvec + result));
            }
        case pnt_selection_statement:
            switch (PC_SubType(node)) {
                case 1:
                    PC_SetToken(tokvec, PC_IF);
                    PC_SetCharToken(tokvec + 1, '(');
                    result = PC_DumpTokens(PC_Child(node, 0), tokvec + 2);
                    PC_SetCharToken(tokvec + result + 2, ')');
                    return (3 + result + PC_DumpTokens(PC_Child(node, 1),
                                                       tokvec + result + 3));
                case 2:
                    PC_SetToken(tokvec, PC_IF);
                    PC_SetCharToken(tokvec + 1, '(');
                    result = PC_DumpTokens(PC_Child(node, 0), tokvec + 2);
                    PC_SetCharToken(tokvec + result + 2, ')');
                    result += PC_DumpTokens(PC_Child(node, 1),
                                            tokvec + result + 3);
                    PC_SetToken(tokvec + result + 3, PC_ELSE);
                    return (4 + result + PC_DumpTokens(PC_Child(node, 2),
                                              tokvec + result + 4));
                case 3:
                    PC_SetToken(tokvec, PC_SWITCH);
                    PC_SetCharToken(tokvec + 1, '(');
                    result = PC_DumpTokens(PC_Child(node, 0), tokvec + 2);
                    PC_SetCharToken(tokvec + result + 2, ')');
                    return (3 + result + PC_DumpTokens(PC_Child(node, 1),
                                                       tokvec + result + 3));
            }
        case pnt_iteration_statement:
            switch (PC_SubType(node)) {
                case 1:
                    PC_SetToken(tokvec, PC_WHILE);
                    PC_SetCharToken(tokvec + 1, '(');
                    result = PC_DumpTokens(PC_Child(node, 0), tokvec + 2);
                    PC_SetCharToken(tokvec + result + 2, ')');
                    return (3 + result + PC_DumpTokens(PC_Child(node, 1),
                                                       tokvec + result + 3));
                case 2:
                    PC_SetToken(tokvec, PC_DO);
                    result = PC_DumpTokens(PC_Child(node, 0), tokvec + 1);
                    PC_SetToken(tokvec + result + 1, PC_WHILE);
                    PC_SetCharToken(tokvec + result + 2, '(');
                    result += PC_DumpTokens(PC_Child(node, 1), tokvec + result + 3);
                    PC_SetCharToken(tokvec + result + 3, ')');
                    PC_SetCharToken(tokvec + result + 4, ';');
                    return (5 + result);
                case 3:
                    PC_SetToken(tokvec, PC_FOR);
                    PC_SetCharToken(tokvec + 1, '(');
                    PC_SetCharToken(tokvec + 2, ';');
                    PC_SetCharToken(tokvec + 3, ';');
                    PC_SetCharToken(tokvec + 4, ')');
                    return (5 + PC_DumpTokens(PC_Child(node, 0), tokvec + 5));
                case 4:
                    PC_SetToken(tokvec, PC_FOR);
                    PC_SetCharToken(tokvec + 1, '(');
                    result = PC_DumpTokens(PC_Child(node, 0), tokvec + 2);
                    PC_SetCharToken(tokvec + result + 2, ';');
                    PC_SetCharToken(tokvec + result + 3, ';');
                    PC_SetCharToken(tokvec + result + 4, ')');
                    return (5 + result + PC_DumpTokens(PC_Child(node, 1),
                                                       tokvec + result + 5));
                case 5:
                    PC_SetToken(tokvec, PC_FOR);
                    PC_SetCharToken(tokvec + 1, '(');
                    PC_SetCharToken(tokvec + 2, ';');
                    result = PC_DumpTokens(PC_Child(node, 0), tokvec + 3);
                    PC_SetCharToken(tokvec + result + 3, ';');
                    PC_SetCharToken(tokvec + result + 4, ')');
                    return (5 + result + PC_DumpTokens(PC_Child(node, 1),
                                                       tokvec + result + 5));
                case 6:
                    PC_SetToken(tokvec, PC_FOR);
                    PC_SetCharToken(tokvec + 1, '(');
                    PC_SetCharToken(tokvec + 2, ';');
                    PC_SetCharToken(tokvec + 3, ';');
                    result = PC_DumpTokens(PC_Child(node, 0), tokvec + 4);
                    PC_SetCharToken(tokvec + result + 4, ')');
                    return (5 + result + PC_DumpTokens(PC_Child(node, 1),
                                                       tokvec + result + 5));
                case 7:
                    PC_SetToken(tokvec, PC_FOR);
                    PC_SetCharToken(tokvec + 1, '(');
                    PC_SetCharToken(tokvec + 2, ';');
                    result = PC_DumpTokens(PC_Child(node, 0), tokvec + 3);
                    PC_SetCharToken(tokvec + result + 3, ';');
                    result += PC_DumpTokens(PC_Child(node, 1), tokvec + result + 4);
                    PC_SetCharToken(tokvec + result + 4, ')');
                    return (5 + result + PC_DumpTokens(PC_Child(node, 2),
                                                       tokvec + result + 5));
                case 8:
                    PC_SetToken(tokvec, PC_FOR);
                    PC_SetCharToken(tokvec + 1, '(');
                    result = PC_DumpTokens(PC_Child(node, 0), tokvec + 2);
                    PC_SetCharToken(tokvec + result + 2, ';');
                    PC_SetCharToken(tokvec + result + 3, ';');
                    result += PC_DumpTokens(PC_Child(node, 1), tokvec + result + 4);
                    PC_SetCharToken(tokvec + result + 4, ')');
                    return (5 + result + PC_DumpTokens(PC_Child(node, 2),
                                                       tokvec + result + 5));

                case 9:
                    PC_SetToken(tokvec, PC_FOR);
                    PC_SetCharToken(tokvec + 1, '(');
                    result = PC_DumpTokens(PC_Child(node, 0), tokvec + 2);
                    PC_SetCharToken(tokvec + result + 2, ';');
                    result += PC_DumpTokens(PC_Child(node, 1), tokvec + result + 3);
                    PC_SetCharToken(tokvec + result + 3, ';');
                    PC_SetCharToken(tokvec + result + 4, ')');
                    return (5 + result + PC_DumpTokens(PC_Child(node, 2),
                                                       tokvec + result + 5));
                case 10:
                    PC_SetToken(tokvec, PC_FOR);
                    PC_SetCharToken(tokvec + 1, '(');
                    result = PC_DumpTokens(PC_Child(node, 0), tokvec + 2);
                    PC_SetCharToken(tokvec + result + 2, ';');
                    result += PC_DumpTokens(PC_Child(node, 1), tokvec + result + 3);
                    PC_SetCharToken(tokvec + result + 3, ';');
                    result += PC_DumpTokens(PC_Child(node, 2), tokvec + result + 4);
                    PC_SetCharToken(tokvec + result + 4, ')');
                    return (5 + result + PC_DumpTokens(PC_Child(node, 3),
                                                       tokvec + result + 5));
            }
        case pnt_jump_statement:
            switch (PC_SubType(node)) {
                case 1:
                    PC_SetToken(tokvec, PC_GOTO);
                    result = PC_DumpTokens(PC_Child(node, 0), tokvec + 1);
                    PC_SetCharToken(tokvec + result + 1, ';');
                    return (2 + result);
                case 2:
                    PC_SetToken(tokvec, PC_CONTINUE);
                    PC_SetCharToken(tokvec + 1, ';');
                    return (2);
                case 3:
                    PC_SetToken(tokvec, PC_BREAK);
                    PC_SetCharToken(tokvec + 1, ';');
                    return (2);
                case 4:
                    PC_SetToken(tokvec, PC_RETURN);
                    PC_SetCharToken(tokvec + 1, ';');
                    return (2);
                case 5:
                    PC_SetToken(tokvec, PC_RETURN);
                    result = PC_DumpTokens(PC_Child(node, 0), tokvec + 1);
                    PC_SetCharToken(tokvec + result + 1, ';');
                    return (2 + result);
            }
        case pnt_expression:
            switch (PC_SubType(node)) {
                case 1:
                    return (PC_DumpTokens(PC_Child(node, 0), tokvec));
                case 2:
                    result = PC_DumpTokens(PC_Child(node, 0), tokvec);
                    PC_SetCharToken(tokvec + result, ',');
                    return (1 + result + PC_DumpTokens(PC_Child(node, 1),
                                                       tokvec + result + 1));
            }
        case pnt_assignment_expression:
            switch (PC_SubType(node)) {
                case 1:
                    return (PC_DumpTokens(PC_Child(node, 0), tokvec));
                case 2:
                    result = PC_DumpTokens(PC_Child(node, 0), tokvec);
                    result += PC_DumpTokens(PC_Child(node, 1), tokvec + result);
                    return (result + PC_DumpTokens(PC_Child(node, 2), tokvec + result));
            }
        case pnt_assignment_operator: /* Always a token node */
        case pnt_conditional_expression:
            switch (PC_SubType(node)) {
                case 1:
                    return (PC_DumpTokens(PC_Child(node, 0), tokvec));
                case 2:
                    result = PC_DumpTokens(PC_Child(node, 0), tokvec);
                    PC_SetCharToken(tokvec + result, '?');
                    result += PC_DumpTokens(PC_Child(node, 1), tokvec + result + 1);
                    PC_SetCharToken(tokvec + result + 1, ':');
                    return (2 + result + PC_DumpTokens(PC_Child(node, 2),
                                                       tokvec + result + 2));
            }
        case pnt_constant_expression:
            return (PC_DumpTokens(PC_Child(node, 0), tokvec));
        case pnt_logical_or_expression:
            switch (PC_SubType(node)) {
                case 1:
                    return (PC_DumpTokens(PC_Child(node, 0), tokvec));
                case 2:
                    result = PC_DumpTokens(PC_Child(node, 0), tokvec);
                    PC_SetToken(tokvec + result, PC_LOGICAL_OR);
                    return (1 + result + PC_DumpTokens(PC_Child(node, 1),
                                                       tokvec + result + 1));
            }
        case pnt_logical_and_expression:
            switch (PC_SubType(node)) {
                case 1:
                    return (PC_DumpTokens(PC_Child(node, 0), tokvec));
                case 2:
                    result = PC_DumpTokens(PC_Child(node, 0), tokvec);
                    PC_SetToken(tokvec + result, PC_LOGICAL_AND);
                    return (1 + result + PC_DumpTokens(PC_Child(node, 1),
                                                       tokvec + result + 1));
            }
        case pnt_inclusive_or_expression:
            switch (PC_SubType(node)) {
                case 1:
                    return (PC_DumpTokens(PC_Child(node, 0), tokvec));
                case 2:
                    result = PC_DumpTokens(PC_Child(node, 0), tokvec);
                    PC_SetCharToken(tokvec + result, '|');
                    return (1 + result + PC_DumpTokens(PC_Child(node, 1),
                                                       tokvec + result + 1));
            }
        case pnt_exclusive_or_expression:
            switch (PC_SubType(node)) {
                case 1:
                    return (PC_DumpTokens(PC_Child(node, 0), tokvec));
                case 2:
                    result = PC_DumpTokens(PC_Child(node, 0), tokvec);
                    PC_SetCharToken(tokvec + result, '^');
                    return (1 + result + PC_DumpTokens(PC_Child(node, 1),
                                                       tokvec + result + 1));
            }
        case pnt_and_expression:
            switch (PC_SubType(node)) {
                case 1:
                    return (PC_DumpTokens(PC_Child(node, 0), tokvec));
                case 2:
                    result = PC_DumpTokens(PC_Child(node, 0), tokvec);
                    PC_SetCharToken(tokvec + result, '&');
                    return (1 + result + PC_DumpTokens(PC_Child(node, 1),
                                                       tokvec + result + 1));
            }
        case pnt_equality_expression:
            switch (PC_SubType(node)) {
                case 1:
                    return (PC_DumpTokens(PC_Child(node, 0), tokvec));
                case 2:
                    result = PC_DumpTokens(PC_Child(node, 0), tokvec);
                    PC_SetToken(tokvec + result, PC_EQUAL);
                    return (1 + result + PC_DumpTokens(PC_Child(node, 1),
                                                       tokvec + result + 1));
                case 3:
                    result = PC_DumpTokens(PC_Child(node, 0), tokvec);
                    PC_SetToken(tokvec + result, PC_NOT_EQUAL);
                    return (1 + result + PC_DumpTokens(PC_Child(node, 1),
                                                       tokvec + result + 1));
            }
        case pnt_relational_expression:
            switch (PC_SubType(node)) {
                case 1:
                    return (PC_DumpTokens(PC_Child(node, 0), tokvec));
                case 2:
                    result = PC_DumpTokens(PC_Child(node, 0), tokvec);
                    PC_SetCharToken(tokvec + result, '<');
                    return (1 + result + PC_DumpTokens(PC_Child(node, 1),
                                                       tokvec + result + 1));
                case 3:
                    result = PC_DumpTokens(PC_Child(node, 0), tokvec);
                    PC_SetCharToken(tokvec + result, '>');
                    return (1 + result + PC_DumpTokens(PC_Child(node, 1),
                                                       tokvec + result + 1));
                case 4:
                    result = PC_DumpTokens(PC_Child(node, 0), tokvec);
                    PC_SetToken(tokvec + result, PC_LE);
                    return (1 + result + PC_DumpTokens(PC_Child(node, 1),
                                                       tokvec + result + 1));
                case 5:
                    result = PC_DumpTokens(PC_Child(node, 0), tokvec);
                    PC_SetToken(tokvec + result, PC_GE);
                    return (1 + result + PC_DumpTokens(PC_Child(node, 1),
                                                       tokvec + result + 1));
            }
        case pnt_shift_expression:
            switch (PC_SubType(node)) {
                case 1:
                    return (PC_DumpTokens(PC_Child(node, 0), tokvec));
                case 2:
                    result = PC_DumpTokens(PC_Child(node, 0), tokvec);
                    PC_SetToken(tokvec + result, PC_LEFT);
                    return (1 + result + PC_DumpTokens(PC_Child(node, 1),
                                                       tokvec + result + 1));
                case 3:
                    result = PC_DumpTokens(PC_Child(node, 0), tokvec);
                    PC_SetToken(tokvec + result, PC_RIGHT);
                    return (1 + result + PC_DumpTokens(PC_Child(node, 1),
                                                       tokvec + result + 1));
            }
        case pnt_additive_expression:
            switch (PC_SubType(node)) {
                case 1:
                    return (PC_DumpTokens(PC_Child(node, 0), tokvec));
                case 2:
                    result = PC_DumpTokens(PC_Child(node, 0), tokvec);
                    PC_SetCharToken(tokvec + result, '+');
                    return (1 + result + PC_DumpTokens(PC_Child(node, 1),
                                                       tokvec + result + 1));
                case 3:
                    result = PC_DumpTokens(PC_Child(node, 0), tokvec);
                    PC_SetCharToken(tokvec + result, '-');
                    return (1 + result + PC_DumpTokens(PC_Child(node, 1),
                                                       tokvec + result + 1));
            }
        case pnt_multiplicative_expression:
            switch (PC_SubType(node)) {
                case 1:
                    return (PC_DumpTokens(PC_Child(node, 0), tokvec));
                case 2:
                    result = PC_DumpTokens(PC_Child(node, 0), tokvec);
                    PC_SetCharToken(tokvec + result, '*');
                    return (1 + result + PC_DumpTokens(PC_Child(node, 1),
                                                       tokvec + result + 1));
                case 3:
                    result = PC_DumpTokens(PC_Child(node, 0), tokvec);
                    PC_SetCharToken(tokvec + result, '/');
                    return (1 + result + PC_DumpTokens(PC_Child(node, 1),
                                                       tokvec + result + 1));
                case 4:
                    result = PC_DumpTokens(PC_Child(node, 0), tokvec);
                    PC_SetCharToken(tokvec + result, '%');
                    return (1 + result + PC_DumpTokens(PC_Child(node, 1),
                                                       tokvec + result + 1));
            }
        case pnt_cast_expression:
            switch (PC_SubType(node)) {
                case 1:
                    return (PC_DumpTokens(PC_Child(node, 0), tokvec));
                case 2:
                    PC_SetCharToken(tokvec, '(');
                    result = PC_DumpTokens(PC_Child(node, 0), tokvec + 1);
                    PC_SetCharToken(tokvec + result + 1, ')');
                    return (2 + result + PC_DumpTokens(PC_Child(node, 1),
                                                       tokvec + result + 2));
            }
        case pnt_unary_expression:
            switch (PC_SubType(node)) {
                case 1:
                    return (PC_DumpTokens(PC_Child(node, 0), tokvec));
                case 2:
                    PC_SetToken(tokvec, PC_INCR);
                    return (1 + PC_DumpTokens(PC_Child(node, 0), tokvec + 1));
                case 3:
                    PC_SetToken(tokvec, PC_DECR);
                    return (1 + PC_DumpTokens(PC_Child(node, 0), tokvec + 1));
                case 4:
                    result = PC_DumpTokens(PC_Child(node, 0), tokvec);
                    return (result + PC_DumpTokens(PC_Child(node, 1), tokvec + result));
                case 5:
                    PC_SetToken(tokvec, PC_SIZEOF);
                    return (1 + PC_DumpTokens(PC_Child(node, 0), tokvec + 1));
                case 6:
                    PC_SetToken(tokvec, PC_SIZEOF);
                    PC_SetCharToken(tokvec + 1, '(');
                    result = PC_DumpTokens(PC_Child(node, 0), tokvec + 2);
                    PC_SetCharToken(tokvec + result + 2, ')');
                    return (3 + result);
            }
        case pnt_unary_operator:      /* Always a token node */
        case pnt_postfix_expression:
            switch (PC_SubType(node)) {
                case 1:
                    return (PC_DumpTokens(PC_Child(node, 0), tokvec));
                case 2:
                    result = PC_DumpTokens(PC_Child(node, 0), tokvec);
                    PC_SetCharToken(tokvec + result, '[');
                    result += PC_DumpTokens(PC_Child(node, 1), tokvec + result + 1);
                    PC_SetCharToken(tokvec + result + 1, ']');
                    return (2 + result);
                case 3:
                    result = PC_DumpTokens(PC_Child(node, 0), tokvec);
                    PC_SetCharToken(tokvec + result, '(');
                    PC_SetCharToken(tokvec + result + 1, ')');
                    return (2 + result);
                case 4:
                    result = PC_DumpTokens(PC_Child(node, 0), tokvec);
                    PC_SetCharToken(tokvec + result, '(');
                    result += PC_DumpTokens(PC_Child(node, 1), tokvec + result + 1);
                    PC_SetCharToken(tokvec + result + 1, ')');
                    return (2 + result);
                case 5:
                    result = PC_DumpTokens(PC_Child(node, 0), tokvec);
                    PC_SetCharToken(tokvec + result, '.');
                    return (1 + result + PC_DumpTokens(PC_Child(node, 1),
                                                       tokvec + result + 1));
                case 6:
                    result = PC_DumpTokens(PC_Child(node, 0), tokvec);
                    PC_SetToken(tokvec + result, PC_DEREF);
                    return (1 + result + PC_DumpTokens(PC_Child(node, 1),
                                                       tokvec + result + 1));
                case 7:
                    result = PC_DumpTokens(PC_Child(node, 0), tokvec);
                    PC_SetToken(tokvec + result, PC_INCR);
                    return (1 + result);
                case 8:
                    result = PC_DumpTokens(PC_Child(node, 0), tokvec);
                    PC_SetToken(tokvec + result, PC_DECR);
                    return (1 + result);
            }
        case pnt_primary_expression:
            switch (PC_SubType(node)) {
                case 1:
                case 2:
                    return (PC_DumpTokens(PC_Child(node, 0), tokvec));
                case 4:
                    PC_SetCharToken(tokvec, '(');
                    result = PC_DumpTokens(PC_Child(node, 0), tokvec + 1);
                    PC_SetCharToken(tokvec + result + 1, ')');
                    return (2 + result);
            }
        case pnt_argument_expression_list:
            switch (PC_SubType(node)) {
                case 1:
                    return (PC_DumpTokens(PC_Child(node, 0), tokvec));
                case 2:
                    result = PC_DumpTokens(PC_Child(node, 0), tokvec);
                    PC_SetCharToken(tokvec + result, ',');
                    return (1 + result + PC_DumpTokens(PC_Child(node, 1),
                                                       tokvec + result + 1));
            }
        case pnt_constant:             /* Always a token node */
        case pnt_identifier:           /* Always a token node */
            break;
    }
}
