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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/parsec/RCS/yyhide.c,v 1.9 1993/11/11 23:29:49 gk5g Exp $";
#endif


#include <andrewos.h>
#include <stdio.h>
#include <parsec.h>
#include <sys/errno.h>

#if SY_B4x
#include <sys/resource.h>
#endif /* SY_B4x */

#define NameList_Size(n) ((n)->used)

#define NAMELIST_GROWSIZE (4)

#define DependListEntry_Node(d) ((d)->defnode)
#define DependListEntry_SubNode(d) ((d)->subdefnode)
#define DependListEntry_Name(d) ((d)->name)
#define DependListEntry_NameList(d) (&((d)->dependencies))

#define DependList_Entry(l,i) (&((l)->entries[(i)]))
#define DependList_Size(l) ((l)->used)
#define DependList_Name(l,i) (DependListEntry_Name(&((l)->entries[(i)])))

#define DEPENDLIST_GROWSIZE (32)

#define OrderListEntry_WhichList(e) ((e)->whichList)
#define OrderListEntry_Index(e) ((e)->index)

#define OrderList_WhichList(o,i) ((o)->entries[(i)].whichList)
#define OrderList_Index(o,i) ((o)->entries[(i)].index)
#define OrderList_Size(o) ((o)->used)

#define ORDERLIST_GROWSIZE (32)

typedef struct {
    int             used, allocated;
    char          **entries;
}               NameList_t;

typedef struct {
    char           *name;
    PC_ParseNode_t *defnode, *subdefnode;       /* Defnode is always either a
                                                 * declaration or a
                                                 * function_definition.  If
                                                 * defnode is a declaration,
                                                 * then subdefnode is the
                                                 * specific element of the
                                                 * init_declarator_list
                                                 * containing name */
    NameList_t      dependencies;
}               DependListEntry_t;

typedef struct {
    int             used, allocated;
    DependListEntry_t *entries;
}               DependList_t;

typedef struct {
    DependList_t   *whichList;
    int             index;
}               OrderListEntry_t;

typedef struct {
    int             used, allocated;
    OrderListEntry_t *entries;
}               OrderList_t;

static DependList_t externNames, nonExternNames, *SaveList;
static OrderList_t orderedDecls;
static PC_ParseNode_t *Tree, *SaveNode;
static char    *ProgramName;
static int      SaveIndex, numSavedTokens;
static PC_Token_t *SavedTokens;
static int      ContainsTypedef();
extern int errno;

extern char	*sys_errlist[];

static void     OutOfMemory(str)
char           *str;
{
    fprintf(stderr, "%s: Out of memory in function \"%s\"\n", ProgramName, str);
    exit(1);
}

static void     NameList_GrowIfNecessary(list)
NameList_t     *list;
{
    if (list->used == list->allocated) {
        if (list->allocated) {
            if (!(list->entries = (char **) realloc(list->entries, (list->allocated + NAMELIST_GROWSIZE) * (sizeof(char *)))))
                OutOfMemory("NameList_GrowIfNecessary");
        }
        else {
            if (!(list->entries = (char **) malloc(NAMELIST_GROWSIZE * (sizeof(char *)))))
                OutOfMemory("NameList_GrowIfNecessary");
        }
        list->allocated += NAMELIST_GROWSIZE;
    }
}

static void     NameList_Init(list)
NameList_t     *list;
{
    list->used = list->allocated = 0;
    list->entries = (char **) 0;
}

static void     NameList_DeInit(list)
NameList_t     *list;
{
    if (list->entries)
        free(list->entries);
}

static int      NameList_Contains(list, name)
NameList_t     *list;
char           *name;
{
    int             i;

    for (i = 0; i < list->used; ++i)
        if (!strcmp(list->entries[i], name))
            return (1);
    return (0);
}

static void     NameList_Add(list, name)
NameList_t     *list;
char           *name;
{
    NameList_GrowIfNecessary(list);
    list->entries[(list->used)++] = name;
}

static void     DependListEntry_DeInit(dle)
DependListEntry_t *dle;
{
    NameList_DeInit(&(dle->dependencies));
}

static void     DependListEntry_Init(dle)
DependListEntry_t *dle;
{
    dle->name = (char *) 0;
    dle->defnode = dle->subdefnode = (PC_ParseNode_t *) 0;
    NameList_Init(&(dle->dependencies));
}

static void     DependListEntry_SetName(dle, name)
DependListEntry_t *dle;
char           *name;
{
    dle->name = name;
}

static void     DependListEntry_SetNode(dle, node)
DependListEntry_t *dle;
PC_ParseNode_t *node;
{
    dle->defnode = node;
}

static void     DependListEntry_SetSubNode(dle, node)
DependListEntry_t *dle;
PC_ParseNode_t *node;
{
    dle->subdefnode = node;
}

static void     DependList_GrowIfNecessary(list)
DependList_t   *list;
{
    if (list->used == list->allocated) {
        if (list->allocated) {
            if (!(list->entries = (DependListEntry_t *) realloc(list->entries, (list->allocated + DEPENDLIST_GROWSIZE) * (sizeof(DependListEntry_t)))))
                OutOfMemory("DependList_GrowIfNecessary");
        }
        else {
            if (!(list->entries = (DependListEntry_t *) malloc(DEPENDLIST_GROWSIZE * (sizeof(DependListEntry_t)))))
                OutOfMemory("DependList_GrowIfNecessary");
        }
        list->allocated += DEPENDLIST_GROWSIZE;
    }
}

static int      qsort_dependlistnamecmp(dle1, dle2)
DependListEntry_t *dle1, *dle2;
{
    return (strcmp(DependListEntry_Name(dle1), DependListEntry_Name(dle2)));
}

static int      DependList_Contains(list, name)
DependList_t   *list;
char           *name;
{
    int             low = 0, high = list->used - 1, mid, cmp;

    while (low <= high) {
        mid = ((low + high) >> 1);
        cmp = strcmp(name, DependListEntry_Name(&(list->entries[mid])));
        if (!cmp)
            return (1);
        if (high == (low + 1))
            ++low;
        else {
            if (cmp > 0)
                low = mid + 1;
            else
                high = mid - 1;
        }
    }
}

static void     DependList_DeInit(list)
DependList_t   *list;
{
    int             i;

    for (i = 0; i < list->used; ++i)
        DependListEntry_DeInit(&(list->entries[i]));
    if (list->entries)
        free(list->entries);
}

static void     DependList_Init(list)
DependList_t   *list;
{
    list->used = list->allocated = 0;
    list->entries = (DependListEntry_t *) 0;
}

static void     DependList_SortByName(list)
DependList_t   *list;
{
    qsort(list->entries, list->used, (sizeof(DependListEntry_t)), qsort_dependlistnamecmp);
}

static void     DependList_Delete(list, index)
DependList_t   *list;
int             index;
{
    int             i;

    --(list->used);
    for (i = index; i < list->used; ++i)
        bcopy(&(list->entries[i + 1]), &(list->entries[i]), (sizeof(DependListEntry_t)));
}

static void     DependList_AddDependency(list, index, name)
DependList_t   *list;
int             index;
char           *name;
{
    NameList_Add(DependListEntry_NameList(&(list->entries[index])), name);
}

static int      DependList_Add(list, node, name)
DependList_t   *list;
PC_ParseNode_t *node;
char           *name;
{
    int             newIndex;

    DependList_GrowIfNecessary(list);
    DependListEntry_Init(&(list->entries[newIndex = (list->used)]));
    DependListEntry_SetName(&(list->entries[list->used]), name);
    DependListEntry_SetNode(&(list->entries[(list->used)++]), node);
    return (newIndex);
}

static void     DependList_AddSubNode(list, index, node)
DependList_t   *list;
int             index;
PC_ParseNode_t *node;
{
    DependListEntry_SetSubNode(&(list->entries[index]), node);
}

static void     OrderListEntry_Init(ole)
OrderListEntry_t *ole;
{
    ole->whichList = (DependList_t *) 0;
    ole->index = -1;
}

static void     OrderListEntry_SetWhichList(ole, list)
OrderListEntry_t *ole;
DependList_t   *list;
{
    ole->whichList = list;
}

static void     OrderListEntry_SetIndex(ole, index)
OrderListEntry_t *ole;
int             index;
{
    ole->index = index;
}

static void     OrderList_DeInit(list)
OrderList_t    *list;
{
    if (list->entries)
        free(list->entries);
}

static void     OrderList_GrowIfNecessary(list)
OrderList_t    *list;
{
    if (list->used == list->allocated) {
        if (list->allocated) {
            if (!(list->entries = (OrderListEntry_t *) realloc(list->entries, (list->allocated + ORDERLIST_GROWSIZE) * (sizeof(OrderListEntry_t)))))
                OutOfMemory("OrderList_GrowIfNecessary");
        }
        else {
            if (!(list->entries = (OrderListEntry_t *) malloc(ORDERLIST_GROWSIZE * (sizeof(OrderListEntry_t)))))
                OutOfMemory("OrderList_GrowIfNecessary");
        }
        list->allocated += ORDERLIST_GROWSIZE;
    }
}

static void     OrderList_Init(list)
OrderList_t    *list;
{
    list->used = list->allocated = 0;
    list->entries = (OrderListEntry_t *) 0;
}

static void     OrderList_Add(list, whichList, index)
OrderList_t    *list;
DependList_t   *whichList;
int             index;
{
    OrderList_GrowIfNecessary(list);
    OrderListEntry_Init(&(list->entries[list->used]));
    OrderListEntry_SetWhichList(&(list->entries[list->used]), whichList);
    OrderListEntry_SetIndex(&(list->entries[(list->used)++]), index);
}

#ifdef FIRSTTRYDIDNTWORK
static int      qsort_orderlistcmp(ole1, ole2)
OrderListEntry_t *ole1, *ole2;
{
    DependListEntry_t *dle1 = DependList_Entry(OrderListEntry_WhichList(ole1), OrderListEntry_Index(ole1)), *dle2 = DependList_Entry(OrderListEntry_WhichList(ole2), OrderListEntry_Index(ole2));
    NameList_t     *nl1 = DependListEntry_NameList(dle1), *nl2 = DependListEntry_NameList(dle2);
    int             dependencies1 = NameList_Size(nl1), dependencies2 = NameList_Size(nl2);

    if (NameList_Contains(nl1, DependListEntry_Name(dle2)))
        return (1);
    if (NameList_Contains(nl2, DependListEntry_Name(dle1)))
        return (-1);
    if (dependencies1 < dependencies2)
        return (-1);
    if (dependencies1 > dependencies2)
        return (1);
    return (0);
}

static void     OrderList_Sort(list)
OrderList_t    *list;
{
    qsort(list->entries, list->used, (sizeof(OrderListEntry_t)), qsort_orderlistcmp);
}

#endif

static void     OrderList_Sort(list)   /* Looks like it's going to have to be
                                        * a bubble sort (ugh) */
OrderList_t    *list;
{
    int             changesMade, i, j;
    OrderListEntry_t *ole1, *ole2, tmp;
    DependListEntry_t *dle1, *dle2;
    NameList_t     *nl1, *nl2;

    do {
        changesMade = 0;
        for (i = 0; i < list->used; ++i) {
            for (j = i + 1; j < list->used; ++j) {
                ole1 = &(list->entries[i]);
                ole2 = &(list->entries[j]);
                dle1 = DependList_Entry(OrderListEntry_WhichList(ole1),
                                        OrderListEntry_Index(ole1));
                dle2 = DependList_Entry(OrderListEntry_WhichList(ole2),
                                        OrderListEntry_Index(ole2));
                nl1 = DependListEntry_NameList(dle1);
                nl2 = DependListEntry_NameList(dle2);
                if (NameList_Contains(nl1, DependListEntry_Name(dle2))) {
                    if (NameList_Contains(nl2, DependListEntry_Name(dle1))) {
                        fprintf(stderr, "%s: Discovered loop in initializers!\n    %s initialized with %s and vice versa!\n    Cannot continue, exiting...\n", ProgramName, DependListEntry_Name(dle1), DependListEntry_Name(dle2));
                        exit(1);
                    }
                    changesMade = 1;
                    bcopy(ole1, &tmp, (sizeof(OrderListEntry_t)));
                    bcopy(ole2, ole1, (sizeof(OrderListEntry_t)));
                    bcopy(&tmp, ole2, (sizeof(OrderListEntry_t)));
                }
            }
        }
    } while (changesMade);
}

static void     PrintTokens(tokvec, numTokens)
PC_Token_t     *tokvec;
int             numTokens;
{
    int             i;

    for (i = 0; i < numTokens; ++i) {
        printf("%s", PC_TokenChars(&(tokvec[i])));
        if (PC_IsCharToken(&(tokvec[i]))) {
            switch (PC_TokenChar(&(tokvec[i]))) {
                case ';':
                case ',':
                case '{':
                case '}':
                case '?':
                case ':':
                    putchar('\n');
                    break;
                default:
                    putchar(' ');
                    break;
            }
        }
        else
            putchar(' ');
    }
}

static int      IsYYIdentifier(str)
char           *str;
{
    return ((!strncmp(str, "yy", 2))
            || (!strncmp(str, "YY", 2)));
}

static int      ContainsStorageClassSpecifier(node)
PC_ParseNode_t *node;
{
    switch (PC_NodeType(node)) {
        case pnt_declaration_specifiers:
            switch (PC_SubType(node)) {
                case 1:
                case 2:
                    return (1);
                case 4:
                case 6:
                    return (ContainsStorageClassSpecifier(PC_Child(node, 1)));
            }
            break;
        case pnt_storage_class_specifier:
            return (1);
    }
    return (0);
}

static int      ContainsExtern(node)
PC_ParseNode_t *node;
{
    switch (PC_NodeType(node)) {
        case pnt_declaration_specifiers:
            switch (PC_SubType(node)) {
                case 1:
                    return (ContainsExtern(PC_Child(node, 0)));
                case 2:
                    return (ContainsExtern(PC_Child(node, 0))
                            || ContainsExtern(PC_Child(node, 1)));
                case 4:
                case 6:
                    return (ContainsExtern(PC_Child(node, 1)));
            }
            return (0);
        case pnt_storage_class_specifier:
            return (PC_TokenVal(PC_NodeToken(node)) == PC_EXTERN);
    }
    return (0);
}

static void     PrintNode(node)
PC_ParseNode_t *node;
{
    int             numTokens;
    PC_Token_t     *tokvec;

    numTokens = PC_CountTokens(node);
    if (!(tokvec = (PC_Token_t *) malloc(numTokens * (sizeof(PC_Token_t)))))
        OutOfMemory("PrintNode");
    (void) PC_DumpTokens(node, tokvec);
    PrintTokens(tokvec, numTokens);
}

static char    *FindName(node)
PC_ParseNode_t *node;
{
    switch (PC_NodeType(node)) {
        case pnt_init_declarator:
        case pnt_direct_declarator:
            return (FindName(PC_Child(node, 0)));
        case pnt_declarator:
            return (FindName(PC_Child(node, PC_SubType(node) - 1)));
        case pnt_identifier:
            return (PC_TokenText(PC_NodeToken(node)));
    }
    return ((char *) 0);
}

static int      EnumerateDependencies(node, depth, descend, parentVal, whichChild)
PC_ParseNode_t *node;
int             depth, *descend, parentVal, whichChild;
{
    switch (PC_NodeType(node)) {
        case pnt_direct_abstract_declarator:
        case pnt_type_name:
        case pnt_argument_expression_list:
            *descend = 0;
            break;
        case pnt_postfix_expression:
            switch (PC_SubType(node)) {
                case 4:
                case 5:
                case 6:
                    *descend = 0;
                    (void) PC_PreWalkTree(PC_Child(node, 0), EnumerateDependencies, 0);
                    break;
            }
            break;
        case pnt_identifier:
            DependList_AddDependency(SaveList, SaveIndex, FindName(node));
            break;
    }
    return 0;
}

static int      EnumerateDeclaration(node, depth, descend, parentVal, whichChild)
PC_ParseNode_t *node;
int             depth, *descend, parentVal, whichChild;
{
    int             index;

    switch (PC_NodeType(node)) {
        case pnt_init_declarator_list:
            break;
        case pnt_declarator:
        case pnt_direct_declarator:
        case pnt_identifier:
        case pnt_init_declarator:
            index = DependList_Add(SaveList, SaveNode, FindName(node));
            DependList_AddSubNode(SaveList, index, node);
            if ((PC_NodeType(node) == pnt_init_declarator)
                && (PC_SubType(node) == 2)) {
                SaveIndex = index;
                (void) PC_PreWalkTree(PC_Child(node, 1), EnumerateDependencies, 0);
            }
            /* Fall through to next case */
        default:
            *descend = 0;
            break;
    }
    return 0;
}

static int      EnumerateDeclarations(node, depth, descend, parentVal, whichChild)
PC_ParseNode_t *node;
int             depth, *descend, parentVal, whichChild;
{
    switch (PC_NodeType(node)) {
        case pnt_module:
        case pnt_external_declaration:
            break;
        case pnt_declaration:
            if ((PC_SubType(node) == 2)
                && (!ContainsTypedef(PC_Child(node, 0)))) {
                SaveNode = node;
                SaveList = ContainsExtern(PC_Child(node, 0)) ?
                      &externNames : &nonExternNames;
                (void) PC_PreWalkTree(PC_Child(node, 1), EnumerateDeclaration, 0);
            }
            /* Fall through to next case */
        default:
            *descend = 0;
            break;
    }
    return 0;
}

static void     PrintTaggedESUDefn(node)
PC_ParseNode_t *node;
{
    switch (PC_NodeType(node)) {
        case pnt_declaration_specifiers:
            switch (PC_SubType(node)) {
                case 1:
                case 5:
                    break;
                case 2:
                case 6:
                    PrintTaggedESUDefn(PC_Child(node, 1));
                    break;
                case 3:
                    PrintTaggedESUDefn(PC_Child(node, 0));
                    break;
                case 4:
                    PrintTaggedESUDefn(PC_Child(node, 0));
                    PrintTaggedESUDefn(PC_Child(node, 1));
                    break;
            }
            break;
        case pnt_type_specifier:
            switch (PC_SubType(node)) {
                case 10:
                case 11:
                    PrintTaggedESUDefn(PC_Child(node, 0));
                    break;
            }
            break;
        case pnt_enum_specifier:
        case pnt_struct_or_union_specifier:
            if (PC_SubType(node) == 2) {
                PrintNode(node);
                printf(";\n");
            }
            break;
    }
}

static int      ContainsTaggedESUDefn(node)
PC_ParseNode_t *node;
{
    switch (PC_NodeType(node)) {
        case pnt_declaration_specifiers:
            switch (PC_SubType(node)) {
                case 2:
                case 6:
                    return (ContainsTaggedESUDefn(PC_Child(node, 1)));
                case 3:
                    return (ContainsTaggedESUDefn(PC_Child(node, 0)));
                case 4:
                    return (ContainsTaggedESUDefn(PC_Child(node, 0))
                            || ContainsTaggedESUDefn(PC_Child(node, 1)));
            }
            break;
        case pnt_type_specifier:
            switch (PC_SubType(node)) {
                case 10:
                case 11:
                    return (ContainsTaggedESUDefn(PC_Child(node, 0)));
            }
            break;
        case pnt_enum_specifier:
        case pnt_struct_or_union_specifier:
            return (PC_SubType(node) == 2);
    }
    return (0);
}

static int      ContainsTypedef(node)
PC_ParseNode_t *node;
{
    switch (PC_NodeType(node)) {
        case pnt_declaration_specifiers:
            switch (PC_SubType(node)) {
                case 1:
                    return (ContainsTypedef(PC_Child(node, 0)));
                case 2:
                    return (ContainsTypedef(PC_Child(node, 0))
                            || ContainsTypedef(PC_Child(node, 1)));
                case 4:
                case 6:
                    return (ContainsTypedef(PC_Child(node, 1)));
            }
            break;
        case pnt_storage_class_specifier:
            return (PC_TokenVal(PC_NodeToken(node)) == PC_TYPEDEF);
    }
    return (0);
}

static int      PrintTypedefOrTaggedESUDefn(node, depth, descend, parentVal, whichChild)
PC_ParseNode_t *node;
int             depth, *descend, parentVal, whichChild;
{
    switch (PC_NodeType(node)) {
        case pnt_module:
        case pnt_external_declaration:
            break;
        case pnt_declaration:
            if (ContainsTypedef(PC_Child(node, 0)))
                PrintNode(node);
            else {
                if (ContainsTaggedESUDefn(PC_Child(node, 0))) {
                    PrintTaggedESUDefn(PC_Child(node, 0));
                }
            }
            /* Fall through to next case */
        default:
            *descend = 0;
            break;
    }
    return 0;
}

static void     EliminateRedundantExterns()
{
    int             i = 0, j = 0, cmp;
    char           *name;

    DependList_SortByName(&externNames);
    DependList_SortByName(&nonExternNames);
    while ((i < DependList_Size(&externNames))
           && (j < DependList_Size(&nonExternNames))) {
        cmp = strcmp((name = DependList_Name(&externNames, i)),
                     DependList_Name(&nonExternNames, j));
        if (!cmp) {
            DependList_Delete(&externNames, i);
/*            Verbose(5, "Eliminated redundant declaration for %s\n", name);*/
            ++j;
        }
        else {
            if (cmp < 0) {
                ++i;
            }
            else {
                ++j;
            }
        }
    }
}

static void     CreateOrderList()
{
    int             i;

    OrderList_Init(&orderedDecls);
    for (i = 0; i < DependList_Size(&externNames); ++i)
        OrderList_Add(&orderedDecls, &externNames, i);
    for (i = 0; i < DependList_Size(&nonExternNames); ++i)
        OrderList_Add(&orderedDecls, &nonExternNames, i);
    OrderList_Sort(&orderedDecls);
}

static void     PrintNonTaggedESUDefn(node)
PC_ParseNode_t *node;
{
    switch (PC_NodeType(node)) {
        case pnt_declaration_specifiers:
            switch (PC_SubType(node)) {
                case 1:
                case 5:
                    PrintNode(PC_Child(node, 0));
                    break;
                case 2:
                case 6:
                    PrintNode(PC_Child(node, 0));
                    PrintNonTaggedESUDefn(PC_Child(node, 1));
                    break;
                case 3:
                    PrintNonTaggedESUDefn(PC_Child(node, 0));
                    break;
                case 4:
                    PrintNonTaggedESUDefn(PC_Child(node, 0));
                    PrintNonTaggedESUDefn(PC_Child(node, 1));
                    break;
            }
            break;
        case pnt_type_specifier:
            switch (PC_SubType(node)) {
                case 10:
                case 11:
                    PrintNonTaggedESUDefn(PC_Child(node, 0));
                    break;
                default:
                    PrintNode(node);
                    break;
            }
            break;
        case pnt_struct_or_union_specifier:
            if (PC_SubType(node) == 2) {
                PrintNode(PC_Child(node, 0));
                PrintNode(PC_Child(node, 1));
            }
            else
                PrintNode(node);
            break;
        case pnt_enum_specifier:
            if (PC_SubType(node) == 2) {
                printf("enum ");
                PrintNode(PC_Child(node, 0));
            }
            else
                PrintNode(node);
            break;
    }
}

static void     PrintDeclaration(dle)
DependListEntry_t *dle;
{
    PC_ParseNode_t *declaration = DependListEntry_Node(dle), *declarator = DependListEntry_SubNode(dle);
    char           *name;

    if (IsYYIdentifier(name = DependListEntry_Name(dle))
        && (!ContainsStorageClassSpecifier(PC_Child(declaration, 0)))) {
/*        Verbose(6, "Making declaration of %s static\n", name);*/
        printf("static\n");
    }
    PrintNonTaggedESUDefn(PC_Child(declaration, 0));
    PrintNode(declarator);
    printf(";\n");
/*    Verbose(5, "Printed declaration of %s\n", name);*/
}

static void     PrintDeclarations()
{
    int             i;

    for (i = 0; i < OrderList_Size(&orderedDecls); ++i)
        PrintDeclaration(DependList_Entry(OrderList_WhichList(&orderedDecls, i),
                                          OrderList_Index(&orderedDecls, i)));
}

static int      PrintDeclInFunction(node, depth, descend, parentVal, whichChild)
PC_ParseNode_t *node;
int             depth, *descend, parentVal, whichChild;
{
    switch (PC_NodeType(node)) {
        case pnt_init_declarator_list:
            break;
        case pnt_init_declarator:
            fprintf(stderr, "%s: Extern variable %s initialized inside function body.\n    Cannot continue, exiting...\n", ProgramName, FindName(node));
            exit(1);
        case pnt_declarator:
        case pnt_direct_declarator:
        case pnt_identifier:
            if (!DependList_Contains(&nonExternNames, FindName(node))) {
                PrintTokens(SavedTokens, numSavedTokens);
                PrintNode(node);
                printf(";\n");
            }
            /* Fall through to next case */
        default:
            *descend = 0;
            break;
    }
    return 0;
}

static void     PrintDeclsInFunction(node)
PC_ParseNode_t *node;
{
    numSavedTokens = PC_CountTokens(PC_Child(node, 0));
    if (!(SavedTokens = (PC_Token_t *) malloc(numSavedTokens * (sizeof(PC_Token_t)))))
        OutOfMemory("PrintDeclsInFunction");
    (void) PC_DumpTokens(PC_Child(node, 0), SavedTokens);
    (void) PC_PreWalkTree(PC_Child(node, 1), PrintDeclInFunction, 0);
    free(SavedTokens);
}

static void     PrintFunctionDefn2(node)
PC_ParseNode_t *node;
{
    switch (PC_NodeType(node)) {
        case pnt_function_definition:
            switch (PC_SubType(node)) {
                case 1:
                    PrintNode(PC_Child(node, 0));
                    PrintFunctionDefn2(PC_Child(node, 1));
                    break;
                case 2:
                    PrintNode(PC_Child(node, 0));
                    PrintNode(PC_Child(node, 1));
                    PrintFunctionDefn2(PC_Child(node, 2));
                    break;
            }
            break;
        case pnt_function_body:
            switch (PC_SubType(node)) {
                case 1:
                    PrintFunctionDefn2(PC_Child(node, 0));
                    break;
                case 2:
                    PrintNode(PC_Child(node, 0));
                    PrintFunctionDefn2(PC_Child(node, 1));
                    break;
            }
            break;
        case pnt_compound_statement:
            switch (PC_SubType(node)) {
                case 1:
                    PrintNode(node);
                    break;
                case 2:
                case 3:
                    printf("{\n");
                    PrintFunctionDefn2(PC_Child(node, 0));
                    printf("}\n");
                    break;
                case 4:
                    printf("{\n");
                    PrintFunctionDefn2(PC_Child(node, 0));
                    PrintFunctionDefn2(PC_Child(node, 1));
                    printf("}\n");
                    break;
            }
            break;
        case pnt_declaration_list:
            switch (PC_SubType(node)) {
                case 1:
                    PrintFunctionDefn2(PC_Child(node, 0));
                    break;
                case 2:
                    PrintFunctionDefn2(PC_Child(node, 0));
                    PrintFunctionDefn2(PC_Child(node, 1));
                    break;
            }
            break;
        case pnt_declaration:
            if ((PC_SubType(node) == 2)
                && (ContainsExtern(PC_Child(node, 0))))
                PrintDeclsInFunction(node);
            else
                PrintNode(node);
            break;
        case pnt_statement_list:
            switch (PC_SubType(node)) {
                case 1:
                    PrintFunctionDefn2(PC_Child(node, 0));
                    break;
                case 2:
                    PrintFunctionDefn2(PC_Child(node, 0));
                    PrintFunctionDefn2(PC_Child(node, 1));
                    break;
            }
            break;
        case pnt_statement:
            PrintFunctionDefn2(PC_Child(node, 0));
            break;
        default:
            PrintNode(node);
            break;
    }
}

static int      PrintFunctionDefn(node, depth, descend, parentVal, whichChild)
PC_ParseNode_t *node;
int             depth, *descend, parentVal, whichChild;
{
    char           *name;

    switch (PC_NodeType(node)) {
        case pnt_module:
        case pnt_external_declaration:
            break;
        case pnt_function_definition:
            switch (PC_SubType(node)) {
                case 1:
                    if (IsYYIdentifier(name = FindName(PC_Child(node, 0)))) {
/*                        Verbose(4, "Making function %s static\n", name);*/
                        printf("static\n");
                    }
                    break;
                case 2:
                    if (IsYYIdentifier(name = FindName(PC_Child(node, 1)))
                    && (!ContainsStorageClassSpecifier(PC_Child(node, 0)))) {
/*                        Verbose(4, "Making function %s static\n", name);*/
                        printf("static\n");
                    }
                    break;
            }
            PrintFunctionDefn2(node);
/*            Verbose(3, "Printed definition of function %s\n", name);*/
            /* Fall through to next case */
        default:
            *descend = 0;
            break;
    }
    return 0;
}

static void     yyhide()
{

    /*
     * First we enumerate the names, parse-tree locations and dependencies of
     * all declarations; extern declarations in one list and non-externs in
     * another
     */
    DependList_Init(&externNames);
    DependList_Init(&nonExternNames);
/*    Verbose(2, "Performing pre-scan of tree to find forward declarations\n");*/
    (void) PC_PreWalkTree(Tree, EnumerateDeclarations, 0);
    /* Next we eliminate all redundant extern declarations */
/*    Verbose(2, "Eliminating redundant forward declarations\n");*/
    EliminateRedundantExterns();

    /*
     * Next we walk the tree, printing out all typedefs, tagged struct, enum
     * and union declarations we find (in order), but without dumping any
     * definitions
     */
/*    Verbose(2, "Printing all typedefs and tagged struct/union/enum definitions\n");*/
    (void) PC_PreWalkTree(Tree, PrintTypedefOrTaggedESUDefn, 0);

    /*
     * Next we combine the two DependLists into the orderlist, which gets
     * sorted by dependency
     */
/*    Verbose(2, "Creating declaration-dependency list\n");*/
    CreateOrderList();
    /* Now we dump all declarations in the order specified by the order list */
/*    Verbose(2, "Printing all non-function definitions in dependency order\n");*/
    PrintDeclarations();
    OrderList_DeInit(&orderedDecls);
    DependList_DeInit(&externNames);
    /* Finally we dump all function definitions */
/*    Verbose(2, "Printing all function definitions\n");*/
    (void) PC_PreWalkTree(Tree, PrintFunctionDefn, 0);
    DependList_DeInit(&nonExternNames);
}

main(argc, argv)
int             argc;
char          **argv;
{
    extern int      optind;
    extern char    *optarg;
    char            verboseprefix[100], *tmp = rindex(argv[0], '/');
    int             c, verbosity = 0;

#if SY_B4x
    int             whichresource;
    char           *resname;
    struct rlimit   rl;
#endif /* SY_B4x */

    ProgramName = tmp ? (tmp + 1) : argv[0];
/*    sprintf(verboseprefix, "[%s]:", ProgramName);
    Verbose_SetUp(stderr, verboseprefix, &verbosity, (unsigned int *) 0);*/
    while ((c = getopt(argc, argv, "vr:")) != EOF) {
        switch (c) {
            case 'r':
#if SY_B4x
                switch (optarg[0]) {
                    case 'c':
                        whichresource = RLIMIT_CPU;
                        resname = "CPU time";
                        break;
                    case 'd':
                        whichresource = RLIMIT_DATA;
                        resname = "data size";
                        break;
                    case 's':
                        whichresource = RLIMIT_STACK;
                        resname = "stack size";
                        break;
                    case 'm':
                        whichresource = RLIMIT_RSS;
                        resname = "memory (resident set) size";
                        break;
                    default:
                        fprintf(stderr, "Usage: %s [-r[c|d|s|m][u|num]] [-v[v[v...]]]\n", ProgramName);
                        exit(1);
                }
                if (getrlimit(whichresource, &rl)) {
                    fprintf(stderr, "%s: Could not get current resource limit for %s (%s)\n",
                             ProgramName, resname, sys_errlist[errno]);
                    exit(1);
                }
                if (optarg[1] == 'u') {
                    rl.rlim_cur = rl.rlim_max;
                }
                else {
                    rl.rlim_cur = atoi(optarg + 1);
                }
                if (setrlimit(whichresource, &rl)) {
                    fprintf(stderr, "%s: Could not set current resource limit for %s to %d (%s)\n",
                             ProgramName, resname, rl.rlim_cur,
                             sys_errlist[errno]);
                    exit(1);
                }
                fprintf(stderr, "%s: Set limit on %s to %d\n", ProgramName, resname, rl.rlim_cur);
                break;
#else /* SY_B4x */
                fprintf(stderr, "%s: The -r option is not implemented for this system type\n", ProgramName);
                exit(1);
#endif /* SY_B4x */
            case 'v':
                ++verbosity;
                break;
            default:
                fprintf(stderr, "Usage: %s [-r[c|d|s|m][u|num]] [-v[v[v...]]]\n", ProgramName);
                exit(1);
        }
    }
/*    Verbose(1, "Beginning parse of standard input\n");*/
    Tree = PC_Parse(1);
    if (Tree) {
/*        Verbose(2, "Successfully completed parse of standard input\n");*/
        yyhide();
/*        Verbose(1, "Input successfully \"yyhidden\"\n");*/
        exit(0);
    }
    if (PC_ParseError)
        fprintf(stderr, "%s: Error (%s) in parsing standard input\n",
                ProgramName, PC_ParseError);
    else
        fprintf(stderr, "%s: Error in parsing standard input\n",
                ProgramName);
    exit(1);
}
