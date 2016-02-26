/* (C) Universitaet Passau 1986-1991 */
/*****************************************************************/
/*         FILE : planar.h                                       */
/*        AUTOR : Uwe Schnieders                                 */
/*        UPDATE: 23.07.90                                       */
/*****************************************************************/


/*****************************************************************/
/*       TYPEDEF                                                 */
/*****************************************************************/

typedef Sgraph  PQTree;
typedef ADRlist edge_Set;
typedef Snode   PQNode;
typedef int     PQTYPE;


struct leave_att { Sedge leaveinhalt;
                 } ;

struct Qnode_att { ADRlist sons;
                 } ;

struct Pnode_att { ADRlist sons;
                 } ;

struct node_att  { PQTYPE type ;
                   Snode  parent;
                   union { struct leave_att atl;
                           struct Qnode_att atq;
                           struct Pnode_att atp; } ;
                 } ;

