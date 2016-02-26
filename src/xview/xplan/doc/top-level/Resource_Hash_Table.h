/* Top Level Code: Group 7 Resource_Hash_Table.h */

/*   This is the header file for the hashing functions used to maintain
*  a hash table for the PERT/Gantt chart task resources names.  The file
*  "Data_Base.c" explains how the hash table is used.  Also, the functions
*  for creating and using the hash table are defined at the end of that file.
*/

#ifndef _string_table_h_
#define _string_table_h_

struct string_node {
   char *name;
   unsigned namelen;
   struct string_node *next;  /* for resolution by chaining */
};

struct string_table {
   unsigned nelms;     /* number of elements in the table */
   struct string_node **nodes;
};

struct string_table *string_table_create(unsigned);
void string_table_initialize(struct string_table *);

void string_table_destroy(struct string_table *);
void string_table_empty(struct string_table *);

unsigned string_table_hash(char *, unsigned, unsigned);
struct string_node *string_table_insert(struct string_table *, char *,
					unsigned);
struct string_node *string_table_search(struct string_table *, char *,
					unsigned);

void string_table_dump(struct string_table *);

#endif

