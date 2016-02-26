/* FILE string_table.c ****************************************
 *
 * xplan - project planning tool
 * Copyright (C) 1992 Brian Gaubert, Mark M. Lacey, Richard Malingkas,
 * and Mike Marlow.
 * 
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation.
 * 
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License (distributed with this program in the file
 * COPYING) for more details.
 * 
 * If you did not received a copy of the GNU General Public License
 * along with this program, write to the Free Software Foundation,
 * Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 * 
 * Since this was a project for a one semester software engineering
 * course, the authors will not be offering support for the product
 * after its release.
 *
 * DESCRIPTION OF CONTENTS
 *
 * Functions for keeping track of a table of unique strings.
 *
*/   

#include <stdio.h>
#include "string_table.h"

/* FUNCTION string_table_create ****************************************

   PURPOSE

   Creates a string_table with the correct number of array elements.

   SAMPLE CALL

   table = string_table_create(nelms);
   
   INPUTS

   nelms --- The number of elements in the table.  This is actually
   the number of lists that the table will contain, with each list
   having zero or more nodes containing strings.

   OUTPUTS

   table --- A newly created table.

   AUTHOR/AUDITOR/TESTER

   Author.... Mark M. Lacey, 7 Nov 1992
   Tester.... Mark M. Lacey, 7 Nov 1992

   MODIFICATIONS (most recent to least)

*/
struct string_table *string_table_create(unsigned nelms)
{
   struct string_table *new_table;
   
   new_table = (struct string_table *) malloc(sizeof(struct string_table));
   
   new_table->nelms = nelms;
   new_table->nodes = (struct string_node **) malloc(nelms * sizeof(struct
								    string_node *));
   
   return new_table;
}

/* FUNCTION string_table_initialize ****************************************

   PURPOSE

   Initializes a string_table by setting all the list heads to NULL.

   SAMPLE CALL

   string_table_initialize(table);
   
   INPUTS

   table --- The table to be initialized.

   AUTHOR/AUDITOR/TESTER

   Author.... Mark M. Lacey, 7 Nov 1992
   Tester.... Mark M. Lacey, 7 Nov 1992

   MODIFICATIONS (most recent to least)

*/
void string_table_initialize(struct string_table *tableptr)
{
   unsigned index;
   
   for (index = 0; index < tableptr->nelms; ++index) {
      tableptr->nodes[index] = NULL;
   }
}

/* FUNCTION string_table_destroy ****************************************

   PURPOSE

   Destroys the list pointers in the string table.  It does not empty
   the lists that each head points to --- string_table_empty is used
   for that purpose.

   SAMPLE CALL

   string_table_destroy(table);
   
   INPUTS

   table --- The table to be destroyed.

   AUTHOR/AUDITOR/TESTER

   Author.... Mark M. Lacey, 7 Nov 1992
   Tester.... Mark M. Lacey, 7 Nov 1992

   MODIFICATIONS (most recent to least)

*/
void string_table_destroy(struct string_table *tableptr)
{
   /* Free the node pointers */
   free(tableptr->nodes);
   /* Free the table struct itself */
   free(tableptr);
}

/* FUNCTION string_table_empty ****************************************

   PURPOSE

   Empties out a previously created string table.

   SAMPLE CALL

   string_table_empty(table);
   
   INPUTS

   table --- The table to be emptied.

   AUTHOR/AUDITOR/TESTER

   Author.... Mark M. Lacey, 7 Nov 1992
   Tester.... Mark M. Lacey, 7 Nov 1992

   MODIFICATIONS (most recent to least)

*/
void string_table_empty(struct string_table *tableptr)
{
   unsigned index;
   struct string_node *node, *next;
   
   /* For each list in the table, clear the list */
   for(index = 0; index < tableptr->nelms; ++index) {
      node = tableptr->nodes[index];
      /* For each node in the list, free memory for the string and */
      /* then the node itself */
      while (node) {
	 next = node->next;
	 free(node->name);
	 free(node);
	 node = next;
      }
   }
}

/* FUNCTION string_table_hash ****************************************

   PURPOSE

   Finds the list in the hash table that the string should be added to.

   SAMPLE CALL

   hashval = string_table_hash(name, namelen, nelms);
   
   INPUTS

   name --- A pointer to the string that is to be added to the list.

   namelen --- The length of name.

   nelms --- The number of lists in the table the string will be added
   to.

   OUTPUTS

   hashval --- The number of the list that the element should be added
   ot.

   AUTHOR/AUDITOR/TESTER

   Author.... Mark M. Lacey, 7 Nov 1992
   Auditor...
   Tester....

   MODIFICATIONS (most recent to least)

*/
unsigned string_table_hash(char *name, unsigned namelen, unsigned nelms)
{
   if (namelen == 0) {
      return 0;
   } else {
      return ((name[0]<<8)+name[namelen-1]) % nelms;
   }
}

/* FUNCTION string_table_insert ****************************************

   PURPOSE

   Searches for an entry in the string table.  If it is found, it
   returns a pointer to it.  Otherwise, it adds it to the table and
   returns a pointer to the new entry.

   SAMPLE CALL

   node = string_table_insert(table, name, namelen);
   
   INPUTS

   table --- A pointer to the table to be added to.

   name --- A pointer to the string that is to be added to the table.

   namelen --- The length of name.

   OUTPUTS

   node --- The node that is added to the table, or if the string was
   already present, the node that contained it.

   AUTHOR/AUDITOR/TESTER

   Author.... Mark M. Lacey, 7 Nov 1992
   Tester.... Mark M. Lacey, 7 Nov 1992

   MODIFICATIONS (most recent to least)

*/
struct string_node *string_table_insert(struct string_table *tableptr,
					char *name,
					unsigned namelen)
{
   unsigned index;
   struct string_node *traverse, *current;
   
   index = string_table_hash(name, namelen, tableptr->nelms);
   
   /* If this is not the head of this list... */
   if (tableptr->nodes[index]) {
      traverse = tableptr->nodes[index];
      
      while (traverse) {
	 current = traverse;
	 if (namelen == traverse->namelen) {
	    if (strncmp(name, traverse->name, namelen) == 0) {
	       return traverse;
	    }
	 }
	 traverse = traverse->next;
      }
      
      /* Since it wasn't found, insert it */
      current->next = (struct string_node *) malloc(sizeof(struct
							   string_node));

      current = current->next;
      
      current->namelen = namelen;
      current->name = (char *) malloc(namelen+1);
      if (namelen != 0) {
	 strncpy(current->name, name, namelen);
      }
      current->name[namelen] = '\0';
      current->name = NULL;
   } else {
      tableptr->nodes[index] = (struct string_node *) malloc(sizeof(struct
								    string_node));
      tableptr->nodes[index]->namelen = namelen;
      tableptr->nodes[index]->name = (char *) malloc(namelen+1);
      if (namelen != 0) {
	 strncpy(tableptr->nodes[index]->name, name, namelen);
      }
      tableptr->nodes[index]->name[namelen] = '\0';
      tableptr->nodes[index]->next = NULL;
      
      return tableptr->nodes[index];
   }
}

/* FUNCTION string_table_insert ****************************************

   PURPOSE

   Searches for an entry in the string table.  If it is found, it
   returns a pointer toit.  Otherwise, it returns NULL.

   SAMPLE CALL

   
   node = string_table_search(table, name, namelen);
   
   INPUTS

   table --- A pointer to the table to be added to.

   name --- A pointer to the string that is to be added to the table.

   namelen --- The length of name.

   OUTPUTS

   node --- The node that is found in the table, or NULL if the string
   is not found.

   AUTHOR/AUDITOR/TESTER

   Author.... Mark M. Lacey, 7 Nov 1992
   Tester.... Mark M. Lacey, 7 Nov 1992

   MODIFICATIONS (most recent to least)

*/
struct string_node *string_table_search(struct string_table *tableptr,
					char *name,
					unsigned namelen)
{
   unsigned index;
   struct string_node *traverse;
   
   index = string_table_hash(name, namelen, tableptr->nelms);
   
   /* If this is not the head of this list... */
   if (tableptr->nodes[index]) {
      traverse = tableptr->nodes[index];
      
      while (traverse) {
	 if (namelen == traverse->namelen) {
	    if (strncmp(name, traverse->name, namelen) == 0) {
	       return traverse;
	    }
	 }
	 traverse = traverse->next;
      }
      
      return NULL;
   } else {
      return NULL;
   }
}

/* FUNCTION string_table_dump ****************************************

   PURPOSE

   Dumps a string table to the screen.

   SAMPLE CALL

   string_table_dump(table);
   
   INPUTS

   table --- A pointer to the table to be dumped.

   AUTHOR/AUDITOR/TESTER

   Author.... Mark M. Lacey, 7 Nov 1992
   Tester.... Mark M. Lacey, 7 Nov 1992

   MODIFICATIONS (most recent to least)

*/
void string_table_dump(struct string_table *tableptr)
{
   unsigned index;
   struct string_node *traverse;
   
   printf("DUMPING STRING TABLE\n");
   /* For each item in the table, print the associated list, if one */
   /* exists */
   for (index = 0; index < tableptr->nelms; ++index) {
      traverse = tableptr->nodes[index];
      if (traverse) {
	 printf("%d: ", index);
	 while (traverse) {
	    printf("%s\t", traverse->name);
	    traverse = traverse->next;
	 }
	 printf("\n");
      }
   }
}
