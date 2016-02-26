/* Top Level Code: Group 7 writefile.c */

/* FILE writefile.c **********************************************************
 *
 * Description of contents
 *   
 *      This file will contian the function writefile. The function writefile
 *    will be in charge of writing the information to the file from the
 *    database.
 *    
 * Header files referenced
 *
 *   There are none at this time.
 *
 * Author.... Brian Gaubert
 *
 * FILE prototype.c */

/* FUNCTION writefile.c *******************************************************
 *
 * Purpose
 *
 *    The purpose of this function is to write the information from the
 *   database to the file. This will be done by using the pointers from the
 *   database structures.
 * 
 *
 * Sample call
 *
 *    retval = writefile(tasklist *data, FILE *ptr)
 *
 * Inputs
 *
 *    data      This is the pointer of the first task in the database
 *    ptr       This is the pointer of the file to add the task to.
 *
 * Outputs
 *
 *    retval   This is the value that will be returned if the write funnction 
 *              fails;
 *
 *
 * Author.... Brian Gaubert
 *
 *  Processing narrative:
 *
 *	 This module is used to save the task information into a file of the
 *	users choice. The parameters of this routine will include the pointer
 *	to the outputfile and the task-list pointer of the first task in
 *	the list. Within the module, there will be two pointers for the
 *	traversal of the lists. The first pointer tem-task-ptr is the pointer
 *	that will move along the main task list. The second pointer is the
 *	pointer that will go through the subtask lists. This routine will run
 *	recursively so that each task list will have a pointer that will go
 *	through its current main task list and a pointer to run through the
 *	subtask lists. When the function returns to the previous call of the
 *	function ( a recursion has been done ) the tem-task-list will go to the
 *	next node in the list, and the sub-task-list will be set to the
 *	tem-task-list which in tern sets both of the pointers to the top of the
 *	next list in the list.
 *
 *	The database will be accessed from this routine by the pointer value
 *	of task-list. All of the variables within the struct of the task-list
 *	pointer coming in from the call of the function will be places in the
 *	print format and written directly to the file. The resources and
 *	dependencies have a list of their own for each task. These lists will
 *	be outputted to the file in the order that they were put into the
 *	database. 
 *
 * 
 *  PDL for writefile:
 * PROCEDURE write-file
 * INTERFACE ptr(file pointer),task-list;
 * ptr IS FILE PTR;
 * tem-task-list IS PTR OF TASK-LIST
 * sub-task-list IS PTR OF TASK-LIST
 *  CALL get-main-task-list PROCEDURE WITH sub-task-list;
 *  WRITE (``task begin\n'');
 *
 *  DO WHILE tem-task-list != NULL
 *    WRITE (ptr,``task = {\n'');
 *    WRITE (ptr,''\t name = %s;\n'', sub-task-list->name);
 *    WRITE (ptr,''\t description = %s;\n'',sub-task-list->desc);
 *   WRITE (ptr.''\t planned-start = %d;\n",sub-task-list->planned-start-date);
 *    WRITE (ptr.''\t planned-end = %d;\n'',sub-task-list->planned-end-date);
 *    WRITE (ptr.''\t actual-start = %d;\n'',sub-task-list->actual-start-date);
 *    WRITE (ptr.''\t actual-end = %d;\n'',sub-task-list->actual-end-date);
 *   WRITE(ptr."\t forecast-start = %d;\n",sub-task-list->forecast-start-date);
 *    WRITE (ptr.''\t forecast-end = %d;\n'',sub-task-list->forecast-end-date);
 *   WRITE (ptr"\t earliest-start = %d;\n",sub-task-list->earliest-start-date);
 *    WRITE (ptr.''\t earliest-end = %d;\n'',sub-task-list->earliest-end-date);
 *    WRITE (ptr.''\t latest-start = %d;\n'',sub-task-list->latest-start-date);
 *    WRITE (ptr.''\t latest-end = %d;\n'',sub-task-list->latest-end-date);
 *    WRITE (ptr,''\t float = %d;\n'',sub-task-list->fload-time);
 *    WRITE (ptr,''\t milestone = %s;\n'',sub-task-list->milestone);
 *    WRITE (ptr,''\t deliverable = %s;\n'',sub-task-list->deliverable);
 *    IF sub-task-list->resource-list != NULL
 *      WRITE (ptr,''\t\tbegin resources;\n'')
 *      DO WHILE sub-task-list->resource-list != NULL
 *        WRITE (ptr,''\t\t\t \"%s\"\n'',sub-task-list->resource-list);
 *        sub-task-list->resource-list = sub-task-list->resource-list->next;
 *      END WHILE
 *      WRITE (ptr,''\t\tend resources;\n'');
 *    ENDIF
 *    IF sub-task-list->dependancy-list != NULL THEN
 *       WRITE (ptr,''\t\tbegin dependencies;\n'')
 *      DO WHILE sub-task-list->dependency-list != NULL
 *        WRITE (ptr,''\t\t\t \"%s\"\n'',sub-task-list->dependency-list);
 *        task-list->dependency-list = sub-task-list->dependency-list->next;
 *      END WHILE
 *      WRITE (ptr,''\t\tend dependencies;\n'');
 *    ENDIF
 *    WRITE (ptr,''\tparent = %s;\n'',sub-task-list->parent);
 *     DO WHILE sub-task-list->subtask != NULL
 *
 *
 * FUNCTION writefile */
int writefile(tasklist *data, FILE *ptr)
 {
  tasklist tem-task-list;
  tasklist sub-task-list;

  get-main-task-list( sub-task-list );
  fprintf(ptr,"task begin\n");

   while( tem-task-list != NULL )
     {
     fprintf(ptr,"task = {\n");
     fprintf(ptr,"\t name = %s;\n", sub-task-list->name);
     fprintf(ptr,"\t description = %s;\n",sub-task-list->desc);
     fprintf(ptr."\t planned-start = %d;\n",sub-task-list->planned-start-date);
     fprintf(ptr."\t planned-end = %d;\n",sub-task-list->planned-end-date);
     fprintf(ptr."\t actual-start = %d;\n",sub-task-list->actual-start-date);
     fprintf(ptr."\t actual-end = %d;\n",sub-task-list->actual-end-date);
     fprintf(ptr."\t forecast-start = %d;\n",sub-task-list->forecast-start-date);
     fprintf(ptr."\t forecast-end = %d;\n",sub-task-list->forecast-end-date);
     fprintf(ptr."\t earliest-start = %d;\n",sub-task-list->earliest-start-date);
     fprintf(ptr."\t earliest-end = %d;\n",sub-task-list->earliest-end-date);
     fprintf(ptr."\t latest-start = %d;\n",sub-task-list->latest-start-date);
     fprintf(ptr."\t latest-end = %d;\n",sub-task-list->latest-end-date);
     fprintf(ptr,"\t float = %d;\n",sub-task-list->fload-time);
     fprintf(ptr,"\t milestone = %s;\n",sub-task-list->milestone);
     fprintf(ptr,"\t deliverable = %s;\n",sub-task-list->deliverable);
     
     if( sub-task-list->resource-list != NULL )
       {
       fprintf (ptr,"\t\tbegin resources;\n")
        while ( sub-task-list->resource-list != NULL )
	  {
            fprintf (ptr,"\t\t\t \"%s\"\n",sub-task-list->resource-list);
            sub-task-list->resource-list = sub-task-list->resource-list->next;
          }
       fprintf (ptr,"\t\tend resources;\n");
       }

     if( sub-task-list->dependancy-list != NULL )
       {
        fprintf (ptr,"\t\tbegin dependencies;\n")
        while( sub-task-list->dependency-list != NULL )
	  {
           fprintf (ptr,"\t\t\t \"%s\"\n",sub-task-list->dependency-list);
           task-list->dependency-list = sub-task-list->dependency-list->next;
          }
       fprintf (ptr,"\t\tend dependencies;\n");
       }
     fprintf (ptr,"\tparent = %s;\n",sub-task-list->parent);
     while( sub-task-list->subtask != NULL )
       {
         writefile(sub-task-list->subtask,ptr);
	 sub-task-list->subtask = sub-task-list->subtask->next;
	 printf(ptr, "\t end task\n");
       }
     tem-task-list = tem-task-list->next;
     sub-task-list = tem-task-list;

   }

} /* End FUNCTION writefile */
      
