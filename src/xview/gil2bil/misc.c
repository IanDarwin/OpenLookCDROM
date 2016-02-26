/*
 * Misc routines for parsing GIL and doing something with it.
 * The "something" is defined in some other module, NOT here!.
 * We just call process_node() with each GIL node.
 */

parse_header(char *s)
{
	printf("GIL Header found: %s\n", s);
}

walk_tree(char *root)
{
	printf("Tree walker version 0.0\n");
	printf("Not written yet.\n");
	/* for each node...
	 *	do_node(node);
	 */
}
