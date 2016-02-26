/* (C) Universitaet Passau 1986-1991 */
/* Sgraph Source, 1988-1990 by Michael Himsolt */

#ifndef SLIST_HEADER
#define SLIST_HEADER





typedef	struct	slist {
	struct	slist	*pre,   *suc;
	Attributes	attrs;
}
	*Slist;

#define	empty_slist	((Slist)NULL)

#define	for_slist(list, l) \
	{ if (((l) = (list)) != (Slist)NULL) do {
#define	end_for_slist(list, l) \
	} while (((l) = (l)->suc) != list); }


extern	Slist	new_slist			();
extern	Slist	add_immediately_to_slist	();
extern	Slist	add_to_slist			();
extern	Slist	subtract_immediately_from_slist	();
extern	Slist	subtract_from_slist		();
extern	Slist	add_slists			();
extern	Slist	add_slists_disjoint		();
extern	Slist	subtract_slists			();
extern	void	free_slist			();
extern	Slist	copy_slist			();



extern	int	slist_contains_exactly_one_element	();
extern	Slist	contains_slist_element 			();
extern	int	slist_intersects_slist			();
extern	int	size_of_slist				();


extern	Slist	make_slist_of_sgraph		();
extern	Slist	make_slist_of_sourcelist	();
extern	Slist	make_slist_of_targetlist	();


#endif
