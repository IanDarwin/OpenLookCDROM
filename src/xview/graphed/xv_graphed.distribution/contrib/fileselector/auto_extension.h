/* (C) Universitaet Passau 1986-1991 */
typedef struct komp_list {
	char			*komponent;
	struct komp_list	*next;
} *Komp_list;


extern void		dispose_komplist();
extern Komp_list	filename_komponents();
extern Komp_list	rexp_komponents();
extern char		*get_name_of_lists();


