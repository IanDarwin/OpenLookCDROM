
#pragma ident   "@(#)dircache.h 1.2     93/01/13"

#ifdef USE_PROTOTYPES

int dircache_add(struct dircache *dcache, char *name, struct dirlist *dlist);
void dircache_delete(struct dircache *dcache, char *name);
void dircache_shrink(struct dircache *dcache, int newsize);
void free_dircache(struct dircache *dcache);
struct dirlist *cache_lookup(struct dircache *dcache, char *dirname);
struct dirlist_header *new_dirlist_header(char *name, struct dirlist *dlist);
void free_dirlist_header(struct dirlist_header *head);
void show_dircache(struct dircache *dcache);

#else

int dircache_add();
void dircache_delete();
void dircache_shrink();
void free_dircache();
struct dirlist *cache_lookup();
struct dirlist_header *new_dirlist_header();
void free_dirlist_header();
void show_dircache();

#endif
