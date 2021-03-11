#include <stdio.h>
#include "resolvinfo.h"


SCNTAB *p_symtab, *p_debug, *p_line, *p_head_scns, *p_dynsym;

static char	*prog_name = "resolv_file";

static Elf32_Ehdr *
dump_elf_header(elf_file, filename)
Elf *elf_file;
char     *filename;
{
	Elf32_Ehdr  *elf_head_p;
	int         class, data;

	if ( (elf_head_p = elf32_getehdr(elf_file)) == NULL) {
                (void)fprintf(stderr, "%s: %s: %s\n", prog_name, filename, elf_errmsg(-1));
		return NULL;
	}

	return elf_head_p;
}


/*
 * Get the section descriptor and set the size of the
 * data returned.  Data is byte-order converted.
 */

VOID_P 
get_scndata(fd_scn, size)
Elf_Scn *fd_scn;
size_t    *size;
{
	Elf_Data *p_data;

	p_data = 0;
	if( (p_data = elf_getdata(fd_scn, p_data)) == 0 ||
		p_data->d_size == 0)
	{
		return NULL;
	}

	*size = p_data->d_size;
	return( p_data->d_buf );
}

struct resolv_info* dump_string_table(elf_file, s, num_scns, filename)
Elf    *elf_file;
SCNTAB *s;
int    num_scns;
char *filename;
{
	size_t section_size;
	unsigned char *strtab;
	int beg_of_string;
	int counter = 0;
	int str_off;
	int i;
	struct resolv_info *all_list=NULL;

	for(i=1; i<num_scns; i++, s++)
	{
   	    if((s->p_shdr->sh_type != SHT_STRTAB) && (strcmp(s->scn_name, ".comment") != 0))
		continue;
	    str_off = 0;

	    section_size = 0;
	    if ((strtab = (unsigned char *)get_scndata(s->p_sd, &section_size)) == NULL)
		continue;

	    if (section_size>0) {
		char str[256];
		int j=0;
		beg_of_string = 0;
		while (section_size--) {
		    unsigned char c = *strtab++;
		    if(beg_of_string)
			beg_of_string = 0;
		    str_off++;
		    switch(c) {
		        case '\0':
			    beg_of_string = 1;
			    str[j]='\0';
			    if (strlen(str)>12) {
			      if(strcmp(&str[j-12],"_classheader")==0) {
				   struct file_list *file_elem;
				   struct file_list *file_visit;
				   str[j-12]='\0';
			           if (all_list == NULL) {
				        all_list=(struct resolv_info*)malloc(sizeof(struct resolv_info));
					all_list->classheader_list = NULL;
					all_list->getclassinfo_list = NULL;
				   }
				   file_visit=all_list->classheader_list;
				   while(file_visit != NULL && strcmp(file_visit->name,str) != 0) file_visit=file_visit->prev;
				   if (file_visit == NULL) {
  				       file_elem = (struct file_list*)malloc(sizeof(struct file_list));
				       file_elem->name=strdup(str);
				       file_elem->prev=all_list->classheader_list;
				       all_list->classheader_list=file_elem;
			           }
			      } else
			      if(strcmp(&str[j-14],"__GetClassInfo")==0){
				   struct file_list *file_elem;
				   str[j-14]='\0';
			           if (all_list == NULL)
				        all_list=(struct resolv_info*)malloc(sizeof(struct resolv_info));
				   file_elem = (struct file_list*)malloc(sizeof(struct file_list));
				   file_elem->name=strdup(str);
				   file_elem->prev=all_list->getclassinfo_list;
				   all_list->getclassinfo_list=file_elem;
			      }
		            }
			    j=0;
			    break;
		         default:	
			    str[j++]=c;
		    }
		}
	    }
	}
	return all_list;
}

struct resolv_info *dump_section_table(elf_file, elf_head_p, filename)
Elf        *elf_file;
Elf32_Ehdr *elf_head_p;
char       *filename;
{

	static SCNTAB *buffer, *p_scns;

	Elf32_Shdr    *p_shdr;
	Elf32_Shdr    *shdr_p;
	Elf_Scn       *scn, *scnfd;
	Elf32_Half    SCN_TYPE;
	char   *offset = NULL;
	char   *s_name = NULL;
	int           found;
	int           num=1;
	static int    num_scns;


	SCN_TYPE = 0;


	found = 0;
	scn = 0;

	num_scns = elf_head_p->e_shnum;
	if ((buffer = (SCNTAB *)calloc (num_scns, sizeof(SCNTAB))) == NULL)
	{
		(void) fprintf(stderr, "%s: %s: cannot calloc space\n",
			prog_name, filename);
		return NULL;
	}
	p_symtab = (SCNTAB *)0;
	p_debug = (SCNTAB *)0;
	p_line = (SCNTAB *)0;
	p_dynsym = (SCNTAB *)0;
	p_scns = buffer;
	p_head_scns = buffer;
	
	while ( (scn = elf_nextscn(elf_file, scn) ) != 0)
	{
		if( (p_shdr = elf32_getshdr(scn) ) == 0)
		{
			/*(void)fprintf(stderr, "%s: %s: cannot get section header\n",
			prog_name, filename);*/
                (void)fprintf(stderr, "%s: %s: %s\n", prog_name, filename, elf_errmsg(-1));
			return NULL;
		}
		s_name = (char *)elf_strptr(elf_file, elf_head_p->e_shstrndx, p_shdr->sh_name);
		buffer->scn_name = s_name;
		buffer->p_shdr   = p_shdr;
		buffer->p_sd   =  scn;

		if (p_shdr->sh_type == SHT_SYMTAB)
		{
			found += 1;
			p_symtab = buffer;
		}
		if (strcmp(s_name, ".debug") == 0)
			p_debug = buffer;
		if (strcmp(s_name, ".line") == 0)
			p_line = buffer;
		if (p_shdr->sh_type == SHT_DYNSYM)
			p_dynsym = buffer;	
		
		buffer++;

	}

	return dump_string_table(elf_file, p_scns, num_scns, filename);
}


struct resolv_info *resolv_file(filename)
char * filename;
{
	Elf        *elf_file;
	Elf        *arf;
	Elf32_Ehdr *elf_head_p;
	Elf32_Shdr *shdr_p;
	int        fd;
	Elf_Kind   file_type;
	char       *strng;
	struct resolv_info *all_list;
	struct stat buf;

	Elf_Cmd cmd;
	errno = 0;

	if (stat(filename,&buf) == -1 || ((buf.st_mode & S_IFMT) == S_IFDIR))
        {
		return NULL;
	}

	if(elf_version(EV_CURRENT) == EV_NONE)
	{
		(void)fprintf(stderr,"%s: Libelf is out of date\n", prog_name);
		return NULL;
	}

	if ((fd = open((filename), O_RDONLY)) == -1)
	{
		(void)fprintf(stderr, "%s: %s: cannot read\n", 
			prog_name, filename);
		return NULL;
	}
	cmd = ELF_C_READ;
	if ( (elf_file = elf_begin(fd, cmd, (Elf *) 0)) == NULL )
	{
                (void)fprintf(stderr, "%s: %s: %s\n", prog_name, filename, elf_errmsg(-1));
		return NULL;
	}
	file_type = elf_kind(elf_file);
	if (file_type == ELF_K_ELF)
	{
		elf_head_p = dump_elf_header(elf_file, filename);
		if (elf_head_p == (Elf32_Ehdr *)0)
		{
			elf_end(elf_file);
			close(fd);
			return NULL;
		}
		all_list = dump_section_table(elf_file, elf_head_p, filename);
		elf_end(elf_file);
		close(fd);
		return all_list;
	}
	else
	{
		(void)fprintf(stderr, "%s: %s: invalid file type\n",
			prog_name, filename);
		elf_end(elf_file);
		close(fd);
		return NULL;
	}
}

#ifdef TESTINGONLYTESTING

main(argc, argv)
	int argc;
	char *argv[];
{
	extern  int optind;	
 	int         optopt;	
	int	    optchar;
	extern char *optarg;    
	struct resolv_info *all_list;
	struct file_list*  file_elem;

	if (argc !=2) {
	  fprintf(stderr,"Usage: resolvinfo.test object_file\n");
	  exit(1);
	} else if (fopen(argv[1],"r") == NULL) {
	  fprintf(stderr,"resolvinfo.test ERROR: object_file(%s) does not exist\n",argv[1]);
	  exit(1);
	}
	all_list=resolv_file(argv[1]);

	if (all_list == NULL) {
	  fprintf(stderr,"resolvinfo.test: object file (%s) does not have any classheader and classinfo\n",argv[1]);
	  exit(1);
	}
	printf("Class Header of <%s>\n",argv[1]);
        file_elem=all_list->classheader_list;
	while (file_elem != NULL) {
	    printf("  %s\n",file_elem->name);
	    file_elem=file_elem->prev;
	}

	printf("Class Info of <%s>\n",argv[1]);
        file_elem=all_list->getclassinfo_list;
	while (file_elem != NULL) {
	    printf("  %s\n",file_elem->name);
	    file_elem=file_elem->prev;
	}
	return 0;
}

#endif



