
/**************************************************************************
   Touchup a bitmap graphics editor for the Sun Workstation running SunView
   Copyright (c) 1988 by Raymond Kreisel
   1/22/88 @ Suny Stony Brook

   This program may be redistributed without fee as long as this copyright
   notice is intact.

==> PLEASE send comments and bug reports to one of the following addresses:

	   Ray Kreisel
	   CS Dept., SUNY at Stony Brook, Stony Brook NY 11794

	   UUCP: {allegra, philabs, pyramid, research}!sbcs!rayk   
	   ARPA-Internet: rayk@sbcs.sunysb.edu			
	   CSnet: rayk@suny-sb
	   (If nobody is home at any of the above addresses try:
		S72QKRE@TOWSONVX.BITNET			        )

 "If I get home before daylight, I just might get some sleep tonight...."

**************************************************************************/
/**************************************************************************
	file: disk_io.c
	purpose: This file handle most of the file I/O stuff
	  mostly load, save and file completion

	modifications:
		date:	Tue Mar 22 22:04:58 EST 1988
		author:	rayk
		changes:add comments

		date:	Thu Apr 21 20:14:58 EDT 1988
		author:	rayk
		changes:when you hit return in filename the caret moves
			to textstring

		date:	Thu Apr 21 20:14:58 EDT 1988
		author:	rayk
		changes:when you do a load cut/paste buffer it will
			automagicly bring up the cut/paste command menu

		date:	Wed Jun 1 1:22:18 EDT 1988
		author:	rayk
		changes:fixed bug in resize color canvases

		date:	Wed Jun 15 14:40:48 EDT 1988
		author:	rayk
		changes:added a check in file save to check is the
			current filename is a directory
**************************************************************************/

#include "header.h"

#include <sys/types.h>
#include <sys/dir.h>
#include <sys/file.h>
#include <pwd.h>
#include <sys/stat.h>

colormap_t colormap;
extern  int   errno;

/*
 * Let's do file completion on what we have in the file name prompt
 */
make_new_name(item, event)
Panel_item      item;
Event           *event;
{
        strcpy(file_name,(char*)panel_get_value(file_panel));
        if (complete(file_name))
      	  window_bell(panel);
        panel_set(file_panel,PANEL_VALUE,file_name,0);
}


/* This function, written by Marc J Newberger,
 * will do both login name completion and file name completion, DAM fast.
 * That means as fast as the csh does it.
 */
int complete(template)

    char    *template;

{

    char    dirName[255];
    char   *prefix;    
    int     pref_len;
    char   *suffix;     
    char   *p, *q;
    char    first;
    char    nonUnique;
    char    twiddleUserCompletion;

    struct  direct     *nameEntry;
    DIR                *dirChan;
    struct  passwd     *pwdEntry;

    /*
     *  First do a little parsing of the input. Separate the
     *  prefix template from the directory if there is one.
     */
    twiddleUserCompletion= 0;
    prefix= template+strlen(template);
    while (*(--prefix) != '/' && prefix >= template);

    /*
     *  See if a directory was specified:
     */
    if (prefix < template) {
        /*
         *  No /'s, could be either a username completion or
         *  a completion in the current directory.
         */
        if (template[0] == '~') {
            prefix++;
            twiddleUserCompletion= 1;
            }
        else {
            strcpy(dirName, ".");
            }
        }
    else if (prefix == template) {
        /*
         *  Special case !! The directory excluding the trailing
         *  '/' is zero length. It's the root:
         */
        strcpy(dirName, "/");
        }
    else {
        /*
         *  We're completing a file in a directory.
         *  The directory may be lead by a ~<username> abbreviation.
         *  If that's the case expand it.
         */
        if (template[0] == '~') {
            /*
             *  We need to do twiddle directory expansion.
             *  See if it's our directory:
             */
            if (template[1] == '/') {
                strcpy(dirName, getenv("HOME"));
		if ( &template[1] != prefix )
		  {
                    p= dirName+strlen(dirName);
    		    q= &template[1];
                    while (q < prefix) {
                        *p= *q;
                        p++, q++;
                        }
                    *p= 0;
		  }
                }
            else {
                /*
                 * It's someone else's. Let our fingers
                 * do the walking. (Why the fuck do they call it
                 * the "yellow pages" anyway. They're white pages
                 * dammit !  If they were YELLOW pages, we could
                 * say ypmatch "Automobile, Dealers, Retail", and
                 * things like that !).
                 */
                for (p= dirName, q= &template[1];
                        (*p= *q) != '/';
                            p++, q++);
                *p= 0;
                if (!(pwdEntry= getpwnam(dirName))) {
                    return errno;
                    }
                strcpy(dirName, pwdEntry->pw_dir);
                p= dirName+strlen(dirName);
                while (q < prefix) {
                    *p= *q;
                    p++, q++;
                    }
                *p= 0;
                }
            }
        else {
            /*
             *  It's a vanilla directory. Strip it out.
             */
            strncpy(dirName, template, prefix-template);
            dirName[prefix-template]= 0;
            }
        }
    /*
     *  Bump prefix past the '/'.
     */
    prefix++;

    /*
     *  Get the prefix length and a pointer to the end of the
     *  prefix.
     */
    pref_len= strlen(prefix);
    suffix= template + strlen(template);

    /*
     *  See whether we're doing filename or username completion:
     */
    if (!twiddleUserCompletion) {

        /*
         *  It's filename completion. Read through the directory:
         */
        if ((dirChan= opendir(dirName)) == 0) {
            return errno;
            }

        first= 1;
        nonUnique= 0;
        for (;;) {
            if (!(nameEntry= readdir(dirChan))) {
                break;
                }
            if (!strncmp(prefix, nameEntry->d_name, pref_len)) {
                /*
                 *  We have a file that matches the template.
                 *  If it's the first one, we fill the completion
                 *  suffix with it. Otherwise we scan and pare down
                 *  the suffix.
                 */
                if (first) {
                    first=  0 ;
                    strcpy(suffix, nameEntry->d_name+pref_len);
                    }
                else {
                    nonUnique= 1;
                    p= suffix;
                    q= nameEntry->d_name+pref_len;
                    while (*p == *q) {
                        ++p; ++q;
                        }
                    *p= 0;

                    /*
                     *  A little optimization: If p == suffix, we
                     *  were unable to do any extension of the name.
                     *  We might as well quit here.
                     */
                    if (p == suffix) {
                        break;
                        }
                    }
                }
            }

        closedir(dirChan);
        }
    else {
        /*
         *  Do ~Username completion. Start by resetting the passwd file.
         */
        setpwent();

        first= 1;
        nonUnique= 0;
        for (;;) {
            if (!(pwdEntry= getpwent())) {
                break;
                }
            if (!strncmp(prefix, pwdEntry->pw_name, pref_len)) {
                /*
                 *  We have a user that matches the template.
                 *  If it's the first one, we fill the completion
                 *  suffix with it. Otherwise we scan and pare down
                 *  the suffix.
                 */
                if (first) {
                    first=  0 ;
                    strcpy(suffix, pwdEntry->pw_name+pref_len);
                    }
                else {
                    p= suffix;
                    q= pwdEntry->pw_name+pref_len;
                    while (*p == *q) {
                        ++p; ++q;
                        }

                    /*
                     *  Here there is a possibility of seeing the
                     *  same username twice. For this reason, we
                     *  only set nonUnique to 1 if we're shortening
                     *  the suffix. This means that the new name is
                     *  distinct from any name we've seen.
                     */
                    if (*p) {
                        nonUnique= 1;
                        *p= 0;
                        }

                    /*
                     *  A little optimization: If p == suffix, we
                     *  were unable to do any extension of the name.
                     *  We might as well quit here.
                     */
                    if (p == suffix) {
                        break;
                        }
                    }
                }
            }
        }

    /*
     *  If nothing matched, return a -1, if there was non-uniqueness
     *  return -2.
     */ 
    if (first) {
        return -1;
        }
    else if (nonUnique) {
        return -2;
        }
    else {
        return 0;
        }

}


/*
 * We just got a "load" button event and we want to load in the current
 * filename.
 */
load_file()
{
FILE *fp,*fopen();
int err=0;
struct rasterfile file_header;
int load_area;
char temp_file[MAX_FILE_NAME];
struct pixrect *temp_pr;

  /*
   * Copy the current filename and expand the ~ if it is there
   */
  strcpy(temp_file,(char*)panel_get_value(file_panel));
  get_full_path(temp_file,file_name);
  load_area = (int)panel_get_value(load_cycle);
  
  fp = fopen(file_name,"r");
  if (fp == NULL)
	{
         ERRORstr("Cannot open file: ",file_name);
	 fclose(fp);
	 return;
	}

  /*
   * Try to read the header of the raster file and check if it is color
   */
  err = pr_load_header(fp,&file_header);
  if (file_header.ras_maplength > 3*256) err=1;
  if (err)
	{
	 ERROR("Cannot load the rasterfile header.");
	 fclose(fp);
	 return;
	}

  if (load_area == LOAD_ALL)  
    {
     clear_screen();
     colormap.map[0] = red;
     colormap.map[1] = green;
     colormap.map[2] = blue;
     colormap.type = file_header.ras_maptype;
     colormap.length = file_header.ras_maplength/3;
     /*
      * load in the colormap for the raster file
      */
     if (pr_load_colormap(fp,&file_header,&colormap))
	{
	 ERROR("Cannot load the rasterfile colormap.");	
	 fclose(fp);
	 return;
	}
    }
    else
    {
     /*
      * skip the colormap for the raster file
      */
     if (pr_load_colormap(fp,&file_header,NULL))
	{
	 ERROR("Cannot load the rasterfile colormap.");	
	 fclose(fp);
	 return;
	}
    }

  /*
   * reset the memory pixrect and load the baby in
   */
  MY_pr_destroy(undo_pr);
  undo_pr = (struct pixrect *)pr_load_image(fp,&file_header,&colormap);

  if (undo_pr == NULL)
	{
         ERROR("Cannot allocate pixrect for loaded image file, not enough memory!");
	 fclose(fp);
	 return;
	}

  fclose(fp);

  /*
   * convert 1 bit deep bitmaps to 8 bits deeps if we are on a color machine
   */
  if ((image_depth >1) && (undo_pr->pr_depth == 1))
    {
	temp_pr = my_mem_create(file_header.ras_width,file_header.ras_height,
			image_depth);
	pr_rop(temp_pr,0,0,file_header.ras_width,file_header.ras_height,
			PIX_SRC,undo_pr,0,0);
	MY_pr_destroy(undo_pr);
	undo_pr = temp_pr;
    }



  /*
   * We are loading to the main drawing area so lets set all
   * the color maps
   */
  if (load_area == LOAD_ALL)  
    {
     if (image_depth > 1)
       {
	my_put_colormap();
	set_color();
        /*
         * new fix for color retained canvases for version 2.3
         */
        (void)window_set(canvas,
	    CANVAS_WIDTH,		file_header.ras_width,
	    CANVAS_HEIGHT,		file_header.ras_height,
	    0);
       }
     else
       {
        set_mono();
        (void)window_set(canvas,
	    CANVAS_WIDTH,		file_header.ras_width,
	    CANVAS_HEIGHT,		file_header.ras_height,
	    0);
       }
     panel_set(save_cycle,PANEL_VALUE, SAVE_ALL,0);
     image_wid = file_header.ras_width;
     image_hgt = file_header.ras_height;
     pw_write(pw,0,0, image_wid,image_hgt, PIX_SRC, undo_pr,0,0);
     sprintf(temp_file,"%d",image_hgt);
     panel_set(height_text,PANEL_VALUE,temp_file,0);
     sprintf(temp_file,"%d",image_wid);
     panel_set(width_text,PANEL_VALUE,temp_file,0);
    }
  else
    {
    /*
     * we load the image into the cut/paste buffer
     */
    MY_pr_destroy(cut_buffer_pr);
    cut_buffer_pr = my_mem_create(file_header.ras_width,file_header.ras_height,image_depth);
    pr_rop(cut_buffer_pr,0,0,file_header.ras_width,file_header.ras_height,
			PIX_SRC,undo_pr,0,0);
    MY_pr_destroy(undo_pr);
    undo_pr = my_mem_create(image_wid,image_hgt,image_depth);
    panel_set(region_choice,PANEL_VALUE,MOVE,0);
    panel_set(command_choice,PANEL_VALUE,SEL_REG,0);
    (void)window_set(brush_panel, WIN_SHOW,FALSE, 0);
    (void)window_set(region_panel, WIN_SHOW,TRUE, 0);
    print_msg("Object copied to Cut/Paste buffer. Hold down the RIGHT mouse button to drag the object.");
    mouse_parms();
    }
}



/*
 * Save a file out to the current filename
 */
save_file()
{
FILE *fp,*fopen();
int type = RT_STANDARD;
int copy_flag = TRUE;
char temp_file[MAX_FILE_NAME];

 hide_msg();
 /*
  * Is the raster file to be run-length encode or not
  */
 if((int)panel_get_value(compress_cycle))
	type = RT_BYTE_ENCODED;
 else
	type = RT_STANDARD;
  
  /*
   * Copy the current filename and expand the ~ if it is there
   */
 strcpy(temp_file,(char*)panel_get_value(file_panel));
 get_full_path(temp_file,file_name);

 if (file_is_dir(file_name))
	{
         ERROR("The current filename is a directory !");
	 return;
	}

 if (file_exist(file_name))
	{
         if (!confirm("Overwrite existing file ?"))
	 return;
	}
 /*
  * dump standard SUN raster file with color table to disk file
  */
 fp = fopen(file_name,"w");
  if (fp == NULL)
	{
	 fclose(fp);
         ERRORstr("Cannot write to file: ",file_name);
	 return;
	}
 /*
  * Save the whole drawing area out to the disk file
  */
 if (SAVE_ALL == (int)panel_get_value(save_cycle))
   {
     clean_point();
     clean_region();
     fat_done();
     finish_text();
     save_screen();

     colormap.map[0] = red;
     colormap.map[1] = green;
     colormap.map[2] = blue;
     if (image_depth > 1)
       {
     	colormap.type = RMT_EQUAL_RGB;
     	colormap.length = 256;
       }
     else
       {
     	colormap.type = RMT_NONE;
     	colormap.length = 0;
       }

     if (!undo_pr)
        undo_pr = my_mem_create(image_wid,image_hgt,image_depth);
     pr_rop(undo_pr,0,0,image_wid,image_hgt,PIX_SRC,pw->pw_prretained,0,0);
     pr_dump(undo_pr,fp,&colormap,type,copy_flag);
   }
 else
   {
  /*
   * just save the cut/Paste buffer out to the file
   */
     if (cut_buffer_pr == NULL)
	{
	 ERROR("The Cut/Paste buffer is empty");
	 fclose(fp);
	 return;
	}
     colormap.map[0] = red;
     colormap.map[1] = green;
     colormap.map[2] = blue;
     if (image_depth > 1)
       {
     	colormap.type = RMT_EQUAL_RGB;
     	colormap.length = 256;
       }
     else
       {
     	colormap.type = RMT_NONE;
     	colormap.length = 0;
       }
     pr_dump(cut_buffer_pr,fp,&colormap,type,copy_flag);
   }
 fclose(fp);
}


/*
 * Check if a file exist or not
 */
file_exist(file_name)
char *file_name;
{
FILE *fp;
  fp = fopen(file_name,"r");
  fclose(fp);
  if (fp != NULL)
	return(TRUE);
  else
	return(FALSE);
}


/*
 * Check if the file is a directory
 */
file_is_dir(file_name)
char *file_name;
{
struct stat buf;

  if (file_exist(file_name))
  {
    stat(file_name,&buf);
    if (buf.st_mode & S_IFDIR)
	return(TRUE);
    else
	return(FALSE);
  }
  else
      return(FALSE);
}


/*
 * Take a filename with a ~ character at the begining and return
 * the full path name to that file
 */
get_full_path(template,full_path)
char template[];
char full_path[];
{
    char   *p, *q;
    struct  passwd     *pwdEntry;
        /*
         *  We're completing a file in a directory.
         *  The directory may be lead by a ~<username> abbreviation.
         *  If that's the case expand it.
         */
        if (template[0] == '~') {
            /*
             *  We need to do twiddle directory expansion.
             *  See if it's our directory:
             */
            if (template[1] == '/') {
                strcpy(full_path, getenv("HOME"));
		strcat(full_path,&template[1]);
                }
            else {
                /*
                 * It's someone else's. Let our fingers
                 * do the walking. (Why the fuck do they call it
                 * the "yellow pages" anyway. They're white pages
                 * dammit !  If they were YELLOW pages, we could
                 * say ypmatch "Automobile, Dealers, Retail", and
                 * things like that !).
                 */
                for (p= full_path, q= &template[1];
                        (*p= *q) != '/';
                            p++, q++);
                *p= 0;
                if (!(pwdEntry= getpwnam(full_path))) {
                    return errno;
                    }
                strcpy(full_path, pwdEntry->pw_dir);
		strcat(full_path,q);
                }
	}
	else
	  	strcpy(full_path,template);
}		 

