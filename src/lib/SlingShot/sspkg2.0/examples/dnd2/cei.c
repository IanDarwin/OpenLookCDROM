/*
 * Simple Classing Engine (CE) Interface
 *
 * Author: Jan Andersson - janne@torpa.se
 *
 *	@(#) cei.c 1.1 92/10/15 
 */

#include <stdio.h>

#include "cei.h"

/*
 * static variable definitions
 */
static CE_NAMESPACE f_name_space, t_name_space;
static CE_ENTRY ftype_ent, ttype_ent;
static CE_ATTRIBUTE fns_type_attr, fns_filename_attr;
static CE_ATTRIBUTE type_icon_attr, type_icon_mask_attr;
static CE_ATTRIBUTE type_open_attr;
static CE_ATTRIBUTE type_fgcolor_attr, type_bgcolor_attr;
static CE_ATTRIBUTE fns_type, fns_name;

/*
 * Initialize CE, get namespace and attribute id's.
 */
int 
cei_open(err_msg)
  char           *err_msg;
{
  int             status;

  /*
   * Initialize the Classing Engine.
   */
  status = ce_begin(NULL);
  if (status) {
    sprintf(err_msg,
      "Error Initializing Classing Engine Database - Error no: %d.\n",
      status);
    return (status);
  }

  /*
   * Get Namespace (Files and Types) Entries.
   */
  f_name_space = ce_get_namespace_id("Files");
  if (!f_name_space) {
    sprintf(err_msg, "Cannot find File Namespace\n");
    ce_end();
    return (-1);
  }
  t_name_space = ce_get_namespace_id("Types");
  if (!t_name_space) {
    sprintf(err_msg, "Cannot find Type Namespace\n");
    ce_end();
    return (-1);
  }

  /*
   * Get the attributes ID's we need
   */
  fns_type_attr = ce_get_attribute_id(f_name_space, "FNS_TYPE");
  fns_filename_attr = ce_get_attribute_id(f_name_space, "FNS_FILENAME");
  type_icon_attr = ce_get_attribute_id(t_name_space, "TYPE_ICON");
  type_icon_mask_attr = ce_get_attribute_id(t_name_space, "TYPE_ICON_MASK");
  type_open_attr = ce_get_attribute_id(t_name_space, "TYPE_OPEN");
  type_fgcolor_attr = ce_get_attribute_id(t_name_space, "TYPE_FGCOLOR");
  type_bgcolor_attr = ce_get_attribute_id(t_name_space, "TYPE_BGCOLOR");


  if (!fns_type_attr) {
    sprintf(err_msg, "No FNS_TYPE in Files Namespace\n");
    ce_end();
    return (-1);
  }
  if (!fns_filename_attr) {
    sprintf(err_msg, "No FNS_FILENAME in Files Namespace\n");
    ce_end();
    return (-1);
  }
  if (!type_icon_attr) {
    sprintf(err_msg, "No TYPE_ICON in Types Namespace\n");
    ce_end();
    return (-1);
  }
  if (!type_icon_mask_attr) {
    sprintf(err_msg, "No TYPE_ICON_MASK in Types Namespace\n");
    ce_end();
    return (-1);
  }
  if (!type_open_attr) {
    sprintf(err_msg, "No TYPE_OPEN in Types Namespace\n");
    ce_end();
    return (-1);
  }
  if (!type_fgcolor_attr) {
    sprintf(err_msg, "No TYPE_FGCOLOR in Types Namespace\n");
    ce_end();
    return (-1);
  }
  if (!type_bgcolor_attr) {
    sprintf(err_msg, "No TYPE_BGCOLOR in Types Namespace\n");
    ce_end();
    return (-1);
  }

  return (0);
}

/*
 * Return CE attributes for file 'filename'.
 */
int
cei_get_attr(filename, attr, err_msg)
  char           *filename;
  Cei_Attributes *attr;
  char           *err_msg;
{
  char            buf[256];
  int             fd, argcount, bufsize;

  attr->name = attr->open = attr->icon = attr->icon = NULL;
  attr->fgcolor = attr->bgcolor = NULL;

  if (!filename || strlen(filename) <= 0)
    return -1;

  if ((fd = open(filename, 0)) == -1) {
    sprintf(err_msg, "Cannot open: %s\n", filename);
    return -1;
  }
  bufsize = read(fd, buf, sizeof(buf));
  close(fd);
  if (bufsize <= 0) {
    sprintf(err_msg, "Empty file or Directory: %s\n", filename);
    return -1;
  }

  /*
   * Get a matching entry in the files namespace
   */
  argcount = 3;
  ftype_ent = ce_get_entry(f_name_space, argcount, filename,
    buf, bufsize);
  if (!ftype_ent) {
    sprintf(err_msg, "No match in Files Namespace\n");
    return -1;
  }

  /* get file type (FNS_TYPE) */
  fns_type = ce_get_attribute(f_name_space, ftype_ent, fns_type_attr);
  if (!fns_type) {
    sprintf(err_msg, "No FNS_TYPE for entry in Files Namespace\n");
    return -1;
  }

  /* Get a matching entry in the types namespace */
  argcount = 1;
  ttype_ent = ce_get_entry(t_name_space, argcount, fns_type);
  if (!ttype_ent) {
    sprintf(err_msg, "No match in Types namespace\n");
    return -1;
  }

  attr->name = ce_get_attribute(f_name_space, ftype_ent, fns_filename_attr);
  attr->open = ce_get_attribute(t_name_space, ttype_ent, type_open_attr);
  attr->icon = ce_get_attribute(t_name_space, ttype_ent, type_icon_attr);
  attr->icon_mask = ce_get_attribute(t_name_space, ttype_ent, type_icon_mask_attr);
  attr->fgcolor = ce_get_attribute(t_name_space, ttype_ent, type_fgcolor_attr);
  attr->bgcolor = ce_get_attribute(t_name_space, ttype_ent, type_bgcolor_attr);
  return 0;
}

/*
 * End CE session.
 */
void
cei_close()
{
  ce_end();
}

#ifdef TEST
main()
{
  Cei_Attributes  attr;
  char            filename[1024];
  char            msg[1024];

  if (cei_open(msg) != 0) {
    fprintf(stderr, msg);
    exit(4);
  }

  /*
   * Start loop to read in filenames
   */
  while (1) {
    fprintf(stdout, "\nFilename: ");
    gets(filename);
    if ((strcmp(filename, "q")) == 0)
      break;
    if (cei_get_attr(filename, &attr, msg) == 0) {
      printf("     Name: %s \n", attr.name);
      printf("     Open: %s \n", attr.open);
      printf("     Icon: %s \n", attr.icon);
      printf("Mask Icon: %s \n", attr.icon_mask);
      printf("   Colors: %s, %s \n", attr.fgcolor, attr.bgcolor);
    }
    else
      fprintf(stderr, msg);
  }
  cei_close();
}

#endif
