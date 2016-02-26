
#ifndef phonetool_HEADER
#include "phonetool.h"
#endif

#include <xview/textsw.h>
#include <sys/stat.h>
#include <assert.h>


/*
 * Create object `login_menu' in the specified instance.
 */

Xv_opaque phone_msg_login_menu_create(ip, owner)
   caddr_t     *ip;
   Xv_opaque   owner;
{
#define               MAXLINE    100

   extern Menu_item   login_menu_handler();
   char               name[MAXPATHLEN+10];
   char               line[MAXLINE];
   char               *menu_text;
   FILE               *fptr;
   FILE               *fopen();
   Menu_item          mi;
   struct stat        statbuf;
   Xv_opaque          obj;

   /*
    * Open the .phonerc file.  Look in home dir for it.
    */

   strcpy(name, getenv("HOME"));

   /*
    * If it returns null, then the file doesn't exist, or the user
    * has messed with $HOME.  So, make the menu blank.
    */

   strcat(name, "/.phonerc");

   if (stat(name, &statbuf) != 0) {

      /*
       * The file does not exist in this directory.  Put up an error notice
       * telling the user to create a .phonerc file.
       */

      fprintf(stderr, "Can't find your .phonerc file.\n");
      fprintf(stderr, "You should have a .phonerc file in your home \n");
      fprintf(stderr, "directory if you wish to have a \"login id's\" menu.\n");
      fprintf(stderr, "The .phonerc file contains the logins or email\n");
      fprintf(stderr, "addresses of people to whom you might wish to send\n");
      fprintf(stderr, "phone messages.  These names will appear in the\n");
      fprintf(stderr, "\"login id's\" menu.\n");
      fprintf(stderr, "The .phonerc file may contain comments on lines\n");
      fprintf(stderr, "starting with a pound (#) sign.\n");
      
      /* 
       * Create the menu with no items.
       */
 
      obj = (Xv_opaque) xv_create(owner, MENU, NULL);

      return(obj);

   }

   if ((fptr = fopen(name, "r")) == NULL) {

      /*
       * Can't open file.  Put up a notice to this effect.
       */

      fprintf(stderr, "Can't open or read your .phonerc file.\n");
      fprintf(stderr, "Please check to be sure this file is readable,\n");
      fprintf(stderr, "and exists in your home directory.\n");
      exit(0);
   }

   /* 
    * Create the menu with no items at first.
    */
 
   obj = (Xv_opaque) xv_create(owner, MENU, NULL);

   fgets(line, MAXLINE, fptr);

   while(!feof(fptr)) {

      /*
       * If this line doesn't start with a comment "#" or a newline, then
       * write its contents into the menu.
       */

      if (((strncmp("#", line, 1)) != 0) && ((strncmp("\n", line, 1)) != 0)) {

         /*
          * Take out the trailing newline.
          */

         (void) strtok(line, "\n");

         /*
          * Malloc space for the text for this menu item.
          */

         menu_text = (char *) malloc(strlen(line) + 1);
         (void) strcpy(menu_text, line);

         /*
          * Add the entry to the menu.
          */

         mi = (Menu_item) xv_create(NULL, MENUITEM,
            XV_KEY_DATA, INSTANCE, ip,
            MENU_STRING, menu_text,
            MENU_GEN_PROC, login_menu_handler,
            NULL);

         (void) xv_set(obj,
            MENU_APPEND_ITEM, mi,
            NULL);
      }
 
      /*
       * Get another line.  Go ahead and write over what was there before.
       */

      fgets(line, MAXLINE, fptr);

   }
 
   /*
    * Close the .bt file.
    */

   fclose(fptr);

   return(obj);

}




/*
 * Initialize an instance of object `phonemsg_frame'.
 */

phone_msg_phonemsg_frame_objects
   *phone_msg_phonemsg_frame_objects_initialize(ip, owner)
   phone_msg_phonemsg_frame_objects   *ip;
   Xv_opaque                          owner;
{
   if (!ip && !(ip = (phone_msg_phonemsg_frame_objects *) calloc(1, sizeof 
      (phone_msg_phonemsg_frame_objects))))
      return (phone_msg_phonemsg_frame_objects *) NULL;
   if (!ip->phonemsg_frame)
      ip->phonemsg_frame = phone_msg_phonemsg_frame_phonemsg_frame_create(ip, 
         owner);
   if (!ip->message_control)
      ip->message_control = phone_msg_phonemsg_frame_message_control_create(ip, 
         ip->phonemsg_frame);
   if (!ip->login_button)
      ip->login_button = phone_msg_phonemsg_frame_login_button_create(ip, 
         ip->message_control);
   if (!ip->to_text)
      ip->to_text = phone_msg_phonemsg_frame_to_text_create(ip, 
         ip->message_control);
   if (!ip->caller_text)
      ip->caller_text = phone_msg_phonemsg_frame_caller_text_create(ip, 
         ip->message_control);
   if (!ip->company_text)
      ip->company_text = phone_msg_phonemsg_frame_company_text_create(ip, 
         ip->message_control);
   if (!ip->phone_text)
      ip->phone_text = phone_msg_phonemsg_frame_phone_text_create(ip, 
         ip->message_control);
   if (!ip->message_setting)
      ip->message_setting = phone_msg_phonemsg_frame_message_setting_create(ip, 
         ip->message_control);
   if (!ip->other_message)
      ip->other_message = phone_msg_phonemsg_frame_other_message_create(ip, 
         ip->message_control);
   if (!ip->message_textsw)
      ip->message_textsw = phone_msg_phonemsg_frame_message_textsw_create(ip, 
         ip->phonemsg_frame);
   if (!ip->send_control)
      ip->send_control = phone_msg_phonemsg_frame_send_control_create(ip, 
         ip->phonemsg_frame);
   if (!ip->send_button)
      ip->send_button = phone_msg_phonemsg_frame_send_button_create(ip, 
         ip->send_control);
   if (!ip->clear_button)
      ip->clear_button = phone_msg_phonemsg_frame_clear_button_create(ip, 
         ip->send_control);

   return ip;
}



/*
 * Create object `phonemsg_frame' in the specified instance.
 */

Xv_opaque phone_msg_phonemsg_frame_phonemsg_frame_create(ip, owner)
   caddr_t     *ip;
   Xv_opaque   owner;
{
   Xv_opaque               obj;
   Xv_opaque               phonemsg_frame_image;
   static unsigned short   phonemsg_frame_bits[] = {
#include "phone.icon"
   };
	
   phonemsg_frame_image = xv_create(0, SERVER_IMAGE,
      SERVER_IMAGE_BITS, phonemsg_frame_bits,
      SERVER_IMAGE_DEPTH, 1,
      XV_WIDTH, 64,
      XV_HEIGHT, 64,
      0);

   obj = xv_create(owner, FRAME,
      XV_KEY_DATA, INSTANCE, ip,
      XV_WIDTH, 321,
      XV_HEIGHT, 399,
      XV_LABEL, "Phone Message",
      FRAME_CLOSED, FALSE,
      FRAME_SHOW_FOOTER, TRUE,
      FRAME_ICON, xv_create(0, ICON,
         ICON_IMAGE, phonemsg_frame_image,
         0),
      0);

   return obj;
}



/*
 * Create object `message_control' in the specified instance.
 */

Xv_opaque phone_msg_phonemsg_frame_message_control_create(ip, owner)
   caddr_t     *ip;
   Xv_opaque   owner;
{
   Xv_opaque   obj;
	
   obj = xv_create(owner, PANEL,
      XV_KEY_DATA, INSTANCE, ip,
      XV_X, 0,
      XV_Y, 0,
      XV_WIDTH, WIN_EXTEND_TO_EDGE,
      XV_HEIGHT, 312,
      WIN_BORDER, FALSE,
      0);

   return obj;
}



/*
 * Create object `login_button' in the specified instance.
 */

Xv_opaque phone_msg_phonemsg_frame_login_button_create(ip, owner)
   caddr_t     *ip;
   Xv_opaque   owner;
{
   Xv_opaque   obj;
	
   obj = xv_create(owner, PANEL_BUTTON,
      XV_KEY_DATA, INSTANCE, ip,
      XV_HELP_DATA, "phonetool:login_button",
      XV_X, 219,
      XV_Y, 11,
      XV_WIDTH, 87,
      XV_HEIGHT, 20,
      PANEL_LABEL_STRING, "login id's",
      PANEL_ITEM_MENU, phone_msg_login_menu_create(ip, NULL),
      0);

   return obj;
}



/*
 * Create object `to_text' in the specified instance.
 */

Xv_opaque phone_msg_phonemsg_frame_to_text_create(ip, owner)
   caddr_t     *ip;
   Xv_opaque   owner;
{
   Xv_opaque   obj;
	
   obj = xv_create(owner, PANEL_TEXT,
      XV_KEY_DATA, INSTANCE, ip,
      XV_HELP_DATA, "phonetool:to_text",
      XV_X, 11,
      XV_Y, 15,
      XV_WIDTH, 189,
      XV_HEIGHT, 15,
      PANEL_LABEL_STRING, "To:",
      PANEL_VALUE_X, 40,
      PANEL_VALUE_Y, 15,
      PANEL_LAYOUT, PANEL_HORIZONTAL,
      PANEL_VALUE_DISPLAY_LENGTH, 20,
      PANEL_VALUE_STORED_LENGTH, MAX_STORED_LENGTH,
      PANEL_MAX_VALUE, 0,
      PANEL_MIN_VALUE, 0,
      PANEL_READ_ONLY, FALSE,
      0);

   return obj;
}




/*
 * Create object `caller_text' in the specified instance.
 */

Xv_opaque phone_msg_phonemsg_frame_caller_text_create(ip, owner)
   caddr_t     *ip;
   Xv_opaque   owner;
{
   Xv_opaque   obj;
	
   obj = xv_create(owner, PANEL_TEXT,
      XV_KEY_DATA, INSTANCE, ip,
      XV_HELP_DATA, "phonetool:caller_text",
      XV_X, 32,
      XV_Y, 45,
      XV_WIDTH, 253,
      XV_HEIGHT, 15,
      PANEL_LABEL_STRING, "Caller:",
      PANEL_VALUE_X, 85,
      PANEL_VALUE_Y, 45,
      PANEL_LAYOUT, PANEL_HORIZONTAL,
      PANEL_VALUE_DISPLAY_LENGTH, 25,
      PANEL_VALUE_STORED_LENGTH, MAX_STORED_LENGTH,
      PANEL_MAX_VALUE, 0,
      PANEL_MIN_VALUE, 0,
      PANEL_READ_ONLY, FALSE,
      0);

   return obj;
}




/*
 * Create object `company_text' in the specified instance.
 */

Xv_opaque phone_msg_phonemsg_frame_company_text_create(ip, owner)
   caddr_t     *ip;
   Xv_opaque   owner;
{
   Xv_opaque   obj;
	
   obj = xv_create(owner, PANEL_TEXT,
      XV_KEY_DATA, INSTANCE, ip,
      XV_HELP_DATA, "phonetool:company_text",
      XV_X, 9,
      XV_Y, 63,
      XV_WIDTH, 276,
      XV_HEIGHT, 15,
      PANEL_LABEL_STRING, "Company:",
      PANEL_VALUE_X, 85,
      PANEL_VALUE_Y, 63,
      PANEL_LAYOUT, PANEL_HORIZONTAL,
      PANEL_VALUE_DISPLAY_LENGTH, 25,
      PANEL_VALUE_STORED_LENGTH, MAX_STORED_LENGTH,
      PANEL_MAX_VALUE, 0,
      PANEL_MIN_VALUE, 0,
      PANEL_READ_ONLY, FALSE,
      0);

   return obj;
}




/*
 * Create object `phone_text' in the specified instance.
 */

Xv_opaque phone_msg_phonemsg_frame_phone_text_create(ip, owner)
   caddr_t     *ip;
   Xv_opaque   owner;
{
   Xv_opaque   obj;
	
   obj = xv_create(owner, PANEL_TEXT,
      XV_KEY_DATA, INSTANCE, ip,
      XV_HELP_DATA, "phonetool:phone_text",
      XV_X, 31,
      XV_Y, 81,
      XV_WIDTH, 254,
      XV_HEIGHT, 15,
      PANEL_LABEL_STRING, "Phone:",
      PANEL_VALUE_X, 85,
      PANEL_VALUE_Y, 81,
      PANEL_LAYOUT, PANEL_HORIZONTAL,
      PANEL_VALUE_DISPLAY_LENGTH, 25,
      PANEL_VALUE_STORED_LENGTH, MAX_STORED_LENGTH,
      PANEL_MAX_VALUE, 0,
      PANEL_MIN_VALUE, 0,
      PANEL_READ_ONLY, FALSE,
      0);

   return obj;
}




/*
 * Create object `message_setting' in the specified instance.
 */

Xv_opaque phone_msg_phonemsg_frame_message_setting_create(ip, owner)
   caddr_t     *ip;
   Xv_opaque   owner;
{
   Xv_opaque   obj;
	
   obj = xv_create(owner, PANEL_TOGGLE, PANEL_FEEDBACK, PANEL_MARKED,
      XV_KEY_DATA, INSTANCE, ip,
      XV_HELP_DATA, "phonetool:message_setting",
      XV_X, 16,
      XV_Y, 104,
      XV_WIDTH, 235,
      XV_HEIGHT, 185,
      PANEL_VALUE_X, 16,
      PANEL_VALUE_Y, 121,
      PANEL_LAYOUT, PANEL_VERTICAL,
      PANEL_CHOICE_STRINGS,
         "Please return this call.",
         "Will call again.",
         "URGENT - return this call ASAP.",
         "Wants to see you.",
         "Stopped by to see you.",
         "Returned your call.",
         0,
      PANEL_LABEL_STRING, "Message:",
      0);

   return obj;
}




/*
 * Create object `other_message' in the specified instance.
 */

Xv_opaque phone_msg_phonemsg_frame_other_message_create(ip, owner)
   caddr_t     *ip;
   Xv_opaque   owner;
{
   Xv_opaque   obj;
	
   obj = xv_create(owner, PANEL_MESSAGE,
      XV_KEY_DATA, INSTANCE, ip,
      XV_X, 16,
      XV_Y, 296,
      XV_WIDTH, 43,
      XV_HEIGHT, 13,
      PANEL_LABEL_STRING, "Other:",
      PANEL_LABEL_BOLD, TRUE,
      0);

   return obj;
}




/*
 * Create object `message_textsw' in the specified instance.
 */

Xv_opaque phone_msg_phonemsg_frame_message_textsw_create(ip, owner)
   caddr_t     *ip;
   Xv_opaque   owner;
{
   Xv_opaque   obj;
	
   obj = xv_create(owner, TEXTSW,
      XV_KEY_DATA, INSTANCE, ip,
      XV_HELP_DATA, "phonetool:message_textsw",
      XV_X, 16,
      XV_Y, 316,
      XV_WIDTH, WIN_EXTEND_TO_EDGE,
      XV_HEIGHT, 48,
      OPENWIN_SHOW_BORDERS, TRUE,
      0);

   return obj;
}




/*
 * Create object `send_control' in the specified instance.
 */

Xv_opaque phone_msg_phonemsg_frame_send_control_create(ip, owner)
   caddr_t     *ip;
   Xv_opaque   owner;
{
   Xv_opaque   obj;
	
   obj = xv_create(owner, PANEL,
      XV_KEY_DATA, INSTANCE, ip,
      XV_X, 0,
      XV_Y, 369,
      XV_WIDTH, WIN_EXTEND_TO_EDGE,
      XV_HEIGHT, WIN_EXTEND_TO_EDGE,
      WIN_BORDER, FALSE,
      0);

   return obj;
}




/*
 * Create object `send_button' in the specified instance.
 */

Xv_opaque phone_msg_phonemsg_frame_send_button_create(ip, owner)
   caddr_t     *ip;
   Xv_opaque   owner;
{
   extern void   send_button_handler();
   Xv_opaque     obj;
	
   obj = xv_create(owner, PANEL_BUTTON,
      XV_KEY_DATA, INSTANCE, ip,
      XV_HELP_DATA, "phonetool:send_button",
      XV_X, 66,
      XV_Y, 4,
      XV_WIDTH, 103,
      XV_HEIGHT, 20,
      PANEL_LABEL_STRING, "Send Message",
      PANEL_NOTIFY_PROC, send_button_handler,
      0);

   return obj;
}




/*
 * Create object `clear_button' in the specified instance.
 */

Xv_opaque phone_msg_phonemsg_frame_clear_button_create(ip, owner)
   caddr_t     *ip;
   Xv_opaque   owner;
{
   extern void   clear_button_handler();
   Xv_opaque     obj;
	
   obj = xv_create(owner, PANEL_BUTTON,
      XV_KEY_DATA, INSTANCE, ip,
      XV_HELP_DATA, "phonetool:clear_button",
      XV_X, 206,
      XV_Y, 4,
      XV_WIDTH, 50,
      XV_HEIGHT, 20,
      PANEL_LABEL_STRING, "Clear",
      PANEL_NOTIFY_PROC, clear_button_handler,
      0);

   return obj;
}

