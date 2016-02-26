
#ifndef phonetool_HEADER
#include "phonetool.h"
#endif

#include <xview/notice.h>
#include <xview/textsw.h>

void main(argc, argv)
   int    argc;
   char   **argv;
{

   /*
    * Initialize XView.
    */

   xv_init(XV_INIT_ARGC_PTR_ARGV, &argc, argv, 0);

   /*
    * Set the global tag to unique numbers.
    */

   INSTANCE = xv_unique_key();
	
   /*
    * Initialize user interface components.
    */

   phone_msg_phonemsg_frame = phone_msg_phonemsg_frame_objects_initialize(NULL,
      NULL);
	
   /*
    * Turn control over to XView.
    */

   xv_main_loop(phone_msg_phonemsg_frame->phonemsg_frame);

   exit(0);
}

								     
								      

/*
 * Menu handler for `login_menu'.
 */

Menu_item
login_menu_handler(item, op)
	Menu_item	item;
	Menu_generate	op;
{
   phone_msg_phonemsg_frame_objects   *ip;
   char                               login[MAX_STORED_LENGTH];
   
   ip = (phone_msg_phonemsg_frame_objects *) xv_get(item,
      XV_KEY_DATA, INSTANCE);
	
   switch (op) {

   case MENU_DISPLAY:
      break;

   case MENU_DISPLAY_DONE:
      break;

   case MENU_NOTIFY:
      /* Put id into the To: field. */
      strcpy(login, (char *) xv_get(item, MENU_STRING));
      (void) xv_set(ip->to_text,
	 PANEL_VALUE, login,
	 NULL);
      break;

   case MENU_NOTIFY_DONE:
      break;
   }

   return item;
}



/*
 * Notify callback function for `send_button'.
 */

void send_button_handler(item, event)
   Panel_item   item;
   Event        *event;
{
   extern int                         put_up_error_notice();
   extern void                        clear_phone_message();

   phone_msg_phonemsg_frame_objects   *ip;
   int                                value;
   char                               to[MAX_STORED_LENGTH];
   char                               caller[MAX_STORED_LENGTH];
   char                               company[MAX_STORED_LENGTH];
   char                               phone[MAX_STORED_LENGTH];
   char                               message[MAX_MESSAGE_LENGTH];
   char                               msg_buf[5500];
   char                               *mail_str;
   int                                text_len;
   char                               text_message[MAX_TEXT_LENGTH];
   Textsw_index                       next_position;
   int                                result;

   ip = (phone_msg_phonemsg_frame_objects *) xv_get(
      item, XV_KEY_DATA, INSTANCE);

   /*
    * Get person's name to send message to. 
    */

   (void) strcpy(to, (char *) xv_get(ip->to_text, PANEL_VALUE));

   /*
    * Get the caller's name, company, and phone number. 
    */

   (void) strcpy(caller,  (char *) xv_get(ip->caller_text, PANEL_VALUE));
   (void) strcpy(company, (char *) xv_get(ip->company_text, PANEL_VALUE));
   (void) strcpy(phone,   (char *) xv_get(ip->phone_text, PANEL_VALUE));

   /*
    * Get the message.  See O'Reilly p. 150 for meaning of bit values.
    */

   value = (int) xv_get(ip->message_setting, PANEL_VALUE);

   switch(value) {

   case 0: /* 000000 nothing selected */
      break;

   case 1: /* 000001 the first one is selected */
      strcat(message, (char *) xv_get(ip->message_setting,
         PANEL_CHOICE_STRING, 0));
      break;

   case 2: /* 000010 the second one is selected */
      strcat(message, (char *) xv_get(ip->message_setting,
         PANEL_CHOICE_STRING, 1));
      break;

   case 3: /* 000011 */
      strcat(message, (char *) xv_get(ip->message_setting,
         PANEL_CHOICE_STRING, 0));
      strcat(message, "\n");
      strcat(message, (char *) xv_get(ip->message_setting,
         PANEL_CHOICE_STRING, 1));
      break;

   case 4: /* 000100 the third one is selected */
      strcat(message, (char *) xv_get(ip->message_setting,
         PANEL_CHOICE_STRING, 2));
      break;

   case 5: /* 000101 */
      strcat(message, (char *) xv_get(ip->message_setting,
         PANEL_CHOICE_STRING, 0));
      strcat(message, "\n");
      strcat(message, (char *) xv_get(ip->message_setting,
         PANEL_CHOICE_STRING, 2));
      break;

   case 6: /* 000110 */
      strcat(message, (char *) xv_get(ip->message_setting,
         PANEL_CHOICE_STRING, 1));
      strcat(message, "\n");
      strcat(message, (char *) xv_get(ip->message_setting,
         PANEL_CHOICE_STRING, 2));
      break;

   case 7: /* 000111 */
      strcat(message, (char *) xv_get(ip->message_setting,
         PANEL_CHOICE_STRING, 0));
      strcat(message, "\n");
      strcat(message, (char *) xv_get(ip->message_setting,
         PANEL_CHOICE_STRING, 1));
      strcat(message, "\n");
      strcat(message, (char *) xv_get(ip->message_setting,
         PANEL_CHOICE_STRING, 2));
      break;

   case 8: /* 001000 fourth one selected */
      strcat(message, (char *) xv_get(ip->message_setting,
         PANEL_CHOICE_STRING, 3));
      break;

   case 9: /* 001001 */
      strcat(message, (char *) xv_get(ip->message_setting,
         PANEL_CHOICE_STRING, 0));
      strcat(message, "\n");
      strcat(message, (char *) xv_get(ip->message_setting,
         PANEL_CHOICE_STRING, 3));
      break;

   case 10: /* 001010 */
      strcat(message, (char *) xv_get(ip->message_setting,
         PANEL_CHOICE_STRING, 1));
      strcat(message, "\n");
      strcat(message, (char *) xv_get(ip->message_setting,
         PANEL_CHOICE_STRING, 3));
      break;

   case 11: /* 001011 */
      strcat(message, (char *) xv_get(ip->message_setting,
         PANEL_CHOICE_STRING, 0));
      strcat(message, "\n");
      strcat(message, (char *) xv_get(ip->message_setting,
         PANEL_CHOICE_STRING, 1));
      strcat(message, "\n");
      strcat(message, (char *) xv_get(ip->message_setting,
         PANEL_CHOICE_STRING, 3));
      break;

   case 12: /* 001100 */
      strcat(message, (char *) xv_get(ip->message_setting,
         PANEL_CHOICE_STRING, 2));
      strcat(message, "\n");
      strcat(message, (char *) xv_get(ip->message_setting,
         PANEL_CHOICE_STRING, 3));
      break;

   case 13: /* 001101 */
      strcat(message, (char *) xv_get(ip->message_setting,
         PANEL_CHOICE_STRING, 0));
      strcat(message, "\n");
      strcat(message, (char *) xv_get(ip->message_setting,
         PANEL_CHOICE_STRING, 2));
      strcat(message, "\n");
      strcat(message, (char *) xv_get(ip->message_setting,
         PANEL_CHOICE_STRING, 3));
      break;

   case 14: /* 001110 */
      strcat(message, (char *) xv_get(ip->message_setting,
         PANEL_CHOICE_STRING, 1));
      strcat(message, "\n");
      strcat(message, (char *) xv_get(ip->message_setting,
         PANEL_CHOICE_STRING, 2));
      strcat(message, "\n");
      strcat(message, (char *) xv_get(ip->message_setting,
         PANEL_CHOICE_STRING, 3));
      break;

   case 15: /* 001111 */
      strcat(message, (char *) xv_get(ip->message_setting,
         PANEL_CHOICE_STRING, 0));
      strcat(message, "\n");
      strcat(message, (char *) xv_get(ip->message_setting,
         PANEL_CHOICE_STRING, 1));
      strcat(message, "\n");
      strcat(message, (char *) xv_get(ip->message_setting,
         PANEL_CHOICE_STRING, 2));
      strcat(message, "\n");
      strcat(message, (char *) xv_get(ip->message_setting,
         PANEL_CHOICE_STRING, 3));
      break;

   case 16: /* 010000 fifth one selected */
      strcat(message, (char *) xv_get(ip->message_setting,
         PANEL_CHOICE_STRING, 4));
      break;

   case 17: /* 010001 */
      strcat(message, (char *) xv_get(ip->message_setting,
         PANEL_CHOICE_STRING, 0));
      strcat(message, "\n");
      strcat(message, (char *) xv_get(ip->message_setting,
         PANEL_CHOICE_STRING, 4));
      break;

   case 18: /* 010010 */
      strcat(message, (char *) xv_get(ip->message_setting,
         PANEL_CHOICE_STRING, 1));
      strcat(message, "\n");
      strcat(message, (char *) xv_get(ip->message_setting,
         PANEL_CHOICE_STRING, 4));
      break;

   case 19: /* 010011 */
      strcat(message, (char *) xv_get(ip->message_setting,
         PANEL_CHOICE_STRING, 0));
      strcat(message, "\n");
      strcat(message, (char *) xv_get(ip->message_setting,
         PANEL_CHOICE_STRING, 1));
      strcat(message, "\n");
      strcat(message, (char *) xv_get(ip->message_setting,
         PANEL_CHOICE_STRING, 4));
      break;

   case 20: /* 010100 */
      strcat(message, (char *) xv_get(ip->message_setting,
         PANEL_CHOICE_STRING, 2));
      strcat(message, "\n");
      strcat(message, (char *) xv_get(ip->message_setting,
         PANEL_CHOICE_STRING, 4));
      break;

   case 21: /* 010101 */
      strcat(message, (char *) xv_get(ip->message_setting,
         PANEL_CHOICE_STRING, 0));
      strcat(message, "\n");
      strcat(message, (char *) xv_get(ip->message_setting,
         PANEL_CHOICE_STRING, 2));
      strcat(message, "\n");
      strcat(message, (char *) xv_get(ip->message_setting,
         PANEL_CHOICE_STRING, 4));
      break;

   case 22: /* 010110 */
      strcat(message, (char *) xv_get(ip->message_setting,
         PANEL_CHOICE_STRING, 1));
      strcat(message, "\n");
      strcat(message, (char *) xv_get(ip->message_setting,
         PANEL_CHOICE_STRING, 2));
      strcat(message, "\n");
      strcat(message, (char *) xv_get(ip->message_setting,
         PANEL_CHOICE_STRING, 4));
      break;

   case 23: /* 010111 */
      strcat(message, (char *) xv_get(ip->message_setting,
         PANEL_CHOICE_STRING, 0));
      strcat(message, "\n");
      strcat(message, (char *) xv_get(ip->message_setting,
         PANEL_CHOICE_STRING, 1));
      strcat(message, "\n");
      strcat(message, (char *) xv_get(ip->message_setting,
         PANEL_CHOICE_STRING, 2));
      strcat(message, "\n");
      strcat(message, (char *) xv_get(ip->message_setting,
         PANEL_CHOICE_STRING, 4));
      break;

   case 24: /* 011000 */
      strcat(message, (char *) xv_get(ip->message_setting,
         PANEL_CHOICE_STRING, 3));
      strcat(message, "\n");
      strcat(message, (char *) xv_get(ip->message_setting,
         PANEL_CHOICE_STRING, 4));
      break;

   case 25: /* 011001 */
      strcat(message, (char *) xv_get(ip->message_setting,
         PANEL_CHOICE_STRING, 0));
      strcat(message, "\n");
      strcat(message, (char *) xv_get(ip->message_setting,
         PANEL_CHOICE_STRING, 3));
      strcat(message, "\n");
      strcat(message, (char *) xv_get(ip->message_setting,
         PANEL_CHOICE_STRING, 4));
      break;

   case 26: /* 011010 */
      strcat(message, (char *) xv_get(ip->message_setting,
         PANEL_CHOICE_STRING, 1));
      strcat(message, "\n");
      strcat(message, (char *) xv_get(ip->message_setting,
         PANEL_CHOICE_STRING, 3));
      strcat(message, "\n");
      strcat(message, (char *) xv_get(ip->message_setting,
         PANEL_CHOICE_STRING, 4));
      break;

   case 27: /* 011011 */
      strcat(message, (char *) xv_get(ip->message_setting,
         PANEL_CHOICE_STRING, 0));
      strcat(message, "\n");
      strcat(message, (char *) xv_get(ip->message_setting,
         PANEL_CHOICE_STRING, 1));
      strcat(message, "\n");
      strcat(message, (char *) xv_get(ip->message_setting,
         PANEL_CHOICE_STRING, 3));
      strcat(message, "\n");
      strcat(message, (char *) xv_get(ip->message_setting,
         PANEL_CHOICE_STRING, 4));
      break;

   case 28: /* 011100 */
      strcat(message, (char *) xv_get(ip->message_setting,
         PANEL_CHOICE_STRING, 2));
      strcat(message, "\n");
      strcat(message, (char *) xv_get(ip->message_setting,
         PANEL_CHOICE_STRING, 3));
      strcat(message, "\n");
      strcat(message, (char *) xv_get(ip->message_setting,
         PANEL_CHOICE_STRING, 4));
      break;

   case 29: /* 011101 */
      strcat(message, (char *) xv_get(ip->message_setting,
         PANEL_CHOICE_STRING, 0));
      strcat(message, "\n");
      strcat(message, (char *) xv_get(ip->message_setting,
         PANEL_CHOICE_STRING, 2));
      strcat(message, "\n");
      strcat(message, (char *) xv_get(ip->message_setting,
         PANEL_CHOICE_STRING, 3));
      strcat(message, "\n");
      strcat(message, (char *) xv_get(ip->message_setting,
         PANEL_CHOICE_STRING, 4));
      break;

   case 30: /* 011110 */
      strcat(message, (char *) xv_get(ip->message_setting,
         PANEL_CHOICE_STRING, 1));
      strcat(message, "\n");
      strcat(message, (char *) xv_get(ip->message_setting,
         PANEL_CHOICE_STRING, 2));
      strcat(message, "\n");
      strcat(message, (char *) xv_get(ip->message_setting,
         PANEL_CHOICE_STRING, 3));
      strcat(message, "\n");
      strcat(message, (char *) xv_get(ip->message_setting,
         PANEL_CHOICE_STRING, 4));
      break;

   case 31: /* 011111 */
      strcat(message, (char *) xv_get(ip->message_setting,
         PANEL_CHOICE_STRING, 0));
      strcat(message, "\n");
      strcat(message, (char *) xv_get(ip->message_setting,
         PANEL_CHOICE_STRING, 1));
      strcat(message, "\n");
      strcat(message, (char *) xv_get(ip->message_setting,
         PANEL_CHOICE_STRING, 2));
      strcat(message, "\n");
      strcat(message, (char *) xv_get(ip->message_setting,
         PANEL_CHOICE_STRING, 3));
      strcat(message, "\n");
      strcat(message, (char *) xv_get(ip->message_setting,
         PANEL_CHOICE_STRING, 4));
      break;

   case 32: /* 100000 sixth one selected */
      strcat(message, (char *) xv_get(ip->message_setting,
         PANEL_CHOICE_STRING, 5));
      break;

   case 33: /* 100001 */
      strcat(message, (char *) xv_get(ip->message_setting,
         PANEL_CHOICE_STRING, 0));
      strcat(message, "\n");
      strcat(message, (char *) xv_get(ip->message_setting,
         PANEL_CHOICE_STRING, 5));
      break;

   case 34: /* 100010 */
      strcat(message, (char *) xv_get(ip->message_setting,
         PANEL_CHOICE_STRING, 1));
      strcat(message, "\n");
      strcat(message, (char *) xv_get(ip->message_setting,
         PANEL_CHOICE_STRING, 5));
      break;

   case 35: /* 100011 */
      strcat(message, (char *) xv_get(ip->message_setting,
         PANEL_CHOICE_STRING, 0));
      strcat(message, "\n");
      strcat(message, (char *) xv_get(ip->message_setting,
         PANEL_CHOICE_STRING, 1));
      strcat(message, "\n");
      strcat(message, (char *) xv_get(ip->message_setting,
         PANEL_CHOICE_STRING, 5));
      break;

   case 36: /* 100100 */
      strcat(message, (char *) xv_get(ip->message_setting,
         PANEL_CHOICE_STRING, 2));
      strcat(message, "\n");
      strcat(message, (char *) xv_get(ip->message_setting,
         PANEL_CHOICE_STRING, 5));
      break;

   case 37: /* 100101 */
      strcat(message, (char *) xv_get(ip->message_setting,
         PANEL_CHOICE_STRING, 0));
      strcat(message, "\n");
      strcat(message, (char *) xv_get(ip->message_setting,
         PANEL_CHOICE_STRING, 2));
      strcat(message, "\n");
      strcat(message, (char *) xv_get(ip->message_setting,
         PANEL_CHOICE_STRING, 5));
      break;

   case 38: /* 100110 */
      strcat(message, (char *) xv_get(ip->message_setting,
         PANEL_CHOICE_STRING, 1));
      strcat(message, "\n");
      strcat(message, (char *) xv_get(ip->message_setting,
         PANEL_CHOICE_STRING, 2));
      strcat(message, "\n");
      strcat(message, (char *) xv_get(ip->message_setting,
         PANEL_CHOICE_STRING, 5));
      break;

   case 39: /* 100111 */
      strcat(message, (char *) xv_get(ip->message_setting,
         PANEL_CHOICE_STRING, 0));
      strcat(message, "\n");
      strcat(message, (char *) xv_get(ip->message_setting,
         PANEL_CHOICE_STRING, 1));
      strcat(message, "\n");
      strcat(message, (char *) xv_get(ip->message_setting,
         PANEL_CHOICE_STRING, 2));
      strcat(message, "\n");
      strcat(message, (char *) xv_get(ip->message_setting,
         PANEL_CHOICE_STRING, 5));
      break;

   case 40: /* 101000 */
      strcat(message, (char *) xv_get(ip->message_setting,
         PANEL_CHOICE_STRING, 3));
      strcat(message, "\n");
      strcat(message, (char *) xv_get(ip->message_setting,
         PANEL_CHOICE_STRING, 5));
      break;

   case 41: /* 101001 */
      strcat(message, (char *) xv_get(ip->message_setting,
         PANEL_CHOICE_STRING, 0));
      strcat(message, "\n");
      strcat(message, (char *) xv_get(ip->message_setting,
         PANEL_CHOICE_STRING, 3));
      strcat(message, "\n");
      strcat(message, (char *) xv_get(ip->message_setting,
         PANEL_CHOICE_STRING, 5));
      break;

   case 42: /* 101010 */
      strcat(message, (char *) xv_get(ip->message_setting,
         PANEL_CHOICE_STRING, 1));
      strcat(message, "\n");
      strcat(message, (char *) xv_get(ip->message_setting,
         PANEL_CHOICE_STRING, 3));
      strcat(message, "\n");
      strcat(message, (char *) xv_get(ip->message_setting,
         PANEL_CHOICE_STRING, 5));
      break;

   case 43: /* 101011 */
      strcat(message, (char *) xv_get(ip->message_setting,
         PANEL_CHOICE_STRING, 0));
      strcat(message, "\n");
      strcat(message, (char *) xv_get(ip->message_setting,
         PANEL_CHOICE_STRING, 1));
      strcat(message, "\n");
      strcat(message, (char *) xv_get(ip->message_setting,
         PANEL_CHOICE_STRING, 3));
      strcat(message, "\n");
      strcat(message, (char *) xv_get(ip->message_setting,
         PANEL_CHOICE_STRING, 5));
      break;

   case 44: /* 101100 */
      strcat(message, (char *) xv_get(ip->message_setting,
         PANEL_CHOICE_STRING, 2));
      strcat(message, "\n");
      strcat(message, (char *) xv_get(ip->message_setting,
         PANEL_CHOICE_STRING, 3));
      strcat(message, "\n");
      strcat(message, (char *) xv_get(ip->message_setting,
         PANEL_CHOICE_STRING, 5));
      break;

   case 45: /* 101101 */
      strcat(message, (char *) xv_get(ip->message_setting,
         PANEL_CHOICE_STRING, 0));
      strcat(message, "\n");
      strcat(message, (char *) xv_get(ip->message_setting,
         PANEL_CHOICE_STRING, 2));
      strcat(message, "\n");
      strcat(message, (char *) xv_get(ip->message_setting,
         PANEL_CHOICE_STRING, 3));
      strcat(message, "\n");
      strcat(message, (char *) xv_get(ip->message_setting,
         PANEL_CHOICE_STRING, 5));
      break;

   case 46: /* 101110 */
      strcat(message, (char *) xv_get(ip->message_setting,
         PANEL_CHOICE_STRING, 1));
      strcat(message, "\n");
      strcat(message, (char *) xv_get(ip->message_setting,
         PANEL_CHOICE_STRING, 2));
      strcat(message, "\n");
      strcat(message, (char *) xv_get(ip->message_setting,
         PANEL_CHOICE_STRING, 3));
      strcat(message, "\n");
      strcat(message, (char *) xv_get(ip->message_setting,
         PANEL_CHOICE_STRING, 5));
      break;

   case 47: /* 101111 */
      strcat(message, (char *) xv_get(ip->message_setting,
         PANEL_CHOICE_STRING, 0));
      strcat(message, "\n");
      strcat(message, (char *) xv_get(ip->message_setting,
         PANEL_CHOICE_STRING, 1));
      strcat(message, "\n");
      strcat(message, (char *) xv_get(ip->message_setting,
         PANEL_CHOICE_STRING, 2));
      strcat(message, "\n");
      strcat(message, (char *) xv_get(ip->message_setting,
         PANEL_CHOICE_STRING, 3));
      strcat(message, "\n");
      strcat(message, (char *) xv_get(ip->message_setting,
         PANEL_CHOICE_STRING, 5));
      break;

   case 48: /* 110000 the last two are selected */
      strcat(message, (char *) xv_get(ip->message_setting,
         PANEL_CHOICE_STRING, 4));
      strcat(message, "\n");
      strcat(message, (char *) xv_get(ip->message_setting,
         PANEL_CHOICE_STRING, 5));
      break;

   case 49: /* 110001 */
      strcat(message, (char *) xv_get(ip->message_setting,
         PANEL_CHOICE_STRING, 0));
      strcat(message, "\n");
      strcat(message, (char *) xv_get(ip->message_setting,
         PANEL_CHOICE_STRING, 4));
      strcat(message, "\n");
      strcat(message, (char *) xv_get(ip->message_setting,
         PANEL_CHOICE_STRING, 5));
      break;

   case 50: /* 110010 */
      strcat(message, (char *) xv_get(ip->message_setting,
         PANEL_CHOICE_STRING, 1));
      strcat(message, "\n");
      strcat(message, (char *) xv_get(ip->message_setting,
         PANEL_CHOICE_STRING, 4));
      strcat(message, "\n");
      strcat(message, (char *) xv_get(ip->message_setting,
         PANEL_CHOICE_STRING, 5));
      break;

   case 51: /* 110011 */
      strcat(message, (char *) xv_get(ip->message_setting,
         PANEL_CHOICE_STRING, 0));
      strcat(message, "\n");
      strcat(message, (char *) xv_get(ip->message_setting,
         PANEL_CHOICE_STRING, 1));
      strcat(message, "\n");
      strcat(message, (char *) xv_get(ip->message_setting,
         PANEL_CHOICE_STRING, 4));
      strcat(message, "\n");
      strcat(message, (char *) xv_get(ip->message_setting,
         PANEL_CHOICE_STRING, 5));
      break;

   case 52: /* 110100 */
      strcat(message, (char *) xv_get(ip->message_setting,
         PANEL_CHOICE_STRING, 2));
      strcat(message, "\n");
      strcat(message, (char *) xv_get(ip->message_setting,
         PANEL_CHOICE_STRING, 4));
      strcat(message, "\n");
      strcat(message, (char *) xv_get(ip->message_setting,
         PANEL_CHOICE_STRING, 5));
      break;

   case 53: /* 110101 */
      strcat(message, (char *) xv_get(ip->message_setting,
         PANEL_CHOICE_STRING, 0));
      strcat(message, "\n");
      strcat(message, (char *) xv_get(ip->message_setting,
         PANEL_CHOICE_STRING, 2));
      strcat(message, "\n");
      strcat(message, (char *) xv_get(ip->message_setting,
         PANEL_CHOICE_STRING, 4));
      strcat(message, "\n");
      strcat(message, (char *) xv_get(ip->message_setting,
         PANEL_CHOICE_STRING, 5));
      break;

   case 54: /* 110110 */
      strcat(message, (char *) xv_get(ip->message_setting,
         PANEL_CHOICE_STRING, 1));
      strcat(message, "\n");
      strcat(message, (char *) xv_get(ip->message_setting,
         PANEL_CHOICE_STRING, 2));
      strcat(message, "\n");
      strcat(message, (char *) xv_get(ip->message_setting,
         PANEL_CHOICE_STRING, 4));
      strcat(message, "\n");
      strcat(message, (char *) xv_get(ip->message_setting,
         PANEL_CHOICE_STRING, 5));
      break;

   case 55: /* 110111 */
      strcat(message, (char *) xv_get(ip->message_setting,
         PANEL_CHOICE_STRING, 0));
      strcat(message, "\n");
      strcat(message, (char *) xv_get(ip->message_setting,
         PANEL_CHOICE_STRING, 1));
      strcat(message, "\n");
      strcat(message, (char *) xv_get(ip->message_setting,
         PANEL_CHOICE_STRING, 2));
      strcat(message, "\n");
      strcat(message, (char *) xv_get(ip->message_setting,
         PANEL_CHOICE_STRING, 4));
      strcat(message, "\n");
      strcat(message, (char *) xv_get(ip->message_setting,
         PANEL_CHOICE_STRING, 5));
      break;

   case 56: /* 111000 */
      strcat(message, (char *) xv_get(ip->message_setting,
         PANEL_CHOICE_STRING, 3));
      strcat(message, "\n");
      strcat(message, (char *) xv_get(ip->message_setting,
         PANEL_CHOICE_STRING, 4));
      strcat(message, "\n");
      strcat(message, (char *) xv_get(ip->message_setting,
         PANEL_CHOICE_STRING, 5));
      break;

   case 57: /* 111001 */
      strcat(message, (char *) xv_get(ip->message_setting,
         PANEL_CHOICE_STRING, 0));
      strcat(message, "\n");
      strcat(message, (char *) xv_get(ip->message_setting,
         PANEL_CHOICE_STRING, 3));
      strcat(message, "\n");
      strcat(message, (char *) xv_get(ip->message_setting,
         PANEL_CHOICE_STRING, 4));
      strcat(message, "\n");
      strcat(message, (char *) xv_get(ip->message_setting,
         PANEL_CHOICE_STRING, 5));
      break;

   case 58: /* 111010 */
      strcat(message, (char *) xv_get(ip->message_setting,
         PANEL_CHOICE_STRING, 1));
      strcat(message, "\n");
      strcat(message, (char *) xv_get(ip->message_setting,
         PANEL_CHOICE_STRING, 3));
      strcat(message, "\n");
      strcat(message, (char *) xv_get(ip->message_setting,
         PANEL_CHOICE_STRING, 4));
      strcat(message, "\n");
      strcat(message, (char *) xv_get(ip->message_setting,
         PANEL_CHOICE_STRING, 5));
      break;

   case 59: /* 111011 */
      strcat(message, (char *) xv_get(ip->message_setting,
         PANEL_CHOICE_STRING, 0));
      strcat(message, "\n");
      strcat(message, (char *) xv_get(ip->message_setting,
         PANEL_CHOICE_STRING, 1));
      strcat(message, "\n");
      strcat(message, (char *) xv_get(ip->message_setting,
         PANEL_CHOICE_STRING, 3));
      strcat(message, "\n");
      strcat(message, (char *) xv_get(ip->message_setting,
         PANEL_CHOICE_STRING, 4));
      strcat(message, "\n");
      strcat(message, (char *) xv_get(ip->message_setting,
         PANEL_CHOICE_STRING, 5));
      break;

   case 60: /* 111100 */
      strcat(message, (char *) xv_get(ip->message_setting,
         PANEL_CHOICE_STRING, 2));
      strcat(message, "\n");
      strcat(message, (char *) xv_get(ip->message_setting,
         PANEL_CHOICE_STRING, 3));
      strcat(message, "\n");
      strcat(message, (char *) xv_get(ip->message_setting,
         PANEL_CHOICE_STRING, 4));
      strcat(message, "\n");
      strcat(message, (char *) xv_get(ip->message_setting,
         PANEL_CHOICE_STRING, 5));
      break;

   case 61: /* 111101 */
      strcat(message, (char *) xv_get(ip->message_setting,
         PANEL_CHOICE_STRING, 0));
      strcat(message, "\n");
      strcat(message, (char *) xv_get(ip->message_setting,
         PANEL_CHOICE_STRING, 2));
      strcat(message, "\n");
      strcat(message, (char *) xv_get(ip->message_setting,
         PANEL_CHOICE_STRING, 3));
      strcat(message, "\n");
      strcat(message, (char *) xv_get(ip->message_setting,
         PANEL_CHOICE_STRING, 4));
      strcat(message, "\n");
      strcat(message, (char *) xv_get(ip->message_setting,
         PANEL_CHOICE_STRING, 5));
      break;

   case 62: /* 111110 */
      strcat(message, (char *) xv_get(ip->message_setting,
         PANEL_CHOICE_STRING, 1));
      strcat(message, "\n");
      strcat(message, (char *) xv_get(ip->message_setting,
         PANEL_CHOICE_STRING, 2));
      strcat(message, "\n");
      strcat(message, (char *) xv_get(ip->message_setting,
         PANEL_CHOICE_STRING, 3));
      strcat(message, "\n");
      strcat(message, (char *) xv_get(ip->message_setting,
         PANEL_CHOICE_STRING, 4));
      strcat(message, "\n");
      strcat(message, (char *) xv_get(ip->message_setting,
         PANEL_CHOICE_STRING, 5));
      break;

   case 63: /* 111111 all of them selected */
      strcat(message, (char *) xv_get(ip->message_setting,
         PANEL_CHOICE_STRING, 0));
      strcat(message, "\n");
      strcat(message, (char *) xv_get(ip->message_setting,
         PANEL_CHOICE_STRING, 1));
      strcat(message, "\n");
      strcat(message, (char *) xv_get(ip->message_setting,
         PANEL_CHOICE_STRING, 2));
      strcat(message, "\n");
      strcat(message, (char *) xv_get(ip->message_setting,
         PANEL_CHOICE_STRING, 3));
      strcat(message, "\n");
      strcat(message, (char *) xv_get(ip->message_setting,
         PANEL_CHOICE_STRING, 4));
      strcat(message, "\n");
      strcat(message, (char *) xv_get(ip->message_setting,
         PANEL_CHOICE_STRING, 5));
      break;
   }

   strcat(message, "\n\n");

   /*
    * Compose the header.
    */

   sprintf(msg_buf, "From:    %s\nCompany: %s\nPhone:   %s\n\n", caller,
      company, phone);

   /*
    * Get the text, if any.
    */

   text_len = (int) xv_get(ip->message_textsw, TEXTSW_LENGTH);

   if (text_len > MAX_TEXT_LENGTH) {

      /*
       * Put up error notice.
       */

      result = put_up_error_notice(item, TEXT);
      return;
   }

   /*
    * Put the text into the buffer.
    */

   next_position = (Textsw_index) xv_get(ip->message_textsw,
      TEXTSW_CONTENTS, 0, text_message, text_len);

   if (next_position != text_len) {

      /*
       * We've got more than we can use, still.  Put up error message.
       */

      result = put_up_error_notice(item, TEXT);
      return;
   }

   else {

      /*
       * Null terminate the text message.
       */

      text_message[text_len] = '\0';
   }

   /*
    * Put it all together.
    */

   strcat(msg_buf, message);
   strcat(msg_buf, text_message);

   /*
    * Send the mail.
    */

   mail_str = (char *)malloc(strlen(msg_buf) + 1 + 55);
   sprintf(mail_str, "mail -s \"**Phone Message**\" %s << END_MSG\n%s\nEND_MSG\n", to, msg_buf);

   /*
    * Check for mail system errors.  
    */

   if (system(mail_str)) {
      result = put_up_error_notice(item, MAIL);
   }

   else {

      /*
       * Clear the message.
       */

      clear_phone_message(ip);
   }

   free(mail_str);

}




/*
 * Notify callback function for `clear_button'.
 */

void clear_button_handler(item, event)
   Panel_item   item;
   Event        *event;
{
   extern void                        clear_phone_message();
   phone_msg_phonemsg_frame_objects   *ip;

   ip = (phone_msg_phonemsg_frame_objects *) xv_get(item, XV_KEY_DATA, 
      INSTANCE);

   /*
    * Clear the message.
    */

   clear_phone_message(ip);

}




/*
 * Clear the form routine.
 */

void clear_phone_message(ip)
   phone_msg_phonemsg_frame_objects   *ip;
{
   /*
    * Clear the message.
    */

   (void) xv_set(ip->to_text,
      PANEL_VALUE, "",
      NULL);
   (void) xv_set(ip->caller_text,
      PANEL_VALUE, "",
      NULL);
   (void) xv_set(ip->company_text,
      PANEL_VALUE, "",
      NULL);
   (void) xv_set(ip->phone_text,
      PANEL_VALUE, "",
      NULL);
   (void) xv_set(ip->message_setting,
      PANEL_VALUE, 0,
      NULL);
   (void) textsw_reset(ip->message_textsw, 50, 50); /* 50 = x & y coords */
}




/*
 * This routine posts a notice for mailing errors.
 */

int put_up_error_notice(item, notice_type)
   Panel_item               item;
   enum Error_notice_type   notice_type;

{
   Panel   panel;
   int     result;

   panel = (Panel) xv_get(item, PANEL_PARENT_PANEL);

   switch(notice_type) {

   case TEXT:
      result = notice_prompt(panel, NULL,
         NOTICE_MESSAGE_STRINGS,
            "The message in the Other: text pane",
            "is too long.",
            "",
            "Please edit it to less than",
            "5000",
            "characters.",
            NULL,
         NOTICE_BUTTON_YES, "Okay",
         NULL);
      break;

   case MAIL:
      result = notice_prompt(panel, NULL,
         NOTICE_MESSAGE_STRINGS,
            "This message cannot be sent.",
            "",
            "Check the login name in the To: field",
            "to be sure it contains a valid mail address,",
            "then try to send the message again.",
            NULL,
         NOTICE_BUTTON_YES, "Okay",
         NULL);
      break;
   }

   return(result);
}
