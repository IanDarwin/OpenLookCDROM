
#ifndef	phonetool_HEADER
#define	phonetool_HEADER

#include <sys/types.h>
#include <sys/param.h>
#include <stdio.h>
#include <xview/xview.h>
#include <xview/xv_xrect.h>
#include <xview/attr.h>
#include <xview/panel.h>


#define MAX_STORED_LENGTH    40     /* Maximum chars in a text field */
#define MAX_MESSAGE_LENGTH   140    /* Maximum size of toggle messages */
#define MAX_TEXT_LENGTH      5000   /* Maximum num chars in the textsw */
                                    /* NOTE: If you change this last value,
                                     * change the value in the notice
                                     * prompt in phonetool.c put_up_error_notice
                                     * routine, too.
                                     */

enum Error_notice_type {
   TEXT,
   MAIL,
   NO_DOT_FILE,
   CANNOT_OPEN_DOT_FILE
};

extern Attr_attribute   INSTANCE;
extern Xv_opaque        phone_msg_login_menu_create();

typedef struct {
   Xv_opaque   phonemsg_frame;
   Xv_opaque   message_control;
   Xv_opaque   login_button;
   Xv_opaque   to_text;
   Xv_opaque   caller_text;
   Xv_opaque   company_text;
   Xv_opaque   phone_text;
   Xv_opaque   message_setting;
   Xv_opaque   other_message;
   Xv_opaque   message_textsw;
   Xv_opaque   send_control;
   Xv_opaque   send_button;
   Xv_opaque   clear_button;
} phone_msg_phonemsg_frame_objects;

extern phone_msg_phonemsg_frame_objects 
   *phone_msg_phonemsg_frame_objects_initialize();

extern Xv_opaque   phone_msg_phonemsg_frame_phonemsg_frame_create();
extern Xv_opaque   phone_msg_phonemsg_frame_message_control_create();
extern Xv_opaque   phone_msg_phonemsg_frame_login_button_create();
extern Xv_opaque   phone_msg_phonemsg_frame_to_text_create();
extern Xv_opaque   phone_msg_phonemsg_frame_caller_text_create();
extern Xv_opaque   phone_msg_phonemsg_frame_company_text_create();
extern Xv_opaque   phone_msg_phonemsg_frame_phone_text_create();
extern Xv_opaque   phone_msg_phonemsg_frame_message_setting_create();
extern Xv_opaque   phone_msg_phonemsg_frame_other_message_create();
extern Xv_opaque   phone_msg_phonemsg_frame_message_textsw_create();
extern Xv_opaque   phone_msg_phonemsg_frame_send_control_create();
extern Xv_opaque   phone_msg_phonemsg_frame_send_button_create();
extern Xv_opaque   phone_msg_phonemsg_frame_clear_button_create();



extern phone_msg_phonemsg_frame_objects   *phone_msg_phonemsg_frame;

#endif
