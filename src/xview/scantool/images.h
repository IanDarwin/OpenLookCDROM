
/*  @(#)images.h 1.2 90/04/02
 *
 *  Various images used by the different versions of scantool.
 *
 *  Copyright (c) Rich Burridge - Sun Microsystems.
 *                                All rights reserved.
 *
 *  Permission is given to distribute these sources, as long as the
 *  introductory messages are not removed, and no monies are exchanged.
 *
 *  No responsibility is taken for any errors or inaccuracies inherent
 *  either to the comments or the code of this program, but if reported
 *  to me, then an attempt will be made to fix them.
 */

unsigned short grey_image[16] =
                 {
                   0xAAAA, 0x5555, 0xAAAA, 0x5555,
                   0xAAAA, 0x5555, 0xAAAA, 0x5555,
                   0xAAAA, 0x5555, 0xAAAA, 0x5555,
                   0xAAAA, 0x5555, 0xAAAA, 0x5555
                 } ;

unsigned short icon_image[] = {
#include "scantool.icon"
} ;

unsigned short button_invert_image[] = {
#include "button.invert.icon"
} ;

unsigned short button_normal_image[] = {
#include "button.normal.icon"
} ;

unsigned short switch_invert_image[] = {
#include "switch.invert.cursor"
} ;

unsigned short switch_normal_image[] = {
#include "switch.normal.cursor"
} ;

unsigned short exclaim_image[] = {
#include "exclaim.icon"
} ;

unsigned short main_cursor_array[16] = {
#include "main.cursor"
} ;

unsigned short hour_cursor_array[16] = {
#include <images/hglass.cursor>
} ;
 
unsigned short help_cursor_array[16] = {
#include "help.cursor"
} ;
 
unsigned short frame_cursor_array[16] = {
#include "frame.cursor"
} ;
