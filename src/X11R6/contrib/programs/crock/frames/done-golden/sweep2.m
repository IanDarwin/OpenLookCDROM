#define sweep2_width 165
#define sweep2_height 250
static char sweep2_bits[] = {
 0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
 0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
 0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
 0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
 0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
 0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
 0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
 0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
 0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
 0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
 0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
 0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
 0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
 0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
 0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0xe0,0x3f,0x00,
 0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
 0x00,0x00,0x00,0xfc,0xff,0x03,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
 0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0xfe,0xff,0x0f,0x00,0x00,0x00,
 0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x80,
 0xff,0xff,0x1f,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
 0x00,0x00,0x00,0x00,0x00,0xc0,0xff,0xff,0x7f,0x00,0x00,0x00,0x00,0x00,0x00,
 0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0xe0,0xff,0xff,0x7f,
 0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
 0x00,0x00,0xe0,0xff,0xff,0xff,0x01,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
 0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0xe0,0xff,0xff,0xff,0x03,0x00,0x00,
 0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0xf0,
 0xff,0xff,0xff,0x03,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
 0x00,0x00,0x00,0x00,0x00,0xf8,0xff,0xff,0xff,0x07,0x00,0x00,0x00,0x00,0x00,
 0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0xfc,0xff,0xff,0xff,
 0x07,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
 0x00,0x00,0xfc,0xff,0xff,0xff,0x07,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
 0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0xfc,0xff,0xff,0xff,0x0f,0x00,0x00,
 0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0xfe,
 0xff,0xff,0xff,0x0f,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
 0x00,0x00,0x00,0x00,0x00,0xfe,0xff,0xff,0xff,0x0f,0x00,0x00,0x00,0x00,0x00,
 0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0xfe,0xff,0xff,0xff,
 0x0f,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
 0x00,0x00,0xfe,0xff,0xff,0xff,0x0f,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
 0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0xfe,0xff,0xff,0xff,0x0f,0x00,0x00,
 0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0xfe,
 0xff,0xff,0xff,0x07,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
 0x00,0x00,0x00,0x00,0x00,0xfe,0xff,0xff,0xff,0x07,0x00,0x00,0x00,0x00,0x00,
 0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0xfc,0xff,0xff,0xff,
 0x07,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
 0x00,0x00,0xfc,0xff,0xff,0xff,0x07,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
 0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0xfc,0xff,0xff,0xff,0x03,0x00,0x00,
 0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0xf8,
 0xff,0xff,0xff,0x03,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
 0x00,0x00,0x00,0x00,0x00,0xf8,0xff,0xff,0xff,0x03,0x00,0x00,0x00,0x00,0x00,
 0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0xf0,0xff,0xff,0xff,
 0x01,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
 0x00,0x00,0xf0,0xff,0xff,0xff,0x01,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
 0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0xe0,0xff,0xff,0xff,0x01,0x00,0x00,
 0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0xe0,
 0xff,0xff,0xff,0x01,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
 0x00,0x00,0x00,0x00,0x00,0xe0,0xff,0xff,0xff,0x03,0x00,0x00,0x00,0x00,0x00,
 0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0xe0,0xff,0xff,0xff,
 0x03,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
 0x00,0x00,0xe0,0xff,0xff,0xff,0x03,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
 0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0xe0,0xff,0xff,0xff,0x01,0x00,0x00,
 0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0xf0,
 0xff,0xff,0xff,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
 0x00,0x00,0x00,0x00,0x00,0xf0,0xff,0xff,0x7f,0x00,0x00,0x00,0x00,0x00,0x00,
 0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0xf0,0xff,0xff,0x7f,
 0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
 0x00,0x00,0xfc,0xff,0xff,0x7f,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
 0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0xff,0xff,0xff,0x7f,0x00,0x00,0x00,
 0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0xff,
 0xff,0xff,0x7f,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
 0x00,0x00,0x00,0x00,0xe0,0xff,0xff,0xff,0x7f,0x00,0x00,0x00,0x00,0x00,0x00,
 0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0xe0,0xff,0xff,0xff,0x3f,
 0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
 0x00,0xf0,0xff,0xff,0xff,0x3f,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
 0x00,0x00,0x00,0x00,0x00,0x00,0x00,0xf8,0xff,0xff,0xff,0x3f,0x00,0x00,0x00,
 0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0xfc,0xff,
 0xff,0xff,0x1f,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
 0x00,0x00,0x00,0x00,0xfc,0xff,0xff,0xff,0x1f,0x00,0x00,0x00,0x00,0x00,0x00,
 0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0xfe,0xff,0xff,0xff,0x0f,
 0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
 0x00,0xfe,0xff,0xff,0xff,0x03,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
 0x00,0x00,0x00,0x00,0x00,0x00,0x80,0xff,0xff,0xff,0xff,0x07,0x00,0x00,0x00,
 0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x80,0xff,0xff,
 0xff,0xff,0x07,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
 0x00,0x00,0x00,0xc0,0xff,0xff,0xff,0xff,0x3f,0x00,0x00,0x00,0x00,0x00,0x00,
 0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0xc0,0xff,0xff,0xff,0xff,0x3f,
 0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
 0xf0,0xff,0xff,0xff,0xff,0xff,0x03,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
 0x00,0x00,0x00,0x00,0x00,0x00,0xf0,0xff,0xff,0xff,0xff,0xff,0x07,0x00,0x00,
 0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0xfc,0xff,0xff,
 0xff,0xff,0xff,0x3f,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
 0x00,0x00,0x00,0xfc,0xff,0xff,0xff,0xff,0xff,0x7f,0x00,0x00,0x00,0x00,0x00,
 0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0xfe,0xff,0xff,0xff,0xff,0xff,
 0xff,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
 0xff,0xff,0xff,0xff,0xff,0xff,0xff,0x01,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
 0x00,0x00,0x00,0x00,0x00,0x80,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0x03,0x00,
 0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x80,0xff,0xff,0xff,
 0xff,0xff,0xff,0xff,0x07,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
 0x00,0x00,0xc0,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0x07,0x00,0x00,0x00,0x00,
 0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0xc0,0xff,0xff,0xff,0xff,0xff,0xff,
 0xff,0x0f,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0xf0,
 0xff,0xff,0xff,0xff,0xff,0xff,0xff,0x1f,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
 0x00,0x00,0x00,0x00,0x00,0xf0,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0x3f,0x00,
 0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0xf8,0xff,0xff,0xff,
 0xff,0xff,0xff,0xff,0xff,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
 0x00,0x00,0xf8,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0x00,0x00,0x00,0x00,
 0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0xfe,0xff,0xff,0xff,0xff,0xff,0xff,
 0xff,0xff,0x01,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0xfe,
 0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0x03,0x00,0x00,0x00,0x00,0x00,0x00,
 0x00,0x00,0x00,0x00,0x80,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0x0f,
 0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x80,0xff,0xff,0xff,0xff,
 0xff,0xff,0xff,0xff,0xff,0x0f,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
 0x00,0xc0,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0x0f,0x00,0x00,0x00,
 0x00,0x00,0x00,0x00,0x00,0x00,0x00,0xe0,0xff,0xff,0xff,0xff,0xff,0xff,0xff,
 0xff,0xff,0x0f,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0xf8,0xff,
 0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0x1f,0x00,0x00,0x00,0x00,0x00,0x00,
 0x00,0x00,0x00,0x00,0xf8,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0x1f,
 0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0xfe,0xff,0xff,0xff,0xff,
 0xff,0xff,0xff,0xff,0xff,0x3f,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
 0x00,0xfe,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0x7f,0x00,0x00,0x00,
 0x00,0x00,0x00,0x00,0x00,0x00,0x80,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,
 0xff,0xff,0x7f,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x80,0xff,0xff,
 0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0x7f,0x00,0x00,0x00,0x00,0x00,0x00,
 0x00,0x00,0x00,0xc0,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0x7f,
 0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0xc0,0xff,0xff,0xff,0xff,0xff,
 0xff,0xff,0xff,0xff,0xff,0xff,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
 0xf0,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0x00,0x00,0x00,
 0x00,0x00,0x00,0x00,0x00,0x00,0xf0,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,
 0xff,0xff,0xff,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0xf8,0xff,0xff,
 0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0x00,0x00,0x00,0x00,0x00,0x00,
 0x00,0x00,0x00,0xf8,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xf1,0xff,0xff,0xff,
 0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0xf8,0xff,0xff,0xff,0xff,0xff,
 0xff,0xff,0x01,0xf8,0xff,0x7f,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
 0xf8,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0x01,0xf0,0xff,0x7f,0x00,0x00,0x00,
 0x00,0x00,0x00,0x00,0x00,0x00,0xf8,0xff,0xff,0xff,0xff,0xff,0xff,0x7f,0x00,
 0x00,0xff,0x7f,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0xf8,0xff,0xff,
 0xff,0xff,0xff,0xff,0x7f,0x00,0x00,0xff,0x7f,0x00,0x00,0x00,0x00,0x00,0x00,
 0x00,0x00,0x00,0xfe,0xff,0xff,0xff,0xff,0xff,0xff,0x7f,0x00,0x00,0xfc,0x3f,
 0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0xfe,0xff,0xff,0xff,0xff,0xff,
 0xff,0x3f,0x00,0x00,0xf8,0x3f,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
 0xfe,0x7f,0xe0,0xff,0xff,0xff,0xff,0x3f,0x00,0x00,0xc0,0x1f,0x00,0x00,0x00,
 0x00,0x00,0x00,0x00,0x00,0x00,0xfe,0x3f,0xf0,0xff,0xff,0xff,0xff,0x3f,0x00,
 0x00,0xc0,0x1f,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0xfe,0x3f,0xf0,
 0xff,0xff,0xff,0xff,0x3f,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
 0x00,0x00,0x00,0xfe,0x1f,0xf0,0xff,0xff,0xff,0xff,0x7f,0x00,0x00,0x00,0x00,
 0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0xfe,0x1f,0xf8,0xff,0xff,0xff,
 0xff,0x7f,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
 0xfe,0x1f,0xf8,0xff,0xff,0xff,0xff,0x7f,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
 0x00,0x00,0x00,0x00,0x00,0x00,0xfc,0x1f,0xf8,0xff,0xff,0xff,0xff,0x7f,0x00,
 0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0xfc,0x1f,0xf8,
 0xff,0xff,0xff,0xff,0x7f,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
 0x00,0x00,0x00,0xfc,0x1f,0xf0,0xff,0xff,0xff,0xff,0xff,0x00,0x00,0x00,0x00,
 0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0xfc,0x1f,0xf8,0xff,0xff,0xff,
 0xff,0xff,0x01,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
 0xf8,0x1f,0xfc,0xff,0xff,0xff,0xff,0xff,0x01,0x00,0x00,0x00,0x00,0x00,0x00,
 0x00,0x00,0x00,0x00,0x00,0x00,0xf8,0x1f,0xfe,0xff,0xff,0xff,0xff,0xff,0x01,
 0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0xfc,0x9f,0xff,
 0xff,0xff,0xff,0xff,0xff,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
 0x00,0x00,0x00,0xfc,0x9f,0xff,0xff,0xff,0xff,0xff,0xff,0x00,0x00,0x00,0x00,
 0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0xfc,0xff,0xff,0xff,0xff,0xff,
 0xff,0xff,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
 0xfc,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
 0x00,0x00,0x00,0x00,0x00,0x00,0xf8,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0x01,
 0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0xf8,0xff,0xff,
 0xff,0xff,0xff,0xff,0xff,0x01,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
 0x00,0x00,0x00,0xf8,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0x01,0x00,0x00,0x00,
 0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0xf0,0xff,0xff,0xff,0xff,0xff,
 0xff,0xff,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
 0xe0,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0x01,0x00,0x00,0x00,0x00,0x00,0x00,
 0x00,0x00,0x00,0x00,0x00,0x00,0xe0,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0x01,
 0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0xf0,0xff,0xff,
 0xff,0xff,0xff,0xff,0xff,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
 0x00,0x00,0x00,0xe0,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0x00,0x00,0x00,0x00,
 0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0xe0,0xff,0xff,0xff,0xff,0xff,
 0xff,0x7f,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
 0xe0,0xff,0xff,0xff,0xff,0xff,0xff,0x7f,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
 0x00,0x00,0x00,0x00,0x00,0x00,0xc0,0xff,0xff,0xff,0xff,0xff,0xff,0x1f,0x00,
 0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0xc0,0xff,0xff,
 0xff,0xff,0xff,0xff,0x1f,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
 0x00,0x00,0x00,0xc0,0xff,0xff,0xff,0xff,0xff,0xff,0x0f,0x00,0x00,0x00,0x00,
 0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0xc0,0xff,0xff,0xff,0xff,0xff,
 0xff,0x0f,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
 0x80,0xff,0xff,0xff,0xff,0xff,0xff,0x07,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
 0x00,0x00,0x00,0x00,0x00,0x00,0x80,0xff,0xff,0xff,0xff,0xff,0xff,0x07,0x00,
 0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0xc0,0xff,0xff,
 0xff,0xff,0xff,0xff,0x07,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
 0x00,0x00,0x00,0xc0,0xff,0xff,0xff,0xff,0xff,0xff,0x07,0x00,0x00,0x00,0x00,
 0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0xc0,0xff,0xff,0xff,0xff,0xff,
 0xff,0xff,0x03,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
 0xc0,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0x03,0x00,0x00,0x00,0x00,0x00,0x00,
 0x00,0x00,0x00,0x00,0x00,0x00,0xc0,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0x7f,
 0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0xc0,0xff,0xff,
 0xff,0xff,0xff,0xff,0xff,0x7f,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
 0x00,0x00,0x00,0x80,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0x0f,0x00,0x00,
 0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0xc0,0xff,0xff,0xff,0xff,0xff,
 0xff,0xff,0xff,0x1f,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
 0x80,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0x00,0x00,0x00,0x00,0x00,
 0x00,0x00,0x00,0x00,0x00,0x00,0x80,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,
 0xff,0x01,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0xc0,0xff,0xff,
 0xff,0xff,0xff,0xff,0xff,0xff,0xff,0x0f,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
 0x00,0x00,0x00,0x80,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0x1f,0x00,
 0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x80,0xff,0xff,0xff,0xff,0xff,
 0xff,0xff,0xff,0xff,0x7f,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
 0x80,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0x00,0x00,0x00,0x00,
 0x00,0x00,0x00,0x00,0x00,0x00,0x80,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,
 0xff,0xff,0x07,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0xff,0xff,
 0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0x07,0x00,0x00,0x00,0x00,0x00,0x00,
 0x00,0x00,0x00,0x00,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0x3f,
 0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0xff,0xff,0xff,0xff,0xff,
 0xff,0xff,0xff,0xff,0xff,0x7f,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
 0x00,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0x01,0x00,0x00,
 0x00,0x00,0x00,0x00,0x00,0x00,0x00,0xfe,0xff,0xff,0xff,0xff,0xff,0xff,0xff,
 0xff,0xff,0xff,0x01,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0xf8,0xff,
 0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0x07,0x00,0x00,0x00,0x00,0x00,
 0x00,0x00,0x00,0x00,0xfc,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,
 0x07,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0xf0,0xff,0xff,0xff,0xff,
 0xff,0xff,0xff,0xff,0xff,0xff,0x1f,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
 0x00,0xf0,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0x1f,0x00,0x00,
 0x00,0x00,0x00,0x00,0x00,0x00,0x00,0xc0,0xff,0xff,0xff,0xff,0xff,0xff,0xff,
 0xff,0xff,0xff,0x7f,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0xc0,0xff,
 0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0x7f,0x00,0x00,0x00,0x00,0x00,
 0x00,0x00,0x00,0x00,0x80,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,
 0x3f,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x80,0xff,0xff,0xff,0xff,
 0xff,0xff,0xff,0xff,0xff,0xff,0x1f,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
 0x00,0x80,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0x1f,0x00,0x00,
 0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0xff,0xff,0xff,0xff,0xff,0xff,0xff,
 0xff,0xff,0xff,0x0f,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0xfe,
 0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0x0f,0x00,0x00,0x00,0x00,0x00,
 0x00,0x00,0x00,0x00,0x00,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,
 0x0f,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0xff,0xff,0xff,0x7f,
 0xfe,0xff,0xff,0xff,0xff,0xff,0x07,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
 0x00,0x00,0xff,0xff,0xff,0x7f,0xfc,0xff,0xff,0xff,0xff,0xff,0x07,0x00,0x00,
 0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0xfc,0xff,0xff,0x7f,0x00,0x00,0xc0,
 0xff,0xff,0xff,0x03,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0xfe,
 0xff,0xff,0x7f,0x00,0x00,0xc0,0xff,0xff,0xff,0x03,0x00,0x00,0x00,0x00,0x00,
 0x00,0x00,0x00,0x00,0x00,0xfc,0xff,0xff,0xff,0x00,0x00,0x80,0xff,0xff,0xff,
 0x03,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0xf8,0xff,0xff,0xff,
 0x00,0x00,0xc0,0xff,0xff,0xff,0x01,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
 0x00,0x00,0xf8,0xff,0xff,0xff,0x00,0x00,0xe0,0xff,0xff,0xff,0x01,0x00,0x00,
 0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0xf8,0xff,0xff,0xff,0x00,0x00,0xe0,
 0xff,0xff,0xff,0x01,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0xe0,
 0xff,0xff,0xff,0x01,0x00,0xf0,0xff,0xff,0x7f,0x00,0x00,0x00,0x00,0x00,0x00,
 0x00,0x00,0x00,0x00,0x00,0xf0,0xff,0xff,0xff,0x00,0x00,0xf0,0xff,0xff,0x7f,
 0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0xc0,0xff,0xff,0xff,
 0x00,0x00,0xf0,0xff,0xff,0x3f,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
 0x00,0x00,0xc0,0xff,0xff,0xff,0x01,0x00,0xf8,0xff,0xff,0x3f,0x00,0x00,0x00,
 0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0xff,0xff,0xff,0xff,0x01,0x00,0xf8,
 0xff,0xff,0x1f,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x80,0xff,
 0xff,0xff,0xff,0x01,0x00,0xf8,0xff,0xff,0x1f,0x00,0x00,0x00,0x00,0x00,0x00,
 0x00,0x00,0x00,0x00,0xf0,0xff,0xff,0xff,0xff,0x01,0x00,0xfc,0xff,0xff,0x1f,
 0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0xf0,0xff,0xff,0xff,0xff,
 0x01,0x00,0xfe,0xff,0xff,0x0f,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
 0x00,0xff,0xff,0xff,0xff,0xff,0x01,0x00,0xfe,0xff,0xff,0x07,0x00,0x00,0x00,
 0x00,0x00,0x00,0x00,0x00,0x00,0x00,0xff,0xff,0xff,0xff,0xff,0x01,0x00,0xfe,
 0xff,0xff,0x03,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0xc0,0xff,0xff,
 0xff,0xff,0xff,0x00,0x00,0xff,0xff,0xff,0x03,0x00,0x00,0x00,0x00,0x00,0x00,
 0x00,0x00,0x00,0xc0,0xff,0xff,0xff,0xff,0xff,0x00,0x00,0xff,0xff,0xff,0x03,
 0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0xfc,0xff,0xff,0xff,0xff,0x7f,
 0x00,0x80,0xff,0xff,0xff,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
 0xfc,0xff,0xff,0xff,0xff,0x7f,0x00,0x80,0xff,0xff,0xff,0x00,0x00,0x00,0x00,
 0x00,0x00,0x00,0x00,0x00,0x00,0xfe,0xff,0xff,0xff,0xff,0x3f,0x00,0xc0,0xff,
 0xff,0xff,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0xfe,0xff,0xff,
 0xff,0xff,0x3f,0x00,0xc0,0xff,0xff,0x7f,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
 0x00,0x00,0x80,0xff,0xff,0xff,0xff,0xff,0x07,0x00,0xf0,0xff,0xff,0x3f,0x00,
 0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0xc0,0xff,0xff,0xff,0xff,0xff,0x07,
 0x00,0xf0,0xff,0xff,0x3f,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0xe0,
 0xff,0xff,0xff,0xff,0xff,0x01,0x00,0xe0,0xff,0xff,0x1f,0x00,0x00,0x00,0x00,
 0x00,0x00,0x00,0x00,0x00,0xf0,0xff,0xff,0xff,0xff,0xff,0x01,0x00,0xf0,0xff,
 0xff,0x0f,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0xfe,0xff,0xff,0xff,
 0xff,0x3f,0x00,0x00,0xf0,0xff,0xff,0x07,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
 0x00,0x00,0xfe,0xff,0xff,0xff,0xff,0x3f,0x00,0x00,0xf0,0xff,0xff,0x07,0x00,
 0x00,0x00,0x00,0x00,0x00,0x00,0x00,0xc0,0xff,0xff,0xff,0xff,0xff,0x07,0x00,
 0x00,0xfc,0xff,0xff,0x03,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0xe0,0xff,
 0xff,0xff,0xff,0xff,0x07,0x00,0x00,0xfc,0xff,0xff,0x03,0x00,0x00,0x00,0x00,
 0x00,0x00,0x00,0x00,0xf0,0xff,0xff,0xff,0xff,0xff,0x00,0x00,0x00,0xfe,0xff,
 0xff,0x01,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0xfc,0xff,0xff,0xff,0xff,
 0xff,0x00,0x00,0x00,0xff,0xff,0xff,0x01,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
 0x80,0xff,0xff,0xff,0xff,0xff,0x1f,0x00,0x00,0xc0,0xff,0xff,0x7f,0x00,0x00,
 0x00,0x00,0x00,0x00,0x00,0x00,0x80,0xff,0xff,0xff,0xff,0xff,0x0f,0x00,0x00,
 0xc0,0xff,0xff,0x7f,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0xc0,0xff,0xff,
 0xff,0xff,0xff,0x03,0x00,0x00,0xc0,0xff,0xff,0x3f,0x00,0x00,0x00,0x00,0x00,
 0x00,0x00,0x00,0xc0,0xff,0xff,0xff,0xff,0xff,0x03,0x00,0x00,0xc0,0xff,0xff,
 0x3f,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0xe0,0xff,0xff,0xff,0xff,0x1f,
 0x00,0x00,0x00,0xc0,0xff,0xff,0x07,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
 0xf0,0xff,0xff,0xff,0xff,0x1f,0x00,0x00,0x00,0xc0,0xff,0xff,0x07,0x00,0x00,
 0x00,0x00,0x00,0x00,0x00,0x00,0xf8,0xff,0xff,0xff,0xff,0x1f,0x00,0x00,0x00,
 0xe0,0xff,0xff,0x03,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0xfc,0xff,0xff,
 0xff,0xff,0x1f,0x00,0x00,0x00,0xe0,0xff,0xff,0x03,0x00,0x00,0x00,0x00,0x00,
 0x00,0x00,0x00,0xfc,0xff,0xff,0xff,0xff,0x01,0x00,0x00,0x00,0xe0,0xff,0xff,
 0x01,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0xfe,0xff,0xff,0xff,0xff,0x00,
 0x00,0x00,0x00,0xe0,0xff,0xff,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
 0xfe,0xff,0xff,0xff,0x1f,0x00,0x00,0x00,0x00,0xe0,0xff,0x1f,0x00,0x00,0x00,
 0x00,0x00,0x00,0x00,0x00,0x80,0xff,0xff,0xff,0xff,0x1f,0x00,0x00,0x00,0x00,
 0xf0,0xff,0x1f,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0xc0,0xff,0xff,0xff,
 0xff,0x01,0x00,0x00,0x00,0x00,0xf0,0xff,0x0f,0x00,0x00,0x00,0x00,0x00,0x00,
 0x00,0x00,0xe0,0xff,0xff,0xff,0xff,0x01,0x00,0x00,0x00,0x00,0xe0,0xff,0x0f,
 0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0xf0,0xff,0xff,0xff,0x0f,0x00,0x00,
 0x00,0x00,0x00,0xf8,0xff,0x07,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0xf0,
 0xff,0xff,0xff,0x07,0x00,0x00,0x00,0x00,0x00,0xf8,0xff,0x07,0x00,0x00,0x00,
 0x00,0x00,0x00,0x00,0x00,0xf8,0xff,0xff,0xff,0x00,0x00,0x00,0x00,0x00,0x00,
 0xfc,0xff,0x0f,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0xfc,0xff,0xff,0xff,
 0x00,0x00,0x00,0x00,0x00,0x00,0xfe,0xff,0x0f,0x00,0x00,0x00,0x00,0x00,0x00,
 0x00,0x00,0xfe,0xff,0x1f,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0xff,0xff,0x7f,
 0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0xff,0xff,0x1f,0x00,0x00,0x00,0x00,
 0x00,0x00,0x80,0xff,0xff,0xff,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x80,0xff,
 0xff,0x03,0x00,0x00,0x00,0x00,0x00,0x00,0xc0,0xff,0xff,0xff,0x01,0x00,0x00,
 0x00,0x00,0x00,0x00,0xc0,0xff,0x3f,0x02,0x00,0x00,0x00,0x00,0x00,0x00,0xc0,
 0xff,0xff,0xff,0x03,0x00,0x00,0x00,0x00,0x00,0x00,0xc0,0xff,0x3f,0x00,0x00,
 0x00,0x00,0x00,0x00,0x00,0xe0,0xff,0xff,0xff,0x7f,0x00,0x00,0x00,0x00,0x00,
 0x00,0xe0,0xff,0x3f,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0xe0,0xff,0xff,0xff,
 0x7f,0x00,0x00,0x00,0x00,0x00,0x00,0xe0,0xff,0x3f,0x00,0x00,0x00,0x00,0x00,
 0x00,0x00,0xe0,0xff,0xff,0xff,0xff,0x7f,0x00,0x00,0x00,0x00,0x00,0xe0,0xff,
 0x3f,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0xc0,0xff,0xff,0xff,0xff,0x7f,0x00,
 0x00,0x00,0x00,0x00,0xe0,0xff,0x3f,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0xc0,
 0xff,0xff,0xff,0xff,0xff,0x00,0x00,0x00,0x00,0x00,0xc0,0xff,0x7f,0x00,0x00,
 0x00,0x00,0x00,0x00,0x00,0x80,0xff,0xff,0xff,0xff,0xff,0x00,0x00,0x00,0x00,
 0x00,0x80,0xff,0xff,0x01,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0xfe,0xff,0xff,
 0xff,0x3f,0x00,0x00,0x00,0x00,0x00,0x80,0xff,0xff,0x01,0x00,0x00,0x00,0x00,
 0x00,0x00,0x00,0xf0,0xff,0xff,0xff,0x3f,0x00,0x00,0x00,0x00,0x00,0x00,0xff,
 0xff,0x03,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
 0x00,0x00,0x00,0x00,0x00,0xfe,0xff,0x07,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
 0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0xfc,0xff,0x0f,0x00,
 0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
 0x00,0x00,0xf8,0xff,0x0f,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
 0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0xf0,0xff,0x1f,0x00,0x00,0x00,0x00,
 0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0xe0,
 0xff,0x3f,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
 0x00,0x00,0x00,0x00,0x00,0xe0,0xff,0xff,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
 0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0xc0,0xff,0xff,0x00,
 0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
 0x00,0x00,0x80,0xff,0xff,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
 0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x80,0xff,0xff,0x00,0x00,0x00,0x00,
 0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
 0xfe,0xff,0x01,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
 0x00,0x00,0x00,0x00,0x00,0x00,0xf8,0x7f,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
 0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x80,0x3f,0x00,
 0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
 0x00,0x00,0x00,0x00,0x0f,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
 0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
 0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
 0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
 0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
 0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
 0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
 0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
 0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
 0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
 0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
 0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
 0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
 0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00};
