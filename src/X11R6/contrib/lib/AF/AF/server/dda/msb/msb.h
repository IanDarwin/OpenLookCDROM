/*
 * *****************************************************************
 * *                                                               *
 * *    Copyright (c) Digital Equipment Corporation, 1993          *
 * *                                                               *
 * *   All Rights Reserved.  Unpublished rights  reserved  under   *
 * *   the copyright laws of the United States.                    *
 * *                                                               *
 * *   The software contained on this media  is  proprietary  to   *
 * *   and  embodies  the  confidential  technology  of  Digital   *
 * *   Equipment Corporation.  Possession, use,  duplication  or   *
 * *   dissemination of the software and media is authorized only  *
 * *   pursuant to a valid written license from Digital Equipment  *
 * *   Corporation.                                                *
 * *                                                               *
 * *   RESTRICTED RIGHTS LEGEND   Use, duplication, or disclosure  *
 * *   by the U.S. Government is subject to restrictions  as  set  *
 * *   forth in Subparagraph (c)(1)(ii)  of  DFARS  252.227-7013,  *
 * *   or  in  FAR 52.227-19, as applicable.                       *
 * *                                                               *
 * *****************************************************************
 */

/*
 *	Microsoft Sound Board Driver definitions
 */
#ifndef MSB_H
#define MSB_H

#include <sys/types.h>
#include <sys/ioctl.h>

struct msb_info {
   int flag;			/* Is open boolean */
};

#define DEV_MSB_STR "msb   "

/*	Reset ioctl	*/
typedef enum {
  MSB_RESET,
  MSB_RESET_AUTOCALIBRATE 
} msb_reset_type;

typedef struct {
  msb_reset_type cmd;
} msb_reset_t;
#define MSB_RESET	_IOW(0,0,msb_reset_t)

/*	get io info ioctl */
typedef struct {
  char * start;
  u_int  size;
} msb_buffer_info_t;
typedef struct {
  msb_buffer_info_t read;
  msb_buffer_info_t write;
} msb_io_info_t;
#define MSB_GET_IO_INFO	_IOR(0,1,msb_io_info_t)

/*	get device info ioctl	*/
typedef enum {
  MSB_MUSTANG_SOUND,
  MSB_MICROSOFT_SOUND
} msb_device_type;

typedef enum {
  CODEC_AD1848_J = 0,
  CODEC_AD1848_K,
  CODEC_UNKNOWN
} msb_codec_type;
#define MSB_MAX_CODECS CODEC_UNKNOWN

typedef enum {
  MIDI_YMF262,
  MIDI_UNKNOWN
} msb_midi_type;

typedef struct {
  u_int type;
  u_int version;
} msb_dev_ident_t;

typedef struct {
  msb_dev_ident_t msb_ident;
  msb_dev_ident_t codec_ident;
  msb_dev_ident_t midi_ident;
} msb_dev_info_t;

#define MSB_GET_DEV_INFO _IOR(0,2,msb_dev_info_t)

/* read and write  ioctl */
typedef enum {
  MSB_IO_START,
  MSB_IO_STOP,
  MSB_IO_UPDATE
} msb_io_cmd_type;

typedef struct {
  msb_io_cmd_type cmd;
  u_int inputOffset;
  u_int	outputOffset;
} msb_io_t;
#define MSB_READ	_IOWR(0,3,msb_io_t)
#define MSB_WRITE	_IOWR(0,4,msb_io_t)

/* select input ioctl */
typedef enum {
  MSB_LINE_SRC=0,
  MSB_AUX1_SRC,
  MSB_MIC_SRC,
  MSB_PM_DAC_SRC,
  MSB_AUX1,
  MSB_AUX2,
  MSB_DAC,
  MSB_MIX_CTL
} msb_channel_types;
#define MSB_CHANNEL_FIRST MSB_LINE_SRC
#define MSB_CHANNEL_LAST MSB_MIX_CTL
#define MSB_CHANNEL_TOTAL (MSB_CHANNEL_LAST-MSB_CHANNEL_FIRST+1)

typedef struct {
  msb_channel_types left;
  msb_channel_types right;
} msb_input_t;
#define MSB_SELECT_INPUT	_IOW(0,5,msb_input_t)

/* set gain ioctl */

#define MSB_M_LINE_SRC 	(1<<MSB_LINE_SRC)
#define MSB_M_MIC_SRC  	(1<<MSB_MIC_SRC)
#define MSB_M_AUX1_SRC 	(1<<MSB_AUX1_SRC)
#define MSB_M_PM_DAC_SRC  (1<<MSB_PM_DAC_SRC)
#define MSB_M_AUX1     	(1<<MSB_AUX1)
#define MSB_M_AUX2 	(1<<MSB_AUX2)
#define MSB_M_DAC	(1<<MSB_DAC)
#define MSB_M_MIX_CTL	(1<<MSB_MIX_CTL)

#define MSB_M_ALL_INPUT MSB_M_LINE_SRC | MSB_M_MIC_SRC | \
			MSB_M_AUX1_SRC  | MSB_M_PM_DAC_SRC
#define MSB_M_ALL_OUTPUT MSB_M_AUX1 | MSB_M_AUX2 | MSB_M_DAC | MSB_M_MIX_CTL
#define MSB_M_ALL_CHANNELS MSB_M_ALL_INPUT | MSB_M_ALL_OUTPUT
#define MSB_M_INVALID_CHANNELS ~MSB_M_ALL_CHANNELS

typedef enum {
  MSB_ENABLE = 0,
  MSB_DISABLE
} msb_enable_type;

typedef struct {
  u_int channel_mask_left;
  u_int channel_mask_right;
  int	value;
  msb_enable_type enable;
} msb_gain_t;

#define MSB_SET_GAIN		_IOW(0,6,msb_gain_t)

/* set sample type ioctl */

typedef enum {
  MSB_RATE_5512_5=0,
  MSB_RATE_6615,
  MSB_RATE_8000,
  MSB_RATE_9600,
  MSB_RATE_11025,
  MSB_RATE_16000,
  MSB_RATE_18900,
  MSB_RATE_22050,
  MSB_RATE_27428_57,
  MSB_RATE_32000,
  MSB_RATE_33075,
  MSB_RATE_37800,
  MSB_RATE_44100,
  MSB_RATE_48000
} msb_rate_type;
#define MSB_RATE_FIRST MSB_RATE_5512_5
#define MSB_RATE_LAST MSB_RATE_48000
#define MSB_RATE_TOTAL (MSB_RATE_LAST-MSB_RATE_FIRST+1)

typedef enum {
  MSB_FORMAT_ALAW=0,
  MSB_FORMAT_MULAW,
  MSB_FORMAT_PCM_8,
  MSB_FORMAT_PCM_16
} msb_format_type;
#define MSB_FORMAT_FIRST MSB_FORMAT_ALAW
#define MSB_FORMAT_LAST MSB_FORMAT_PCM_16
#define MSB_FORMAT_TOTAL (MSB_FORMAT_LAST-MSB_FORMAT_FIRST+1)

typedef enum {
  MSB_MODE_MONO = 1,
  MSB_MODE_STEREO
} msb_mode_type;

typedef struct {
  msb_rate_type rate;
  msb_format_type format;
  msb_mode_type type;
} msb_sample_type_t;

#define MSB_SET_SAMPLE_TYPE _IOW(0,9,msb_sample_type_t)

/* register read and write ioctl */
typedef enum {
  MSB_AUTOSEL_CONFIG,
  MSB_AUTOSEL_VERSION,
  MSB_CODEC_ADDR,
  MSB_CODEC_DATA,
  MSB_CODEC_STATUS,
  MSB_CODEC_PIO,
  MSB_MIDI_STATUS,
  MSB_MIDI_ADDRESS0,
  MSB_MIDI_DATA0, 
  MSB_MIDI_ADDRESS1,
  MSB_MIDI_DATA1
} msb_reg_types_t;

typedef struct {
  msb_reg_types_t reg_num;
  u_int data;
} msb_reg_io_t;

#define MSB_REG_READ	_IOWR(0,11,msb_reg_io_t)
#define MSB_REG_WRITE   _IOWR(0,12,msb_reg_io_t)

/* debug ioct */
typedef struct {
  u_int debug_mask;
  u_int prev_mask;
} msb_debug_t;

#define MSB_M_DEBUG_NONE	0x00
#define MSB_M_DEBUG_INFO	0x01
#define MSB_M_DEBUG_IO		0x02
#define MSB_M_DEBUG_LOCATION	0x04
#define MSB_M_DEBUG_ERROR	0x08
#define MSB_M_DEBUG_PROBE	0x10
#define MSB_M_DEBUG_ATTACH	0x20
#define MSB_M_DEBUG_VERBOSE	0x40
#define MSB_M_DEBUG_MULTI_OPEN	0x80
#define MSB_M_DEBUG_ALL		(MSB_M_DEBUG_INFO     | MSB_M_DEBUG_IO     | \
				 MSB_M_DEBUG_LOCATION | MSB_M_DEBUG_ERROR  | \
				 MSB_M_DEBUG_PROBE    | MSB_M_DEBUG_ATTACH | \
				 MSB_M_DEBUG_VERBOSE  | MSB_M_DEBUG_MULTI_OPEN)

#define MSB_DEBUG	_IOWR(0,13,msb_debug_t)

#endif	/* MSB_H */
