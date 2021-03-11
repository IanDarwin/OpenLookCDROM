

/*
	$Disclaimer: 
 * Permission to use, copy, modify, and distribute this software and its 
 * documentation for any purpose is hereby granted without fee, 
 * provided that the above copyright notice appear in all copies and that 
 * both that copyright notice, this permission notice, and the following 
 * disclaimer appear in supporting documentation, and that the names of 
 * IBM, Carnegie Mellon University, and other copyright holders, not be 
 * used in advertising or publicity pertaining to distribution of the software 
 * without specific, written prior permission.
 * 
 * IBM, CARNEGIE MELLON UNIVERSITY, AND THE OTHER COPYRIGHT HOLDERS 
 * DISCLAIM ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING 
 * ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS.  IN NO EVENT 
 * SHALL IBM, CARNEGIE MELLON UNIVERSITY, OR ANY OTHER COPYRIGHT HOLDER 
 * BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY 
 * DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, 
 * WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS 
 * ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE 
 * OF THIS SOFTWARE.
 *  $
*/
#ifndef UNITS_H
#define UNITS_H

#define units_PICASperINCH	(6.0)
#define units_POINTSperINCH	(72.0)
#define units_MILSperINCH	(1000.0)
#define units_INCHESperFOOT	(12.0)
#define units_INCHESperYARD	(36.0)
#define units_INCHESperROD	(5.5 * units_INCHESperYARD)
#define units_INCHESperFURLONG	(40 * units_INCHESperROD)
#define units_INCHESperMILE	(5280 * units_INCHESperFOOT)
#define units_INCHESperLINK	(7.92)
#define units_INCHESperCHAIN	(100 * units_INCHESperLINK)
#define units_INCHESperFATHOM	(6 * units_INCHESperFOOT)
#define units_INCHESperNAUTMILE	(6O76.1155 * units_INCHESperFOOT)
#define units_INCHESperLEAGUE	(3 * units_INCHESperNAUTMILE)
#define units_INCHESperCUBIT	(18.0)
#define units_INCHESperLIGHTYEAR (5878510000000.0 * units_INCHESperMILE)
#define units_INCHESperPARSEC	(3.262 * units_INCHESperLIGHTYEAR)

#define units_INCHESperMETER	(39.37007262)

#define units_ANGSTROMSperMETER	(10000000000.0)
#define units_MICRONSperMETER	(1000000.0)
#define units_MILLIMETERSperMETER (1000.0)
#define units_CENTIMETERSperMETER (100.0)
#define units_METERSperKILOMETER  (1000.0)

#define units_RADIANSperDEGREE	(3.14159265 / 180.0)

#define units_CELSIUSperFAHRENHEIT (5.0 / 9.0)
#define units_FAHRENHEITperCELSIUS (9.0 / 5.0)

#define units_CELSIUSinFAHRENHEIT(cel) \
	(units_FAHRENHEITperCELSIUS * (cel) + 32.0)
#define units_FAHRENHEITinCELSIUS(fah) \
	(units_CELSIUSperFAHRENHEIT * ((fah) - 32.0))

#endif
