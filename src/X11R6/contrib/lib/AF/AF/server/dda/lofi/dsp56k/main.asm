;
; Copyright 1993 by Digital Equipment Corporation, Maynard, Massachusetts.
; 
; Permission to use, copy, modify, distribute, and sell this software and its 
; documentation for any purpose is hereby granted without fee, provided that 
; the above copyright notice appear in all copies and that both that 
; copyright notice and this permission notice appear in supporting 
; documentation, and that the name of Digital not be used in advertising or 
; publicity pertaining to distribution of the software without specific, 
; written prior permission.  Digital makes no representations about the 
; suitability of this software for any purpose.  It is provided "as is" 
; without express or implied warranty.
; 
; DIGITAL DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING 
; ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL 
; DIGITAL BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY 
; DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN 
; AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF 
; OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
;
;------------------------------------------------------------------------------
;
;	Customized version of crt056y.asm for Lo-Fi.
;	Used by AudioFile server for LoFi too!
;
; Modfied by: 
;  ?/?/91 	tml	from motorola original.
;  4/11/91 	tml	change boot sequence, mode bits.
;			add host port initialization.  fix ipl levels.
;  1/23/92	tml	CHange to mode 0, placing reset at 0.
;			Manipulate memory organization, use same crt file
;			for AudioFile and devices area.
;------------------------------------------------------------------------------
	; $Id: main.asm,v 1.9 1993/11/12 17:51:59 tml Exp $

	opt so,xr
	page 132,66,3,3

	include "lofihw.asm"
	include	"intequ.asm"
	include	"ioequlc.asm"

;
;  This file contains the definitions for the DSP/Host common mailbox area.
;  It is automatically (with CPP) derived from "dsp.h"
;
	include "dsp.asm"

;------------------------------------------------------------------------------
;
;  Local (on-chip) RAM
;
;    Some variables are shadowed here to minimize the number of accesses to
;    shared, off-chip RAM.
;

	section	local_ram
	global local_hifi_time
	global local_codec_time
	org	x:$0

local_hifi_time		dc	$0
local_codec_time	dc	$0

	endsec
	
;------------------------------------------------------------------------------
;
;  This reset code resides in "high" memory at 0xe000 and is used
;  exactly once (at reset/startup).
;
	section	hw_reset
	org	p:$e000
	movec	#1,sp			;init stack pointer
	movec	#0,sr			;clear loop flag/interrupt mask bits

	movec	#$0,omr			; mode 0
	nop

;
; Copy program into on-chip program memory (512 words).  (Takes advantage of
; the fact that the 32K word shared memory is mapped to 0x0000 and 0x8000)
;

	move	#$0,r1
	move	#$1,n1
	move	#$8000,r0
	move	#$1,n0
	do	#$200,end2
	move	p:(r0)+,a0
	move	a0,p:(r1)+
end2

	move	#0,r0			; zero x and y internal memory
	move	#1,n0
	move	#0,a0
	do	#$200,end3
	move	a0,x:(r0)
	move	a0,y:(r0)+
	nop	
end3
	jmp	start

	endsec
	
;------------------------------------------------------------------------------
;
;  Exception vectors (gets loaded into low memory)
;


	section reset

	org	p:I_IRQA
	jsr	<codec_isr	; CODEC interrupt

	org	p:I_SSITD
	jsr 	<ssi_int	; SSI tx data

	org	p:I_SSITDE
	jsr 	<ssi_int	; SSI tx data w/ error


;------------------------------------------------------------------------------
;
;  The remaining code resides in the on-chip program memory.
;

	global	start
start

        and	#$f3,mr
        and	#$bf,ccr

	movep	#$0,x:m_ipr		; all disabled.
	movep	#$6,x:m_bcr		; 0 wait in memory, 6 in Y:IO
	movep	#$1,x:m_pbc		; Enable port B as Host port.

	bset	#2,x:$ffe8		; enable host commands

;
;   Configure the DSP port
;	CRA and CRB set the DSP port configuration.  The values come from 
;	x:dsp_cra, and x:dsp_crb, which are set by the host before startup.
;

	movep	x:dsp_cra,x:<<m_cra
	movep	x:dsp_crb,x:<<m_crb

;  Intialize the SSI port (Port C)
;	For many DSP port devices, these bits set the sample rate.  The
;	value comes from x:portc_bits, and is set by the host before 
;	startup.
;
	movep	#$0001f8,x:m_pcc	; set up SCI pins as parallel port
	movep	#$7,x:m_pcddr		; output bits

	movep	x:portc_bits,x:m_pcd	; set the port C bits

	movep	#$0,x:<<m_tx		; init SSI

;
;  Initialize the buffer address registers.  The values are read from the
;  shared memory area and are assumed to be initialized by the host before 
;  the DSP is let out of reset.
;
	move	x:codec_start0,r0
	move	x:codec_len0,m0
	move	x:codec_ofs0,n0

	move	x:codec_start1,r1
	move	x:codec_len1,m1
	move	x:codec_ofs1,n1

	move	x:hifi_start0,r2
	move	x:hifi_len0,m2
	move	x:hifi_ofs0,n2

	move	x:hifi_start1,r3
	move	x:hifi_len1,m3
	move	x:hifi_ofs1,n3

;
;    Set the host flags to tell the host we are here.
;
	bset	#3,x:m_hcr		; set host flag #2 (HF2)
	bset	#4,x:m_hcr		; set host flag #3 (HF3)

	move	#>1,x0			; always 1
	movep	#$3401,x:m_ipr		; set IPLs:
					;     CODEC = 1, Host = 1, SSI = 2

	and	#$fc,mr			; enable all IPLs
	
wait
	jmp	wait			; spin loop

;------------------------------------------------------------------------------
;
;  SSI Interrupt handler
;
;    This routine gets called for each WORD.  The transmit frame 
;    sync (TFS) bit in the SSI status register (SR) is used to differentiate
;    between channels.  If the TFS bit is set, then the word is the first
;    in the frame (channel A).  If it is clear, then the word is for Channel 
;    B.
;
;  **NOTE:  this routine assumes exclusive use of the A register
;
	global	ssi_int
ssi_int
	jclr	#m_tfs,x:<<m_sr,ssi_rx_b	; which channel?

; Channel 0 (A)
	bclr	#1,x:<<m_crb		; toggle OF1 (SC1)

	movep	x:m_rx,x:(r2)		; get receive data to buffer

	movep	x:(r2+n2),x:m_tx	; transmit data, send it
	move	#0,a1
	move	a1,x:(r2+n2)		; backfill silence

	lua	(r2)+,r2		; increment
	rti

; Channel 1 (B)

ssi_rx_b
	bset	#1,x:<<m_crb
	movep	x:m_rx,x:(r3)		; get receive data to buffer

	movep	x:(r3+n3),x:m_tx	; transmit data, send it
	move	#0,a1
	move	a1,x:(r3+n3)		; backfill silence

	lua	(r3)+,r3		; increment

;  Increment and update the hifi clock
	move	x:hifi_time,a
	add	x0,a			; x0 should always be 1
	move	a1,x:hifi_time

	rti


;------------------------------------------------------------------------------
;
;  CODEC Interrupt Handler (IRQA)
;
;
;  **NOTE:  this routine assumes exclusive use of the B register
;
	global	codec_isr

codec_isr

; Clear interrupt by reading CODEC 0's IR
	move	#CIR,b0
	move	b0,y:ACSR
	movep	y:<<ACODEC0,b0		; do read

	move	#CBb,b0
	move	b0,y:ACSR		; point to B channel buffer

; Fetch and write outgoing data (backfill with silence)

	move	#$ffffff,b1		; silence
	movep	x:(r0+n0),y:<<ACODEC0		; send it
	move	b1,x:(r0+n0)
	movep	x:(r1+n1),y:<<ACODEC1		; send it
	move	b1,x:(r1+n1)

; Read and store incoming data

	movep	y:<<ACODEC0,x:(r0)+	; store and increment
	movep	y:<<ACODEC1,x:(r1)+	; store and increment

;  Increment and update the codec clock
	move	x:codec_time,b
	add	x0,b			; x0 should always be 1
	move	b1,x:codec_time

	rti

	endsec
