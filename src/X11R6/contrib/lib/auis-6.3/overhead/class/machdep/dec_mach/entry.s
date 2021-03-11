/* ********************************************************************** *\
 *         Copyright IBM Corporation 1988,1991 - All Rights Reserved      *
 *        For full copyright information see:'andrew/config/COPYRITE'     *
\* ********************************************************************** */

/* $Disclaimer: 
Permission to use, copy, modify, and distribute this software and its 
documentation for any purpose is hereby granted without fee, 
provided that the above copyright notice appear in all copies and that 
both that copyright notice, this permission notice, and the following 
disclaimer appear in supporting documentation, and that the names of 
IBM, Carnegie Mellon University, and other copyright holders, not be 
used in advertising or publicity pertaining to distribution of the software 
without specific, written prior permission.

IBM, CARNEGIE MELLON UNIVERSITY, AND THE OTHER COPYRIGHT HOLDERS 
DISCLAIM ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING 
ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS.  IN NO EVENT 
SHALL IBM, CARNEGIE MELLON UNIVERSITY, OR ANY OTHER COPYRIGHT HOLDER 
BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY 
DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, 
WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS 
ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE 
OF THIS SOFTWARE.
 $ */

/* The code that follows was written for a MIPS R2000 or R3000 series
 * processor.  The code was written by Zalman Stern from the description in
 * the "MIPS R2000 RISC Architecture" by Gerry Kane
 */

/* Might be better to let the assembler fill the delay slots below, but I did
 * it myself. In any event, the code should still work even if the next line
 * is removed.
 */
	.set noreorder
	.data
	.globl	class_ErrorReturnValue
	.text
	.globl	class_Lookup

#include <machine/regdef.h>

#ifdef __STDC__
#define ClassEntry(n) \
	.ent	ClassEntry ## n\
	.globl	ClassEntry ## n\
_ClassEntry ## n:\
	b	ClassEntry\
	  li	t0, n\
	.end		ClassEntry ## n
#else
#define ClassEntry(n) \
	.ent	ClassEntry/**/n;\
	.globl	ClassEntry/**/n;\
ClassEntry/**/n:\
	b	MainClassEntry;\
	  li	t0, n;\
	.end	ClassEntry/**/n
#endif

#include <../common/entrydefs.h>

	.ent	MainClassEntry
MainClassEntry:
#define framesize 48
	subu	sp, framesize
	.frame	sp, framesize, $31
/* Not sure if I should include the argument registers in the save mask.
 * That is how it is done now though.
 */
	.mask 0x800100f0, 4
	sw	ra, framesize - 4(sp) /* register 31 */
/* We need one temporary which spans the call to class_Lookup to hold the
 * classprocedure index. Register s0 is allocated for this and must be
 * saved.
 */
	sw	s0, framesize - 8(sp) /* register 16 */
/* Save the argument and floating point argument registers.
 * It might not be necessary to save the fp arg registers.
 * **********No need to save a0 since it is passed in soley for the use of this routine.********* this is wrong, a0 should be saved some classprocedures actually do reference their first arg which they expect to be in a0 -rr2b
 * We just pass it through to class_Lookup so it works out fine.
 * All other argument registers need to be saved though.
 */
	sw	a3, framesize - 12(sp) /* register 7 */
	sw	a2, framesize - 16(sp) /* register 6 */
	sw	a1, framesize - 20(sp) /* register 5 */
	sw	a0, framesize - 24(sp)
/* I hope storing a double works even if a single precision arg was passed.
 */
	.fmask	0x00005000, 28
	s.d	$f14, framesize - 32(sp) /* register f14 */
	s.d	$f12, framesize - 40(sp) /* register f12 */
/* 2 words of save area in here for arguments to class_Lookup.
 * Finally save the return address.
 * Multiply index by four to get offset into classprocedure table.
 */
	sll	s0, t0, 2
/* Call class_Lookup, setting up second argument in the delay slot.
 */
	jal	class_Lookup
	  addi	a1, s0, 0
/* Restore what state we can now.
 */
	lw	a0, framesize - 24(sp)
	lw	a1, framesize - 20(sp)
	lw	a2, framesize - 16(sp)
	lw	a3, framesize - 12(sp)
	l.d	$f12, framesize - 40(sp)
	l.d	$f14, framesize - 32(sp)
/* Test for an error return from class_Lookup.
 */
	beq	v0, $0, error
	  lw	ra, framesize - 4(sp) /* Restore return address. */
	add	t0, s0, v0
	lw	t0, (t0)
	lw	s0, framesize - 8(sp)
	j	t0
	  add	sp, sp, framesize
error:
/* This next instruction assumes the assmebler is inteligent and will convert
 * the lw into the following:
 *	lui	v0, upper_16_bits_sign_adjusted(_class_ErrorReturnValue)
 *	lw	v0, v0, lower_16_bits(_class_ErrorReturnValue)
 */
	lw	v0, class_ErrorReturnValue
	lw	s0, framesize - 8(sp)
	j	ra
	  add	sp, sp, framesize
	.end MainClassEntry
