;History:46,1

_TEXT	SEGMENT  BYTE PUBLIC 'CODE'
_TEXT	ENDS
_DATA	SEGMENT  WORD PUBLIC 'DATA'
_DATA	ENDS
CONST	SEGMENT  WORD PUBLIC 'CONST'
CONST	ENDS
_BSS	SEGMENT  WORD PUBLIC 'BSS'
_BSS	ENDS

bios_seg segment at 40h
	org	9
bios_conout	label	far
bios_seg ends

DGROUP	GROUP	CONST,	_BSS,	_DATA
	ASSUME  CS: _TEXT, DS: DGROUP, SS: DGROUP, ES: DGROUP

parm		equ	ss:[bp]

_TEXT	SEGMENT

 public		_asmputc

putc_stack		struc
 putc_bp	dw	?
 putc_return	dd	?
 char		db	?
putc_stack		ends

_asmputc	proc	far
		push	bp
		mov	bp,sp
		mov	al,parm.char
		call	bios_conout
		pop	bp
		ret
_asmputc	endp

_TEXT		ends
		end
