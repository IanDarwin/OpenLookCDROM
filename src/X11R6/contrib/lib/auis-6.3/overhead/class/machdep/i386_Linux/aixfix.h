/* ********************************************************************** *\
 *         Copyright IBM Corporation 1988,1991 - All Rights Reserved      *
 *        For full copyright information see:'andrew/config/COPYRITE'     *
\* ********************************************************************** */

/* MACROS TO HELP PORTABILITITY */

#ifndef AIX
#define RP_LENGTH( rp ) ( rp->r_length )
#define IS_RP_EXTERN( rp ) ( rp->r_extern )
#define IS_RP_PC_REL( rp ) ( rp->r_pcrel )
#define SYM_TYPE( sp ) ( sp->n_type & N_TYPE )
#define IS_EXTERN_SYM( sp ) ( sp->n_type & N_EXT )
#else
#define nlist syment
#define r_address r_vaddr
#define r_symbolnum r_symndx
#define relocation_info reloc
#define N_EXT C_EXT
#define N_TYPE N_SECT
#define RP_LENGTH( rp ) ( \
  ( ( rp->r_type == 2 ) || ( rp->r_type == 3 ) ) ? 0 \
: ( ( ( rp->r_type == 4 ) || ( rp->r_type == 5 ) ) ? 1 \
  : ( ( ( rp->r_type == 6 ) || ( rp->r_type == 7 ) \
     || ( rp->r_type == 9 ) || ( rp->r_type == 12 ) ) ? 2 \
    : ( ( rp->r_type == 8 ) ? 3 \
      : /* rp->r_type == R_ABS or R_SEG86 or R_SEG286 */ -1 ) ) ) )
#define IS_RP_EXTERN( rp ) ( ( rp->r_symndx & S_BSS ) != S_BSS )
#define IS_RP_PC_REL( rp ) ( \
    ( rp->r_type == 3 ) || ( rp->r_type == 5 ) \
 || ( rp->r_type == 7 ) || ( rp->r_type == 9 ) \
 || ( rp->r_type == 12 ) )
#define SYM_TYPE( sp ) ( sp->n_sclass & N_SECT )
#define IS_EXTERN_SYM( sp ) ( ( sp->n_sclass & N_CLASS ) == C_EXT )
#define N_BADMAG( x ) BADMAG( x )
#define N_TXTOFF( x ) A_TEXTPOS( x )
#endif


