#ifndef	_DICT_H_
#define	_DICT_H_

#include "gen_da.h"

/******* useful constants: **********/

	/* export: */

#define EnvNameDictPath "WORDG2B_PATH"

#define Dict_OK         0
#define Dict_NOT_OK     1

#define CSegmentDelimiter       '/'

	/* private: */

#define	NFirstHzTable	32768

#define HashHz(bp) ( ( ( *(bp) & 0x7f ) << 8 ) | *((bp)+1) )
#define NHzBytes        2
#define NInitPdaSons    2

/******* useful data types: **********/

/* public: */

#ifndef Dict_BYTE
#define Dict_BYTE
typedef	unsigned char	Byte, *PtrByte ;
#endif/*Dict_BYTE*/

/* private: */

typedef	struct {
	PtrDynArray	pdaSons ;

	Byte		hzByte1 ;
	Byte		hzByte2 ;
	unsigned char	cFreq ;	
	unsigned char	ucNClass ;
	unsigned long	ulWordClasses ;
	float		fProb ;
} TrieNode, *PtrTrieNode ;

struct _HandleDict { 
	PtrTrieNode *	paFirstHzTrieNode ;
	int		nMaxNumberOfHzInWord ;
} ;

/* public: */

typedef	struct _HandleDict *	HandleDict ;

/******** private global variables for internal implementation: ********/

#ifdef  OBSOLETE
extern	PtrTrieNode	raFirstHzTrieNode[ ];
#else /*OBSOLETE*/
#endif/*OBSOLETE*/
extern  int		giOutTrieLevel ;
extern  FILE *		gFileOut ;
extern  FILE *		gFileIn ;

/******** exported functions: ********/

/***********************/
/*$C
 * Dict_LoadHzCiDictionary -- load the Hanzi Ci (word) dictionary with
 *              word class information.
 * IN:   the compiled dictionary file name.
 *
 * PRE:  the compiled dictionary file must exist and be correct.
 * POST: the dictionary file is opened and read into memory.
 *
 * RETURN     HandleDict if loading is OK;
 *      otherwise, fatal error and exit.
 *        
 */
/*EXPORT_FUN*/  HandleDict      Dict_LoadHzCiDictionary( /* cpDICTFileName */ );

/***********************/
/*$C
 * Dict_MaxNumberofHzInWord -- 
 *
 * IN HandleDict
 *
 * RETURN the maximum number of hanzi of the longest word(s) in the dictionary.
 *
 * PRE:  the compiled dictionary file must exist and be correct.
 * POST: no change
 *
 */
/*EXPORT_FUN*/  int     Dict_MaxNumberofHzInWord( /* IN handleDict */ );

/***********************/
/*$C
 * Dict_GetHzCiInfo --
 *
 * IN   handleDict is a handle to the data structure of the Dictionary of HzCi.
 * IN   bp is a pointer to a hanzi word (not necessarily null-terminated).
 * IN   nHzCount is the no. of hanzi to be processed in bp.
 *
 * OUT  *ucAddrNClass holds the number of classes that the hanzi word belongs to;
 *      == 0 means the hanzi word is not in the dictionary.
 * OUT  *ulAddrClassValues holds the bit mask of the class of the hanzi word;
 *      == 0 means the hanzi word is not in the dictionary.
 * OUT  *fAddrProb holds the probability (0.0 .. 1.0) of the hanzi word;
 *      == 0.0 means the hanzi word is not in the dictionary.
 *
 * PRE:  bp != NULL & has nHzCount hanzi.
 * POST: no side effect to global environment.
 *
 * RETURN     Dict_OK if OK;
 *        Dict_NOT_OK if can't find the given word in the handleDict.
 */
/*EXPORT_FUN*/  int     Dict_GetHzCiInfo( /* handleDict, bp, nHzCount, iAddrNClass, ulAddrClassValues, fAddrProb */ );

/***********************/
/*$C
 * Dict_GetHzCiMaximalMatchQueue --
 *
 * IN   handleDict is a handle to the data structure of the Dictionary of HzCi.
 * IN   bp is a pointer to a hanzi word (not necessarily null-terminated).
 * IN   nHzCount is the no. of hanzi to be processed in bp.
 *
 * PRE:  bp != NULL & has nHzCount hanzi.
 * POST: no side effect to global environment.
 *
 * RETURN     Dict_OK if OK;
 *        Dict_NOT_OK if error.
 */
/*EXPORT_FUN*/  int     Dict_GetHzCiMaximalMatchQueue( /* handleDict, bp, addr_nHzCount */ );

/***********************/
/*$C
 * Dict_FirstInMaximalMatchQueue -- Get information of First entry in MaximalMatchQueue
 *
 * RETURN     PtrTrieNode or NULL
 */
/*EXPORT_FUN*/  PtrTrieNode     Dict_FirstInMaximalMatchQueue();

/***********************/
/*$C
 * Dict_Next_InMaximalMatchQueue -- Get information of Next entry in MaximalMatchQueue
 * -- must be used after calling to Dict_FirstInMaximalMatchQueue().
 *
 * RETURN     PtrTrieNode or NULL
 */
/*EXPORT_FUN*/  PtrTrieNode     Dict_Next_InMaximalMatchQueue();

/***********************/
/*$C
 * Dict_Last_InMaximalMatchQueue -- Get information of Last_ entry in MaximalMatchQueue
 *
 * RETURN     PtrTrieNode or NULL
 */
/*EXPORT_FUN*/  PtrTrieNode     Dict_Last_InMaximalMatchQueue();

/***********************/
/*$C
 * Dict_Prev_InMaximalMatchQueue -- Get information of Prev entry in MaximalMatchQueue
 * -- must be used after calling to Dict_Last_InMaximalMatchQueue().
 *
 * RETURN     PtrTrieNode or NULL
 */
/*EXPORT_FUN*/  PtrTrieNode     Dict_Prev_InMaximalMatchQueue();


#endif/*_DICT_H_*/
