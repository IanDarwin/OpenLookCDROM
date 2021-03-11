/******************************************************************
Copyright 1993, 1994 by Digital Equipment Corporation, Maynard, Massachusetts,

                        All Rights Reserved

Permission to use, copy, modify, and distribute this software and its 
documentation for any purpose and without fee is hereby granted, 
provided that the above copyright notice appear in all copies and that
both that copyright notice and this permission notice appear in 
supporting documentation, and that the names of Digital or MIT not be
used in advertising or publicity pertaining to distribution of the
software without specific, written prior permission.  

DIGITAL DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING
ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL
DIGITAL BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR
ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,
ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
SOFTWARE.

  Author: Hiroyuki Miyamoto  Digital Equipment Corporation
                             miyamoto@jrd.dec.com
******************************************************************/

#include <X11/Xlibint.h>
#include "FrameMgr.h"

/* Convenient macro */

#define _UNIT(n)   ((int)(n) & 0xff)
#define _NUMBER(n) (((int)(n) >> 8) & 0xff)

/* For byte swapping */

#define Swap16(p, n) ((p)->byte_swap ?       \
		      (((n) << 8 & 0xff00) | \
		       ((n) >> 8 & 0xff)     \
		      ) : n)
#define Swap32(p, n) ((p)->byte_swap ?            \
		      (((n) << 24 & 0xff000000) | \
		       ((n) <<  8 & 0xff0000) |   \
		       ((n) >>  8 & 0xff00) |     \
		       ((n) >> 24 & 0xff)         \
		      ) : n)

/* Type definition */

typedef struct _Iter *Iter;

typedef struct _FrameInst *FrameInst;

typedef union {
    int num;		/* For BARRAY */
    FrameInst fi;	/* For POINTER */
    Iter iter;		/* For ITER */
} ExtraDataRec, *ExtraData;

typedef struct _Chain {
	ExtraDataRec d;
	int frame_no;
	struct _Chain *next;
} ChainRec, *Chain;

typedef struct _ChainMgr {
    Chain top;
    Chain tail;
} ChainMgrRec, *ChainMgr;

typedef struct _ChainIter {
    Chain cur;
} ChainIterRec, *ChainIter;

typedef struct _FrameIter {
    Iter iter;
    Bool counting;
    unsigned int counter;
    int end;
    struct _FrameIter* next;
} FrameIterRec, *FrameIter;

typedef struct _FrameInst {
    XimFrame template;
    ChainMgrRec cm;
    int cur_no;
} FrameInstRec;

typedef void  (*IterStartWatchProc)(
#if NeedFunctionPrototypes
				    Iter it, void* client_data
#endif
				    );

typedef struct _Iter {
    XimFrame template;
    int max_count;
    Bool allow_expansion;
    ChainMgrRec cm;
    int cur_no;
    IterStartWatchProc start_watch_proc;
    void* client_data;
    Bool start_counter;
} IterRec;

typedef struct _FrameMgr {
    XimFrame frame;
    FrameInst fi;
    char* area;
    int idx;
    Bool byte_swap;
    int total_size;
    FrameIter iters;
} FrameMgrRec;

typedef union {
    int num;          /* For BARRAY and PAD */
    struct {          /* For COUNTER_* */
	Iter iter;        
	Bool is_byte_len;
    } counter;
} XimFrameTypeInfoRec, *XimFrameTypeInfo;

/* Special values */
#define NO_VALUE -1
#define NO_VALID_FIELD -2

#if NeedFunctionPrototypes
static FrameInst FrameInstInit(XimFrame frame);
static void FrameInstFree(FrameInst fi);
static XimFrameType FrameInstGetNextType(FrameInst fi, XimFrameTypeInfo info);
static XimFrameType FrameInstPeekNextType(FrameInst fi, XimFrameTypeInfo info);
static FmStatus FrameInstSetSize(FrameInst fi, int num);
static FmStatus FrameInstSetIterCount(FrameInst fi, int num);
static int FrameInstGetTotalSize(FrameInst fi);
static void FrameInstReset(FrameInst fi);

static Iter IterInit(XimFrame frame, int count);
static void IterFree(Iter it);
static int FrameInstGetSize(FrameInst fi);
static int IterGetSize(Iter it);
static XimFrameType IterGetNextType(Iter it, XimFrameTypeInfo info);
static XimFrameType IterPeekNextType(Iter it, XimFrameTypeInfo info);
static FmStatus IterSetSize(Iter it, int num);
static FmStatus IterSetIterCount(Iter it, int num);
static int IterGetTotalSize(Iter it);
static void IterReset(Iter it);
static Bool IterIsLoopEnd(Iter it, Bool* myself);
static void IterSetStartWatch(Iter it, IterStartWatchProc proc, void* client_data);
static void _IterStartWatch(Iter it, void* client_data);

static ExtraData ChainMgrGetExtraData(ChainMgr cm, int frame_no);
static ExtraData ChainMgrSetData(ChainMgr cm, int frame_no,
				 ExtraDataRec data);
static Bool ChainIterGetNext(ChainIter ci, int* frame_no, ExtraData d);
static int _FrameInstIncrement(XimFrame frame, int count);
static int _FrameInstDecrement(XimFrame frame, int count);
static int _FrameInstGetItemSize(FrameInst fi, int cur_no);
static Bool FrameInstIsIterLoopEnd(FrameInst fi);

static FrameIter _FrameMgrAppendIter(FrameMgr fm, Iter it, int end);
static FrameIter _FrameIterCounterIncr(FrameIter fitr, int i);
static void _FrameMgrRemoveIter(FrameMgr fm, FrameIter it);
static Bool _FrameMgrIsIterLoopEnd(FrameMgr fm);
static Bool _FrameMgrProcessPadding(FrameMgr fm, FmStatus* status);
#else
FrameInst FrameInstInit();
static void FrameInstFree();
static XimFrameType FrameInstGetNextType();
static XimFrameType FrameInstPeekNextType();
static FmStatus FrameInstSetSize();
static FmStatus FrameInstSetIterCount();
static int FrameInstGetTotalSize();
static void FrameInstReset();

static Iter IterInit();
static void IterFree();
static int FrameInstGetSize();
static int IterGetSize();
static XimFrameType IterGetNextType();
static XimFrameType IterPeekNextType();
static FmStatus IterSetSize();
static FmStatus IterSetIterCount();
static int IterGetTotalSize();
static void IterReset();
static Bool IterIsLoopEnd();
static void IterSetStartWatch();
static void _IterStartWatch();
static ExtraData ChainMgrGetExtraData();
static ExtraData ChainMgrSetData();
static Bool ChainIterGetNext();
static int _FrameInstIncrement();
static int _FrameInstDecrement();
static int _FrameInstGetItemSize();
static Bool FrameInstIsIterLoopEnd();

static FrameIter _FrameMgrAppendIter();
static FrameIter _FrameIterCounterIncr();
static void _FrameMgrRemoveIter();
static Bool _FrameMgrIsIterLoopEnd();
static Bool _FrameMgrProcessPadding();
#endif

#define IterGetIterCount(it) ((it)->allow_expansion ? \
			      NO_VALUE : (it)->max_count)

#define IterFixIteration(it) ((it)->allow_expansion = False)

#define IterSetStarter(it) ((it)->start_counter = True)

#define ChainMgrInit(cm) (cm)->top = (cm)->tail = NULL
#define ChainMgrFree(cm) {\
    Chain tmp, cur = (cm)->top;\
\
    while (cur) {\
	tmp = cur->next;\
	Xfree(cur);\
	cur = tmp;\
    }\
}
#define ChainIterInit(ci, cm) {\
    (ci)->cur = (cm)->top;\
}
/* ChainIterFree has nothing to do. */
#define ChainIterFree(ci)

#define FrameInstIsEnd(fi) ((fi)->template[(fi)->cur_no].type == EOL)

#if NeedFunctionPrototypes
FrameMgr FrameMgrInit(XimFrame frame, char* area, Bool byte_swap)
#else
FrameMgr FrameMgrInit(frame, area, byte_swap)
XimFrame frame;
void* area;
Bool byte_swap;
#endif
{
    FrameMgr fm;

    fm = (FrameMgr)Xmalloc(sizeof(FrameMgrRec));

    fm->frame = frame;
    fm->fi = FrameInstInit(frame);
    fm->area = (char *)area;
    fm->idx = 0;
    fm->byte_swap = byte_swap;
    fm->total_size = NO_VALUE;
    fm->iters = NULL;

    return fm;
}

#if NeedFunctionPrototypes
void FrameMgrInitWithData(FrameMgr fm, XimFrame frame, void* area,
			  Bool byte_swap)
#else
void FrameMgrInitWithData(fm, frame, area, byte_swap)
FrameMgr fm;
XimFrame frame;
void* area;
Bool byte_swap;
#endif
{
    fm->frame = frame;
    fm->fi = FrameInstInit(frame);
    fm->area = (char *)area;
    fm->idx = 0;
    fm->byte_swap = byte_swap;
    fm->total_size = NO_VALUE;
}

#if NeedFunctionPrototypes
void FrameMgrFree(FrameMgr fm)
#else
void FrameMgrFree(fm)
FrameMgr fm;
#endif
{
    FrameInstFree(fm->fi);
    Xfree(fm);
}

#if NeedFunctionPrototypes
FmStatus FrameMgrSetBuffer(FrameMgr fm, void* area)
#else
FmStatus FrameMgrSetBuffer(fm, area)
FrameMgr fm;
void* area;
#endif
{
    if (fm->area) {
	return FmBufExist;
    }
    fm->area = (char *)area;
    return FmSuccess;
}

#if NeedFunctionPrototypes
FmStatus _FrameMgrPutToken(FrameMgr fm, void* data, int data_size)
#else
FmStatus _FrameMgrPutToken(fm, data, data_size)
FrameMgr fm;
void* data;
int data_size;
#endif
{
    XimFrameType type;
    XimFrameTypeInfoRec info;

    if (fm->total_size != NO_VALUE && fm->idx >= fm->total_size)
	return FmNoMoreData;

    type = FrameInstGetNextType(fm->fi, &info);

    if (type & COUNTER_MASK) {
	unsigned long input_length;

	if (info.counter.is_byte_len) {
	    if ((input_length = IterGetTotalSize(info.counter.iter))
		== NO_VALUE) {
		return FmCannotCalc;
	    }
	} else {
	    if ((input_length = IterGetIterCount(info.counter.iter))
		== NO_VALUE) {
		return FmCannotCalc;
	    }
	}
	if (type == COUNTER_BIT8) {
	    *(CARD8*)(fm->area + fm->idx) = input_length;
	    fm->idx++;
	} else if (type == COUNTER_BIT16) {
	    *(CARD16*)(fm->area + fm->idx) = Swap16(fm, input_length);
	    fm->idx += 2;
	} else if (type == COUNTER_BIT32) {
	    *(CARD32*)(fm->area + fm->idx) = Swap32(fm, input_length);
	    fm->idx += 4;
	}
	_FrameMgrPutToken(fm, data, data_size);
	return FmSuccess;
    }

    if (type == BIT8) {
	if (data_size == sizeof(unsigned char)) {
	    unsigned long num = *(unsigned char*)data;
	    *(CARD8*)(fm->area + fm->idx) = num;
	} else if (data_size == sizeof(unsigned short)) {
	    unsigned long num = *(unsigned short*)data;
	    *(CARD8*)(fm->area + fm->idx) = num;
	} else if (data_size == sizeof(unsigned int)) {
	    unsigned long num = *(unsigned int*)data;
	    *(CARD8*)(fm->area + fm->idx) = num;
	} else if (data_size == sizeof(unsigned long)) {
	    unsigned long num = *(unsigned long*)data;
	    *(CARD8*)(fm->area + fm->idx) = num;
	} else {
	    ;/* Should never reached */
	}
	fm->idx++;
	return FmSuccess;
    } else if (type == BIT16) {
	if (data_size == sizeof(unsigned char)) {
	    unsigned long num = *(unsigned char*)data;
	    *(CARD16*)(fm->area + fm->idx) = Swap16(fm, num);
	} else if (data_size == sizeof(unsigned short)) {
	    unsigned long num = *(unsigned short*)data;
	    *(CARD16*)(fm->area + fm->idx) = Swap16(fm, num);
	} else if (data_size == sizeof(unsigned int)) {
	    unsigned long num = *(unsigned int*)data;
	    *(CARD16*)(fm->area + fm->idx) = Swap16(fm, num);
	} else if (data_size == sizeof(unsigned long)) {
	    unsigned long num = *(unsigned long*)data;
	    *(CARD16*)(fm->area + fm->idx) = Swap16(fm, num);
	} else {
	    ;/* Should never reached */
	}
	fm->idx += 2;
	return FmSuccess;
    } else if (type == BIT32) {
	if (data_size == sizeof(unsigned char)) {
	    unsigned long num = *(unsigned char*)data;
	    *(CARD32*)(fm->area + fm->idx) = Swap32(fm, num);
	} else if (data_size == sizeof(unsigned short)) {
	    unsigned long num = *(unsigned short*)data;
	    *(CARD32*)(fm->area + fm->idx) = Swap32(fm, num);
	} else if (data_size == sizeof(unsigned int)) {
	    unsigned long num = *(unsigned int*)data;
	    *(CARD32*)(fm->area + fm->idx) = Swap32(fm, num);
	} else if (data_size == sizeof(unsigned long)) {
	    unsigned long num = *(unsigned long*)data;
	    *(CARD32*)(fm->area + fm->idx) = Swap32(fm, num);
	} else {
	    ;/* Should never reached */
	}
	fm->idx += 4;
	return FmSuccess;
    }
     else if (type == BARRAY && info.num != NO_VALUE) {
	if (info.num > 0) {
	    bcopy(*(char**)data, fm->area + fm->idx, info.num);
	    fm->idx += info.num;
	}
	return FmSuccess;
    } else if (type == BARRAY && info.num == NO_VALUE) {
	return FmInvalidCall;
    } else if (type == PADDING && info.num != NO_VALUE) {
        fm->idx += info.num;
	return _FrameMgrPutToken(fm, data, data_size);
    } else if (type == PADDING && info.num == NO_VALUE) {
        return FmInvalidCall;
    } else if (type == ITER) {
	return FmInvalidCall;
    } else if (type == EOL) {
	return FmEOD;
    } else {
	; /* Should never be reached */
    }
}

#if NeedFunctionPrototypes
FmStatus _FrameMgrGetToken(FrameMgr fm , void* data, int data_size)
#else
FmStatus _FrameMgrGetToken(fm, data, data_size)
FrameMgr fm;
void* data;
int data_size;
#endif
{
    XimFrameType type;
    static XimFrameTypeInfoRec info; /* memory */
    ExtraData d;
    FrameIter fitr;

    if (fm->total_size != NO_VALUE && fm->idx >= fm->total_size)
	return FmNoMoreData;

    type = FrameInstGetNextType(fm->fi, &info);

    if (type & COUNTER_MASK) {
	int end;
	FrameIter client_data;

	type &= ~COUNTER_MASK;
	if (type == BIT8) {
	    end = *(CARD8*)(fm->area + fm->idx);
	}
	else if (type == BIT16) {
	    end = Swap16(fm, *(CARD16*)(fm->area + fm->idx));
	}
	else if (type == BIT32) {
	    end = Swap32(fm, *(CARD32*)(fm->area + fm->idx));
	}
	if (client_data = _FrameMgrAppendIter(fm, info.counter.iter, end)) {
	    IterSetStarter(info.counter.iter);
	    IterSetStartWatch(info.counter.iter, 
			      _IterStartWatch, (void*)client_data);
	}
    }

    type &= ~COUNTER_MASK;
    if (type == BIT8) {
	if (data_size == sizeof(unsigned char)) {
	    *(unsigned char*)data = *(CARD8*)(fm->area + fm->idx);
	} else if (data_size == sizeof(unsigned short)) {
	    *(unsigned short*)data = *(CARD8*)(fm->area + fm->idx);
	} else if (data_size == sizeof(unsigned int)) {
	    *(unsigned int*)data = *(CARD8*)(fm->area + fm->idx);
	} else if (data_size == sizeof(unsigned long)) {
	    *(unsigned long*)data = *(CARD8*)(fm->area + fm->idx);
	} else {
	    ;/* Should never reached */
	}
	fm->idx++;
	if (fitr = _FrameIterCounterIncr(fm->iters, 1/*BIT8*/)) {
	    _FrameMgrRemoveIter(fm, fitr);
	}
	return FmSuccess;
    } else if (type == BIT16) {
	if (data_size == sizeof(unsigned char)) {
	    *(unsigned char*)data =
		Swap16(fm, *(CARD16*)(fm->area + fm->idx));
	} else if (data_size == sizeof(unsigned short)) {
	    *(unsigned short*)data =
		Swap16(fm, *(CARD16*)(fm->area + fm->idx));
	} else if (data_size == sizeof(unsigned int)) {
	    *(unsigned int*)data =
		Swap16(fm, *(CARD16*)(fm->area + fm->idx));
	} else if (data_size == sizeof(unsigned long)) {
	    *(unsigned long*)data =
		Swap16(fm, *(CARD16*)(fm->area + fm->idx));
	} else {
	    ;/* Should never reached */
	}
	fm->idx += 2;
	if (fitr = _FrameIterCounterIncr(fm->iters, 2/*BIT16*/)) {
	    _FrameMgrRemoveIter(fm, fitr);
	}
	return FmSuccess;
    } else if (type == BIT32) {
	if (data_size == sizeof(unsigned char)) {
	    *(unsigned char*)data =
		Swap32(fm, *(CARD32*)(fm->area + fm->idx));
	} else if (data_size == sizeof(unsigned short)) {
	    *(unsigned short*)data =
		Swap32(fm, *(CARD32*)(fm->area + fm->idx));
	} else if (data_size == sizeof(unsigned int)) {
	    *(unsigned int*)data =
		Swap32(fm, *(CARD32*)(fm->area + fm->idx));
	} else if (data_size == sizeof(unsigned long)) {
	    *(unsigned long*)data =
		Swap32(fm, *(CARD32*)(fm->area + fm->idx));
	} else {
	    ;/* Should never reached */
	}
	fm->idx += 4;
	if (fitr = _FrameIterCounterIncr(fm->iters, 4/*BIT32*/)) {
	    _FrameMgrRemoveIter(fm, fitr);
	}
	return FmSuccess;
    }
      else if (type == BARRAY && info.num != NO_VALUE) {
	if (info.num > 0) {
	    *(char**)data = fm->area + fm->idx;
	    fm->idx += info.num;
	    if (fitr = _FrameIterCounterIncr(fm->iters, info.num)) {
		_FrameMgrRemoveIter(fm, fitr);
	    }
	} else {
	    *(char**)data = NULL;
	}
	return FmSuccess;
    } else if (type == BARRAY && info.num == NO_VALUE) {
	return FmInvalidCall;
    } else if (type == PADDING && info.num != NO_VALUE) {
        fm->idx += info.num;
	if (fitr = _FrameIterCounterIncr(fm->iters, info.num)) {
	    _FrameMgrRemoveIter(fm, fitr);
	}
	return _FrameMgrGetToken(fm, data, data_size);
    } else if (type == PADDING && info.num == NO_VALUE) {
        return FmInvalidCall;
    } else if (type == ITER) {
	return FmInvalidCall;	/* if comes here, it's a bug! */
    } else if (type == EOL) {
	return FmEOD;
    } else {
	; /* Should never be reached */
    }
}


#if NeedFunctionPrototypes
FmStatus FrameMgrSetSize(FrameMgr fm, int barray_size)
#else
FmStatus FrameMgrSetSize(fm, barray_size)
FrameMgr fm;
int barray_size;
#endif
{
    if (FrameInstSetSize(fm->fi, barray_size) == FmSuccess)
	return FmSuccess;
    else
	return FmNoMoreData;
}


#if NeedFunctionPrototypes
FmStatus FrameMgrSetIterCount(FrameMgr fm, int count)
#else
FmStatus FrameMgrSetIterCount(fm, count)
FrameMgr fm;
int count;
#endif
{
    if (FrameInstSetIterCount(fm->fi, count) == FmSuccess)
	return FmSuccess;
    else
	return FmNoMoreData;
}


#if NeedFunctionPrototypes
FmStatus FrameMgrSetTotalSize(FrameMgr fm, int total_size)
#else
FmStatus FrameMgrSetTotalSize(fm, total_size)
FrameMgr fm;
int total_size;
#endif
{
    fm->total_size = total_size;
    return FmSuccess;
}


#if NeedFunctionPrototypes
int FrameMgrGetTotalSize(FrameMgr fm)
#else
int FrameMgrGetTotalSize(fm)
FrameMgr fm;
#endif
{
    return FrameInstGetTotalSize(fm->fi);
}


#if NeedFunctionPrototypes
int FrameMgrGetSize(FrameMgr fm)
#else
int FrameMgrGetSize(fm)
FrameMgr fm;
#endif
{
    register int ret_size;

    ret_size = FrameInstGetSize(fm->fi);
    if (ret_size == NO_VALID_FIELD)
	return NO_VALUE;

    return ret_size;
}


#if NeedFunctionPrototypes
FmStatus FrameMgrSkipToken(FrameMgr fm, int skip_count)
#else
FmStatus FrameMgrSkipToken(fm, skip_count)
FrameMgr fm;
int skip_count;
#endif
{
    XimFrameType type;
    XimFrameTypeInfoRec info;
    register int i;

    if (fm->total_size != NO_VALUE && fm->idx >= fm->total_size)
	return FmNoMoreData;

    for (i = 0; i < skip_count; i++) {
	type = FrameInstGetNextType(fm->fi, &info);
	type &= ~COUNTER_MASK;

	if (type == BIT8) {
	    fm->idx++;
	} else if (type == BIT16) {
	    fm->idx += 2;
	} else if (type == BIT32) {
	    fm->idx += 4;
	}
	  else if (type == BARRAY && info.num != NO_VALUE) {
	    fm->idx += info.num;
	} else if (type == BARRAY && info.num == NO_VALUE) {
	    return FmInvalidCall;
	} else if (type == PADDING && info.num != NO_VALUE) {
	    fm->idx += info.num;
	    return FrameMgrSkipToken(fm, skip_count);
	} else if (type == PADDING && info.num == NO_VALUE) {
	    return FmInvalidCall;
	} else if (type == ITER) {
	    return FmInvalidCall;
	} else if (type == EOL) {
	    return FmEOD;
	} else {
	    ; /* Should never be reached */
	}
    }
    return FmSuccess;
}


#if NeedFunctionPrototypes
void FrameMgrReset(FrameMgr fm)
#else
void FrameMgrReset(fm)
FrameMgr fm;
#endif
{
    fm->idx = 0;
    FrameInstReset(fm->fi);
}


#if NeedFunctionPrototypes
Bool FrameMgrIsIterLoopEnd(FrameMgr fm, FmStatus* status)
#else
Bool FrameMgrIsIterLoopEnd(fm, status)
FrameMgr fm;
FmStatus* status;
#endif
{
    do {
	if (_FrameMgrIsIterLoopEnd(fm)) {
	    return(True);
	}
    } while (_FrameMgrProcessPadding(fm, status));
    
    return(False);
}


/* Internal routines */

#if NeedFunctionPrototypes
static Bool _FrameMgrIsIterLoopEnd(FrameMgr fm)
#else
static Bool _FrameMgrIsIterLoopEnd(fm)
FrameMgr fm;
#endif
{
    return(FrameInstIsIterLoopEnd(fm->fi));
}


#if NeedFunctionPrototypes
static Bool _FrameMgrProcessPadding(FrameMgr fm, FmStatus* status)
#else
static Bool _FrameMgrProcessPadding(fm, status)
FrameMgr fm;
FmStatus* status;
#endif
{
    XimFrameTypeInfoRec info;
    XimFrameType next_type = FrameInstPeekNextType(fm->fi, &info);
    FrameIter fitr;

    if ((next_type == PADDING) && (info.num != NO_VALUE)) {
        next_type = FrameInstGetNextType(fm->fi, &info);
        fm->idx += info.num;
	if (fitr = _FrameIterCounterIncr(fm->iters, info.num)) {
	    _FrameMgrRemoveIter(fm, fitr);
	}
	*status = FmSuccess;
	return(True);
    } 
    else if ((next_type == PADDING) && (info.num == NO_VALUE)) {
	*status = FmInvalidCall;
	return(True);
    }
    else {
	*status = FmSuccess;
	return(False);
    }
}


#if NeedFunctionPrototypes
static FrameInst FrameInstInit(XimFrame frame)
#else
static FrameInst FrameInstInit(frame)
XimFrame frame;
#endif
{
    FrameInst fi;
    register int i;

    fi= (FrameInst)Xmalloc(sizeof(FrameInstRec));

    fi->template = frame;
    fi->cur_no = 0;
    ChainMgrInit(&fi->cm);
    return fi;
}

#if NeedFunctionPrototypes
static void FrameInstFree(FrameInst fi)
#else
static void FrameInstFree(fi)
FrameInst fi;
#endif
{
    ChainIterRec ci;
    int frame_no;
    ExtraDataRec d;

    ChainIterInit(&ci, &fi->cm);

    while (ChainIterGetNext(&ci, &frame_no, &d)) {
	register XimFrameType type;
	type = fi->template[frame_no].type;
	if (type == ITER) {
	    if (d.iter)
		IterFree(d.iter);
	} else if (type == POINTER) {
	    if (d.fi)
		FrameInstFree(d.fi);
	}
    }
    ChainIterFree(&ci);
    ChainMgrFree(&fi->cm);
    Xfree(fi);
}

#if NeedFunctionPrototypes
static XimFrameType FrameInstGetNextType(FrameInst fi, XimFrameTypeInfo info)
#else
static XimFrameType FrameInstGetNextType(fi, info)
FrameInst fi;
XimFrameTypeInfo info;
#endif
{
    XimFrameType ret_type;

    ret_type = fi->template[fi->cur_no].type;

    switch (ret_type) {
      case BIT8 :
      case BIT16 :
      case BIT32 :
      case EOL :
	fi->cur_no = _FrameInstIncrement(fi->template, fi->cur_no);
	break;

      case COUNTER_BIT8 :
      case COUNTER_BIT16 :
      case COUNTER_BIT32 :
	if (info) {
	    register int offset, iter_idx;

	    info->counter.is_byte_len = 
		((int)fi->template[fi->cur_no].data & 0xff) == FmCounterByte;
	    offset = (int)fi->template[fi->cur_no].data >> 8;
	    iter_idx = fi->cur_no + offset;
	    if (fi->template[iter_idx].type == ITER) {
		ExtraData d;
		ExtraDataRec dr;

		if ((d = ChainMgrGetExtraData(&fi->cm, iter_idx)) == NULL) {
		    dr.iter = IterInit(&fi->template[iter_idx + 1], NO_VALUE);
		    d = ChainMgrSetData(&fi->cm, iter_idx, dr);
		}
		info->counter.iter = d->iter;
	    } else {
		/* Should not be reached here */
	    }
	}
	fi->cur_no = _FrameInstIncrement(fi->template, fi->cur_no);
	break;

      case BARRAY :
	if (info) {
	    ExtraData d;

	    if ((d = ChainMgrGetExtraData(&fi->cm, fi->cur_no)) == NULL) {
		info->num = NO_VALUE;
	    } else {
		info->num = d->num;
	    }
	}
	fi->cur_no = _FrameInstIncrement(fi->template, fi->cur_no);
	break;

      case PADDING :
	if (info) {
	    register int unit, number, size, i;

	    unit = _UNIT(fi->template[fi->cur_no].data);
	    number = _NUMBER(fi->template[fi->cur_no].data);
	    
	    i = fi->cur_no;
	    size = 0;
	    while (number > 0) {
	        i = _FrameInstDecrement(fi->template, i);
		size += _FrameInstGetItemSize(fi, i);
		number--;
	    }
	    info->num = (unit - (size % unit)) % unit;
	}
	fi->cur_no = _FrameInstIncrement(fi->template, fi->cur_no);
	break;
	
      case ITER :
	{
	    ExtraData d;
	    ExtraDataRec dr;
	    XimFrameType sub_type;


	    if ((d = ChainMgrGetExtraData(&fi->cm, fi->cur_no)) == NULL) {
		dr.iter = IterInit(&fi->template[fi->cur_no + 1], NO_VALUE);
		d = ChainMgrSetData(&fi->cm, fi->cur_no, dr);
	    }
	    sub_type = IterGetNextType(d->iter, info);
	    if (sub_type == EOL) {
		fi->cur_no = _FrameInstIncrement(fi->template, fi->cur_no);
		ret_type = FrameInstGetNextType(fi, info);
	    } else {
		ret_type = sub_type;
	    }
	}
	break;

      case POINTER :
	{
	    ExtraData d;
	    ExtraDataRec dr;
	    XimFrameType sub_type;

	    if ((d = ChainMgrGetExtraData(&fi->cm, fi->cur_no)) == NULL) {
		dr.fi = FrameInstInit(fi->template[fi->cur_no + 1].data);
		d = ChainMgrSetData(&fi->cm, fi->cur_no, dr);
	    }
	    sub_type = FrameInstGetNextType(d->fi, info);
	    if (sub_type == EOL) {
		fi->cur_no = _FrameInstIncrement(fi->template, fi->cur_no);
		ret_type = FrameInstGetNextType(fi, info);
	    } else {
		ret_type = sub_type;
	    }
	}
	break;
      default :
	/* Should never be reached */
	break;
    }
    return ret_type;
}


#if NeedFunctionPrototypes
static XimFrameType FrameInstPeekNextType(FrameInst fi, XimFrameTypeInfo info)
#else
static XimFrameType FrameInstPeekNextType(fi, info)
FrameInst fi;
XimFrameTypeInfo info;
#endif
{
    XimFrameType ret_type;

    ret_type = fi->template[fi->cur_no].type;

    switch (ret_type) {
      case BIT8 :
      case BIT16 :
      case BIT32 :
      case EOL :
	break;

      case COUNTER_BIT8 :
      case COUNTER_BIT16 :
      case COUNTER_BIT32 :
	if (info) {
	    register int offset, iter_idx;

	    info->counter.is_byte_len = 
		((int)fi->template[fi->cur_no].data & 0xff) == FmCounterByte;
	    offset = (int)fi->template[fi->cur_no].data >> 8;
	    iter_idx = fi->cur_no + offset;
	    if (fi->template[iter_idx].type == ITER) {
		ExtraData d;
		ExtraDataRec dr;

		if ((d = ChainMgrGetExtraData(&fi->cm, iter_idx)) == NULL) {
		    dr.iter = IterInit(&fi->template[iter_idx + 1], NO_VALUE);
		    d = ChainMgrSetData(&fi->cm, iter_idx, dr);
		}
		info->counter.iter = d->iter;
	    } else {
		/* Should not be reached here */
	    }
	}
	break;

      case BARRAY :
	if (info) {
	    ExtraData d;

	    if ((d = ChainMgrGetExtraData(&fi->cm, fi->cur_no)) == NULL) {
		info->num = NO_VALUE;
	    } else {
		info->num = d->num;
	    }
	}
	break;

      case PADDING :
	if (info) {
	    register int unit, number, size, i;

	    unit = _UNIT(fi->template[fi->cur_no].data);
	    number = _NUMBER(fi->template[fi->cur_no].data);
	    
	    i = fi->cur_no;
	    size = 0;
	    while (number > 0) {
	        i = _FrameInstDecrement(fi->template, i);
		size += _FrameInstGetItemSize(fi, i);
		number--;
	    }
	    info->num = (unit - (size % unit)) % unit;
	}
	break;
	
      case ITER :
	{
	    ExtraData d;
	    ExtraDataRec dr;
	    XimFrameType sub_type;


	    if ((d = ChainMgrGetExtraData(&fi->cm, fi->cur_no)) == NULL) {
		dr.iter = IterInit(&fi->template[fi->cur_no + 1], NO_VALUE);
		d = ChainMgrSetData(&fi->cm, fi->cur_no, dr);
	    }
	    sub_type = IterPeekNextType(d->iter, info);
	    if (sub_type == EOL) {
		ret_type = FrameInstPeekNextType(fi, info);
	    } else {
		ret_type = sub_type;
	    }
	}
	break;

      case POINTER :
	{
	    ExtraData d;
	    ExtraDataRec dr;
	    XimFrameType sub_type;

	    if ((d = ChainMgrGetExtraData(&fi->cm, fi->cur_no)) == NULL) {
		dr.fi = FrameInstInit(fi->template[fi->cur_no + 1].data);
		d = ChainMgrSetData(&fi->cm, fi->cur_no, dr);
	    }
	    sub_type = FrameInstPeekNextType(d->fi, info);
	    if (sub_type == EOL) {
		ret_type = FrameInstPeekNextType(fi, info);
	    } else {
		ret_type = sub_type;
	    }
	}
	break;
      default :
	/* If comes here, bug! */
	break;
    }
    return ret_type;
}


#if NeedFunctionPrototypes
static Bool FrameInstIsIterLoopEnd(FrameInst fi)
#else
static Bool FrameInstIsIterLoopEnd(fi)
FrameInst fi;
#endif
{
    Bool ret = False;

    if (fi->template[fi->cur_no].type == ITER) {
	ExtraData d = ChainMgrGetExtraData(&fi->cm, fi->cur_no);
	Bool yourself;

	if (d) {
	    ret = IterIsLoopEnd(d->iter, &yourself);
	    if (ret && yourself) {
		fi->cur_no = _FrameInstIncrement(fi->template, fi->cur_no);
	    }
	}
    }

    return(ret);
}


#if NeedFunctionPrototypes
static FrameIter _FrameMgrAppendIter(FrameMgr fm, Iter it, int end)
#else
static FrameIter _FrameMgrAppendIter(fm, it, end)
FrameMgr fm;
Iter it;
int end;
#endif
{
    FrameIter p = fm->iters;

    while (p && p->next) {
	p = p->next;
    }
    if (!p) {
	fm->iters = p = (FrameIter)Xmalloc(sizeof(FrameIterRec));
    }
    else {
	p->next = (FrameIter)Xmalloc(sizeof(FrameIterRec));
	p = p->next;
    }
    if (p) {
	p->iter = it;
	p->counting = False;
	p->counter = 0;
	p->end = end;
	p->next = NULL;
    }

    return(p);
}


#if NeedFunctionPrototypes
static void _FrameMgrRemoveIter(FrameMgr fm, FrameIter it)
#else
static void _FrameMgrRemoveIter(fm, it)
FrameMgr fm;
FrameIter it;
#endif
{
    FrameIter prev, p;

    prev = NULL;
    p = fm->iters;
    while (p) {
	if (p == it) {
	    if (prev) {
		prev->next = p->next;
	    }
	    else {
		fm->iters = p->next;
	    }
	    Xfree(p);
	    break;
	}
	else {
	    prev = p;
	    p = p->next;
	}
    }
}


#if NeedFunctionPrototypes
static FrameIter _FrameIterCounterIncr(FrameIter fitr, int i)
#else
static FrameIter _FrameIterCounterIncr(fitr, i)
FrameIter fitr;
int i;
#endif
{
    FrameIter p = fitr;

    while (p) {
	if (p->counting) {
	    p->counter += i;
	    if (p->counter >= p->end) {
		IterFixIteration(p->iter);
		return(p);
	    }
	}
	p = p->next;
    }
    return(NULL);
}


#if NeedFunctionPrototypes
static void _IterStartWatch(Iter it, void* client_data)
#else
static void _IterStartWatch(it, client_data)
Iter it;
void* client_data;
#endif
{
    FrameIter p = (FrameIter)client_data;
    p->counting = True;
}


#if NeedFunctionPrototypes
static FmStatus FrameInstSetSize(FrameInst fi, int num)
#else
static FmStatus FrameInstSetSize(fi, num)
FrameInst fi;
int num;
#endif
{
    ChainIterRec ci;
    ExtraData d;
    ExtraDataRec dr;
    XimFrameType type;
    register int i;

    i = 0;
    while ((type = fi->template[i].type) != EOL) {
	if (type == BARRAY) {
	    if ((d = ChainMgrGetExtraData(&fi->cm, i)) == NULL) {
		dr.num = -1;
		d = ChainMgrSetData(&fi->cm, i, dr);
	    }
	    if (d->num == NO_VALUE) {
		d->num = num;
		return FmSuccess;
	    }
	} else if (type == ITER) {
	    if ((d = ChainMgrGetExtraData(&fi->cm, i)) == NULL) {
		dr.iter = IterInit(&fi->template[i + 1], NO_VALUE);
		d = ChainMgrSetData(&fi->cm, i, dr);
	    }
	    if (IterSetSize(d->iter, num) == FmSuccess) {
		return FmSuccess;
	    }
	} else if (type == POINTER) {
	    if ((d = ChainMgrGetExtraData(&fi->cm, i)) == NULL) {
		dr.fi = FrameInstInit(fi->template[i + 1].data);
		d = ChainMgrSetData(&fi->cm, i, dr);
	    }
	    if (FrameInstSetSize(d->fi, num) == FmSuccess) {
		return FmSuccess;
	    }
	}
	i = _FrameInstIncrement(fi->template, i);
    }
    return FmNoMoreData;
}


#if NeedFunctionPrototypes
static int FrameInstGetSize(FrameInst fi)
#else
static int FrameInstGetSize(fi)
FrameInst fi;
#endif
{
    XimFrameType type;
    register int i;
    ExtraData d;
    ExtraDataRec dr;

    i = fi->cur_no;
    while ((type = fi->template[i].type) != EOL) {
	if (type == BARRAY) {
	    if ((d = ChainMgrGetExtraData(&fi->cm, i)) == NULL) {
		return NO_VALUE;
	    }
	    return d->num;
	} else if (type == ITER) {
	    int ret_size;
	    if ((d = ChainMgrGetExtraData(&fi->cm, i)) == NULL) {
		dr.iter = IterInit(&fi->template[i + 1], NO_VALUE);
		d = ChainMgrSetData(&fi->cm, i, dr);
	    }
	    ret_size = IterGetSize(d->iter);
	    if (ret_size != NO_VALID_FIELD)
		return ret_size;
	} else if (type == POINTER) {
	    int ret_size;
	    if ((d = ChainMgrGetExtraData(&fi->cm, i)) == NULL) {
		dr.fi = FrameInstInit(fi->template[i + 1].data);
		d = ChainMgrSetData(&fi->cm, i, dr);
	    }
	    ret_size = FrameInstGetSize(d->fi);
	    if (ret_size != NO_VALID_FIELD)
	        return ret_size;
	}
	i = _FrameInstIncrement(fi->template, i);
    }
    return NO_VALID_FIELD;
}


#if NeedFunctionPrototypes
static FmStatus FrameInstSetIterCount(FrameInst fi, int num)
#else
static FmStatus FrameInstSetIterCount(fi, num)
FrameInst fi;
int num;
#endif
{
    ChainIterRec ci;
    ExtraData d;
    ExtraDataRec dr;
    register int i;
    XimFrameType type;

    i = 0;
    while ((type = fi->template[i].type) != EOL) {
	if (type == ITER) {
	    if ((d = ChainMgrGetExtraData(&fi->cm, i)) == NULL) {
		dr.iter = IterInit(&fi->template[i + 1], num);
		(void)ChainMgrSetData(&fi->cm, i, dr);
		return FmSuccess;
	    }
	    if (IterSetIterCount(d->iter, num) == FmSuccess) {
		return FmSuccess;
	    }
	} else if (type == POINTER) {
	    if ((d = ChainMgrGetExtraData(&fi->cm, i)) == NULL) {
		dr.fi = FrameInstInit(fi->template[i + 1].data);
		d = ChainMgrSetData(&fi->cm, i, dr);
	    }
	    if (FrameInstSetIterCount(d->fi, num) == FmSuccess) {
		return FmSuccess;
	    }
	}
	i = _FrameInstIncrement(fi->template, i);
    }
    return FmNoMoreData;
}


#if NeedFunctionPrototypes
static int FrameInstGetTotalSize(FrameInst fi)
#else
static int FrameInstGetTotalSize(fi)
FrameInst fi;
#endif
{
    register int size, i;

    size = 0;

    i = 0;

    while (fi->template[i].type != EOL) {
        size += _FrameInstGetItemSize(fi, i);
	i = _FrameInstIncrement(fi->template, i);
    }
    return size;
}


#if NeedFunctionPrototypes
static void FrameInstReset(FrameInst fi)
#else
static void FrameInstReset(fi)
FrameInst fi;
#endif
{
    ChainIterRec ci;
    int frame_no;
    ExtraDataRec d;

    ChainIterInit(&ci, &fi->cm);

    while (ChainIterGetNext(&ci, &frame_no, &d)) {
	register XimFrameType type;
	type = fi->template[frame_no].type;
	if (type == ITER) {
	    if (d.iter)
		IterReset(d.iter);
	} else if (type == POINTER) {
	    if (d.fi)
		FrameInstReset(d.fi);
	}
    }
    ChainIterFree(&ci);

    fi->cur_no = 0;
}


#if NeedFunctionPrototypes
static Iter IterInit(XimFrame frame, int count)
#else
static Iter IterInit(frame, count)
XimFrame frame;
int count;
#endif
{
    Iter it;
    register XimFrameType type;

    it = (Iter)Xmalloc(sizeof(IterRec));
    it->template = frame;
    it->max_count = (count == NO_VALUE) ? 0 : count;
    it->allow_expansion = count == NO_VALUE;
    it->cur_no = 0;
    it->start_watch_proc = NULL;
    it->client_data = NULL;
    it->start_counter = False;

    type = frame->type;
    if (type & COUNTER_MASK) {  /* COUNTER_XXX cannot be an item of a ITER */
	Xfree(it);
	return NULL;
    }

    if (type == BIT8 || type == BIT16 || type == BIT32
	) {
	/* Do nothing */;
    } else if (type == BARRAY || type == ITER || type == POINTER) {
	ChainMgrInit(&it->cm);
    } else {
	Xfree(it);
	return NULL;/* This should never occur */
    }
    return it;
}


#if NeedFunctionPrototypes
static void IterFree(Iter it)
#else
static void IterFree(it)
Iter it;
#endif
{
    if (it->template->type == BARRAY) {
	ChainMgrFree(&it->cm);
    } else if (it->template->type == ITER) {
	ChainIterRec ci;
	int count;
	ExtraDataRec d;

	ChainIterInit(&ci, &it->cm);
	while (ChainIterGetNext(&ci, &count, &d)) {
	    IterFree(d.iter);
	}
	ChainIterFree(&ci);
	ChainMgrFree(&it->cm);
    } else if (it->template->type == POINTER) {
	ChainIterRec ci;
	int count;
	ExtraDataRec dr;

	ChainIterInit(&ci, &it->cm);
	while (ChainIterGetNext(&ci, &count, &dr)) {
	    FrameInstFree(dr.fi);
	}
	ChainIterFree(&ci);
	ChainMgrFree(&it->cm);
    }
    Xfree(it);
}


#if NeedFunctionPrototypes
static Bool IterIsLoopEnd(Iter it, Bool* myself)
#else
static Bool IterIsLoopEnd(it, myself)
Iter it;
Bool* myself;
#endif
{
    Bool ret = False;
    *myself = False;

    if (!it->allow_expansion && (it->cur_no == it->max_count)) {
	*myself = True;
	ret = True;
    }
    else if (it->template->type == POINTER) {
	ExtraData d = ChainMgrGetExtraData(&it->cm, it->cur_no);
	if (d) {
	    if (FrameInstIsIterLoopEnd(d->fi)) {
		ret = True;
	    }
	    else {
		if (FrameInstIsEnd(d->fi)) {
		    it->cur_no++;
		    if (!it->allow_expansion && (it->cur_no == it->max_count)) {
			*myself = True;
			ret = True;
		    }
		}
	    }
	}
    }
    else if (it->template->type == ITER) {
	ExtraData d = ChainMgrGetExtraData(&it->cm, it->cur_no);
	if (d) {
	    Bool yourself;
	    if (IterIsLoopEnd(d->iter, &yourself)) {
		ret = True;
	    }
	}
    }

    return(ret);
}


#if NeedFunctionPrototypes
static XimFrameType IterGetNextType(Iter it, XimFrameTypeInfo info)
#else
static XimFrameType IterGetNextType(it, info)
Iter it;
XimFrameTypeInfo info;
#endif
{
    XimFrameType type = it->template->type;

    if (it->start_counter) {
	(*it->start_watch_proc)(it, it->client_data);
	it->start_counter = False;
    }

    if (it->cur_no >= it->max_count) {
	if (it->allow_expansion) {
	    it->max_count = it->cur_no + 1;
	} else {
	    return EOL;
	}
    }

    if (type == BIT8 || type == BIT16 || type == BIT32
	) {
	it->cur_no++;
	return type;
    } else if (type == BARRAY) {
	ExtraData d;
	ExtraDataRec dr;

	if (info) {
	    if ((d = ChainMgrGetExtraData(&it->cm, it->cur_no)) == NULL) {
		info->num = NO_VALUE;
	    } else {
		info->num = d->num;
	    }
	}
	it->cur_no++;
	return BARRAY;
    } else if (type == ITER) {
	XimFrameType ret_type;
	ExtraData d;
	ExtraDataRec dr;

	if ((d = ChainMgrGetExtraData(&it->cm, it->cur_no)) == NULL) {
	    dr.iter = IterInit(it->template + 1, NO_VALUE);
	    d = ChainMgrSetData(&it->cm, it->cur_no, dr);
	}

	ret_type = IterGetNextType(d->iter, info);
	if (ret_type == EOL) {
	    it->cur_no++;
	    ret_type = IterGetNextType(it, info);
	}
	return ret_type;
    } else if (type == POINTER) {
	XimFrameType ret_type;
	ExtraData d;
	ExtraDataRec dr;

	if ((d = ChainMgrGetExtraData(&it->cm, it->cur_no)) == NULL) {
	    dr.fi = FrameInstInit(it->template[1].data);
	    d = ChainMgrSetData(&it->cm, it->cur_no, dr);
	}
	
	ret_type = FrameInstGetNextType(d->fi, info);
	if (ret_type == EOL) {
	    it->cur_no++;
	    ret_type = IterGetNextType(it, info);
	}
	return ret_type;
    } else {
	;/* This should never occur */
    }
}


#if NeedFunctionPrototypes
static XimFrameType IterPeekNextType(Iter it, XimFrameTypeInfo info)
#else
static XimFrameType IterPeekNextType(it, info)
Iter it;
XimFrameTypeInfo info;
#endif
{
    XimFrameType type = it->template->type;

    if (!it->allow_expansion && (it->cur_no >= it->max_count)) {
	return(EOL);
    }

    if (type == BIT8 || type == BIT16 || type == BIT32) {
	return(type);
    }
    else if (type == BARRAY) {
	ExtraData d;
	ExtraDataRec dr;

	if (info) {
	    if ((d = ChainMgrGetExtraData(&it->cm, it->cur_no)) == NULL) {
		info->num = NO_VALUE;
	    } else {
		info->num = d->num;
	    }
	}
	return(BARRAY);
    }
    else if (type == ITER) {
	XimFrameType ret_type;
	ExtraData d;
	ExtraDataRec dr;

	if ((d = ChainMgrGetExtraData(&it->cm, it->cur_no)) == NULL) {
	    dr.iter = IterInit(it->template + 1, NO_VALUE);
	    d = ChainMgrSetData(&it->cm, it->cur_no, dr);
	}

	ret_type = IterPeekNextType(d->iter, info);
	if (ret_type == EOL) {
	    ret_type = IterPeekNextType(it, info);
	}
	return(ret_type);
    }
    else if (type == POINTER) {
	XimFrameType ret_type;
	ExtraData d;
	ExtraDataRec dr;

	if ((d = ChainMgrGetExtraData(&it->cm, it->cur_no)) == NULL) {
	    dr.fi = FrameInstInit(it->template[1].data);
	    d = ChainMgrSetData(&it->cm, it->cur_no, dr);
	}
	
	ret_type = FrameInstPeekNextType(d->fi, info);
	if (ret_type == EOL) {
	    ret_type = IterPeekNextType(it, info);
	}
	return(ret_type);
    }
    else {
	;/* If comes here, bug! */
    }
}


#if NeedFunctionPrototypes
static FmStatus IterSetSize(Iter it, int num)
#else
static FmStatus IterSetSize(it, num)
Iter it;
int num;
#endif
{
    XimFrameType type;
    register int i;

    if (!it->allow_expansion && it->max_count == 0) {
	return FmNoMoreData;
    }

    type = it->template->type;
    if (type == BARRAY) {
	ExtraData d;
	ExtraDataRec dr;
	for (i = 0; i < it->max_count; i++) {
	    if ((d = ChainMgrGetExtraData(&it->cm, i)) == NULL) {
		dr.num = NO_VALUE;
		d = ChainMgrSetData(&it->cm, i, dr);
	    }
	    if (d->num == NO_VALUE) {
		d->num = num;
		return FmSuccess;
	    }
	}
	if (it->allow_expansion) {
	    ExtraDataRec dr;
	    dr.num = num;
	    (void)ChainMgrSetData(&it->cm, it->max_count, dr);
	    it->max_count++;

	    return FmSuccess;
	}
	return FmNoMoreData;
    } else if (type == ITER) {
	ExtraData d;
	ExtraDataRec dr;
	for (i = 0; i < it->max_count; i++) {
	    if ((d = ChainMgrGetExtraData(&it->cm, i)) == NULL) {
		dr.iter = IterInit(it->template + 1, NO_VALUE);
		d = ChainMgrSetData(&it->cm, i, dr);
	    }
	    if (IterSetSize(d->iter, num) == FmSuccess) {
		return FmSuccess;
	    }
	}
	if (it->allow_expansion) {
	    ExtraDataRec dr;
	    dr.iter = IterInit(it->template + 1, NO_VALUE);
	    (void)ChainMgrSetData(&it->cm, it->max_count, dr);
	    it->max_count++;

	    if (IterSetSize(dr.iter, num) == FmSuccess) {
		return FmSuccess;
	    }
	}
	return FmNoMoreData;
    } else if (type == POINTER) {
	ExtraData d;
	ExtraDataRec dr;
	for (i = 0; i < it->max_count; i++) {
	    if ((d = ChainMgrGetExtraData(&it->cm, i)) == NULL) {
		dr.fi = FrameInstInit(it->template[1].data);
		d = ChainMgrSetData(&it->cm, i, dr);
	    }
	    if (FrameInstSetSize(d->fi, num) == FmSuccess) {
		return FmSuccess;
	    }
	}
	if (it->allow_expansion) {
	    ExtraDataRec dr;
	    dr.fi = FrameInstInit(it->template[1].data);
	    (void)ChainMgrSetData(&it->cm, it->max_count, dr);
	    it->max_count++;

	    if (FrameInstSetSize(dr.fi, num) == FmSuccess) {
		return FmSuccess;
	    }
	}
	return FmNoMoreData;
    }
    return FmNoMoreData;
}


#if NeedFunctionPrototypes
static int IterGetSize(Iter it)
#else
static int IterGetSize(it)
Iter it;
#endif
{
    register int i;
    ExtraData d;
    ExtraDataRec dr;

    if (it->cur_no >= it->max_count) {
	return NO_VALID_FIELD;
    }

    if (it->template->type == BARRAY) {
	if ((d = ChainMgrGetExtraData(&it->cm, it->cur_no)) == NULL) {
	    return NO_VALUE;
	}
	return d->num;
    } else if (it->template->type == ITER) {
	for (i = it->cur_no; i < it->max_count; i++) {
	    int ret_size;
	    if ((d = ChainMgrGetExtraData(&it->cm, i)) == NULL) {
		dr.iter = IterInit(it->template + 1, NO_VALUE);
		d = ChainMgrSetData(&it->cm, i, dr);
	    }
	    ret_size = IterGetSize(d->iter);
	    if (ret_size == NO_VALID_FIELD)
		continue;
	    else
		return ret_size;
	}
	return NO_VALID_FIELD;
    } else if (it->template->type == POINTER) {
	for (i = it->cur_no; i < it->max_count; i++) {
	    int ret_size;
	    if ((d = ChainMgrGetExtraData(&it->cm, i)) == NULL) {
		dr.fi = FrameInstInit(it->template[1].data);
		d = ChainMgrSetData(&it->cm, i, dr);
	    }
	    ret_size = FrameInstGetSize(d->fi);
	    if (ret_size == NO_VALID_FIELD)
		continue;
	    else
		return ret_size;
	}
	return NO_VALID_FIELD;
    }
    return NO_VALID_FIELD;
}


#if NeedFunctionPrototypes
static FmStatus IterSetIterCount(Iter it, int num)
#else
static FmStatus IterSetIterCount(it, num)
Iter it;
int num;
#endif
{
    XimFrameType type;
    register int i;

    if (it->allow_expansion) {
	it->max_count = num;
	it->allow_expansion = False;
	return FmSuccess;
    }

    if (it->max_count == 0) {
	return FmNoMoreData;
    }

    if (it->template->type == ITER) {
	for (i = 0; i < it->max_count; i++) {
	    ExtraData d;
	    ExtraDataRec dr;
	    if ((d = ChainMgrGetExtraData(&it->cm, i)) == NULL) {
		dr.iter = IterInit(it->template + 1, num);
		(void)ChainMgrSetData(&it->cm, i, dr);
		return FmSuccess;
	    }
	    if (IterSetIterCount(d->iter, num) == FmSuccess)
		return FmSuccess;
	}
	if (it->allow_expansion) {
	    ExtraDataRec dr;
	    dr.iter = IterInit(it->template + 1, num);
	    (void)ChainMgrSetData(&it->cm, it->max_count, dr);
	    it->max_count++;
	    
	    return FmSuccess;
	}
    } else if (it->template->type == POINTER) {
	for (i = 0; i < it->max_count; i++) {
	    ExtraData d;
	    ExtraDataRec dr;
	    if ((d = ChainMgrGetExtraData(&it->cm, i)) == NULL) {
		dr.fi = FrameInstInit(it->template[1].data);
		d = ChainMgrSetData(&it->cm, i, dr);
	    }
	    if (FrameInstSetIterCount(d->fi, num) == FmSuccess)
		return FmSuccess;
	}
	if (it->allow_expansion) {
	    ExtraDataRec dr;
	    dr.fi = FrameInstInit(it->template[1].data);
	    (void)ChainMgrSetData(&it->cm, it->max_count, dr);
	    it->max_count++;

	    if (FrameInstSetIterCount(dr.fi, num) == FmSuccess)
		return FmSuccess;
	}
    }
    return FmNoMoreData;
}


#if NeedFunctionPrototypes
static int IterGetTotalSize(Iter it)
#else
static int IterGetTotalSize(it)
Iter it;
#endif
{
    register int size, i;
    XimFrameType type;

    if (it->allow_expansion)
	return NO_VALUE;
    else if (it->max_count == 0)
	return 0;

    type = it->template->type;

    size = 0;

    if (type == BIT8)
	size = it->max_count;
    else if (type == BIT16)
	size = it->max_count * 2;
    else if (type == BIT32)
	size = it->max_count * 4;
    else if (type == BARRAY) {
	for (i = 0; i < it->max_count; i++) {
	    register int num;
	    ExtraData d;
	    if ((d = ChainMgrGetExtraData(&it->cm, i)) == NULL) {
		return NO_VALUE;
	    }
	    if ((num = d->num) == NO_VALUE)
		return NO_VALUE;
	    size += num;
	}
    } else if (type == ITER) {
	for (i = 0; i < it->max_count; i++) {
	    register int num;
	    ExtraData d;
	    if ((d = ChainMgrGetExtraData(&it->cm, i)) == NULL) {
		return NO_VALUE;
	    }
	    if ((num = IterGetTotalSize(d->iter)) == NO_VALUE)
		return NO_VALUE;
	    size += num;
	}
    } else if (type == POINTER) {
	for (i = 0; i < it->max_count; i++) {
	    register int num;
	    ExtraData d;
	    ExtraDataRec dr;
	    if ((d = ChainMgrGetExtraData(&it->cm, i)) == NULL) {
		dr.fi = FrameInstInit(it->template[1].data);
		d = ChainMgrSetData(&it->cm, i, dr);
	    }
	    if ((num = FrameInstGetTotalSize(d->fi)) == NO_VALUE)
		return NO_VALUE;
	    size += num;
	}
    } else {
	;/* Should never reached */
    }
    return size;
}


#if NeedFunctionPrototypes
static void IterReset(Iter it)
#else
static void IterReset(it)
Iter it;
#endif
{
    if (it->template->type == ITER) {
	ChainIterRec ci;
	int count;
	ExtraDataRec d;

	ChainIterInit(&ci, &it->cm);
	while (ChainIterGetNext(&ci, &count, &d)) {
	    IterReset(d.iter);
	}
	ChainIterFree(&ci);
    } else if (it->template->type == POINTER) {
	ChainIterRec ci;
	int count;
	ExtraDataRec dr;

	ChainIterInit(&ci, &it->cm);
	while (ChainIterGetNext(&ci, &count, &dr)) {
	    FrameInstReset(dr.fi);
	}
	ChainIterFree(&ci);
    }
    it->cur_no = 0;
}


#if NeedFunctionPrototypes
static void IterSetStartWatch(Iter it, 
			      IterStartWatchProc proc, void* client_data)
#else
static void IterSetStartWatch(it, proc, client_data)
Iter it;
IterStartWatchProc proc;
void* client_data;
#endif
{
    it->start_watch_proc = proc;
    it->client_data = client_data;
}


#if NeedFunctionPrototypes
static ExtraData ChainMgrSetData(ChainMgr cm, int frame_no, ExtraDataRec data)
#else
static ExtraData ChainMgrSetData(cm, frame_no, data)
ChainMgr cm;
int frame_no;
ExtraDataRec data;
#endif
{
    Chain cur = (Chain)Xmalloc(sizeof(ChainRec));

    cur->frame_no = frame_no;
    cur->d = data;
    cur->next = NULL;

    if (cm->top == NULL) {
	cm->top = cm->tail = cur;
    } else {
	cm->tail->next = cur;
	cm->tail = cur;
    }
    return &cur->d;
}


#if NeedFunctionPrototypes
static ExtraData ChainMgrGetExtraData(ChainMgr cm, int frame_no)
#else
static ExtraData ChainMgrGetExtraData(cm, frame_no)
ChainMgr cm;
int frame_no;
#endif
{
    Chain cur;

    cur = cm->top;

    while (cur) {
	if (cur->frame_no == frame_no) {
	    return &cur->d;
	}
	cur = cur->next;
    }
    return NULL;
}


#if NeedFunctionPrototypes
static Bool ChainIterGetNext(ChainIter ci, int* frame_no, ExtraData d)
#else
static Bool ChainIterGetNext(ci, frame_no, d)
ChainIter ci;
int* frame_no;
ExtraData d;
#endif
{
    if (ci->cur == NULL)
	return False;

    *frame_no = ci->cur->frame_no;
    *d = ci->cur->d;

    ci->cur = ci->cur->next;

    return True;
}


#if NeedFunctionPrototypes
static int _FrameInstIncrement(XimFrame frame, int count)
#else
static int _FrameInstIncrement(frame, count)
XimFrame frame;
int count;
#endif
{
    XimFrameType type;

    type = frame[count].type;
    type &= ~COUNTER_MASK;
    
    switch (type) {
      case BIT8:
      case BIT16:
      case BIT32:
#if 0 /* Temporary hack - 64 bit support */
      case BIT64:
#endif
      case BARRAY:
      case PADDING:
	return count + 1;
        break;
      case POINTER:
	return count + 2;
	break;
      case ITER:
	return _FrameInstIncrement(frame, count + 1);
	break;
      default:
	break;
    }
    return -1;   /* Error */
}


#if NeedFunctionPrototypes
static int _FrameInstDecrement(XimFrame frame, int count)
#else
static int _FrameInstDecrement(frame, count)
XimFrame frame;
int count;
#endif
{
    register int i;
    XimFrameType type;

    if (count == 0)
	return -1;   /* cannot decrement */
    else if (count == 1) {
	return 0;    /* BOGUS - It should check the contents of data */
    }

    type = frame[count - 2].type;
    type &= ~COUNTER_MASK;

    switch (type) {
      case BIT8:
      case BIT16:
      case BIT32:
      case BARRAY:
      case PADDING:
      case PTR_ITEM:
	return count - 1;
	break;
      case POINTER:
      case ITER:
	i = count - 3;
	while (i >= 0) {
	    if (frame[i].type != ITER)
	        return i + 1;
	    i--;
	}
	return 0;
      default:
	break;
    }
    return -1;   /* Error */
}


#if NeedFunctionPrototypes
static int _FrameInstGetItemSize(FrameInst fi, int cur_no)
#else
static int _FrameInstGetItemSize(fi, cur_no)
FrameInst fi;
int cur_no;
#endif
{
    XimFrameType type;

    type = fi->template[cur_no].type;
    type &= ~COUNTER_MASK;

    switch (type) {
      case BIT8 :
	return 1;
	break;

      case BIT16 :
        return 2;
	break;

      case BIT32 :
	return 4;
	break;

      case BARRAY :
	{
	  ExtraData d;
	  
	  if ((d = ChainMgrGetExtraData(&fi->cm, cur_no)) == NULL)
	      return NO_VALUE;
	  if (d->num == NO_VALUE)
	      return NO_VALUE;
	  return d->num;
	}
	break;

      case PADDING :
	{
	  register int unit, number, size, i;

	  unit = _UNIT(fi->template[cur_no].data);
	  number = _NUMBER(fi->template[cur_no].data);

	  i = cur_no;
	  size = 0;
	  while (number > 0) {
	      i = _FrameInstDecrement(fi->template, i);
	      size += _FrameInstGetItemSize(fi, i);
	      number--;
	  }
	  size = (unit - (size % unit)) % unit;
	  return size;
	}
	break;

      case ITER :
	{
	  ExtraData d;
	  int sub_size;
	  
	  if ((d = ChainMgrGetExtraData(&fi->cm, cur_no)) == NULL)
	      return NO_VALUE;
	  sub_size = IterGetTotalSize(d->iter);
	  if (sub_size == NO_VALUE)
	      return NO_VALUE;
	  return sub_size;
	}
	break;

      case POINTER :
	{
	  ExtraData d;
	  int sub_size;

	  if ((d = ChainMgrGetExtraData(&fi->cm, cur_no)) == NULL)
	      return NO_VALUE;
	  sub_size = FrameInstGetTotalSize(d->fi);
	  if (sub_size == NO_VALUE)
	      return NO_VALUE;
	  return sub_size;
	}
	break;
      default :
	break;
    }
    return NO_VALUE;
}
