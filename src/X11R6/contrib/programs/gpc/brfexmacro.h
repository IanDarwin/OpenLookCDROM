/* $XConsortium: brfexmacro.h,v 5.3 91/10/21 14:33:06 eswu Exp $ */

/*--------------------------------------------------------------------*\
|
|  Copyright (C) 1989,1990, 1991, National Computer Graphics Association
|
|  Permission is granted to any individual or institution to use, copy, or
|  redistribute this software so long as it is not sold for profit, provided
|  this copyright notice is retained.
|
|                         Developed for the
|                National Computer Graphics Association
|                         2722 Merrilee Drive
|                         Fairfax, VA  22031
|                           (703) 698-9600
|
|                                by
|                 SimGraphics Engineering Corporation
|                    1137 Huntington Drive  Unit A
|                      South Pasadena, CA  91030
|                           (213) 255-0900
|---------------------------------------------------------------------
|
| Author        :	mfr / SimGraphics Engineering Corportation
|
| File          :	brfexception.h
| Date		:	Sun Feb 11 00:41:57 PST 1990
| Project       :	PLB
|
| Description	:	Exception handler macro defines
|
| Status        :	Version 1.0
|
| Revisions     :	
|
\*--------------------------------------------------------------------*/


#define INIT_LIST(item)\
{\
        brf_exception[item].brf_ex_type = 0;\
        brf_exception[item].brf_ex_flag = BRF_OK;\
        brf_exception[item].brf_ex_optional[VCOLORT] = 0;\
        brf_exception[item].brf_ex_optional[VCOLORI] = 0;\
        brf_exception[item].brf_ex_optional[VNORM] = 0;\
        brf_exception[item].brf_ex_optional[FCOLORT] = 0;\
        brf_exception[item].brf_ex_optional[FCOLORI] = 0;\
        brf_exception[item].brf_ex_optional[FNORM] = 0;\
        brf_exception[item].brf_ex_optional[EDATA] = 0;\
        brf_exception[item].brf_ex_range[RANGESTART] = NO_RANGE_DATA;\
        brf_exception[item].brf_ex_range[RANGESTOP] = NO_RANGE_DATA;\
        brf_exception[item].brf_ex_rrange[RANGESTART] = NO_RANGE_DATA;\
        brf_exception[item].brf_ex_rrange[RANGESTOP] = NO_RANGE_DATA;\
        brf_exception[item].brf_ex_custom_handler = NULL;\
        brf_exception[item].brf_ex_message = "This Space For Rent\n";\
        brf_exception[item].brf_ex_logic = TRUE;\
        brf_exception[item].brf_ex_numhits = 0;\
}


#define LIST_NONSUP(a,b,c)\
{\
INIT_LIST(listptr);\
brf_exception[listptr].brf_ex_type = a ;\
brf_exception[listptr].brf_ex_flag = b ;\
brf_exception[listptr].brf_ex_message = c ;\
        listptr++;\
}



#define LIST_OPTIONAL(a,b,c,d,e)\
{\
INIT_LIST(listptr);\
brf_exception[listptr].brf_ex_type = a ;\
brf_exception[listptr].brf_ex_flag = b ;\
brf_exception[listptr].brf_ex_optional[c] = TRUE ;\
brf_exception[listptr].brf_ex_custom_handler = d ;\
brf_exception[listptr].brf_ex_message = e ;\
        listptr++;\
}




#define LIST_RRANGE(a,b,c,d,e,f)\
{\
INIT_LIST(listptr);\
brf_exception[listptr].brf_ex_type = a ;\
brf_exception[listptr].brf_ex_flag = b ;\
brf_exception[listptr].brf_ex_rrange[RANGESTART] = c ;\
brf_exception[listptr].brf_ex_rrange[RANGESTOP] = d ;\
brf_exception[listptr].brf_ex_custom_handler = e ;\
brf_exception[listptr].brf_ex_message = f ;\
        listptr++;\
}




#define LIST_INVERT_RRANGE(a,b,c,d,e,f)\
{\
INIT_LIST(listptr);\
brf_exception[listptr].brf_ex_type = a ;\
brf_exception[listptr].brf_ex_flag = b ;\
brf_exception[listptr].brf_ex_logic = FALSE ;\
brf_exception[listptr].brf_ex_rrange[RANGESTART] = c ;\
brf_exception[listptr].brf_ex_rrange[RANGESTOP] = d ;\
brf_exception[listptr].brf_ex_custom_handler = e ;\
brf_exception[listptr].brf_ex_message = f ;\
        listptr++;\
}





#define LIST_IRANGE(a,b,c,d,e,f)\
{\
INIT_LIST(listptr);\
brf_exception[listptr].brf_ex_type = a ;\
brf_exception[listptr].brf_ex_flag = b ;\
brf_exception[listptr].brf_ex_range[RANGESTART] = c ;\
brf_exception[listptr].brf_ex_range[RANGESTOP] = d ;\
brf_exception[listptr].brf_ex_custom_handler = e ;\
brf_exception[listptr].brf_ex_message = f ;\
        listptr++;\
}




#define LIST_INVERT_IRANGE(a,b,c,d,e,f)\
{\
INIT_LIST(listptr);\
brf_exception[listptr].brf_ex_type = a ;\
brf_exception[listptr].brf_ex_flag = b ;\
brf_exception[listptr].brf_ex_logic = FALSE ;\
brf_exception[listptr].brf_ex_range[RANGESTART] = c ;\
brf_exception[listptr].brf_ex_range[RANGESTOP] = d ;\
brf_exception[listptr].brf_ex_custom_handler = e ;\
brf_exception[listptr].brf_ex_message = f ;\
        listptr++;\
}



#define LIST_COLORMODE(a,b,c,d)\
{\
INIT_LIST(listptr);\
brf_exception[listptr].brf_ex_type = a ;\
brf_exception[listptr].brf_ex_flag = b ;\
brf_exception[listptr].brf_ex_custom_handler = c ;\
brf_exception[listptr].brf_ex_message = d ;\
        listptr++;\
}


