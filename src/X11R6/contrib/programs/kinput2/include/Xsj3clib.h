/* $Header: Xsj3clib.h,v 2.2 93/09/02 14:59:45 nao Exp $ */

/*
 * Author: Naoshi Suzuki, SONY Corporation.  (nao@sm.sony.co.jp)
 */

#ifndef _Xsj3clib_h
#define _Xsj3clib_h

#include    "sj3lib.h"
#include    <X11/X.h>

/* #define FORCE_SJIS */
/* #define FORCE_JIS7 */
/* #define FORCE_JIS8 */

/*
 * define for default conversion definition file
 *          and resource file
 */
#ifndef SJ3DEFPATH
#define SJ3DEFPATH              "/usr/lib/X11/sj3def/"
#endif

/*
 * define for default include nesting limit
 */
#ifndef SJRC_INCLUDE_MAX
#define SJRC_INCLUDE_MAX        10
#endif

#define DEF_SJRC_FILE           "sjrc"
#define DEF_SJRK_FILE           "sjrk"
#define DEF_SJHK_FILE           "sjhk"
#define DEF_SJZH_FILE           "sjzh"
#define DEF_SJSB_FILE           "sjsb"

/*
 * Kana-Kanji conversion control key event define
 */
#define KEY_NULL                (0L)
#define KEY_HENKAN_START        (1L<<0)
#define KEY_HENKAN_END          (1L<<1)
#define KEY_MODE_CHANGE         (1L<<2)
#define KEY_TEXT_CHANGE         (1L<<3)
#define KEY_TEXT_FIXED          (1L<<4)
#define KEY_TEXT_FLUSH          (1L<<5)
#define KEY_CAND_START          (1L<<6)
#define KEY_SYMBOL_START        (1L<<7)
#define KEY_HINSI_START         (1L<<8)
#define KEY_SELECT_END          (1L<<9)
#define KEY_SELECT_ABORT        (1L<<10)
#define KEY_SELECT_RIGHT        (1L<<11)
#define KEY_SELECT_LEFT         (1L<<12)
#define KEY_SELECT_UP           (1L<<13)
#define KEY_SELECT_DOWN         (1L<<14)
#define KEY_SELECT_FIRST        (1L<<15)
#define KEY_SELECT_LAST         (1L<<16)
#define KEY_SELECT_NEXTP        (1L<<17)
#define KEY_SELECT_PREVP        (1L<<18)
#define KEY_SELECT_RIGHTMOST    (1L<<19)
#define KEY_SELECT_LEFTMOST     (1L<<20)
#define KEY_RECONNECT           (1L<<21)
#define KEY_BELL                (1L<<22)
#define KEY_DICT_START          (1L<<23)
#define KEY_DICT_CHANGE         (1L<<24)
#define KEY_DICT_REGISTER       (1L<<25)
#define KEY_DICT_CLEAR          (1L<<26)
#define KEY_DICT_END            (1L<<27)
#define KEY_CONTROL             (1L<<28)
#define KEY_FUNC                (1L<<29)

#define KEY_TEXT_CLEAR  (KEY_TEXT_CHANGE|KEY_TEXT_FIXED)
#define KEY_CHANGE      (KEY_HENKAN_END|KEY_MODE_CHANGE \
                |KEY_TEXT_CHANGE|KEY_TEXT_FIXED)
#define KEY_SELECT      (KEY_CAND_START|KEY_SYMBOL_START|KEY_SELECT_END \
                |KEY_SELECT_ABORT|KEY_SELECT_RIGHT|KEY_SELECT_LEFT \
                |KEY_SELECT_UP|KEY_SELECT_DOWN|KEY_SELECT_FIRST \
                |KEY_SELECT_LAST|KEY_SELECT_NEXTP|KEY_SELECT_PREVP \
                |KEY_SELECT_LEFTMOST|KEY_SELECT_RIGHTMOST|KEY_HINSI_START)
#define KEY_DICT        (KEY_DICT_START|KEY_DICT_REGISTER|KEY_DICT_CLEAR \
                |KEY_DICT_CHANGE|KEY_DICT_END)

#ifndef WCHAR_DEFINED
#define WCHAR_DEFINED
#undef wchar
#ifdef WCHAR_LONG
typedef unsigned long  wchar;
#else
typedef unsigned short wchar;
#endif
#endif

/*
 * define for server status
 */
#define CONNECT_OK              0
#define CONNECT_FAIL            -1

/*
 * define for locale
 */
#define JP_SJIS                 0
#define JP_EUC                  1
#define JP_JIS8                 2
#define JP_JIS7                 3
#define JP_OTHER                -1

/*
 * define for buffer size
 */
#define YBUFSIZ                 32
#define RBUFSIZ                 16
#define CANDBUFSIZ              128
#define KANABUFSIZ              256
#define KANJIBUFSIZ             512
#define BUNBUFSIZ               256

/*
 * define for attribute of segment
 */
#define SEG_REVERSED            0
#define SEG_UNDER_LINE          1
#define SEG_NORMAL              2

typedef unsigned long           Xsj3csMode;
typedef unsigned long           Xsj3ccMode;
typedef unsigned long           Xsj3cdMode;
typedef unsigned long           Xsj3cFlag;
typedef unsigned long           Xsj3cEvent;

#define MODE_HIRA               0
#define MODE_ZKATA              1
#define MODE_HKATA              2
#define MODE_HALPHA             3
#define MODE_ZALPHA             4

#define MODE_SJIS               5
#define MODE_EUC                6
#define MODE_JIS                7
#define MODE_KUTEN              8

#define MODE_TOROKU             9
#define MODE_SYOUKYO            10
#define MODE_KANJI              11
#define MODE_EDIT               12
#define MODE_CAND               13
#define MODE_SYMBOL             14
#define MODE_HINSI              15
#define MODE_QUOTE              16

#define MODE_ROLLDOWN           5
#define MODE_ROLLUP             6

#define MODE_HANKAKU            5
#define MODE_ZENKAKU            6
#define MODE_UPPER              7
#define MODE_LOWER              8

#define MODE_CONV_NUM           7   /* HIRA-ZALPHA,ROLLDOWN,ROLLUP          */
#define MODE_INROT_NUM          9   /* HIRA-ZALPHA,SJIS-KUTEN               */
#define MODE_OUTROT_NUM         9   /* HIRA-ZALPHA,HANKAKU-LOWER            */
#define MODE_CODE_NUM           4   /* SJIS-KUTEN                           */
#define MODE_STR_NUM            17  /* HIRA-ZALPHA,SJIS-KUTEN,TOROKU-HINSI  */

/*
 * Roma-Kana Conversion Table Structure
 */
typedef struct sj3rktable {
    unsigned char          *roma;
    unsigned char          *yomi;
    unsigned char          *str;
    int                     rlen;
    int                     ylen;
    struct sj3rktable      *next;
} Xsj3cRKTable;

/*
 * Hiragana-Katakana Conversion Table Structure
 */
typedef struct sj3hktable {
    unsigned char          *hira;
    unsigned char          *zkata;
    unsigned char          *hkata;
    unsigned char          *halpha;
    int                     hlen;
    struct sj3hktable      *next;
} Xsj3cHKTable;

/*
 * Zenkaku-Hankaku Conversion Table Structure
 */
typedef struct sj3zhtable {
    unsigned char          *halpha;
    unsigned char          *zalpha;
    unsigned char          *zkana;
    unsigned char          *hkata;
    struct sj3zhtable      *next;
} Xsj3cZHTable;

/*
 * Data Structure
 */
typedef struct _Xsj3cData {
    int                     len;        /* Length of data           */
    wchar                  *data;
} Xsj3cDataRec,   *Xsj3cData;

typedef Xsj3cDataRec    Xsj3cSymbolRec;
typedef Xsj3cData       Xsj3cSymbol;
typedef Xsj3cDataRec    Xsj3cHinsiRec;
typedef Xsj3cData       Xsj3cHinsi;

/*
 * Candidate Structure
 */
typedef struct _Xsj3cCand {
    int                     len;        /* Length of candidate          */
    wchar                   data[CANDBUFSIZ];  /* Candidate string      */
    SJ3_STUDYREC            dcid;       /* ID for studying              */
} Xsj3cCandRec,   *Xsj3cCand;

/*
 * Dictionary Massage Data Structure
 */
typedef struct _Xsj3cDictMsg {
    int                     len;        /* Length of data           */
    wchar                  *data;
    int                     attr;       /* attribute of data        */
} Xsj3cDictMsgRec,   *Xsj3cDictMsg;

/*
 * Key Table Structure
 */
typedef struct sj3keytable {
    char                   *keyword;
    KeySym                  ksym;
    Xsj3ccMode              modmask;
    Xsj3cEvent              (*func)();
    Xsj3cFlag               inputsame;
    struct sj3keytable     *prev;
    struct sj3keytable     *next;
} Xsj3cKeyTable;

/*
 * Segment structure
 */
typedef struct _Xsj3cSeg {
    int             value;      /* Result value of pre-edit conversion  */
    unsigned char  *str;        /* Pre-edit strings buffer              */
    unsigned char  *sp;         /* Current pointer of pre-edit strings  */
    wchar          *yomi;       /* Yomi characters buffer               */ 
    wchar          *disp;       /* Display/Kanji characters buffer      */
    unsigned char  *oldstr;     /* Old pre-edit strings buffer          */
    int             oldlen;     /* Length old Roma-Kana conversion unit */
    int             n_roma;     /* Romaji/Code counter of pre-edit      */
    int             n_kana;     /* Kana counter of pre-edit strings     */
    int             size;       /* Size of yomi/disp character buffer   */
    int             cur;        /* Current cursor position              */
    int             num;        /* Length of yomi characters            */
    int             dnum;       /* Length of display characters         */
    Xsj3csMode      cursegmode; /* Character kind of yomi characters    */
    Xsj3cFlag       status;     /* Conversion status (noconv or conved) */
    Xsj3cFlag       change;     /* Flag, Need to study or not           */
    Xsj3cFlag       edit;       /* Flag, Available to edit or not       */
    SJ3_STUDYREC    dcid;       /* ID for studying                      */
} Xsj3cSegRec,    *Xsj3cSeg;

/*
 * Dictionary  Data Structure
 */
typedef struct _Xsj3cDictData {
    Xsj3cSeg                seg;        /* Yomi Segment buffer          */
    Xsj3cDictMsg            msg;        /* Message buffers              */
    Xsj3cdMode              mode;       /* Registration or elimination  */
    Xsj3cdMode              status;     /* Current status               */ 
    int                     value;      /* Result value of operation    */
    int                     n_dict;     /* Expanded segment counter     */
} Xsj3cDictDataRec,   *Xsj3cDictData;

/*
 * Buffer for Sj3 Object
 */
typedef struct _Xsj3cBuf {
    int             server;         /* Current conversion server            */
    Xsj3ccMode      convmode;       /* Current conversion mode              */

    /* Converting string data                                               */
    Xsj3cSeg       *input;          /* Segment buffers for input            */
    Xsj3cSeg       *backup;         /* Segment buffers for backup           */
    Xsj3cSeg        current;        /* Segment buffer of current input      */
    int             curseg;         /* Current segment number               */
    int             segnum;         /* Total segment number                 */
    int             backsegnum;     /* Old total segment number             */
    int             convedsegnum;   /* Total converted segment number       */

    /* Current character mode data                                          */
    Xsj3csMode      inputmode;      /* Current input character kind         */
    Xsj3csMode      dispmode;       /* Current display character kind       */

    /* Data for handling dictionary (DictMode)                              */
    Xsj3cDictData   dict;           /* Data for DictMode                    */

    /* Data for selection (SelectMode)                                      */
    Xsj3cCand       candidate;      /* Candidate strings                    */
    Xsj3cSymbol     symbol;         /* Symbol strings                       */
    Xsj3cHinsi      hinsi;          /* Hinsi strings                        */
    int             candnum;        /* Total candidate number               */
    int             curcand;        /* Current candidate number             */
    int             cursymbol;      /* Current symbol number                */
    int             curhinsi;       /* Current hinsi number                 */
    Xsj3ccMode      selectstatus;   /* Status of SelectMode                 */
    int             n_select;       /* Counter for candidates to SelectMode */
    int             candseg;        /* Segment number of current candidate  */

    /* Conversion tables                                                    */
    Xsj3cRKTable   *rktable;        /* Roma-kana conversion table           */ 
    Xsj3cHKTable   *hktable;        /* Hiragana-Katakana conversion table   */
    Xsj3cZHTable   *zhtable;        /* Hankaku-Zenkaku conversion table     */

    /* Other data for conversion                                            */
    unsigned char  *rkdouble;       /* Roma-kana double conversion data     */
    unsigned char  *plosive;        /* Roma-kana plosive conversion data    */

    /* Next sjrc file to read                                               */
    char           *rcfile;

    /* Common resource in sjrc file (Common with sj2/sj3/sjx)               */
    Xsj3cKeyTable  *key;            /* Key-function conversion table        */
                                    /* Custom-flag (.key .sjxkey .ki2key)   */
    Xsj3csMode      inmoderot[MODE_INROT_NUM];/* Input character mode       */
                                    /* Custom-flag (.InitialMode)           */
    int             inmoderotnum;   /* Total input character mode number    */
    Xsj3csMode      outmoderot[MODE_OUTROT_NUM];/* Output character mode    */
                                    /* Custom-flag (.PrintMode)             */
    int             outmoderotnum;  /* Total output character mode number   */
    Xsj3csMode      defcode[MODE_CODE_NUM]; /* Input code rotation          */
                                    /* Custom-flag (.DefaultCode)           */
    int             coderotnum;     /* Total input code rotation number     */
    Xsj3csMode      muhenmode;      /* Character kind after unconverting    */
                                    /* Custom-flag (.MuhenkanMode)          */
    Xsj3csMode      togglemode;     /* Character kind by toggling           */
                                    /* Custom-flag (.MuhenkanInEdit)        */
    Xsj3cFlag       dotoggle;       /* Custom-flag (.MuhenkanToggle)        */
    Xsj3cFlag       throughflg;     /* Trough character flag                */
    wchar          *modestr[MODE_STR_NUM]; /* Character mode strings       */
                                    /* Custom-flag (.guide.[mode])          */
    int             modelen[MODE_STR_NUM];  /* Length of mode strings       */
    Xsj3cFlag       gakusyuu;       /* Custom-flag (.bstudy)                */
    Xsj3cFlag       rkbell;         /* Custom-flag (.rkerrbell)             */
    Xsj3cFlag       flushaconv;     /* Custom-flag (.FlushAfterConversion)  */
    char           *sj3serv;        /* First sj3serv hostname               */
                                    /* Custom-flag (.server)                */
    unsigned char  *setnormal;      /* Custom-flag (.SetNormal)             */
    unsigned char  *throughnext;    /* Custom-flag (.ThroughNext)           */

    /* Original resource (Not exists in sj2/sj3/sjx)                        */
    char           *sj3serv2;       /* Second sj3serv hostname              */
                                    /* Custom-flag (.server2)               */
    Xsj3cFlag       flushiconv;     /* Custom-flag (.FlushInConversion)     */
    Xsj3cFlag       flushsconv;     /* Custom-flag (.FlushSelectConversion) */
    Xsj3cFlag       flusheconv;     /* Custom-flag (.FlushEndConversion)    */
    Xsj3cFlag       alphaconv;      /* Custom-flag (.AlphabetConversion)    */
    Xsj3cFlag       backdisplay;    /* Custom-flag (.BackDisplay)           */
    Xsj3cFlag       beginlastseg;   /* Custom-flag (.BeginConversionLast)   */
    Xsj3cFlag       expandmconv;    /* Custom-flag (.ExpandModeConversion)  */
    Xsj3cFlag       shrinkmconv;    /* Custom-flag (.ShrinkModeConversion)  */
    Xsj3cFlag       expandkconv;    /* Custom-flag (.ExpandKanjiConversion) */
    Xsj3cFlag       shrinkkconv;    /* Custom-flag (.ShrinkKanjiConversion) */
    Xsj3cFlag       shrinkall;      /* Custom-flag (.ShrinkAll)             */
    Xsj3cFlag       henkanseg;      /* Custom-flag (.HenkanSegment)         */
    Xsj3cFlag       muhenseg;       /* Custom-flag (.MuhenkanSegment)       */
    Xsj3cFlag       delchange;      /* Custom-flag (.DeleteChangeSegment)   */
    Xsj3cFlag       flushchange;    /* Custom-flag (.FlushChangeSegment)    */
    Xsj3cFlag       modeconv[MODE_CONV_NUM];/* Custom-flag (.ModeConversion)*/
    Xsj3cFlag       moveloop;       /* Custom-flag (.MoveLoop)              */
    Xsj3cFlag       movebyseg;      /* Custom-flag (.MoveBySegment)         */
    Xsj3cFlag       jumpbyseg;      /* Custom-flag (.JumpBySegment)         */
    Xsj3cFlag       delbyseg;       /* Custom-flag (.DeleteBySegment)       */
    Xsj3cFlag       killbyseg;      /* Custom-flag (.KillBySegment)         */
    Xsj3cFlag       muhencurlast;   /* Custom-flag (.MuhenkanCursorLast)    */
    Xsj3cFlag       editcurlast;    /* Custom-flag (.EditCursorLast)        */
    Xsj3cFlag       flushcurlast;   /* Custom-flag (.FlushCursorLast)       */
    Xsj3cFlag       convedunderline;/* Custom-flag (.ConvertedUnderLine)    */
    Xsj3cFlag       dispmodechange; /* Custom-flag (.DisplayModeChange)     */
    Xsj3cFlag       dellastmove;    /* Custom-flag (.DeleteLastMove)        */
    Xsj3cFlag       kanaonly;       /* Custom-flag (.KanaInputOnly)         */
    Xsj3cFlag       inputsame;      /* Custom-flag (.InputSameTime)         */
    Xsj3cFlag       cntrlsame;      /* Custom-flag (.ControlSameTime)       */
    Xsj3cFlag       selectconv;     /* Custom-flag (.BeforeSelectConversion)*/
    Xsj3cFlag       beforeconv;     /* Custom-flag (.BeforeConversion)      */
    Xsj3cFlag       lastdoubleconv; /* Custom-flag (.LastDoubleConversion)  */
    int             selectcount;    /* Custom-flag (.BeforeSelectCount)     */
    Xsj3cFlag       selectback;     /* Custom-flag (.SelectBackSpaceCurrent)*/
    Xsj3cFlag       candpadding;    /* Custom-flag (.CandidatePadding)      */

} Xsj3cBufRec,    *Xsj3cBuf;

/* Key event handling                               */
extern unsigned char       *Xsj3cGetPreeditArea();
extern Xsj3cEvent           Xsj3cKeyConv();

/* Setting up environment                           */
extern void                 Xsj3cSetInLang();
extern void                 Xsj3cSetOutLang();
extern void                 Xsj3cSetKanaMod();

/* Connection to language conversion server handling */
extern int                  Xsj3cOpen();
extern void                 Xsj3cClose();
extern void                 Xsj3cConnect();

/* Operation of buffers                             */
extern Xsj3cBuf             Xsj3cCreateBuffer();
extern void                 Xsj3cFreeBuffer();
extern void                 Xsj3cClearBuffer();
extern void                 Xsj3cFlushBuffer();
extern void                 Xsj3cFixBuffer();

/* Getting data to draw or send to other            */
extern int                  Xsj3cGetSegNum();
extern int                  Xsj3cGetPosition();
extern wchar               *Xsj3cGetSeg();
extern wchar               *Xsj3cGetModeStr();
extern wchar               *Xsj3cGetConvertedStr();
extern int                  Xsj3cGetConvertedLength();

/* Operartion of candidate data                     */
extern int                  Xsj3cGetCandidateNum();
extern Xsj3cCand            Xsj3cGetCandidates();
extern wchar               *Xsj3cGetCandidate();
extern int                  Xsj3cSetCandidate();
extern void                 Xsj3cEndCandidate();

/* Operartion of symbol data                        */
extern int                  Xsj3cGetSymbolNum();
extern Xsj3cSymbol          Xsj3cGetSymbols();
extern wchar               *Xsj3cGetSymbol();
extern int                  Xsj3cSetSymbol();
extern void                 Xsj3cEndSymbol();

/* Operartion of hinsi data                         */
extern int                  Xsj3cGetHinsiNum();
extern Xsj3cHinsi           Xsj3cGetHinsis();
extern wchar               *Xsj3cGetHinsi();
extern int                  Xsj3cSetHinsi();
extern void                 Xsj3cEndHinsi();

/* Operartion of dictionary data                    */
extern int                  Xsj3cGetDictMsgNum();
extern Xsj3cDictMsg         Xsj3cGetDictMsgs();
extern wchar               *Xsj3cGetDictMsg();
extern void                 Xsj3cDictRegister();
extern void                 Xsj3cDictClear();
extern void                 Xsj3cEndDict();

extern void                 Xsj3cInitializeTables();

#endif /* _Xsj3clib_h */
