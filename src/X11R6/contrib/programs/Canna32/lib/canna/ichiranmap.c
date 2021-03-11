/* Copyright 1992 NEC Corporation, Tokyo, Japan.
 *
 * Permission to use, copy, modify, distribute and sell this software
 * and its documentation for any purpose is hereby granted without
 * fee, provided that the above copyright notice appear in all copies
 * and that both that copyright notice and this permission notice
 * appear in supporting documentation, and that the name of NEC
 * Corporation not be used in advertising or publicity pertaining to
 * distribution of the software without specific, written prior
 * permission.  NEC Corporation makes no representations about the
 * suitability of this software for any purpose.  It is provided "as
 * is" without express or implied warranty.
 *
 * NEC CORPORATION DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE,
 * INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN 
 * NO EVENT SHALL NEC CORPORATION BE LIABLE FOR ANY SPECIAL, INDIRECT OR
 * CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF 
 * USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR 
 * OTHER TORTUOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR 
 * PERFORMANCE OF THIS SOFTWARE. 
 */

#if !defined(lint) && !defined(__CODECENTER__)
static char ichiranmap_id[] = "@(#) 102.1 $Id: ichiranmap.c,v 5.4 1994/04/21 02:44:26 kon Exp $";
#endif /* lint */

extern  DoFuncSequence(),UseOtherKeymap();

static struct funccfunc ichiran_funcs[] = {
  {CANNA_FN_FunctionalInsert	,IchiranBangoKouho		},
  {CANNA_FN_Forward		,IchiranForwardKouho		},
  {CANNA_FN_Backward		,IchiranBackwardKouho		},
  {CANNA_FN_Next		,IchiranNextKouhoretsu		},
  {CANNA_FN_Prev		,IchiranPreviousKouhoretsu	},
  {CANNA_FN_BeginningOfLine	,IchiranBeginningOfKouho	},
  {CANNA_FN_EndOfLine		,IchiranEndOfKouho		},
  {CANNA_FN_DeletePrevious	,IchiranQuit			},
  {CANNA_FN_Henkan		,IchiranForwardKouho		},
  {CANNA_FN_HenkanOrInsert	,IchiranForwardKouho		},
  {CANNA_FN_HenkanOrNothing	,IchiranForwardKouho		},
  {CANNA_FN_Kakutei		,IchiranKakutei			},
  {CANNA_FN_Quit		,IchiranQuit			},
  {CANNA_FN_Nop			,IchiranNop			},
  {CANNA_FN_FuncSequence	,DoFuncSequence			},
  {CANNA_FN_UseOtherKeymap	,UseOtherKeymap			},
  {0				,0				},
};

KanjiModeRec ichiran_mode = {
  searchfunc,
  default_kmap,
  CANNA_KANJIMODE_TABLE_SHARED,
  ichiran_funcs,
};
