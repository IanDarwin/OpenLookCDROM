/*
 * This file is provided for unrestricted use
 * provided that this legend is included on all tape media
 * and as a part of the software program in whole or part.  Users
 * may copy or modify this file without charge, but are not authorized to
 * license or distribute it to anyone else except as part of a product
 * or program developed by the user.
 *
 * THIS FILE IS PROVIDED AS IS WITH NO WARRANTIES OF ANY KIND INCLUDING THE
 * WARRANTIES OF DESIGN, MERCHANTIBILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE, OR ARISING FROM A COURSE OF DEALING, USAGE OR TRADE PRACTICE.
 *
 * This file is provided with no support and without any obligation on the
 * part of Resumix, Inc. to assist in its use, correction,
 * modification or enhancement.
 *
 * Resumix, Inc. SHALL HAVE NO LIABILITY WITH RESPECT TO THE
 * INFRINGEMENT OF COPYRIGHTS, TRADE SECRETS OR ANY PATENTS BY THIS FILE
 * OR ANY PART THEREOF.
 *
 * In no event will Resumix, Inc. be liable for any lost revenue
 * or profits or other special, indirect and consequential damages, even
 * if Resumix has been advised of the possibility of such damages.
 *
 * Resumix, Inc.
 */

#ifndef XVNEWS_CONFIG_H
#define XVNEWS_CONFIG_H

/* This file contains the configurable options of xvnews, please make
   the changes needed below.
 */

/* Sets the default company name. Can be overridden by setting $ORGANIZATION */
#define ORGANIZATION    "The unconfigured xvnews people"

/* The default NNTP server to connect to. Can be overridden by setting $NNTPSERVER */
#define NNTPSERVER	"news"

/* The domain to use. Sets everything after the @ sign for postings. Can be
   overridden by setting $DOMAIN.
 */
#define DOMAIN 		"unconfigured.xvnews.domain"

/* The host to put in headers for the Path line.
 */
#define PATH_HOST	""

/* Some sites that use CNEWS don't want to show the user news groups that
 *  have the status 'x' or '=' as these groups are really not available.
 */
#define CNEWS_DONT_SHOW 1

/* ADD_SIGNATURE defines whether a signature should be added
   One reason not to add a signature is if this is also done
   by another part of the news system, e.g. if you are posting
   through inews
*/
#define ADD_SIGNATURE 1

/* USE_INEWS_TO_POST defines whether inews is used to post.
   Normally you would post through xvnews directly, but certain
   characteristics of your news system (such as security measures)
   might require you to post through a version of inews.
   INEWS_PROGRAM indicates the inews program to use. Some people have
   noted problems using NNTP's inews while running B News. If you have
   posting problems you might look into this.
   Also note that you need a version of inews which accepts
   articles on stdin. C News's inews doesn't, for instance.
   Finally, be warned that the output of inews is not parsed,
   which means that problems with posting will go undetected.
*/
#undef USE_INEWS_TO_POST
#define INEWS_PROGRAM "inews -h"

/* MAIL_PROGRAM is the program used to deliver mail messages.
   /usr/lib/sendmail -t is a good choice, and should work on most systems
   However, sendmail doesn't deal with personal aliases (such as those
   in a users .mailrc. Therefor some people prefer a different local
   mail program such as /ucr/ucb/mail or /usr/bin/mail.
   Any program which can deliver messages delivered on stdin including
   the To: and From: headers will do just fine.
   Note that most mail programs need a '-t' option to work this way,
   so you would change this to "/usr/ucb/Mail -t" on SunOs 4.x.x.
*/
#define MAIL_PROGRAM "/usr/lib/sendmail -t"

/* COMPRESS_PROGRAM is the program which compresses your files,
   usually this will be compress. On SunOS 4.x, use /usr/ucb/compress,
   on SunOS 5.x, use /usr/bin/compress. You could also use gzip, but
   then you should also change the extension below to "gz".
   */
#define COMPRESS_PROGRAM "compress"
#define COMPRESS_EXTENSION "Z"

/* The following two defines set the initial number of articles in a
   newsgroup and the initial number of newsgroups. Xvnews will update
   these numbers if there are more articles or newsgroups, which means
   you *could* define both numbers to be 1. However, for performance
   reasons you will want to allocate some space upfront. You should
   set these numbers to something a bit above your average situation.
   */

#define INIT_NUM_ARTICLES 256
#define INIT_NUM_GROUPS 2048


/* The following define can be used to point to the default
   locale. Normally you can leave this empty, and the default locale
   will be used. */

#define DEFAULT_LOCALE ""

/* The biggest problem porting xvnews so far is the regular
   expression code. Essentially, there are three different types
   of routines for this. I have a string bias towards POSIX
   specifications, so those routines (regcomp, regexec) are the
   default. On both Solaris 1.x and Solaris 2.x, change this to
   SVR4_REGEX. On other systems you might try first
   to see whether POSIX is supported. */

#undef POSIX_REGEX
#define SVR4_REGEX
#undef BSD_REGEX

/* xvnews needs to know the timezone you are in in order to present dates
   in your own time. Unfortunatly, getting the timezone is one of those
   things that make writing portable code so hard, because each system has
   a different method. xvnews knows about two different methods: using ftime,
   and using the timezone variable. The latter method is default, and seems
   to be most common on modern Unix systems, but this won't work on SunOS 4.
   Only define USE_FTIME if you get compilation errors like 
   'timezone undefined'. */
/* #define USE_FTIME */


/* If you use an INN server, you can configure it to send a
   password to request authorization to read and post.
   Define the following to activate this. You will also add the
   inn library to the Makefile.dist or Imakefile. */
#undef  USE_NNRP_PASSWD

/* Some people prefer to keep the article window editable, for
   various reasons.  By default, this is not enabled, as it might
   confuse less knowledgable readers.  Defining this should allow
   you to edit the article window when reading news. */
#undef KEEP_ARTICLE_EDITABLE

#endif /* XVNEWS_CONFIG_H */
