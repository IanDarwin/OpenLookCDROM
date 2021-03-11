\begindata{text,537620332}
\textdsversion{12}
\template{default}
\define{footnote

attr:[Flags OverBar Int Set]
attr:[FontSize PreviousFontSize Point -2]}
\bold{\bigger{The Design of The Remote Andrew Demo}

by Bob Glickstein

}\
\begindata{writestamp,537469056}
Datastream version: 1
%o %A, %Y
helvetica12
724637023
\enddata{writestamp,537469056}
\view{writestampview,537469056,0,0,0}
\begindata{bp,537558144}
\enddata{bp,537558144}
\view{bpv,537558144,1,0,0}
Copyright Carnegie Mellon University 1992  - All Rights Reserved

\smaller{\smaller{$Disclaimer: 

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

 $}}

\begindata{bp,537558208}
\enddata{bp,537558208}
\view{bpv,537558208,2,0,0}
\heading{Introduction}


The Remote Andrew Demo (rdemo for short) is an Internet network service 
provided by the Andrew Toolkit Consortium.  It allows remote users to 
interactively experiment with Andrew Toolkit applications.


\heading{Requirements}


The goal of the demo service was to be an exceedingly easy means for (possibly 
naive) users to play with Andrew software and read our multitude of bboards 
and newsgroups with Messages.  To this end, we required the demo service to be 
as simple to use as issuing a single shell command.


Unfortunately, the security features of X11 mandated that the remote user 
perform an ``xhost'' prior to invoking the service.  In addition, the remote 
user must determine for him or herself the means of getting the Andrew fonts, 
which depends on the version of X11 he or she is running.


While the demo is in progress, the goal was to make it exactly like using 
Andrew software in the normal case, from a guest account.  Demo users should 
be able to edit files in their ``home directory.''  They should be able to run 
messages, console, help, and ez.  They should also have a guided-tour option 
demonstrating ATK's best features.


Exiting the demo should be quick and easy.  The user's prompt should return as 
soon as the demo windows vanish (even though there are many post-session 
housekeeping chores to be done by the same demo process).


Finally, we hoped to be able to keep track of many kinds of information, such 
as when the user began and ended the demo, what actions were selected from 
within the demo, what version of X is the user running (and on what kind of 
display), and so on.


\heading{Components}


\subheading{Fingerd}


Since the Internet ``finger'' service is the gateway to invoking rdemo, the 
network finger-daemon is the first demo-specific code encountered in a normal 
session.


An rdemo host has a modified /etc/fingerd, which is the program invoked by 
inetd to respond to incoming finger requests.  When a request originates as


\leftindent{finger foo@bar}


then the string \italic{foo} is passed on the standard input to the fingerd 
process on machine \italic{bar}.  The standard output and standard error of 
fingerd are attached to the remote terminal which issued the request, and the 
connection is kept open until stdin, stdout, and stderr are closed by fingerd.


The rdemo fingerd switches on the string in the standard input.  In most 
cases, fingerd simply prints an informational message, giving details about 
how the service is used.  If the string begins with \bold{run-demo}, then the 
demo sequence actually begins.


First, the standard input is parsed.  The leading \bold{run-demo} is 
discarded, and the remainder is used in the value for the DISPLAY environment 
variable.


\leftindent{finger run-demo-foo.bar.baz:1.0@hostname}


causes foo.bar.baz:1.0 to be used as the value of DISPLAY.  The hostname can 
be omitted, as in


\leftindent{finger run-demo-:1.0@hostname}


in which case the hostname is computed by the \bold{getpeeraddr} program and 
used by default.


\subheading{The rdemo account and .cshrc}


When the demo is invoked, the fingerd process performs an \bold{exec su 
rdemo}, switching the process to the rdemo account.  The rdemo account's 
password is stored in a protected file readable only by ``nobody,'' which is 
the name of the owner of the fingerd process (from inetd.conf).  Rdemo's login 
shell is /bin/csh, and its .cshrc is executed.  At this point, the process 
owns rdemo's AFS authentication tokens.


Here are the major steps taken by rdemo's .cshrc prior to the appearance of 
the demo window:


\leftindent{\description{1.  A unique ID is generated.  This is used for 
several things, among which are the name of the session's home directory and 
the semaphore file (see below for both).


2.  The responding host is tested to see how many other demo users it is 
currently serving.  This is done by testing for the presence of semaphore 
files in a special directory.  When a session begins, a file is stored.  The 
file is removed when the session ends.  The program which counts these files 
also notices stale files (ones which failed to get removed) and deletes them. 
 If there aren't too many semaphore files found, a semaphore is stored and the 
process continues, otherwise a ``too busy'' message is given.


3.  The PATH and CLASSPATH environment variables are augmented by prepending 
rdemo-specific directories to them.


4.  A home directory for the session is created under the name 
~rdemo/home/\italic{unique-id} (using the unique-id chosen earlier).  The HOME 
environment variable is set to this directory.  A set collection of files are 
copied via \bold{tar} from a secure location into the new home directory. 
 These files include a set of mail subscriptions and an introductory mail 
message.

	The creation of a home directory was deemed necessary because users needed a 
safe directory in which to edit files (and possibly wreak havoc) without 
menacing other demo users or other legitimate Andrew users.


5.  The PROFILES environment variable is set to point to a secure version of 
the \italic{preferences} file.  The preferences in this file were carefully 
chosen to disable all known security holes in the system (see below), so the 
demo user's preferences must not be editable.


6.  Certain files in the synthesized home directory are piped through a macro 
processor.  This is necessary because some files need to refer to others in 
the user's home directory, and the directory's name is not known until now. 
 The preprocessed files refer to $(HOME), and the macro processing step 
resolves the value of that variable.


7.  The core-file size limit is set to 0.  This was to prevent the 
unmanageable growth of a pile of dead bits.


8.  The main demo program is invoked.  This program has control until the user 
exits or the demo timeout period elapses.


9.  Stdin, stdout, and stderr are closed.  This gives the user back his or her 
shell prompt.


10.  Silently, .cshrc attempts to remove the X11r5 font server from the user's 
font path (if it wasn't there, no harm done); the semaphore file is removed; 
any comments recorded by the user are moved to a secure location; the 
synthesized home directory is eradicated; and the AFS authentication tokens 
are discarded.  At this point, the process ends.

}}
\subheading{The main rdemo program}


The main rdemo program (or ``rdemo shell'') is an ATK application which 
displays several buttons, each of which launches a different application.  It 
also displays a description of each application, plus an ATK clock inset to 
remind the user of the 1 hour time limit for the session.  There is also an 
Exit button.


The user is initially prompted for his or her e-mail address.  After 
responding, the user may press any of the buttons; each button launches an 
application.  The rdemo shell forks a new, \italic{unauthenticated} process 
for each application, and remembers the applications it launched.  The user is 
prevented from launching two of the same application at once.  The rdemo shell 
is signalled when a child process exits, and will then allow that application 
to be re-launched.


The Exit button hunts down child processes and kills them prior to thanking 
the user and terminating the program.


\subheading{The applications}


The following applications can be run by the demo user:


\leftindent{\italic{Tour}

This is simply an EZ process where the visited file is the root of a network 
of nodes connected by hyperlink insets.  These nodes are documents which show 
off the features of ATK.


\italic{Ez}

In this incarnation of EZ, the file visited is an editable file in the 
synthesized home directory.  The user can experiment with editing text and 
insets with EZ.


\italic{Comment}

This is an EZ-like program which always writes its buffer to the file 
\bold{.comments} in the synthesized home directory.  Furthermore, it ensures 
that its buffer is saved upon exit, and at one-minute intervals.  A pane in 
the window of this program encourages the user to write comments on the demo 
service.  The multiple automatic saves of the buffer are to prevent the 
comments being lost by exiting without saving (for example).  At the end of a 
session, the \bold{.commentsfile} is moved to a central, secure location.


\italic{Help}

This is just the ordinary Andrew help application.


\italic{Console}

This is the Andrew console application running a default console which does 
not monitor AFS activity.


\italic{Messages}

This is the Andrew Messages application, modified to trust the $HOME 
environment variable (it normally tries to verify the home directory with the 
white pages or /etc/passwd).  It is also modified to disallow the sending of 
mail.  The demo user is given a standard set of subscriptions, including the 
info-andrew mailing list and the andrew-demos bboard.

}
\heading{Security Issues}


We determined that the user must not have access to a shell or to any 
mechanism for invoking arbitrary programs.  Furthermore, one AFS cell insisted 
that they be ``invisible'' to demo users.  Finally, the user should not be 
able to abuse the CPU, the disk, or the network.  Here are descriptions of the 
steps we took to plug the security holes we found.


\subheading{Ness}


Ness was modified to understand the *.SecurityConscious preference, which is 
unmodifiably set to TRUE for the demo user.  This preference prevents Ness 
from invoking arbitrary processes.


\subheading{FLAMES}


The user is given an unmodifiable default FLAMES program which performs 
default mail-filing.  It can't be overwritten, so no FLAMES program can be 
created which either invokes arbitrary processes or sends mail to arbitrary 
destinations.


\subheading{Typetext}


Typetext and related 
insets\footnote{\
\begindata{fnote,537665536}
\textdsversion{12}
tm, tm19, tmv, typescript\
\enddata{fnote,537665536}
\view{fnotev,537665536,3,0,0}} were ``hidden'' so that running shells could 
not be embedded within a document.  ``Hiding'' an ATK class simply involves 
placing a non-functioning variant of that class early on the user's CLASSPATH. 
 (Note that without a shell, the CLASSPATH variable cannot be overridden.) 
 Other ``hidden'' ATK classes or packages include ``filter'' and ``compile.''


\subheading{Messages}


Two of Messages' classes, amsn and sendmsg, were overridden, meaning that 
modified versions of these classes were placed early in the CLASSPATH.  The 
modifications prevent the sending, re-sending, or forwarding of mail.


\subheading{Console}


Console, like Ness, was modified to obey the *.SecurityConscious preference 
(see above).  Also, /dev/kmem was set to read-only root permission, so the 
getstats program (which feeds information to console) runs setuid root.


\subheading{Printing}


We disabled printing by unmodifiably setting the *.printcommand preference to 
``cat > /dev/null.''  We initially overlooked the fact that printed matter, 
before being piped to the command in *.printcommand, was first filtered 
through the command in *.formatcommand, which by default includes a 
\bold{troff} step.  Troff commands, embedded in ATK documents, are capable of 
invoking arbitrary programs, so we unmodifiably set the *.formatcommand 
preference to ``cat.''


\subheading{CellServDB}


To hide the AFS cell which did not wish to be accessible by the demo user, we 
edited this file to remove the appropriate lines.


\subheading{Time Limit}


The session is limited to sixty minutes.


\subheading{User Limit}


No more than three users may be on a machine at one time.


\heading{Machine Configuration}


\subheading{Font server}


A machine wishing to be an rdemo font server must run the X11r5 fs program 
upon startup.  The fsconfig file should point to a directory containing the 
Andrew fonts in pcf format.  Look in misc/fonts for examples.


\subheading{Demo server}


A demo server machine must be configured as follows:


\leftindent{\italic{/.rdemo}

This is a file which contains the password for the rdemo account.  It should 
be readable only by the owner of the finegrd process.


\italic{getstats}

There must be a version of getstats which is setuid root.  This should be 
placed in the bindir_inst directory (see config.csh).


\italic{semaphoredir/hostname}

There must be a directory by this name where the rdemo account can create and 
delete semaphore files.  Semaphoredir is defined in config.csh, and hostname 
is the hostname of the demo server.


\italic{/dev/kmem}

/dev/kmem should be protected from reading by non-root.  This is a paranoid 
security measure.


\italic{/etc/fingerd}

/etc/fingerd should be a link to bindir/fingerd (see config.csh).


\italic{~rdemo/.cshrc}

This should be a link to miscdir/cshrc (see config.csh).  Note that the 
directory ~rdemo should not be writable by the rdemo account.


\italic{~rdemo/home}

This should be a directory where rdemo has write access (it's where 
synthesized home directories will go).

}
In addition, there should be two directories (defined by auditdir and 
commentdir in config.csh) where the rdemo account can insert files.

\enddata{text,537620332}
