@device(postscript)
@make(ReferenceCard)
@definefont(Userfont, 1=<Rawfont "NewCenturySchlbkRoman">,
2=<Rawfont "NewCenturySchlbkBold">,
3=<Rawfont "NewCenturySchlbkBoldItalic">,
4=<Rawfont "NewCenturySchlbkRoman">)

@Style(Font "NewCenturySchoolbook", size 7)
@Style(spacing 1,indent 0)
@Style(leftmargin .33inch)
@style(rightmargin 4.8inches)
@Style(linewidth 4.0inches)
@Style(TopMargin 1inches)
@Style(BottomMargin .01 inches)
@style(date "March 8, 1952")
@modify(MajorHeading,FlushLeft, size 15, Facecode B, above .2 inch,
below .2inch)
@Modify(Heading, Flushleft, size 12, Facecode B, above .2in, below .2in)
@Modify(SubHeading, Flushleft, size 10, Facecode B)
@Modify(heading, font userfont, facecode 2)
@modify(subheading, font userfont, facecode 4,above .2inch,below .2inch)
@Modify(display, rightmargin +0)
@Modify(example,rightmargin +0)
@modify(enumerate, group)
@Tabclear
@Tabset(2.25 inches)

@comment{@Style(
	Hyphenation On,
	TopMargin .5 inch,
	BottomMargin .3 inch,
	Spread 0,
	WidowAction Force,
	Font TimesRoman,
	Size 9
)
@define(bbb, size 11)
@define(bb, indent -3, size 10)
@define(col, columns 2, columnmargin 1inch, columnwidth 3.75inch,
boxed)
@pageheading()}

@begin(majorheading, flushright)
EOS Quick Reference
@end(majorheading)
@begin(heading, flushright)
S. Belville, W. Cattey
@value(date)
@end(heading)

@heading(Logging In)

@begin(enumerate)
At a blank looking workstation, hit any key.  

Type your username at the @t[login:] prompt and hit the <@c[return]>
key.  You should be prompted for your password.

Type your password.  
@end(enumerate)
In a minute, you should get a system prompt that looks something like
@t(athena%).  You are ready to start EOS.  If you mistyped your
username or password, you will get the @t[login:] prompt back; try again.

@heading(starting EOS)

@begin(enumerate)
type: 
@example(eos -c @i[course])
to start the eos program.
Substitute the proper name for @i(course).

Click left to position your EOS window.
@end(enumerate)

@heading (Important Menu Commands)
(Less commonly used commands are on menus instead of buttons.)
@begin(format)
@tabclear
@tabset(+1inch, +.6inch)
@b(Delete Window)@\Delete Window that popped up menu.
@b(Quit)@\Quit application and DESTROY all sub-windows.
@b(Update list)@\Show any changes in list of papers since last update.
@b[Print selected paper(s)]@\Send selected papers to the printer.
@b[Delete selected paper(s)]@\Remove selected papers from server.

@end(format)

@heading (EOS Window Command Buttons)

@enumerate{
Click the left or right with the mouse in the button.

Click left to position the command's display window the first time
you use the command in an EOS session.
}
@subheading(Help -- Help on EOS and affiliated programs.)

@enumerate{
Click the left button to position the help window.

Click on topics or use the menu commands.
}
@newpage
@subheading(Guide -- The online Style Guide.)
If you want to use the style guide type:
@example (attach eos)
Then click on the @t(GUIDE) button.

@subheading(Turn In -- Turn in file for grading)

@begin(enumerate)
Fill in the form box with the assignment name and
assignment number.

Click on the @t(OK) button.
@end(enumerate)

Click on the toggle switch ONLY if you want to turn in a file
instead of the contents of the current editor window.

@subheading(Pick Up -- Pick up graded files)

Any available papers are copied both into files in your working directory,
and into your main editor window.

Click on the @b(CONTINUE) button to hide pickup window.

@subheading(Handouts -- Take in-class handouts.)
@begin(enumerate)
Click the left mouse button to select one handout, or click the right mouse button to select each of several handouts.

Click @t(EDIT) to bring the file into the EOS Editor buffer,
or @t(KEEP) to save the file in your home directory,
or @t(READ) to just view the file in a new window.

Click the @t(HIDE) button to hide the handout window.
@end(enumerate)

@subheading(Exchange -- Exchange files in class.)

@Flushleft[To get other students' files:]

@begin(enumerate)
Click the left mouse button to select one file, or Click the right
mouse button to select each of several files.

Click @t(EDIT) to bring the file into the EOS Editor buffer, or
 @t(KEEP) to save the file in your home directory or @t(READ) 
to just view the file in a new window.

Click the @t(HIDE) button to hide the exchange window.
@end(enumerate)

@Flushleft[To make your file available:]
@begin(enumerate)
Click the @t(SUBMIT) button.

From the dialogue box, choose @t(Use current edit window) to 
submit the contents of your EOS editor window, or @t(Specify a
filename)  to submit a file.

Fill in the form box with the @b(Name) and click on the
 @t(accept) button.

Click the @t(HIDE) button to hide the exchange window.
@end(enumerate)

@newpage()
@heading(Basic Editor Window Commands)

@subheading[File control:]

@begin(format)
@tabclear
@tabset(+.5inch, +1inch, +.5inch)

@b(C-x C-s)@\save buffer
@b(C-x C-f)@\find file

@subheading[Movement:]

@b(C-f)@\forward character@\@b(M-f)@\forward word
@b(C-b)@\backwards character@\@b(M-b)@\backwards word
@b(C-a)@\beginning of line@\@b(C-e)@\end of line
@b(C-n)@\next line@\@b(C-p)@\previous line
@b(C-v)@\next page@\@b(M-v)@\previous page
@b(M-<)@\beginning of buffer@\@b(M->)@\end of buffer
click left mouse button@\Editor caret to mouse position

@subheading[Deletion:]

@b(C-d)@\delete character@\@b(<@c[del]>)@\delete previous character
@b(M-d)@\delete word@\@b(M-<del>)@\delete word backwards
@b(C-k)@\delete to end of line@\@b(M-k)@\delete to end of sentence
@end(format)

@subheading[Deleting regions of text:]

@begin(enumerate)
Highlight the region by clicking the mouse at the start of the
region and dragging to the end of the region.

@b(C-w) deletes the region.  @b(M-w) makes the text
available for retrieval without deleting it.
@end(enumerate)

@subheading[Retrieving deleted text:]
@begin(format)
@tabclear
@tabset(+.6inch, +1inch, +.6inch)

@b(C-y)@\retrieve (yank) text

@subheading[Annotation Menu Commands]

These commands appear on the @t(Page) menu card.

@begin (format)
@b(Insert Note)
@b(Open Notes)
@b(Close Notes)
@end (format)

@end(format)

@newpage()
@heading[Useful Unix Commands]

@begin(description, leftmargin +20, indent -15)

@b(attach)@\attaches a remote file system to your workstation, so that
you can access the files it contains.  

@b(cd) @i[directory]@\puts you in the directory of your choice.  If
you do not give it an argument, it takes you to your home directory.

@b(chmod)@\changes the mode (protection) of a file.  @b[chmod 644
file] allows you to read and write a file, and allows everyone else to
read it.  @b[chmod 600 file] allows you to read and write your file,
but no one else can read it.  See the Athena document @p[Athena
Accounts] for more details.  

@b(cp) @i[source dest]@\copies the file @i[source] onto the file
@i[dest].  

@b(finger) @i[name]@b[@@athena]@\@i[name] can be username, first name,
or last name.  @b[finger] will give you information about the user.
Helpful if you don't know someone's username.  

@b(logout)@\terminates your login session.  If you don't logout when
you leave your workstation, anyone who walks up to the keyboard will
have access to your files.

@b(ls)@\lists the files in a directory.  Defaults to the current working
directory if you do not specify a directory.

@b(man) @i[commandname]@\on-line Unix manual.  Gives you a manual
entry for the @i[commandname].  Use this for any of the commands in
this list to get more information.  

@b(mkdir)@\makes a new directory.

@b(more)@\prints out a file you specify one screen full at a time.

@b(mv) @i[source dest]@\moves (renames) @i[source] to @i[dest].  

@b(pwd)@\print working directory.  Type this when you get lost in the
Unix file system.  It tells you what the current working directory is.

@b(rm -i)@\removes a file you specify.  Unix is most unforgiving;
once you remove a file, you cannot ever get it back.  The @b[-i]
option gives you a chance not to delete something.

@b(xterm &)@\starts up another window that can be used like a
@i[terminal], i.e. just like your login window.  

@end(description)
