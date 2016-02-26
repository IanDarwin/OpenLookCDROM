%
% NeWS interface for the "newsmail" program.
%

#define NAME_TAG        1

% Get the user name of the owner of the NeWS server
cdef ps_username(string s) => NAME_TAG(s)
    UserProfile /UserName known
    { UserProfile /UserName get }
    { (?) } ifelse
    NAME_TAG tagprint typedprint

% Three procedures for sending down an array of strings
cdef ps_begin_list()
    [

cdef ps_end_list()
    ]

cdef ps_string(string s)
    s

% Actually create the icon.
cdef ps_mailwindow(string sender)

/MailWindow DefaultWindow [
    /Text       % Text of the mail item
    /Sender     % Name of the sender
]
classbegin
    /TextFont /Times-Roman findfont 14 scalefont def
    /SmallFont /Times-Roman findfont 10 scalefont def
    /Margin 10 def

    /new { % text sender => instance
	/new super send begin
	    /Sender exch def
	    /Text exch def
	    /Iconic? true def
	    /IconX 50 def
	    /IconY 800 def
	    /IconWidth 64  2 mul def
	    /IconHeight 40  2 mul def

	    IconX IconY         % position of window
	    % Now calculate the width and height of the window
	    TextFont setfont
	    0 Text { stringwidth pop max } forall
	    Margin 2 mul add BorderLeft add BorderRight add % Width

	    TextFont fontheight Text length mul Margin 2 mul add
	    BorderTop add BorderBottom add % Height

	    3 2 roll 1 index sub IconHeight add 3 1 roll % Adjust Y position
	    reshape             % Shape the window
	    IconX IconY move    % Move the icon to its proper place
	    currentdict
	end
    } def

    % Write the mail item into the window.
    /PaintClient {
	1 fillcanvas 0 setgray
	TextFont setfont
	Margin ClientHeight Margin sub TextFont fontheight sub moveto
	Text {
	    show
	    Margin currentpoint exch pop moveto
	    0 TextFont fontheight neg rmoveto
	} forall
    } def

    % Make the Icon look like an envelope.
    /PaintIcon {
	gsave
	IconCanvas setcanvas
	1 fillcanvas 0 strokecanvas
	matrix currentmatrix
	IconWidth IconHeight scale
	0 0 moveto 0 1 lineto .5 .3 lineto 1 1 lineto
	1 0 lineto closepath clip                 % Set flap clipping path
	0 0 moveto .5 .7 lineto 1 0 lineto stroke % Draw two lines clipped by flap
	initclip                                  % Reset clip path
	0 1 moveto .5 .3 lineto 1 1 lineto stroke % Draw flap
	.5 .7 moveto
	setmatrix
	SmallFont setfont Sender cshow
	grestore
    } def

classend def

% Create the window.  We create a new process group so that the window
% does not get zapped when the C program closes its socket.
{
    newprocessgroup
    sender framebuffer /new MailWindow send
    /map exch send
} fork clear
