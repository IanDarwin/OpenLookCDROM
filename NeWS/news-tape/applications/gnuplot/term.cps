%
%  Written by:
% 	Dick Phillips
%	Dave Forslund
%	Los Alamos National Laboratory
%
%
%      Copyright, 1987, 1988 The Regents of the University of California.  This
%      software was produced under a U.S. Government contract (W-7405-
%      ENG-36) by Los Alamos National Laboratory, which is operated by the
%      University of California for the U.S. Department of Energy.  The U.S.
%      Government is licensed to use, reproduce, and distribute this software. 
%      Permission is granted to the public to copy and use this software
%      without charge, provided that this Notice and any statement of
%      authorship are reproduced on all copies.  Neither the Government nor
%      the University makes any warranty, express or implied, or assumes any
%      liability or responsibility for the use of this software.
	  
cdef ps_initialize(string programtitle, hsize, vsize) 
%
%   1 setlinequality			% highest line quality
 /gnudict dictbegin 			% create a private gnudict
%
    /errfile (%stdout) (w) file def
    /errstr 80 string def
    /eout {errfile exch writestring} def
    /nl (\n)def
    /dictprt {
     {exch errstr cvs (  ) append exch errstr cvs (\n) append
     append eout}forall (\n) eout
     } def
%

    /MyWindow DefaultWindow		% redefines /destroy method
      dictbegin
      dictend
      classbegin
         /destroy { createevent dup
          begin
           /Name /InsertValue def
           /Action (quit\n) def
           /Process gnufocus 1 get def
           /Canvas gnufocus 0 get def
          end
          sendevent
         } def
      classend
    def

    /gnufocus currentinputfocus def	% canvas and process for launch
%
    /firstwin true def			% indicates first time ever
    /mywins 0 array def		        % any number of windows
    /nwins 0 def			% none created yet
    /windex -1 def			% initial mywins array index
    /currwin windex def			% same as just created window
    /u1i 0 def
    /u2i hsize def			% x scale parameter for pagesize
    /v1i 0 def
    /v2i vsize def			% y scale parameter for pagesize
    /dxi 10 def				% x offset parameter for pagesize
    /dyi 10 def				% y offset parameter for pagesize
%
    /AReq? true def			% equal scaling? Default is yes

    /fixAR     % fixup aspect ratio
    { dictbegin
       aload pop                        % user extent array; 6 elements
       /v2 exch def /u2 exch def /v1 exch def /u1 exch def
       /dy exch def /dx exch def        % offset values
       clippath pathbbox                % get viewport extent
       /y2 exch dy sub def /x2 exch dx sub def   % retrieve viewport
       /y1 exch dy add def /x1 exch dx add def   % ... and apply offset

       x2 x1 sub y2 y1 sub div /ARvp exch def   % AR of viewport
       u2 u1 sub v2 v1 sub div /ARwi exch def   % AR of window
       ARwi ARvp ge			% calculate new extrema as req'd
         {x2 x1 sub u2 u1 sub div /S exch def
          v1 v2 add y2 y1 sub S div sub 2 div /v1 exch def
          y2 y1 sub S div v1 add /v2 exch def }
         {y2 y1 sub v2 v1 sub div /S exch def
          u1 u2 add x2 x1 sub S div sub 2 div /u1 exch def
          x2 x1 sub S div u1 add /u2 exch def }
       ifelse 
      dx dy u1 v1 u2 v2 6 array astore		% send back corrected data
      dictend pop
    } def
    /printfile (./lpr.ps) def
 
    /calctransform
     { 20 dict begin			% private dictionary
       aload pop			% user extent array; 6 elements
       /v2 exch def /u2 exch def /v1 exch def /u1 exch def
       /dy exch def /dx exch def	% offset values
       clippath pathbbox		% get viewport extent
       /y2 exch dy sub def /x2 exch dx sub def   % retrieve viewport
       /y1 exch dy add def /x1 exch dx add def   % ... and apply offset
       /sx x2 x1 sub u2 u1 sub div def
       /sy y2 y1 sub v2 v1 sub div def
       /tx x1 sx u1 mul sub def
       /ty y1 sy v1 mul sub def
       /umtx [sx 0 0 sy tx ty] def     % save user-space matrix
       umtx concat		       % set CTM
       end
     } def

    /llur				% gets lower left & upper right
     {
      /rectsize 20 def
      gsave
        ThisWindow /ClientCanvas get createoverlay setcanvas
        getwholerect waitprocess aload pop
        3 index 2 index min
        3 index 2 index min
        5 index 4 index max
        5 index 4 index max
        8 -4 roll pop pop pop pop
      grestore
      /rectsize 40 def
     } def

%   /itransform {			% because the real one doesn't work
    /itrans {                       % because the real one doesn't work
     10 dict begin			% private dictionary
     matrix currentmatrix dup dup dup	% 4 copies of CTM
     0 get /sx exch def		3 get /sy exch def
     4 get /tx exch def		5 get /ty exch def
     ty sub sy div
     exch
     tx sub sx div
     exch
     end
    } def

% These are reproduced so that the layout rectangle size can be changed

    /getrect { { x0 y lineto lineto x y0 lineto closepath } getanimated } def
%
    /rectsize 40 def
    /getclick {     % -  =>  x y  (Pauses for a mouse click)
       0 0 {moveto rectsize rectsize neg rect} getanimated waitprocess aload pop
    } def
%
    /getwholerect {
       { getclick
         1 index 1 index
         getrect waitprocess aload pop 4 array astore
       } fork
    } def

%  dictionaries and routines for handling object conversion

/$cvt 128 dict def
$cvt begin

/wout {fileout exch writestring}def
/cr (\n)def

%The following routines are for printing out PostScript objects.
%The command "=f=" will print something in a form that is
%compatable with its input syntax
%for example: the line {add 2 div} =f=
%will print "{add 2 div }".

/=f=  {  0 begin                %placeholder for dict
      /cp  0 def        %cp is used to sum up the current line length.
      typeprint
      end } def

/=f= load 0 64 dict put

/=f= load 0 get begin           %get dictionary for context.

%typeprint executes the type name of an object as a command.
/typeprint  {dup type exec} def
%/typeprint {dup type (                            ) cvs cr append tprint
%                    dup type exec } def

%rmargin is the right limit to the length of a line
/rmargin  72 def

%The following writes an escaped string that may contain special chars.

/wtype 256 string def
wtype 40 1 put wtype 41 1 put wtype 92 1 put    % ()\ escape
0 1 31 {wtype exch 2 put}for                    % [0..31]
127 1 255 {wtype exch 2 put}for                 % [127..255]

/wordfix
   {% word
       {/charcode exch def
        wtype charcode get dup 0 eq
           {pop fileout charcode write}
           {1 eq
               {(\\ )dup 1 charcode put wout}
               {fileout 92 write /wbyte charcode 8(000)cvrs def
                (000)0 3 wbyte length sub getinterval wout wbyte wout
               }ifelse
           }ifelse
       }forall
   }def

%tprint determines if the current string will overflow the line.
%If it does then an carriage return is inserted before printing.
/tprint  {dup length cp add rmargin gt {cr wout /cp 0 def}if
          dup length cp add /cp exch def wout}def

%numberprint prints numbers (followed by a space).
/numberprint {(                  ) cvs tprint ( )tprint}def

% typename commands - typenames print objects of their own type
/integertype {numberprint}def

/realtype {numberprint}def

/marktype {(mark )tprint pop}def

/dicttype {pop (-dictionary- ) tprint} def

/booleantype {(     ) cvs tprint ( ) tprint } def

/arraytype
  {dup xcheck
   {({)tprint{typeprint}forall(})tprint}
   {([)tprint{typeprint}forall(])tprint}ifelse
  }def

/packedarraytype
  {dup xcheck
   {({)tprint{typeprint}forall(})tprint}
   {([)tprint{typeprint}forall(])tprint}ifelse
  }def

/stringtype {(\()wout wordfix (\))wout}def

 /nametype {dup xcheck not{(/)}{()}ifelse exch
           (                       )cvs append ( ) append tprint}def

/nulltype {pop(null )tprint}def

/operatortype { (                              ) cvs
                dup /opstr exch def length 1 sub 0 1 3 -1 roll
                {/i exch def opstr i get dup 39 eq exch 96 eq or
                   {opstr i 32 put} if } for
                opstr (mark) search { pop pop pop ( [ ) tprint }
                { tprint } ifelse  } def

/filetype {pop(-filestream- )tprint}def

/savetype {pop(-savelevel- )tprint}def

/fonttype {pop(-font- )tprint}def

end                        %end of (=f=)context dictionary
 end                       %end of $cvt context
%
   /createcontrols {
%    /can
%     framebuffer newcanvas dup
%     begin
%     /Transparent false store
%     end
%    def
%  newpath 0 0 moveto 350 30 rect can reshapecanvas newpath
%  can setcanvas 700 450 movecanvas
%  1 fillcanvas 0 strokecanvas
%  can /Mapped true put

   /can mywins windex get /ClientCanvas get def

   /notify
   {
    createevent dup
    begin
     /Name /InsertValue def
     /Action (set samples ) ItemValue (        ) cvs append (\n) append def
     /Process gnufocus 1 get def
     /Canvas gnufocus 0 get def
    end
    sendevent
   } def

/items 50 dict dup begin
% /message (Message:) (                    )
%  /Right {} can 0 0 /new MessageItem send
%  10 10 /move 3 index send def
%/button (Button) /notify can 0 0 /new ButtonItem send
%  10 30 /move 3 index send def

   /bigslider (set samples = :) [50 200 50] /Right /notify can 240 20
        /new SliderItem send dup /ItemFrame 1 put
         0 0 /move 3 index send def
end def
/p items forkitems def
items paintitems

   /FillColor  1.0 def

/slideitem { % items fillcolor item => -
gsave
%   dup 4 1 roll                % item items fillcolor item
    /moveinteractive exch send  % item
%   /bbox exch send             % x y w h

%   (Item: x=%, y=%, w=%, h=% Canvas: w=%, h=%) [
%       6 2 roll
%       win begin FrameWidth FrameHeight end
%   ] /printf messages send
grestore
} def

    /slidemgr [
        items { % key item
            exch pop dup /ItemCanvas get        % item can
            MiddleMouseButton [items FillColor  % item can name [ dict color
            6 -1 roll /slideitem cvx] cvx       % can name proc
            DownTransition                      % can name proc action
            4 -1 roll eventmgrinterest          % interest
        } forall
    ] forkeventmgr def

} def             % createcontrols proc

 dictend def			 % end of gnudict definition
%
    /setpaintprocs {
  gnudict begin
      /thisw exch def
        thisw /gnufirst get
        {
          {
           /PaintClient
              {ClientCanvas setcanvas
               sclarray calctransform
               /pic load null ne {pic pause} if
               overlays
               {exch pop mywins exch get /pic get exec pause} forall
%              items paintitems
              } def
           /PaintIcon {
             gsave
              IconCanvas setcanvas
              IconFillColor fillcanvas IconBorderColor strokecanvas
              sclarray calctransform
               IconBorderColor setcolor
               /pic load null ne {pic pause} if
               overlays
               {exch pop mywins exch get /pic get exec pause} forall
             grestore
            } def
           } thisw send
          thisw /gnufirst false put

         thisw /wi get 0 gt             % for spawned window ...
           {/paint thisw send} if
         }                       % true proc
%
         {
           /paint thisw send
         } ifelse                % false proc
  end                           % end of gnudict context
         } def			% end of setpaintprocs procedure
%
    /crewin   			 % creates a new window and stores it in mywins
    { 				% procedure to create windows & insert menu
    gnudict begin
      /windex windex 1 add def   % increment creation index
      /nwins nwins 1 add def
      /currwin windex store      % set currwin to new one
      mywins windex null arrayinsert
         /mywins exch store	 % increase size of array
      mywins windex		 % set up to store window at mywins[windex]

      framebuffer /new MyWindow send	% make the window
%
      dup			 % send class variables
      {
        /FrameLabel
            programtitle ( ) append windex errstr cvs append def
        /ClientMenu
%       /gnumenu
          [
           (New Window)
           (Active Window)
           (Overlay)
           (Zoom)
           (Print)
           (Quit)
          ]
          [                      % menu procedures follow
           { crewin }    % create a new window

           { ThisWindow /wi get gnudict /currwin 3 -1 roll put }  % make active

           {                     % overlay dataset using random colors
             gsave

             {framebuffer createoverlay setcanvas
              0 1 nwins 1 sub
              { /i exch def
                createevent dup begin
                /Action DownTransition def
                /Canvas mywins i get /ClientCanvas get def
                /ClientData i def
                end expressinterest
                /xhair /xhair_m mywins i get /ClientCanvas get setstandardcursor
              } for		 % all current windows
              awaitevent
              /Interest get dup		% make a copy of the interest to revoke
              /ClientData get /whichw exch def
                0 1 nwins 1 sub		% which window to overlay?
                { /i exch def
                  /ptr /ptr_m mywins i get /ClientCanvas get setstandardcursor
                } for
              revokeinterest		% remove it from the interest table
              } fork waitprocess

             ThisWindow dup /pic get null eq	% an insert, not an overlay
             {
               mywins whichw get /pic get /pic exch put
               ThisWindow setpaintprocs
             }
  
             {
               pop				% a true overlay
%              random .9 1 hsbcolor setcolor
               ThisWindow dup /ClientCanvas get setcanvas
               /sclarray get calctransform
               mywins whichw get /pic get exec
               ThisWindow /overlays get dup	     % point to overlay window
               length 1 add (   ) cvs cvn whichw put
             } ifelse

             grestore
           }

           {                                         % zoom

            gnudict begin			% put me on top

             ThisWindow /ClientCanvas get setcanvas
             ThisWindow /sclarray get dup
             gsave
               calctransform
               aload pop pop pop pop pop
               llur				     % get xll,yll xur,yur
%              itransform 4 2 roll		     % transform v2,u2
%              itransform 4 2 roll		     % .. and v1,u1
               itrans 4 2 roll                   % transform v2,u2
               itrans 4 2 roll                   % .. and v1,u1

               6 array astore			     % create an array
             AReq? { fixAR } if
             grestore
             ThisWindow /sclarray 3 -1 roll put
             /paint ThisWindow send

            ThisWindow /firstzoom get
           { 4 (Restore) { ThisWindow /sclarray
               dxi dyi u1i v1i u2i v2i 6 array astore put
               /paint ThisWindow send
%              4 /deleteitem ThisWindow /gnumenu get send
               4 /deleteitem ThisWindow /ClientMenu get send
               ThisWindow /firstzoom true put }

%              /insertitem ThisWindow /gnumenu get send
               /insertitem ThisWindow /ClientMenu get send
             ThisWindow /firstzoom false put } if

            end					% end of gnudict
           }

           {                     % print window contents
            {gnudict begin
             $cvt begin
               /fileout printfile (w) file def
               fileout (%! \n) writestring
               fileout (save \n) writestring
               fileout (/calctransform \n) writestring
               /calctransform load =f=        % convert to external form
               fileout ( def \n) writestring
               ThisWindow /sclarray get dup dup 0 50 put 1 50 put =f=
               (  calctransform \n) 
               fileout exch writestring
%              fileout ( 1 1 itransform pop setlinewidth \n) writestring
%
               currentfont 
               begin
                  FontName (                            ) cvs
                    /fname exch def
                  fileout (/) fname append writestring
                  fileout ( findfont ) writestring
                  FontMatrix 0 get (        ) cvs
                    fileout exch writestring
                  fileout ( scalefont setfont ) writestring
               end
               fileout (\n % End of prologue \n) writestring
               ThisWindow /wi get
               mywins exch get /pic get dup null ne
               { {=f= pause } forall } { pop} ifelse     % convert /pic
               ThisWindow /overlays get
               {
                exch pop mywins exch get /pic get {=f= pause } forall
                fileout (\n) writestring
               } forall
               fileout (\n % $End of data \n) writestring
               fileout (restore showpage \n) writestring
               fileout closefile
	       (lpr -m ) printfile append forkunix % send to printer
             end                        % end of $cvt context
            end                 % end of gnudict context
            } fork pop    % put it in the background
           }
				% send quit
           { /destroy ThisWindow send }

          ]
      /new DefaultMenu send def
%    /ClientMenu [ (Item 1) {} (Gnumenu =>) gnumenu ]
%    /new DefaultMenu send def
      }
      exch send
      dup			 % shape the window interactively
      firstwin
      { 600 550 hsize vsize /reshape 6 -1 roll send }
      { /reshapefromuser exch send } ifelse
%
      dup /gnufirst true put	 % store first flag with window
      dup /firstzoom true put	 % flag for first zoom in window
      dup /wi windex put	 % also, store window index
      dup dxi dyi u1i v1i u2i v2i % initial scaling data
        6 array astore
        /sclarray exch put	 % save scaling for window
      dup /pic null put		 % null data array
      dup /overlays 10 dict put  % dictionary for overlay /pics
      dup			 % make it visible
      /map exch send
      put			 % store window object in array
%
%     createcontrols
    end				 % end of gnudict

    } def			 % crewin definition
%
%
cdef ps_newpath()
%
        {             % opens capture of moveto & lineto commands
           gsave      % save current graphics state
              newpath % initialize the path
%
%
cdef ps_display()
%
	      stroke   % stroke the path
           grestore    % restore graphics state
        }              % add closing brace
        gnudict begin
          firstwin {crewin /firstwin false def }if   % create first window
          /pic exch mywins currwin get dup 4 2 roll put  % defines current /pic 
          dup dxi dyi u1i v1i u2i v2i 			% always restore
          6 array astore				% original scale
          /sclarray exch put 				% for new dataset
          dup /firstzoom get				% remove Restore item
            {pop}					% ..., if needed.
            {dup 4 /deleteitem 3 -1 roll /ClientMenu get send
%           {dup 4 /deleteitem 3 -1 roll /gnumenu get send
                 /firstzoom true put} ifelse
%
          mywins currwin get
        end                     % end of gnudict
        setpaintprocs       % install PaintClient, etc.
%
%
cdef ps_setdash(string s) stroke [s {cvi} forall] 0 setdash
cdef ps_printfile(string str)
	gnudict begin
	  /printfile str def
        end
%
