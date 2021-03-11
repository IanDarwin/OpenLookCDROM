\begindata{text,538350972}
\textdsversion{12}
\template{default}
\define{global
}
\define{sans
menu:[Font,Sans]
attr:[FontFamily AndySans Int 0]}


\center{\bigger{\bigger{\italic{\bigger{\bigger{\bigger{Season's Greetings 

and 

Best Wishes for the New Year}}}}}}}


\center{\smaller{\smaller{(or Merry Christness)}}}

\flushright{ 
\begindata{raster,539103624}
2 0 68266 68266 0 0 57 33
bits 539103624 57 33
n |
g20g200ci |
g70g70fcg40g |
01f0g7ffcge0g |
07e0g7ff001e0	g |
7fc0ge0f001c0	g |
7f8001c0e00380	g |
03g0181c007h |
07g0381fc07h |
0eg0703Hh |
0c!0e07cffeh |
1c1e1c0701fch |
183c380eg3801	80 |
3878700cg3007	80 |
39f9e01cg703f	g |
3fGc018g70fcg |
3fbfg38g77f0g |
0e08g70g7f80g |
i60g7eh |
ie0j |
h01c0j |
h01c0j |
h0380j |
h07k |
h0ek |
g301ck |
g703ck |
ge0f0k |
01c1e0k |
0187c0k |
01Gl |
gfel |
g70l |
\enddata{raster, 539103624}
\view{rasterview,539103624,569,0,0}                 }


\italic{After you empower the Ness, you can click on the bell or the "Trim the 
tree" button for Christmas greetings.  For best results with the bell, use an 
IBM RT/PC workstation.  You can change the decorations on the tree and they 
will still "trim".}


\leftindent{\leftindent{Click me: 
  \
\begindata{cel,538589448}
\V 2
\begindata{raster,539103880}
2 0 68266 68266 0 0 32 32
bits 539103880 32 32
j |
j |
j |
j |
j |
i80 |
g6001c0 |
g600160 |
g600330 |
g600218 |
g60060c |
g603418 |
g606c10 |
gf04830 |
019859a0 |
01083360 |
02040240 |
020402c0 |
02040180 |
0204h |
!06h |
0402h |
0c03h |
180180g |
0c03h |
07feh |
0120h |
01e0h |
gc0h |
j |
j |
j |
\enddata{raster, 539103880}
0 539103880 0 0 0 0 
>OBJ< raster
>VIEW< rasterview
>REF< bell
\enddata{cel,538589448}
\view{celview,538589448,570,38,0} 
     \
\begindata{ness,538752008}
\origin{00\\10 Sep 1990 at 15:30:08 EDT\\wjh:  Fred Hansen\\00}
\template{default}
\define{fullwidth
menu:[Justify,Full Width]
attr:[LeftMargin LeftMargin Cm -25431]
attr:[RightMargin RightMargin Cm -27743]}
\define{sans
menu:[Font,Sans]
attr:[FontFamily AndySans Int 0]}
-- Jingle Bells



extend "bell"   on mouse "any"

	marker m, p

	if mouseaction = mouseleftup then

		-- find the character after the ness (the third object in the file)

		firstobject(thetext())		-- the raster

		firstobject(next(WhereItWas()))	-- the bell

		m := WhereItWas()

		raster_negative(currentinset, -1)

		focus(defaulttext)

		setcurrentselection(defaulttext,

			finish(m))

		textview_line_to_top(defaulttext)

		firstobject(next(m))	-- the ness

		m := WhereItWas()

		m := next(next(m))

		-- replace all text after the bell and up to the end marker

		--	with blank lines

		p := search(start(m), "- * - * -\\n")

		m := next(next(start(replace (extent(m, p), 

//









- * - * -


\\\\     ))))	

		m := replace(next(m), "Jingle Bells.\\n")

		im_ForceUpdate()

		im_ForceUpdate()

		play_notes("T150 L4 E E E  P8 P8")


		play_notes("T150  E E E  P8 P8")

		m := replace(next(m), "Jingle Bells.\\n")

		im_ForceUpdate()


		play_notes("T150  E G C D E")

		m := replace(next(m), "Jingle all the way.\\n")

		im_ForceUpdate()	


		play_notes("T150  P4 F F  F P12  ")

		m := replace(next(m), "Oh, what fun it is to ride \\n")

		im_ForceUpdate()	

		play_notes("T150   L8 F L4 F E E P12 L8 E E  L4 G")


		m := replace(next(m), "In a one horse open sleigh!\\n")

		im_ForceUpdate()	

		play_notes("T150   G F D C ")

		raster_negative(currentinset, -1)

	end if

end mouse   end extend



-- Trim the tree



marker letters := "asdfghjklpoiuytrewqzxcvbnm"


marker decorations := "angel @ ball o backslash \\\\ slash / star * branch - 
bar |"


boolean function initreadonly()

	if isreadonly(currentselection(defaulttext)) then

		textview_toggle_read_only(defaulttext)

	end if

	return TRUE

end function

boolean kludge := initreadonly()


function main()

	marker treestart, m , p

	treestart := search(thetext(),  "TREE")

	celview_set_visible(inset("Trim_Button"))

	celview_set_invisible(inset("Untrim_Button"))

	clearstyles(treestart)

	addstyles(treestart, "\italic{italic}")


	-- clear Jingle Bells

	-- find the character after the ness (the third object in the file)

	firstobject(thetext())		-- the raster

	firstobject(next(WhereItWas()))	-- the bell

	m := WhereItWas()

	firstobject(next(m))	-- the ness

	m := WhereItWas()

	m := next(next(m))

	-- replace all text after the bell and up to the end marker

	--	with blank lines

	p := search(start(m), "- * - * -\\n")

	m := next(next(start(replace (extent(m, p), 

//









- * - * -


\\\\     ))))

end function



function multireplace(text, old, new)

	marker m

	m := search(start(text), old)

	while m /= "" and extent(m, text) /= "" do

		replace(m, new)

		m := search(finish(m), old)

	end while	

end function



extend "Trim_Button" on mouse "any"


	if mouseaction = mouseleftup then

		marker treestart

		marker treeend

		marker word

		treestart := search(thetext(),  "TREE")

		treeend := start(search(finish(treestart), "---"))

		word := token(decorations, letters)

		while word /= "" do

			multireplace(extent(treestart, treeend),

					word,

					next(next(word)))

			word := token(finish(next(next(word))), letters)

		end while

		celview_set_invisible(inset("Trim_Button"))

		celview_set_visible(inset("Untrim_Button"))

		clearstyles(search(thetext(), "TREE"))

		addstyles(treestart, "\underline{underlined}")

	end if

end mouse end extend 


extend "Untrim_Button" on mouse "any"

	if mouseaction = mouseleftup then

		marker treestart

		marker treeend

		marker word

		treestart := search(thetext(),  "TREE")

		treeend := start(search(finish(treestart), "--"))

		word := token(decorations, letters)

		while word /= "" do

			multireplace(extent(treestart, treeend),

					next(next(word)),

					word)

			word := token(finish(next(next(word))), letters)

		end while

		celview_set_visible(inset("Trim_Button"))

		celview_set_invisible(inset("Untrim_Button"))

		clearstyles(treestart)

		addstyles(treestart, "\italic{italic}")

	end if

end mouse end extend


function thetext()

	return base(currentselection(defaulttext))

end function

\enddata{ness,538752008}
\view{nessview,538752008,571,367,100}

}}







- * - * -




\example{ 
 \
\begindata{cel,538590728}
\V 2
\begindata{value,539104136}
>2
\enddata{value,539104136}
10 539104136 1 0 0 0 
>OBJ< value
>VIEW< buttonV
>REF< Trim_Button
\begindata{text,538425508}
\textdsversion{12}
[long] <bodyfont-size> ()

[string] <bodyfont> ()

[string] <label> (Trim the tree)

\enddata{text,538425508}
\enddata{cel,538590728}
\view{celview,538590728,572,108,24}	}\italic{TREE}\example{\italic{ 
 } \
\begindata{cel,538590472}
\V 2
\begindata{value,539104264}
>9
\enddata{value,539104264}
10 539104264 1 0 0 1 
>OBJ< value
>VIEW< buttonV
>REF< Untrim_Button
>LINK< «¼
\begindata{text,538289820}
\textdsversion{12}
[long] <bodyfont-size> ()

[string] <bodyfont> ()

[string] <label> (Untrim the tree)

\enddata{text,538289820}
\enddata{cel,538590472}
\view{celview,538590472,573,120,21} 



                  angel

                 backslash slash

	             starball star ball

               branch  branch star branch

              starball star  branch starbranch

                star branch ballstar

               ball star branch starball

              star ball branch ball starbranch

            star branchball star branch ball star ball

           star branchstar branch ball star branch ball star

              slash star branchball star backslash

             star branch ball ball star branchstar

           starbranch ball branch star star ball star branch

          ball star star ball branch ball branch star star branch

         star branch ball star branch ball star branch ball star star

                  bar bar

                  bar bar

                 -----



}

\begindata{bp,537558784}
\enddata{bp,537558784}
\view{bpv,537558784,575,0,0}
Copyright 1992 Carnegie Mellon University and IBM.  All rights reserved.

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

 $

}}\enddata{text,538350972}
