\begindata{text,5380224}
\textdsversion{12}
\template{messages}
\define{sansserif
menu:[Font~1,Sans Serif]
attr:[FontFamily AndySans Int 0]}
Your help is needed in gathering information vital to the national economy. 
 Please fill out the questionnaire below.  


Instructions:

\leftindent{\description{1) To select from a set of buttons, just click on the 
one you want.

2) To enter text, click in the inner box, type your answer, and then type the 
ENTER or RETURN key.

3) When ready, empower the Ness below.  (Scroll to its end and click the 
button labelled "Empower-I trust the source of this script".)

4) Finally, to send your reply, click the \sansserif{SEND} button.

}}
Thank you, 

Your friendly neighborhood demographer.



What is your favorite color?

	\
\begindata{cel,5605376}
\V 2
\begindata{value,5380736}
>2
\enddata{value,5380736}
10 5380736 1 0 0 0 
>OBJ< value
>VIEW< mbuttonv
>REF< color
\begindata{text,5380864}
\textdsversion{12}
[string] <style> ()

[string] <background-color> ()

[string] <foreground-color> ()

[string] <placement> ()

[long] <bodyfont-size> ()

[string] <bodyfont> ()

[string] <label> (black:blue:green:red)

\enddata{text,5380864}
\enddata{cel,5605376}
\view{celview,5605376,12,0,0}


Please click in the box, type your choice of ice cream, and then press ENTER 
or RETURN:

	\
\begindata{cel,5605888}
\V 2
\begindata{value,5380992}
>0
\enddata{value,5380992}
10 5380992 1 0 0 0 
>OBJ< value
>VIEW< enterstrV
>REF< ice cream
\begindata{text,5381120}
\textdsversion{12}
[string] <style> ()

[string] <background-color> ()

[string] <foreground-color> ()

[long] <bodyfont-size> ()

[string] <bodyfont> ()

[string] <label> (What is your favorite ice cream?)

\enddata{text,5381120}
\enddata{cel,5605888}
\view{celview,5605888,13,357,0}



This is the Ness script.  Please empower it to send your reply.

\begindata{ness,5922816}
\origin{01\\26 Jan 1993 at 11:17:53 EST\\wjh:  Wilfred Hansen\\00}
\template{default}
\define{global
}
\define{sans
menu:[Font,Sans]
attr:[FontFamily AndySans Int 0]}
function sendinfoback()

	-- specify recipient	>>>  EDIT THE NEXT LINE  <<<

	marker recipient := \italic{user-id-to-receive-responses}

	-- get values from the insets

	integer color := value_GetValue(inset("color"))

	marker flavor := value_GetString(inset("ice cream"))

	-- construct message

	marker msg := "Color: " ~ textimage(color) 

			~ "\\nFlavor: " ~ flavor

	-- send the message

	system( "mail -n -s  'questionnaire response' "

			~ recipient 

			~ " <<EOF\\n" ~ msg ~ "\\nEOF\\n")

end function


extend "send button"

	on mouse "any"

		if mouseaction = mouseleftup then

			sendinfoback()

		end if

	end mouse

end extend

\enddata{ness,5922816}
\view{nessview,5922816,24,0,0}

After empowering the Ness script, click this button to send your response.

	\
\begindata{cel,5392384}
\V 2
\begindata{value,8558976}
>1
\enddata{value,8558976}
10 8558976 1 0 0 0 
>OBJ< value
>VIEW< buttonV
>REF< send button
\begindata{text,8559104}
\textdsversion{12}
[string] <style> (3)

[string] <background-color> ()

[string] <foreground-color> ()

[long] <bodyfont-size> ()

[string] <bodyfont> (helvetica)

[string] <label> (SEND)

\enddata{text,8559104}
\enddata{cel,5392384}
\view{celview,5392384,27,0,43\
}\enddata{text,5380224}
