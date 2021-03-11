\begindata{text,537915384}
\textdsversion{12}
\template{default}
\define{global
}
This demo file shows adjusting the volume logarithmically.


 \
\begindata{cel,538429960}
\V 2
\begindata{value,538426120}
>15
\enddata{value,538426120}
10 538426120 1 0 0 0 
>OBJ< value
>VIEW< sliderV
>REF< volume
\begindata{text,538394940}
\textdsversion{12}
[long] <min_value> (01)

[long] <max_value> (60)

[long] <bodyfont-size> (8)

[string] <bodyfont> (andysans)

[string] <label> (Volume)

\enddata{text,538394940}
\enddata{cel,538429960}
\view{celview,538429960,69,0,181} 
      \
\begindata{cel,538430728}
\V 2
\begindata{value,538426504}
>349
\enddata{value,538426504}
10 538426504 1 0 0 0 
>OBJ< value
>VIEW< bargraphV
>REF< speaker
\begindata{text,538393796}
\textdsversion{12}
[long] <min_value> (0)

[long] <max_value> (409)

[long] <bodyfont-size> (8)

[string] <bodyfont> (andysans)

[string] <label> (Speaker)

\enddata{text,538393796}
\enddata{cel,538430728}
\view{celview,538430728,70,0,183} 
    \
\begindata{cel,538430472}
\V 2
10 538426504 1 0 0 0 
>OBJ< value
>VIEW< sliderV
>REF< speakerknob
>LINK< speaker
\begindata{text,538392420}
\textdsversion{12}
[long] <min_value> (-100)

[long] <max_value> (600)

[long] <bodyfont-size> (8)

[string] <bodyfont> (andysans)

[string] <label> (Speaker Knob)

\enddata{text,538392420}
\enddata{cel,538430472}
\view{celview,538430472,71,81,185}


Speaker and Speaker Knob share the same data object, so they always have the 
same values.


\begindata{ness,537682952}
\origin{-1\\unknown date\\????: Unknown User\\00}
\template{default}
\italic{extend} \bold{"volume"}

	\italic{on mouse} \bold{"any"}

		dohit(currentinset, mouseaction, mousex, mousey);

		if mouseaction = mouseleftup then

			integer volume;

			volume := value_GetValue(currentinset);

			volume := round(100.0 * log(float(volume)));

			value_setValue(inset("speaker"), volume);

		end if;

	\italic{end mouse}

\italic{end extend}


function main()

	writefile("/tmp/t", "\bold{Now} is the \bigger{\bigger{\underline{season}}} 
to be \italic{Jolly}.");

end function;\
\enddata{ness,537682952}
\view{nessview,537682952,75,0,0}
-- 
\begindata{bp,537558784}
\enddata{bp,537558784}
\view{bpv,537558784,76,0,0}

-- Copyright 1992 Carnegie Mellon University and IBM.  All rights reserved.

\smaller{\smaller{-- $Disclaimer: 

-- Permission to use, copy, modify, and distribute this software and its 

-- documentation for any purpose is hereby granted without fee, 

-- provided that the above copyright notice appear in all copies and that 

-- both that copyright notice, this permission notice, and the following 

-- disclaimer appear in supporting documentation, and that the names of 

-- IBM, Carnegie Mellon University, and other copyright holders, not be 

-- used in advertising or publicity pertaining to distribution of the software 

-- without specific, written prior permission.

-- 

-- IBM, CARNEGIE MELLON UNIVERSITY, AND THE OTHER COPYRIGHT HOLDERS 

-- DISCLAIM ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING 

-- ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS.  IN NO EVENT 

-- SHALL IBM, CARNEGIE MELLON UNIVERSITY, OR ANY OTHER COPYRIGHT HOLDER 

-- BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY 

-- DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, 

-- WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS 

-- ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE 

-- OF THIS SOFTWARE.

--  $

}}\enddata{text,537915384}
