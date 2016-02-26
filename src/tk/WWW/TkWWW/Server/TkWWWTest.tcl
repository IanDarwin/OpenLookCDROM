# tkWWW tk interface to World Wide Web
# Copyright (C) 1992 Joseph Wang (joe@athena.mit.edu)
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

# ******* The Home Page

set tkWWWHomePage file:/afs/athena.mit.edu/course/other/cdsdev/html/welcome.html

proc go {w} {
  HtLoad [HtParseName [lindex [HtGetReference $w] 1]]
  show_text
}

proc back {} {
  HtBacktrack
  show_text
}

proc home {} {
  global tkWWWHomePage
  HtLoad $tkWWWHomePage
}

proc find {w} {
  HtFind $w
  show_text
}

proc show_text {} {
  set file [open /tmp/output w+]
  puts $file [HtGetText]
  close $file 
  exec more /tmp/output
}


proc show_source {} {
  set file [open /tmp/output w+]
  puts $file [HtGetSource]
  close $file 
  exec more /tmp/output
}

proc load_suffixes {list} {
    foreach item $list {
	eval HtSetSuffix $item
    }
}

proc load_presentations {list} {
    foreach item $list {
	eval HtSetConversionToFrontEnd $item
    }
}

set suffix_list  {
    { ".mime" "www/mime"  1.0 }	
    
    { ".PS" 	"application/postscript"  0.8 }
    { ".eps" 	"application/postscript"  0.8 }
    { ".ai" 	"application/postscript"  0.5 }
    { ".ps" 	"application/postscript"  0.8 }
    
    { ".html" "text/html"  1.0 }		

    { ".c" 	"text/plain"  0.5 }
    { ".h" 	"text/plain"  0.5 }		
    { ".m" 	"text/plain"  0.5 } 
    { ".txt"  "text/plain"  0.5 }

    { ".rtf" 	"application/x-rtf"  1.0 }
    
    { ".snd"  "audio/basic"  1.0 }
    
    { ".bin" 	"application/octet-stream"  1.0 }

    { ".Z" 	"application/x-compressed"  1.0 }
    
    { ".gif"  "image/gif"  1.0 }

    { ".tiff" "image/x-tiff"  1.0 }
    
    { ".jpg"  "image/jpeg"  1.0 }
    { ".JPG"  "image/jpeg"  1.0 }
    { ".JPEG" "image/jpeg"  1.0 }
    { ".jpeg" "image/jpeg"  1.0 }
    
    { ".MPEG" "video/mpeg"  1.0 }
    { ".mpg" "video/mpeg"  1.0 }
    { ".MPG" "video/mpeg"  1.0 }
    { ".mpeg" "video/mpeg"  1.0 }

    { ".tex" "application/tex" 1.0}
    { ".latex" "application/latex" 1.0}
    { ".texi" "application/texinfo" 1.0}
    { ".dvi" "application/dvi" 1.0}
}


load_suffixes $suffix_list
set foo [home]











