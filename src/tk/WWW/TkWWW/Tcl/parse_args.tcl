## parse_args.tcl - Parse command line arguments
## ==============
## Copyright (C) 1992-1993
## Globewide Network Academy
## Macvicar Institute for Educational Software Development
##
## See the file COPYRIGHT for conditions

set tkW3ParseArgsShowInfo 1

## tkW3ParseArgs: Parse arguments
## ----------
## On entry,
##   argv: command line to parse
## On exit,
##   none
## Side effects,
##   Resets various internal variables 

proc tkW3ParseArgs {argv} {
   global tkW3ConfigStartPage tkW3ConfigHomePage tkW3ConfigFile
   global tkW3ParseArgsImages tkW3ParseArgsShowInfo
   global tkW3Version

   set tkW3ParseArgsImages(anchor) ""
   set tkW3ParseArgsImages(ismap) 0

   set argc [llength $argv]
   set iconic 0
   for {set i 0} {$i < $argc} { incr i 1} {
       set flag [string tolower [lindex $argv $i]]
       switch -exact $flag {
	   {--home} -
	   {-wwwhome} {
	       incr i
	       if {$i < $argc} {
		   set tkW3ConfigHomePage [lindex $argv $i]
	       }
	   }
	   {--start} -
	   {-wwwstart} {
	       incr i
	       if {$i < $argc} {
		   set tkW3ConfigStartPage [lindex $argv $i]
	       }
	   }
	   {--noinfo} -
	   {-wwwnoinfo} {
	       set tkW3ParseArgsShowInfo 0
	   }
	   {--iconic} -
	   {-wwwiconic} {
	       wm iconify .
	   }
	   {--iconpos} -
	   {-wwwiconpos} {
	       incr i
	       if {$i < $argc} {
		   set x [lindex $argv $i]
		   incr i
		   wm iconpos . $x [lindex $argv $i]
	       }
	   }
	   {--geometry} -
	   {-wwwgeometry} {
	       incr i
	       if {$i < $argc} {
		   wm geometry . [lindex $argv $i]
	       }
	   }
	   {--imageanc} -
	   {-wwwimageanc} {
	       incr i
	       if {$i < $argc} {
		   set tkW3ParseArgsImages(anchor) [lindex $argv $i]
	       }
	   }
	   {--imageismap} -
	   {-wwwimageismap} {
	       set tkW3ParseArgsImages(ismap) 1
	   }
	   {--help} {
	       puts stdout "\
Arguments:
   --home = Set the home page
   --start = Set the start page
   --config = specify alternative config file
   --noinfo = Start up tkWWW without showing the info window
   --iconic = Start up tkWWW in iconic mode
   --iconpos x y = Set tkWWW icon position
   --geometry = Set tkWWW geometry
"
	       exit
	   }
	   {--config} -
	   {-wwwconfig} {
	       incr i
	       if {$i < $argc} {
		   set tkW3ConfigFile [lindex $argv $i]
	       }
	   }
	   {--version} {
	       puts stdout "tkwww version $tkW3Version"
	       exit
	   }
	   {-*} {
	       puts stdout "tkWWW Warning: Unknown command line flag $flag
Type \"tkwww --help\" for valid flags"
	       exit
	   }
	   {default} {
	       set tkW3ConfigStartPage [lindex $argv $i]
	   }
       }
   }
}
