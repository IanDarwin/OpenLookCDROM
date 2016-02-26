## version.tcl - Verify version of the server
## ==============
## Copyright (C) 1992-1993
## Globewide Network Academy
## Macvicar Institute for Educational Software Development
##
## See the file COPYRIGHT for conditions

## This file performs version control

## tkW3VersionVerify: Verify the version of the server
## ----------

proc tkW3VersionVerify {version} {
    if ![regexp {^([0-9]+).([0-9]+)} [HtVersion] version_num major minor] {
	puts stderr "Warning: Cannot find version of tkWWW.server"
	return;
    }
    if {$major==0 && $minor < 9} {
	puts stderr \
"Error: tkWWW frontend is incompatible with tkWWW.server
frontend version: $version  server version: $version_num
You must recompile the tkWWW.server"
      exit 1
   }
   if {$version_num != $version} {
      puts stderr \
"Warning: Frontend version is different from server version
frontend version: $version  server version: $version_num"
   }
}     
