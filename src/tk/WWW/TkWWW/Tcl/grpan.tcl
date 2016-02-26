## grpan.tcl Group Annotations for tkWWW user interface
## ==============
## Copyright (C) 1993
## Globewide Network Academy
## Macvicar Institute for Educational Software Development
##
## See the file COPYRIGHT for conditions

## Adapted from the file w3-mosaic.el
## Copyright (C) William M. Perry (wmperry@indiana.edu)

set tkW3GrpanServerName {}

proc tkW3GrpanFetchAnnotations {url} {
    global tkW3GrpanServerName
    if {tkW3GrpanServerName == ""} {
	tkW3Error "No group annotation server defined!"
	return
    }
    
    set cmd [format "ANN_GET /url=\"%s\";=" $url]
    set info [eval tkW3ServerOpen [split $tkW3GrpanServerName ":"]]
    set response [tkW3ServerSend $cmd]
}

proc tkW3GrpanDeleteGroupAnnotation {url} {
}

proc tkW3GrpanAddGroupAnnotation {url} {
}
