## server.tcl - Communication with backend processes
## ==============
## Copyright (C) 1992-1993
## Globewide Network Academy
## Macvicar Institute for Educational Software Development
##
## See the file COPYRIGHT for conditions

## Conventions:
##   all interaction with the tkW3 server should occur through the
##     procedures in this file

## These routines depend on a structure called server info which consists
## of 
##
## {"the filehandle" "the prompt" "debug"}

## *************************
## Procedures for communicating with the tkW3 server
##    tkW3ServerOpen - opens a connection with the tkW3 server and
##       returns the opening message
##    tkW3ServerSend - sends a string to the tkW3 server
##    tkW3ServerReceive - receives a string from the tkW3 server
##    tkW3ServerClose - shuts down the tkW3 server
## *************************

## tkW3ServerOpen: Open connection with server
## ----------
## On entry,
##   exec_string: string to execute
## On exit,
##   returns info structure
## Side effects,
##   resets the variable tkW3Server

## Remember that tcl returns value of the the last statement executed in 
## a procedure

proc tkW3ServerOpen {exec_string prompt} {
   # open connection with server
   # There should be some way of figuring out if the server responds
   set tkW3Server [open "| $exec_string" w+]
   set info [list $tkW3Server $prompt 0]
   tkW3ServerReceive $info
   return $info
}

## tkW3ServerSend: Send message to tkW3 Server
## ----------
## On entry,
##   w: message to server
## On exit,
##   returns response from server
## WARNING:
##   SERVER MUST MAKE SOME SORT OF RESPONSE OR ELSE THERE WILL BE DEADLOCK

proc tkW3ServerSend {info w} {
   # Print out if debug
   if {[lindex $info 2]} {
       puts stdout $w
   }

   puts [lindex $info 0] $w
   tkW3ServerReceive $info
}

## tkW3ServerReceive: Wait for response from server
## ----------
## On exit,
##   returns response from server
## WARNING:
##   SERVER MUST MAKE SOME SORT OF RESPONSE OR ELSE THERE WILL BE DEADLOCK
##
## Note: If the prompt is set to "EOF" server will read to end of file

proc tkW3ServerReceive {info} {
  set tkW3Server [lindex $info 0]
  set tkW3ServerPrompt [lindex $info 1]
  set tkW3ServerDebug [lindex $info 2]

  set return_string ""

  # *** THIS FLUSH IS VERY IMPORTANT
  # *** If this isn't put here, you are likely to send the application
  # ***    into deadlock

  flush $tkW3Server

  # Get stuff from the server until the prompt string is detected
  # Then return everything.  WARNING.  This will freeze if the prompt
  # string is never detected

  while {1} {
      if {[gets $tkW3Server string] == -1} {
	  if {$tkW3ServerPrompt == "EOF"} { 
	      break
	  } {
	      tkW3ServerError
	  }
      }
      if {$tkW3ServerPrompt != "EOF" && \
	  [regexp $tkW3ServerPrompt $string]} {
	  break;
      }
      append return_string $string
      append return_string "\n"
  }

  # Print out if debug
  if {$tkW3ServerDebug} {
      puts stdout $return_string
  }

  # Make sure you remove newlines from the end of return string.
  # Otherwise, strange things will happen.
  # 
  # What will happen is this when you try to evaluate
  #   tkW3Goto [tkW3ParseName $page]
  # tkW3ParseName calls this procedure which returns with 
  #   file:filename.html\n\n
  # When tkW3Goto then calls this procedure with that as an arg
  # the second newline will cause two "tkW3:" prompts to appear
  # messing up the sychronoization between the GUI and the backend.

  string trim $return_string
}

## tkW3ServerClose: Close connection with server
## ----------
## Side effects:
##   Closes the pipe to the server, resets the server value

proc tkW3ServerClose {info} {
    set tkW3Server [lindex $info 0]
    if {$tkW3Server != ""} {
	# Tell the server to exit
	catch "puts $tkW3Server \"exit\""
	# Make sure the message gets there
	catch "flush $tkW3Server"
	# Close the pipe
	catch "close $tkW3Server"
    }
}

proc tkW3ServerError {} {
    tkW3OutputError "Server process no longer exists" 1
}

proc tkW3ServerSetPrompt {info prompt} {
    list [lindex $info 0] $prompt [lindex $info 2]
}

proc tkW3ServerSetDebug {info debug} {
    list [lindex $info 0] [lindex $info 1] $debug
}

proc tkW3ServerConnect {machine port} {
    set info [tkW3ServerOpen "telnet $machine $port" "Escape"]
    tkW3ServerSetPrompt $info "EOF"
}
