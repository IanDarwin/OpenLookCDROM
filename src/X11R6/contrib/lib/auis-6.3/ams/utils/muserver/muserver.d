\begindata{text, 537473024}
\textdsversion{12}
\template{roff}



\begindata{bp,537529600}
\enddata{bp,537529600}
\view{bpv,537529600,0,0,0}

\indent1{


\majorheading{MUSERVER(8)     (October 30, 1991)    MUSERVER(8)

}


}\indent1{\tempindentneg1{NAME 

}muserver - master update server process 

\tempindentneg1{
SYNOPSIS 

}\bold{/usr/local/etc/muserver }

\tempindentneg1{
DESCRIPTION 

}This server forks to become a "update" process and a "server" process. The 
update process watches the master update files as specified by 
\italic{/etc/AndrewSetup}, and sends them to the server process. The server 
process receives periodic updates from the update process and services 
requests on the server port (currently 6666). 


Master update server requests are a single line with a bboard name (short 
form) followed by a newline. The server will respond with a the bboard name, 
and if the lookup is successful, a directory specification and the 6 character 
timestamp for the message followed by a newline. The directory specification 
is " A" for \italic{OfficialBboardRoot}, " B" for \italic{LocalBboardRoot}, " 
C" for \italic{ExternalBboardRoot}, and " /<path> " for other bboards. 


A line beginning with "$q" indicates the client is finished with requests. 


For best results, all the requests should be sent, then all the responses read 
to minimize waiting for the connection round trip. 


The server requires no special privileges, and may be run as any user. 

\tempindentneg1{
STATUS 

}Log files are written by both the update and server process to help track 
problems. See the FILES section below for filenames. The logs are only written 
when an error occurs. 


To get status information from the server, do "telnet <host> 6666". Once 
connected, type "$i\\n" to get status information as follows: 

$i

lastupdate: Wed Oct 30 11:25:47 1991

num updates: 1

conns active: 1

conns done: 8

last time: 0.039060

avg time: 0.056637

avg requests: 113

max active: 2

$i



Once this command has been sent, the current connection will not be included 
in the statistics. The $q command may be used to cleanly terminate the 
connection. 


To reset the statistics, send a SIGHUP to the server process (the one with the 
highest pid). This will reset the "conns done", the "avg time", and the "avg 
requests" fields. 


To stop the server, kill the update process (the one with the lowest pid). The 
server process will immediately notice, write statistics to the log file, and 
exit. 

\tempindentneg1{
FILES 

}/etc/AndrewSetup        Specifies root directories for various bboards

/tmp/muserver.log       log file for the server process

/tmp/muserver.old       previous log file for the server process

/tmp/mupdate.log        log file for the update process

/tmp/mupdate.old        previous log file for the update process








































































































































\majorheading{Page         (printed 3/18/92)

}
\begindata{bp,537680200}
\enddata{bp,537680200}
\view{bpv,537680200,1,0,0}

}\enddata{text,537473024}
