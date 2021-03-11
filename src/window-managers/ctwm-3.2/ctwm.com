$! put in the right path
$ ctwm:=$disk$users:[joe.bin]ctwm
$ vue$suppress_output_popup
$! insert the right path
$ define ctwm_welcome_file "@/disk$users:[joe.xpm]welcome.xpm"
$ ctwm
