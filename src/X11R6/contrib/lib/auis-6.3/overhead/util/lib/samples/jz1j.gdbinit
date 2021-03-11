handle 28 noprint nostop
handle 4 noprint nostop
handle 12 noprint nostop

set screensize 0
set editing off

define name
print ((struct basicobject *) $)->methods->info.name
end
define doggies
echo ClassListEntries - 1 is 
print ClassListEntries -1
echo That element of the ClassList table is 
print ClassList[$]
echo Setting the relocation address...\n
print $.baseaddr
set-rel $
echo done.\nNow use 'add-file <filename.dog>' to add the symbol table. \n
end
define lastdog
print ClassList[ClassListEntries-1].className
print *((struct classload *)&$)
print  &$
end
define prevdog
print ((struct classload *)$ - 1).className
print *((struct classload *)&$)
print  &$
end
define nextdog
print ((struct classload *)$ + 1).className
print *((struct classload *)&$)
print  &$
end
define setdog
print ((struct classload *)$).baseaddr
set-rel $
echo done.\nNow use 'add-file <filename.dog>' to add the symbol table. \n
end

define msi
stepi
x/i $iar
end

define mni
stepi
x/i $iar
end

define regs
info registers
end

break class_EnterInfo
cond 1 base
comm 1
printf "add-file %s.dog 0x%lx\n", namekey, base
end
