# Make sure fontsuffix is defined for backwards compatible behaviour
#FONTSUFFIX=snf
FONTSUFFIX=pcf
FONTMAKER=bdfto${FONTSUFFIX}
DIRBUILDER=mkfontdir

# For Openwindows, you need to set FONTSUFFIX to fb and 
# DIRBUILDER to bldfamily and
# CHANGE each FONTMAKER line to a convertfont line as shown below
#FONTSUFFIX=fb
#DIRBUILDER=bldfamily

all: shownonascii heb6x13.${FONTSUFFIX} heb8x13.${FONTSUFFIX} heb8x13B.${FONTSUFFIX} mailto-hebrew

heb6x13.${FONTSUFFIX}: heb6x13.bdf
#	convertfont -o heb6x13 heb6x13.bdf
	-${FONTMAKER} heb6x13.bdf > heb6x13.${FONTSUFFIX}
	-${DIRBUILDER}

heb8x13.${FONTSUFFIX}: heb8x13.bdf
#	convertfont -o heb8x13 heb8x13.bdf
	-${FONTMAKER} heb8x13.bdf > heb8x13.${FONTSUFFIX}
	-${DIRBUILDER}

heb8x13B.${FONTSUFFIX}: heb8x13B.bdf
#	convertfont -o heb8x13B heb8x13B.bdf
	-${FONTMAKER} heb8x13B.bdf > heb8x13B.${FONTSUFFIX}
	-${DIRBUILDER}

shownonascii: Xshownonascii
	sed -e s@/u/nsb/lib/fonts@`pwd`@ < Xshownonascii > shownonascii
	chmod +x shownonascii

mailto-hebrew: Xmailto-hebrew
	sed -e s@/u/nsb/lib/fonts@`pwd`@ < Xmailto-hebrew > mailto-hebrew
	chmod +x mailto-hebrew

clean:
	rm -f heb6x13.${FONTSUFFIX} heb8x13.${FONTSUFFIX} heb8x13B.${FONTSUFFIX}  shownonascii mailto-hebrew fonts.dir
