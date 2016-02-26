#
# 12/20/91 - skt
#
images: ../images/color/s_winter ../images/bandw/s_winter

../images/color/s_winter: ../images/color Img-uncompact Images.1
	cd ..; image-stuff/Img-uncompact < image-stuff/Images.1

../images/bandw/s_winter: ../images/bandw Img-uncompact Images.2
	cd ..; image-stuff/Img-uncompact < image-stuff/Images.2

Img-uncompact: Img-uncompact.c
	cc -o $@ $@.c

images.swap: ../images/swap ../images/swap/s_winter

../images/swap/s_winter: ../images/color/s_winter
	cd ..; image-stuff/Img-mk-swap

../images/color: ../images
	mkdir $@

../images/bandw: ../images
	mkdir $@

../images/swap: ../images
	mkdir $@

../images:
	mkdir $@
