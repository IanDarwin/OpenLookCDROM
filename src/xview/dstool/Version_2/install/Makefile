##################################
#                                #
#            dstool              #
#                                #
#                                #
# Center for Applied Mathematics #
#       Cornell University       #
#                                #
#                                #
#            8/20/91             #
##################################

MAKEFILE = Makefile

message:
	@echo "This is the dstool top directory. Please see the dstool Installation Manual."

all:
	@echo "Creating the full dstool distribution..."
	@make dstool
	@make dslibs
	@make dstool_nowin

dstool:
	@echo "Making the dstool executable..."
	@cd src; make dstool

dslibs:
	@echo "Making the dstool libraries..."
	@cd src; make dslibs

dstool_nowin:
	@echo "Making the dstool_nowin executable..."
	@cd src; make dstool_nowin

clean:
	@echo "Making clean..."
	@cd src; make clean
	@echo "done"
