:
# List of parameters with their values.
# Set the values here!

# Maximal phase dimension:
NFMAX=50

# Maximal number of active parameters:
NACTMX=4

# Maximal dimension of continuation problem (NFMAX+NACTMAX):
NDIM=54

# Maximal number of parameters:
NPARMX=50

# Maximal number of user's functions:
IUFMAX=50

# Maximal number of test functions:
IFMAX=20

# Maximal number of point types:
NPTYP=4

# Maximal number of values associated with point:
NVALPT=10

# Maximal number of bifurcation values:
NBFVL=5

# Maximal number of processed points for one step (buffer size):
NBUF=10

# Maximal number of numerical parameters:
NRPC=40

NDIM1=NDIM-1
NFMAX2=NFMAX*NFMAX
NPTYP1=NPTYP+1
NRVC=3*NFMAX+NPARMX+IUFMAX+1


# Directory where the subdirectories can be found
ROOT_DIR=/home/gucken/dstoolv2/contrib/locbif/loclbf

# Create .dim files with parameters in every subdirectory
# Subdirectory: algebrbf
SUB_DIR=algebrbf
DIM_FILE=$SUB_DIR.dim
(
echo -n "      parameter ("
echo -n "NFMAX=$NFMAX"
echo -n ")"
echo
) > $ROOT_DIR/$SUB_DIR/$DIM_FILE

# Subdirectory: beetlebf
SUB_DIR=beetlebf
DIM_FILE=$SUB_DIR.dim
(
echo -n "      parameter ("
echo -n "NDIM=$NDIM, IFMAX=$IFMAX, NPTYP=$NPTYP"
echo -n ")"
echo
echo -n "      parameter ("
echo -n "NDIM1=$NDIM1"
echo -n ")"
echo
) > $ROOT_DIR/$SUB_DIR/$DIM_FILE

# Subdirectory: complbf
SUB_DIR=complbf
DIM_FILE=$SUB_DIR.dim
(
echo -n "      parameter ("
echo -n "NFMAX=$NFMAX, NPARMX=$NPARMX, NACTMX=$NACTMX, "
echo -n "IUFMAX=$IUFMAX, NBFVL=$NBFVL"
echo -n ")"
echo
echo -n "      parameter ("
echo -n "NRPC=$NRPC, NRVC=$NRVC"
echo -n ")"
echo
) > $ROOT_DIR/$SUB_DIR/$DIM_FILE

# Subdirectory: epbf
SUB_DIR=epbf
DIM_FILE=$SUB_DIR.dim
(
echo -n "      parameter ("
echo -n "NFMAX=$NFMAX, NDIM=$NDIM, IFMAX=$IFMAX, NVALPT=$NVALPT, NPARMX=$NPARMX"
echo -n ")"
echo
echo -n "      parameter ("
echo -n "NPTYP=$NPTYP, NBFVL=$NBFVL"
echo -n ")"
echo
echo -n "      parameter ("
echo -n "NFMAX2=$NFMAX2, NPTYP1=$NPTYP1"
echo -n ")"
echo
) > $ROOT_DIR/$SUB_DIR/$DIM_FILE

# Subdirectory: epinf
SUB_DIR=epinf
DIM_FILE=$SUB_DIR.dim
(
echo -n "      parameter ("
echo -n "NFMAX=$NFMAX, NPARMX=$NPARMX, IUFMAX=$IUFMAX"
echo -n ")"
echo
echo -n "      parameter ("
echo -n "NACTMX=$NACTMX, NBFVL=$NBFVL"
echo -n ")"
echo
echo -n "      parameter ("
echo -n "NRVC=$NRVC"
echo -n ")"
echo
) > $ROOT_DIR/$SUB_DIR/$DIM_FILE

# Subdirectory: fpbf
SUB_DIR=fpbf
DIM_FILE=$SUB_DIR.dim
(
echo -n "      parameter ("
echo -n "NFMAX=$NFMAX, NDIM=$NDIM, IFMAX=$IFMAX, NPARMX=$NPARMX"
echo -n ")"
echo
echo -n "      parameter ("
echo -n "NFMAX2=$NFMAX2"
echo -n ")"
echo
) > $ROOT_DIR/$SUB_DIR/$DIM_FILE

# Subdirectory: fpinf
SUB_DIR=fpinf
DIM_FILE=$SUB_DIR.dim
(
echo -n "      parameter ("
echo -n "NFMAX=$NFMAX, NPARMX=$NPARMX, IUFMAX=$IUFMAX"
echo -n ")"
echo
echo -n "      parameter ("
echo -n "NACTMX=$NACTMX, NBFVL=$NBFVL"
echo -n ")"
echo
echo -n "      parameter ("
echo -n "NRVC=$NRVC"
echo -n ")"
echo
) > $ROOT_DIR/$SUB_DIR/$DIM_FILE

# Subdirectory: integrbf
SUB_DIR=integrbf
DIM_FILE=$SUB_DIR.dim
(
echo -n "      parameter ("
echo -n "NFMAX=$NFMAX"
echo -n ")"
echo
) > $ROOT_DIR/$SUB_DIR/$DIM_FILE

# Subdirectory: lcbf
SUB_DIR=lcbf
DIM_FILE=$SUB_DIR.dim
(
echo -n "      parameter ("
echo -n "NFMAX=$NFMAX, NDIM=$NDIM, IFMAX=$IFMAX, NPARMX=$NPARMX"
echo -n ")"
echo
echo -n "      parameter ("
echo -n "NFMAX2=$NFMAX2"
echo -n ")"
echo
) > $ROOT_DIR/$SUB_DIR/$DIM_FILE

# Subdirectory: lcinf
SUB_DIR=lcinf
DIM_FILE=$SUB_DIR.dim
(
echo -n "      parameter ("
echo -n "NFMAX=$NFMAX, NPARMX=$NPARMX, IUFMAX=$IUFMAX"
echo -n ")"
echo
echo -n "      parameter ("
echo -n "NACTMX=$NACTMX, NBFVL=$NBFVL"
echo -n ")"
echo
echo -n "      parameter ("
echo -n "NRVC=$NRVC"
echo -n ")"
echo
) > $ROOT_DIR/$SUB_DIR/$DIM_FILE

# Subdirectory: linbf
SUB_DIR=linbf
DIM_FILE=$SUB_DIR.dim
(
echo -n "      parameter ("
echo -n "NFMAX=$NFMAX, NDIM=$NDIM, IFMAX=$IFMAX, NVALPT=$NVALPT"
echo -n ")"
echo
echo -n "      parameter ("
echo -n "NPARMX=$NPARMX, IUFMAX=$IUFMAX, NPTYP=$NPTYP, NBUF=$NBUF"
echo -n ")"
echo
echo -n "      parameter ("
echo -n "NRPC=$NRPC, NRVC=$NRVC"
echo -n ")"
echo
echo -n "      parameter ("
echo -n "NFMAX2=$NFMAX2, NPTYP1=$NPTYP1"
echo -n ")"
echo
) > $ROOT_DIR/$SUB_DIR/$DIM_FILE

# Subdirectory: linfpbf
SUB_DIR=linfpbf
DIM_FILE=$SUB_DIR.dim
(
echo -n "      parameter ("
echo -n "NFMAX=$NFMAX, NDIM=$NDIM, NVALPT=$NVALPT, NPARMX=$NPARMX"
echo -n ")"
echo
echo -n "      parameter ("
echo -n "NPTYP=$NPTYP, NBFVL=$NBFVL"
echo -n ")"
echo
echo -n "      parameter ("
echo -n "NFMAX2=$NFMAX2"
echo -n ")"
echo
) > $ROOT_DIR/$SUB_DIR/$DIM_FILE

# Subdirectory: psbf
SUB_DIR=psbf
DIM_FILE=$SUB_DIR.dim
(
echo -n "      parameter ("
echo -n "NFMAX=$NFMAX, NDIM=$NDIM, IFMAX=$IFMAX, NPARMX=$NPARMX"
echo -n ")"
echo
echo -n "      parameter ("
echo -n "NFMAX2=$NFMAX2"
echo -n ")"
echo
) > $ROOT_DIR/$SUB_DIR/$DIM_FILE

# Subdirectory: psinf
SUB_DIR=psinf
DIM_FILE=$SUB_DIR.dim
(
echo -n "      parameter ("
echo -n "NFMAX=$NFMAX, NPARMX=$NPARMX, IUFMAX=$IUFMAX"
echo -n ")"
echo
echo -n "      parameter ("
echo -n "NACTMX=$NACTMX, NBFVL=$NBFVL"
echo -n ")"
echo
echo -n "      parameter ("
echo -n "NRVC=$NRVC"
echo -n ")"
echo
) > $ROOT_DIR/$SUB_DIR/$DIM_FILE


