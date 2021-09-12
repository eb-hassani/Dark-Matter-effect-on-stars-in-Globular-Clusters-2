export OMP_NUM_THREADS="8"
export MESASDK_ROOT=~/mesasdk
source $MESASDK_ROOT/bin/mesasdk_init.sh
export MESA_DIR=~/mesa-r15140
export MESA_RUN_OPTIONAL=1

./clean
./mk
./rn


