#!/bin/sh

export OPENLANE_IMAGE_NAME=efabless/openlane:2022.07.02_01.38.08
export OPENLANE_ROOT=$HOME/r/openlane
export PDK_ROOT=$HOME/r/pdk
export PDK=sky130B

docker run --rm \
  -v $OPENLANE_ROOT:/openlane \
  -v $PDK_ROOT:$PDK_ROOT \
  -v $(pwd):/work \
  -e PDK_ROOT=$PDK_ROOT \
  -u $(id -u $USER):$(id -g $USER) \
  $OPENLANE_IMAGE_NAME \
  /bin/bash -c "./flow.tcl -verbose 2 -overwrite -design /work/src -run_path /work/runs -tag wokwi"
