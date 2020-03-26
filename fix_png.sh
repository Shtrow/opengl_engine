#!/bin/bash

#This script will download glMLite lib, compile missing modules, and put it to your opam directory

GLMLITE_URL='https://github.com/fccm/glMLite.git'

if opam --version >/dev/null 2>&1 ; then 
    echo "opam found" 
    echo "version: $(opam --version)"
else
    echo "opam not found"
    exit -1
fi

eval $(opam env);

mkdir __GLMLITE__;
cd __GLMLITE__;

git clone $GLMLITE_URL ;
cd glMLite;
make &&
make png &&
make install_png &&

cp SRC/*png* $OPAM_SWITCH_PREFIX/lib/glMLite/ &&

cd ../../;

rm -rf __GLMLITE__;



