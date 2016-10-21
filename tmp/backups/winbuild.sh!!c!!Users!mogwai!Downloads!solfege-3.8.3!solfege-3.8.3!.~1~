#!/bin/sh

# MS Windows have other defaults than linux. The 'sed' I have installed on
# my windows machine don't support the -i option.
mv default.config tmp.cfg
sed -e "s/type=external-midiplayer/type=sequencer-device/" tmp.cfg > default.config
rm tmp.cfg

cp solfege.py solfegedebug.py
./configure PYTHON=/c/Python24/python.exe --disable-pygtk-test --enable-winmidi
make skipmanual=yes PYTHON_INCLUDES=-I/c/Python24/include
make winbuild
