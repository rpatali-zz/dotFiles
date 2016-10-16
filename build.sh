#!/bin/sh

make clean && make distclean

./configure --prefix=/usr/local \
            --with-features=huge \
            --enable-rubyinterp \
            --enable-pythoninterp \
            --with-python-config-dir=/usr/local/bin/python-2.7-config \
            --enable-perlinterp \
            --enable-gui=gtk2 \
            --enable-cscope \
            --enable-perlinterp=yes \
            --enable-luainterp=yes \
            --with-lua-prefix=/usr/local \
            --with-luajit \
            --enable-mouseshape \
            --enable-fail-if-missing

make VIMRUNTIMEDIR=/usr/local/share/vim/vim80/

sudo make install
