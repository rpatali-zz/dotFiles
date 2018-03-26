#!/bin/sh

# this script assumes you have homebrew installed.
# if you don't have homebrew installed, use: http://brew.sh/
# This script installs all the dependencies
brew install ruby
brew install python
brew install luajit
# this list is work in progress, i will try this on a new system
# and fill out the rest of the dependencies, some of the more
# obvious ones are listed in the configure step below but i don't
# want to auto install them just yet.
brew install git

git clone git@github.com:vim/vim.git --recursive -b master
cd vim || exit

make clean && make distclean

./configure --prefix=/usr/local \
            --with-features=huge \
            --enable-rubyinterp \
            --enable-pythoninterp \
            --with-python-config-dir=/usr/bin/python-config \
            --enable-perlinterp \
            --enable-gui=gtk2 \
            --enable-cscope \
            --enable-perlinterp=yes \
            --enable-luainterp=yes \
            --with-lua-prefix=/usr/local \
            --with-luajit \
            --enable-fail-if-missing \
            --enable-python3interp \
            --enable-multibyte \
            --enable-fontset

make VIMRUNTIMEDIR=/usr/local/share/vim/vim80/

sudo make install
