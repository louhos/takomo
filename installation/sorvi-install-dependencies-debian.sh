#!/bin/bash

# Last update 10.11.2012
#
# sorvi-install-dependencies-debian
#
# Asentaa vaaditut riippuvuudet soRvi-pakettiin
# http://louhos.github.com/sorvi/asennus.html
# Huom: kokeellinen versio, joitain riippuvuuksia saattaa viela puuttua
# Täydennysehdotukset tervetulleita.
#
# Copyright (C) 2011-2012 Leo Lahti <leo.lahti@iki.fi>
#
# Licence: FreeBSD (keep this notice)


set -e

# XML
sudo apt-get -y install libxml2-dev

# GEOS
sudo apt-get -y install libgeos-dev

# PROJ.4
#sudo apt-get -y install proj
cd ~/bin/
wget http://download.osgeo.org/proj/proj-4.8.0.tar.gz
tar -zxvf proj-4.8.0.tar.gz
cd proj-4.8.0
./configure
make

# CURL
sudo apt-get -y install libcurl3 libcurl4-openssl-dev

# GDAL
sudo apt-get -y install libgdal1-dev libproj-dev

# GLUT / OpenGL
sudo apt-get -y install freeglut3 freeglut3-dev

# Graphviz
sudo apt-get install graphviz
