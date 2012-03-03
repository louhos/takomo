#!/bin/bash
#
# sorvi-install-dependencies-fedora
#
# Asentaa vaaditut riippuvuudet soRvi-pakettiin
# http://sorvi.r-forge.r-project.org/
# Huom: kokeellinen versio, joitain riippuvuuksia saattaa viela puuttua
#
# Testattu Fedoran versiolla 15 (GNOME 3.0)
#
# Käyttö:
#	--no-gdal: älä asenna GDAL-kirjastoa riippuvuuksineen
#
# Copyright (C) 2011 Joona Lehtomäki <joona.lehtomaki@gmail.com>
#
# Licence: FreeBSD (keep this notice)

set -e

# Kehitystyökalut
sudo yum install -y R-devel

# XML
sudo yum install -y libxml2-devel

# GEOS
sudo yum install -y geos-devel

# PROJ.4
sudo yum install -y proj-devel
sudo yum install -y proj-epsg

# CURL
sudo yum install -y libcurl-devel

# LIBJPEG
sudo yum install -y libjpeg-turbo-devel

# LIBPNG
sudo yum install -y libpng-devel

# GDAL
if [ "$1" != "--no-gdal" ]
then
   sudo yum -y install gdal-devel
fi