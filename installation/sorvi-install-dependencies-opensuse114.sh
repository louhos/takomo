#!/bin/bash

# sorvi-install-dependencies-opensuse114
#
# Asentaa vaaditut riippuvuudet soRvi-pakettiin
# http://sorvi.r-forge.r-project.org/
# Huom: kokeellinen versio, joitain riippuvuuksia saattaa viela puuttua
#
# Testattu openSUSEn versiolla 11.4 (GNOME 2.32), muiden versioden käyttö
# saattaa vaatia Geo-ohjelmistolähteen URL:in muokkausta
#
# Käyttö:
#	--no-gdal: älä asenna GDAL-kirjastoa riippuvuuksineen
#
# Copyright (C) 2011 Joona Lehtomäki <joona.lehtomaki@gmail.com>
#
# Licence: FreeBSD (keep this notice)

set -e

# Lisää ohjelmistolähde Geo
sudo zypper --no-gpg-checks ar http://download.opensuse.org/repositories/Application:/Geo/openSUSE_11.4/ Geo
sudo zypper --no-gpg-checks refresh

# Kehitystyökalut
sudo zypper --non-interactive in make
sudo zypper --non-interactive in gcc
sudo zypper --non-interactive in gcc-c++
sudo zypper --non-interactive in gcc-fortran
sudo zypper --non-interactive in build
sudo zypper --non-interactive in R-base-devel

# XML
sudo zypper --non-interactive in libxml2-devel

# GEOS
sudo zypper --non-interactive in libgeos-devel

# PROJ.4
sudo zypper --non-interactive in proj
sudo zypper --non-interactive in libproj-devel

# CURL
sudo zypper --non-interactive in libcurl-devel

# GDAL
if [ "$1" != "--no-gdal" ]
then
   sudo zypper --non-interactive in libgdal-devel
fi
