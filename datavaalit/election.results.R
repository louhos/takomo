##################################################################

# Copyright (C) 2012 Louhos <louhos.github.com>. 
# All rights reserved.
# Authors: Joona Lehtomäki and Leo Lahti

# This program is open source software; you can redistribute it and/or modify 
# it under the terms of the FreeBSD License (keep this notice): 
# http://en.wikipedia.org/wiki/BSD_licenses

# This program is distributed in the hope that it will be useful, 
# but WITHOUT ANY WARRANTY; without even the implied warranty of 
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

##################################################################

# Preliminary scripts to fetch Finnish election data from 
# Ministry of Justice web server.
# Based on sorvi 0.1.89 in develop branch

library(sorvi) 

# Get candidate data for election district 3 (Varsinais-Suomen vaalipiiri)
# candidates.3 <- ReadElectionData("candidates", district.id = 3) 
# candidates.3 <- ReadElectionData("candidates", district.id = "Varsinais-Suomen vaalipiiri") 

# Get party data for election district 3 (Varsinais-Suomen vaalipiiri)
# parties.3 <- ReadElectionData("parties", district.id = 3) 
# parties.3 <- ReadElectionData("parties", district.id = "Varsinais-Suomen vaalipiiri") 

# Get all party data across the whole country
load("~/Rpackages/louhos/data.sorvi/maanmittauslaitos/MML.rda")
parties.all <- ReadAllParties()

# Get all candidate data across the whole country
candidates.all <- ReadAllCandidates()

# -------------------------------------------------------------------------

# Dump into a csv file
write.table(candidates.all, "MoJ_candidates_finland.csv", sep=";", quote=FALSE,
            fileEncoding="iso-8859-1", row.names = FALSE)

write.table(parties.all, "MoJ_parties_finland.csv", sep=";", quote=FALSE,
            fileEncoding="iso-8859-1", row.names = FALSE)

# --------------------------------------------------------------------------

# TODO:
# 1. Add option to give district.id as text (real name)
# 2. Add preprocessing function/s that get real names for IDs where applicable
# 3. Add wrapper to produce a table where the ID fields are provided as text (real names)
# 4. Perhaps combine common_data2.json and common_data.json into one file

# --------------------------------------------------------

# Election district files:
# Helsingin vaalipiiri = ehd_01.csv
# Uudenmaan vaalipiiri = ehd_02.csv
# Varsinais-Suomen vaalipiiri = ehd_03.csv
# Satakunnan vaalipiiri= ehd_04.csv
# Hämeen vaalipiiri = ehd_06.csv
# Pirkanmaan vaalipiiri = ehd_07.csv
# Kymen vaalipiiri = ehd_08.csv
# Etelä-Savon vaalipiiri = ehd_09.csv
# Pohjois-Savon vaalipiiri = ehd_10.csv
# Pohjois-Karjalan vaalipiiri = ehd_11.csv
# Vaasan vaalipiiri = ehd_12.csv
# Keski-Suomen vaalipiiri = ehd_13.csv
# Oulun vaalipiiri = ehd_14.csv
# Lapin vaalipiiri = ehd_15.csv

