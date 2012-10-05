source("funcs.R")

# Reading CSV example files:

# OK
#k-2012-tlt_aaa_maa.csv
csv <- FileNameElectionData("kunnallisvaalit", 2012, "alustava", "alue", 
                            "äänestysaluetaso", "maa", suffix = "-tlt")
tab <- read.csv(csv, sep = ";")

# OK
#k-2012-tlt_aea_maa.csv
csv <- FileNameElectionData("kunnallisvaalit", 2012, "alustava", "ehdokas", 
                            "äänestysaluetaso", "maa", suffix = "-tlt")
tab <- read.csv(csv, sep = ";")

# Reading fails. TODO: Fix this.
#k-2012-tlt_apa_maa.csv
csv <- FileNameElectionData("kunnallisvaalit", 2012, "alustava", "puolue", 
                            "äänestysaluetaso", "maa", suffix = "-tlt")
#tab <- read.csv(csv, sep = ";")

# ----------------------------------------------------------------

# Reading XML example file:

#k-2012-tlt_aa_maa.xml
xml <- FileNameElectionData(election = "kunnallisvaalit", year = 2012, stage = "alustava", info = "äänestysaluetaso", region = "maa", suffix = "-tlt", file.type = "xml")

# NOTE: rather memory-intensive, skip by default
#library(XML)
#doc <- xmlInternalTreeParse(xml)

