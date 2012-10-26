data.dir <- "/home/jlehtoma/Data/Datavaalit2012/vrv"

pre.budget <- read.table(file.path(data.dir, "121009/E_EI_KV2012.csv"), sep=";", 
                         quote="'", header=FALSE)