# List R files in this directory (except this file)
fs <- list.files(pattern = ".R$")

# see also ../testruns/ and merge here at some point

# Files to SKIP from testing
skip.files <- c(
  "20120820-aihepiirianalyysi.R", # -> too complicated to run in a batch
  "20120205-presidentti2012analysis.R", # -> requires personal API key
  "20120115-Presidentti2012.R", # -> requires personal API key
  "20120226-kuntajako.R", # Error in rgdal::writeOGR(uusi.kuntajako, "uudet_kunnat.shp", "uusi.kuntajako",  : Creation of output file failed -> IN UBUNTU / LL
  "20120216-KunnatJaPresidentinvaali.R", # takes ages??
  "20120521-Kuntadata.R"  # takes ages??
	   )


fs <- setdiff(fs, c("RunAll.R", skip.files))

# Run all R files
# for (f in sample(fs)) { 
for (f in fs) { 
  print(paste("Executing", f))
  source(f) 
  rm(list = setdiff(ls(), "fs"))  
}


