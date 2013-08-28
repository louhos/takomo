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
  "20120521-Kuntadata.R",  # takes ages??
  "20111005-HSOpen3_lammittely.R", #Error in download.file(url, destfile, mode = "wb", quiet = TRUE) : cannot open URL 'http://maps.google.com/maps/api/staticmap?center=60.2,24.93&zoom=11&size=640x640&maptype=Map&format=png32&sensor=true' In addition: Warning message: In download.file(url, destfile, mode = "wb", quiet = TRUE) : cannot open: HTTP status was '403 Forbidden'
  "20111010-HSOpen3_raportti.R", # Error in download.file(url, destfile, mode = "wb", quiet = TRUE) :   cannot open URL 'http://maps.google.com/maps/api/staticmap?center=60.2,24.93&zoom=11&size=640x640&maptype=Map&format=png32&sensor=true'
  "20120629-MMLcoast.R", # Never completes..
  "20120911-MOT.R", # Never completes..
  "20111023-oikotie.R", # Oikotie deprecated from sorvi
  "20120920-FinlandCO2.R" # Never completes..
	   )


fs <- setdiff(fs, c("RunAll.R", skip.files))

# Run all R files
# for (f in sample(fs)) { 
for (f in fs) { 
  print(paste("Executing", f))
  source(f) 
  rm(list = setdiff(ls(), "fs"))  
}


