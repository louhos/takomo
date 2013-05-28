# List R files in this directory (except this file)
fs <- list.files(pattern = ".R$")

# see also ../testruns/ and merge here at some point

# Files TO FIX
# See also testing.tmp
fixme.files <-  c("20120820-aihepiirianalyysi.R",
                  "20111206-HSY.R", "20120226-kuntajako.R") # Error in UseMethod("depth") : no applicable method for 'depth' applied to an object of class "NULL"


# Files to SKIP from testing
skip.files <- c(
  "20120205-presidentti2012analysis.R", # -> requires personal API key
  "20120115-Presidentti2012.R", # -> requires personal API key
  "20120207-Presidentti2012_MotionCharts.R", # Takes ages?
  "20120521-Kuntadata.R", # Takes ages?
  "20111005-HSOpen3_lammittely.R", # Error in if (MyMap$url == "OSM") { : argument is of length zero
  "20111010-HSOpen3_raportti.R", # Error in if (MyMap$url == "OSM") { : argument is of length zero
  "20111023-oikotie.R", # Error in if (MyMap$url == "OSM") { : argument is of length zero
  "20111019-PCAxis.R", # Error in if (MyMap$url == "OSM") { : argument is of length zero
  "20111117-muuttoliike.R", # cannot open URL 'http://maps.google.com/maps/api/staticmap?center=60.2,24.93&zoom=10&size=640x640&maptype=Map&format=png32&sensor=true'
  "20111009-kuntien-sukupuolijakauma.R", # cannot open URL 'http://maps.google.com/maps/api/staticmap?center=60.2,24.93&zoom=10&size=640x640&maptype=Map&format=png32&sensor=true'
  "20120206_Niinisto_vs_Haavisto.R", # cannot open URL 'http://maps.google.com/maps/api/staticmap?center=60.2,24.93&zoom=10&size=640x640&maptype=Map&format=png32&sensor=true'
  "20120629-MMLcoast.R", # Never completes
  "20120911-MOT.R", # Never completes
  "20120920-FinlandCO2.R", # Never completes
  "helloworld.R" # Never completes
	   )



fs <- setdiff(fs, c("RunAll.R", fixme.files, skip.files))

# Run all R files
# for (f in sample(fs)) { 
for (f in fs) { 
  print(paste("Executing", f))
  source(f) 
  rm(list = setdiff(ls(), "fs"))  
}


