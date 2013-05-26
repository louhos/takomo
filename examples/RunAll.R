# List R files in this directory (except this file)
fs <- list.files(pattern = ".R$")

# see also ../testruns/ and merge here at some point

# Files TO FIX
# See also testing.tmp
fixme.files <-  c("20120820-aihepiirianalyysi.R")
#		  "20120226-kuntajako.R", # toimii OS X:lla -Juuso
#                  ) 

# Files to SKIP from testing
skip.files <- c(
  "20120205-presidentti2012analysis.R", # -> requires personal API key
  "20120115-Presidentti2012.R" # -> requires personal API key
	   )

fs <- setdiff(fs, c("RunAll.R", fixme.files, skip.files))

# Run all R files
# for (f in sample(fs)) { 
for (f in fs) { 
  print(paste("Executing", f))
  source(f) 
  rm(list = setdiff(ls(), "fs"))  
}


