# List R files in this directory (except this file)
fs <- list.files(pattern = ".R$")

# Files TO FIX
fixme.files <-  c(
		  "20120206_Niinisto_vs_Haavisto.R", 
		  "20120820-aihepiirianalyysi.R", 

		  "20120115-Presidentti2012.R",
		  "20120205-presidentti2012analysis.R",
		  "20120226-kuntajako.R", 

		  "20110117-HRI.R") 



fs <- setdiff(fs, c("RunAll.R", fixme.files))

# Run all R files
for (f in sample(fs)) { 
  print(paste("Executing", f))
  source(f) 
  rm(list = setdiff(ls(), "fs"))
  
}


