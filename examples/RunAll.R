# List R files in this directory (except this file)
fs <- list.files(pattern = ".R$")
fs <- setdiff(fs, c("RunAll.R",
                    "20111005-HSOpen3_lammittely.R", 
		    "20111010-HSOpen3_raportti.R",
		    "20111023-oikotie.R"))

# Run all R files
for (f in fs) { 
  print(paste("Executing", f))
  source(f) 
  rm(list = setdiff(ls(), "fs"))
}


