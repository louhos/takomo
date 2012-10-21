# List R files in this directory (except this file)
fs <- list.files(pattern = ".R$")

# Files TO FIX
fixme.files <-  c("20111005-HSOpen3_lammittely.R", 
		    "20111010-HSOpen3_raportti.R",
		    "20111023-oikotie.R", 
		    "20120115-Presidentti2012.R",
		    "20120205-presidentti2012analysis.R",
		    "20110117-HRI.R", "20110913-apurahat.R", "20120212-KunnatJaPresidentinvaali.R", "20111127-OIVAwms.R", "20120629-MMLcoast.R", "helloworld.R", "20120226-kuntajako.R", "20120206_Niinisto_vs_Haavisto.R", "20120820-aihepiirianalyysi.R", "20111019-PCAxis.R", "20120207-Presidentti2012_MotionCharts.R")

fs <- setdiff(fs, c("RunAll.R", fixme.files))

# Run all R files
for (f in sample(fs)) { 
  print(paste("Executing", f))
  source(f) 
  rm(list = setdiff(ls(), "fs"))
  
}


