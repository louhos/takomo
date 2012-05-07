system("cp -r StatFin StatFin.filtered")

for (di in dir("StatFin.filtered")) {
  d2 <- dir(paste("StatFin.filtered/", di, sep = ""))
  for (di2 in d2) {
    fs <- dir(paste("StatFin.filtered/", di, "/", di2, sep = ""))

    system(paste("rm", paste(paste(paste("StatFin.filtered/", di, "/", di2, "/", sep = ""), setdiff(fs, fs[grep(".csv.gz", fs)]), sep = ""), collapse = " ")))
  }
}


file.list <- c()
for (di in dir("StatFin.filtered")) {
  d2 <- dir(paste("StatFin.filtered/", di, sep = ""))
  for (di2 in d2) {
    fs <- dir(paste("StatFin.filtered/", di, "/", di2, sep = ""))

    fs <- paste(paste("StatFin.filtered/", di, "/", di2, "/", sep = ""), setdiff(fs, fs[grep(".csv.gz", fs)]), sep = "")

    file.list <- c(file.list, fs)

  }
}

