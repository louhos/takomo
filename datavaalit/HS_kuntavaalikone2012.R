# Script for reading HS Kuntavaalikone 2012 data

# Download zip and read the csv file
temp <- tempfile()
download.file("http://files.snstatic.fi/hs/2012/10/HSkuntavaalikone-3-10-2012.zip", temp)
raw.dat <- read.csv(unz(temp, "HSkuntavaalikone-3-10-2012.csv"), sep=";", header=TRUE)
# raw.dat2 <- read.xls(unz(temp, "HSkuntavaalikone-3-10-2012.xls"), encoding="ISO-8859-15")
unlink(temp)

# Fix character encoding
dat <- as.data.frame(apply(raw.dat, 2, iconv, from="ISO-8859-15", to="UTF-8"))

# Remove all free-form arguments
dat <- dat[-grep("Perustelu", names(dat))]

# Assign NA to empty answers
dat[dat==""] <- NA

# Save
save(raw.dat, dat, file="HS_kuntavaalikone_temp_20121007.RData")

# Take Espoo
espoo <- droplevels(subset(dat, Kaupunki=="Espoo"))

# # Get choices for Espoo - No help - does not extract the questions
# library(XML)
# temp <- readHTMLTable("http://www.vaalikone.fi/kunta2012/espoo/")
