sotkanet.indicators <- SotkanetIndicators(type = "table")
as.character(sotkanet.indicators[sotkanet.indicators$indicator == indicator, "indicator.title.fi"])

# Sort indicators by median time correlation
s <- names(sort(sapply(corlist, function (x) {median(na.omit(x))})))
#> unname(sapply(s[1:10], function (nam) {as.character(unique(dats[[nam]]$indicator.title.fi))}))

# Pick some indicators for closer inspection
selected.indicators <- c("Väestö, keskiväkiluku", 
			 "Yksityisten lääkäripalvelujen kustannukset, 1 000 euroa")
		         #"Korkea-asteen koulutuksen saaneet, % 15 vuotta täyttäneistä", 
			 #"16-24 -vuotiaat, % väestöstä",
 			 #"Korkea-asteen koulutuksen saaneet, % 15 vuotta täyttäneistä",      
 			 #"Muu kuin suomi, ruotsi tai saame äidinkielenä / 1000 asukasta")
			 #"Alkoholijuomien myynti asukasta kohti 100 %:n alkoholina, litraa")

# For each indicator,
# Correlate indicators with time in each municipality
corlist <- list()
for (i in names(dats)) {
  dat <- dats[[i]]; 
  dat <- dat[order(dat$year), ]; 
  dat <- dat[!duplicated(dat),]; 
  spl <- split(1:nrow(dat), dat$region.title.fi); 
  cors <- sapply(spl, function(inds) {cor(dat$year[inds], dat$primary.value[inds])})
  corlist[[i]] <- cors
}


#sotkanet.indicators <- sotkanet.indicators[grep("oppilaista", sotkanet.indicators[, 2]),]
remove <- grep("EU", sotkanet.indicators$indicator.title.fi)
remove <- c(remove, grep("Pohjoismaat", sotkanet.indicators$indicator.title.fi))
remove <- c(remove, grep("ikävakioimaton", sotkanet.indicators$indicator.title.fi))
remove <- c(remove, grep("Vammojen ja myrkytysten", sotkanet.indicators$indicator.title.fi))
sotkanet.indicators <- sotkanet.indicators[-remove,]
#idx <- 1:78
#idx <- 1:42
#idx <- 1:6
#idx <- c(idx, grep("15-24", sotkanet.indicators[,2]))
idx <- c(idx, grep("opiskelijoista", sotkanet.indicators[,2]))

inds <- grep("keskiv", sotkanet.df$indicator.title.fi)
inds <- c(inds, grep("opiskelij", sotkanet.df$indicator.title.fi))
sotkanet.df <- sotkanet.df[inds,]


#municipality.info <- GetMunicipalityInfo()
#kunta.maakunta <- FindProvince(as.character(sotkanet.df$region.title.fi), municipality.info)
#regs <- cbind(, Maakunta = maakunnat)
