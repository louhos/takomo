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
