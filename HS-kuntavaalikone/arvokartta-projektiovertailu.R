# Tama tiedosto pohjautuu Juha Törmäsen tekemään vaalikoneanalyysiin
# http://www.loitto.com/tilastot/hsvaalikone12/
#
# Arvokartan tietojen lähde on Helsingin Sanomien vaalikone: www.vaalikone.fi
#
# Koodia on edelleen muokannut Leo Lahti (http://louhos.github.com/contact.html)
#
# Lisenssi: CC-BY-NC 3.0

# ---------------------------------------------------------------------

source("init.R")

# ---------------------------------------------------------------------

# Projisoi data kaksiulotteiseen koordinaatistoon eri metodeilla

methods <- list()

# Faktorianalyysi
methods[["fa"]] <- fa(val, nfactors=2, fm="pa", rotate="varimax")$scores

# PCA
methods[["pca"]] <- princomp(val)$scores[, 1:2]

# PFA
library(dmt)
s <- pfa(t(val), zDimension = 2)
methods[["pfa"]] <- t(getZ(s))

# PPCA
s <- ppca(t(val), zDimension = 2)
methods[["ppca"]] <- t(getZ(s))

# MDS.nonmetric
d <- as.dist(1-cor(t(val)))
fit <- isoMDS(d, k=2)
tab <- data.frame(list(Comp.1 = fit$points[,1], Comp.2 = fit$points[,2]))    
methods[["MDS.nonmetric"]] <- tab

# MDS.classical
fit <- cmdscale(d, eig=TRUE, k=2) # classical MDS
tab <- data.frame(list(Comp.1 = fit$points[,1], Comp.2 = fit$points[,2]))    
methods[["MDS.classical"]] <- tab

# Sammon
# Tuning magic parameter could still improve. 
# Try for instance magic = 0.05.
library(MASS)
fit <- sammon(d, k = 2) 
tab <- data.frame(list(Comp.1 = fit$points[,1], Comp.2 = fit$points[,2]))
rownames(tab) <- rownames(val)
methods[["Sammon"]] <- tab

# IsoMAP
library(vegan)
tr <- spantree(d)
pl <- ordiplot(cmdscale(d), main="cmdscale")
#lines(tr, pl, col="red")
ord <- isomap(d, k=2)
pl <- plot(ord, main="isomap")
lines(tr, pl, col="red")

# SOM
library(kohonen)
tab <- som(scale(val), grid = somgrid(10, 10, "hexagonal"))
# methods[["SOM"]] <- tab

# ----------------------------------------------------------------------

# Visualisoi eri dimensionpudotusmetodeilla annetussa kaupungissa

kaupungit <- levels(ehdokas[,1])
k <- "Siuntio"
plist <- list()

library(ggplot2)
theme_set(theme_bw(10))

for (method in names(methods)) {

  filename <- paste(k, "-", method, '.svg', sep='')

  proj <- as.data.frame(methods[[method]])
  names(proj) <- c("Comp.1", "Comp.2")  
  proj$Puolue <- ehdokas[,2]

  p <- ggplot(data = proj, aes(x = Comp.1, y = Comp.2, col = Puolue)) + geom_point() + ggtitle(paste(k, "/", method))

  plist[[method]] <- p

}

library(gridExtra)
grid.arrange(plist[[1]], plist[[2]], 
	     plist[[3]], plist[[4]], 
	     plist[[5]], plist[[6]], 
	     plist[[7]], 
			 nrow = 3)
