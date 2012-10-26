# Koodi pohjautuu Juha Törmäsen tekemään vaalikoneanalyysiin
# http://www.loitto.com/tilastot/hsvaalikone12/
#
# Koodi lukee ehdokkaiden esikäsitellyn arvotaulukon,  
# tekee faktorianalyysin ja visualisoi tulokset kuntakohtaisesti
#
# Arvokartan tietojen lähde on Helsingin Sanomien vaalikone: www.vaalikone.fi
#
# Koodia on edelleen muokannut Leo Lahti (http://louhos.github.com/contact.html)
#
# Lisenssi: CC-BY-NC 3.0


# ------------------------------------------------------------------

# Load material
source("init.R")

# ----------------------------------------------------------------

# Factor analysis
f <- fa(val, nfactors=2, fm="pa", rotate="varimax")

# Piirretään joka kaupunki svg-kuvaksi
kaupunki <- levels(ehdokas[,1])
for (k in kaupunki) {
  print(paste("Piirretään", k))
  filename <- paste(k, '.svg', sep='')

  # Visualize
  piirra_ehdokkaat(scores=f$scores, 
		groups=ehdokas[,2], group_colors=p.col, 
		names=nimet, urls=ehdokas[,6], 
		xlab="Vasemmisto - Oikeisto", 
		ylab="Liberaali - Konservatiivi", 
		filter=(ehdokas[,1]==k), 
		main=k, 
		filename=filename)
}

