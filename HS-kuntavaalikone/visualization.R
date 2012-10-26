piirra_ehdokkaat <- function(scores, groups, group_colors, filter=T, filename=NA, names=NA, urls=NA, ...) {
	# Käytetään ks-pakettia joukkojen arvioimiseen
	require('ks')

	# SVG?
	draw_svg <- !is.na(filename)
	if (draw_svg) {
		require('RSVGTipsDevice')
		devSVGTips(filename, toolTipMode=2, width=9, height=9)		
	}

	# Ehdokkaiden värit
	lev <- levels(groups)
	cols <- group_colors[groups]

	# Lisätään pieni määrä satunnaisuutta, jotta ehdokkaat eivät mene päällekäin
	scores <- scores + runif(length(scores), -0.05, 0.05)

	# Filteröidään myös rivit, joilla ei ole arvoja tai väriä
	filter <- filter & rowSums(is.na(scores)) == 0 & cols!='#FFFFFF00'

	# Ensin: piirretään pohjakuvaaja
	par(mar=c(6,4,4,6)+0.1, xpd=T)
	xlimit <- c(min(scores[,1], na.rm=T), max(scores[,1], na.rm=T))
	ylimit <- c(min(scores[,2], na.rm=T), max(scores[,2], na.rm=T))	
	plot(1, type="n", xlim=xlimit, ylim=ylimit, bty="n", ...)

	if (draw_svg) setSVGShapeURL("http://www.loitto.com/tilastot/hsvaalikone12/")
	mtext("Poliitikkokartat - http://www.loitto.com/tilastot/hsvaalikone12/", side=1, line=5)

	legend(xlimit[2]+0.1, ylimit[2], 
		legend=lev[group_colors!='#FFFFFF00'], 
		col=group_colors[group_colors!='#FFFFFF00'], 
		pch=15, cex=1.2, pt.cex=2, y.intersp=1.25)

	# Sitten: piirretään jokaiselle puolueelle alue
	for (i in 1:length(lev)) {
		# Haetaan filtteriin sopivat vain tämän puolueen ehdokkaat
		show <- filter & groups==lev[i]

		# jos ainakin kolme ehdokasta...
		if (sum(show) >= 3) {
			# piirretään estimaatti 50% alueesta kernel smoothing -tekniikoiden avulla
			H <- Hpi(scores[show,])
			hhat <- kde(scores[show,], H=H)

			if (draw_svg) setSVGShapeToolTip(title=lev[i])

			plot(hhat, cont=c(50), add=T, col=group_colors[i], drawlabels=F, lwd=2)
		}
	}

	# Lopuksi: piirretään lopullinen kuvaaja tooltippeineen
	if (draw_svg) {
		# pisteet yksi kerrallaan
		for (i in 1:nrow(scores)) {
			if (filter[i]) {
				# lisää ehdokkaalle url jos mahdollista
				if (!is.na(urls) && urls[i] != '') {
					setSVGShapeToolTip(desc1=paste(names[i], groups[i]), desc2=urls[i])
					setSVGShapeURL(urls[i])
				} else {
					setSVGShapeToolTip(desc1=paste(names[i], groups[i]))					
				}

				points(scores[i,1], scores[i,2], pch=1, cex=1.2, col=cols[i])
			}
		}
	} else {
		# pisteet nopeasti samalla kertaa
		points(scores[filter,], col=cols[filter], pch=1, ...)
	}

	if (draw_svg) dev.off()
}