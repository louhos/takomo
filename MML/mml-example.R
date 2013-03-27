# helper functions

readASC = function(filename) {
	dat = read.table(filename,sep=" ",skip=6)
	mdat = data.matrix(dat)
	tmdat = t(mdat)[,nrow(mdat):1]
	return(tmdat)
}

readXYZ = function(filename) {
	library(Matrix)

	dat = read.table(filename)
	xp = (dat$V1-min(dat$V1))/10
	yp = (dat$V2-min(dat$V2))/10

	mat = data.matrix(spMatrix(max(xp)+1,max(yp)+1,xp+1,yp+1,dat$V3))
	return(mat)
}

normalizeValues = function(mat) {
	mat = mat-min(mat)
	mat = mat/max(mat)
	mat = (mat*255)
	return(mat)	
}

plotSurface = function(mat, colorlut) {
	library(rgl)
	dl = dim(mat)
	nmat = normalizeValues(mat)
	col = colorlut[nmat+1]
	surface3d(0:(dl[1]-1),0:(dl[2]-1),mat,color=col)
}

natural.colors = function() {
	colors = colorRampPalette(c("cornflowerblue","darkolivegreen4","chartreuse4","chocolate4"))
	colorlut = colors(100)[c(1,seq(0,25,length.out=7),seq(25,50,length.out=80),seq(50,75,length.out=100),seq(75,100,length.out=255))] 
	return(colorlut)
}

# SET YOUR WORKING DIR

#setwd("~/Desktop/takomo/MML")
setwd(".")

# KAUPPATORI - 2M MAP DATA

mat = readASC("data/L4133C.asc")
# contour(mat)
tsel = normalizeValues(mat[0:600,2400:3000])
plotSurface(tsel/4,natural.colors())

# KAUPPATORI - 10M MAP DATA

mat = readXYZ("data/L4133C.xyz")
tsel = normalizeValues(mat[0:120,480:600])
plotSurface(tsel/16,natural.colors())
