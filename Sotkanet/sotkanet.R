# Forked from http://en.opasnet.org/w/Sandbox#Sotkanet

#library(OpasnetUtilsExt)
library(xtable)

# collect makes a data.frame out of the list object from Sotkanet
# x is the input data
# name is the name for the column
# single is a logical whether there is only a single entry in the x data.

collect <- function(x, name, single = FALSE) {
	out <- data.frame()
	if(single) {out <- data.frame(temp1 = x$id, temp2 = x$title$fi) 
	} else {
		for(i in 1:length(x)) {
			out <- rbind(out, data.frame(temp1 = x[[i]]$id, temp2 = x[[i]]$title$fi))
		}
	}
	colnames(out) <- c(name, paste(name, "Result", sep=""))
	return(out)
}

a <- sotkanet.indicators()

# print(a)

b <- sotkanet.indicators(127)

b <- collect(b, "indicator", TRUE)

# print(xtable(b), type = 'html')

d <- sotkanet.regions()

d <- collect(d, "region")

# print(xtable(d), type = 'html')

e <- sotkanet.data(indicator=127,years=c(2011,2010),genders='female')

e <- merge(b, e)
e <- merge(d, e)

#print(xtable(e),type='html')