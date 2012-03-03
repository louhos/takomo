# Script for loading and analyzing Finnish Presidential election data from year 2012
# Copyright (C) 2011 Juuso Parkkinen <juuso.parkkinen@gmail.com>. All rights reserved.

# This program is free software; you can redistribute it and/or modify
# it under the terms of the FreeBSD License:
# http://en.wikipedia.org/wiki/BSD_licenses

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

# This script was implemented with soRvi version 0.1.44
library(sorvi)

###################################
## USING THE PRESIDENTTI2012 API ##
###################################

# Ask for your own API key, instructions here:
# http://blogit.hs.fi/hsnext/helsingin-sanomat-julkaisee-vaalikoneen-tiedot-avoimena-rajapintana
#API <- "xxxxxxxx"
source("api.R")
# Example queries for different data categories ("questions", "candidates", "useranswers")

# Get all questions
questions <- GetPresidentti2012(category="questions", API=API)
# Get one question with given id
question.57 <- GetPresidentti2012(category="questions", API=API, ID=57)

# Get all candidates
candidates <- GetPresidentti2012(category="candidates", API=API)
# Get only given candidate
candidate.1 <- GetPresidentti2012(category="candidates", API=API, ID=1)

# Get useranswers
# Parameter filter required ("question", "timerange", "topcandidate")
# Using filter 'question' needs question and choice id's'
# List all question ids
sapply(questions$data, function(x) x$id)
# List choice id's for the first question
sapply(questions$data[[1]]$choices, function(x) x$id)
useranswers.question <- GetPresidentti2012(category="useranswers", API=API, filter="question:57:295", page=1, per_page=500, show_total="true")

# Use filter 'timerange' (probably most useful)
useranswers.timerange <- GetPresidentti2012(category="useranswers", API=API, filter="timerange:2011-12-10", page=1, per_page=1000, show_total="true")
# Use filter 'topcandidate'
useranswers.topcandidate <- GetPresidentti2012(category="useranswers", API=API, filter="topcandidate:9", page=1, per_page=500, show_total="true")

# Process question and choice texts
Questions <- data.frame(ID=sapply(questions$data, function(x) x$id))
Questions$Text <- sapply(questions$data, function(x) x$text)
Choices <- list(ID=lapply(questions$data, function(y) sapply(y$choices, function(x) x$id)))
Choices$Text <- lapply(questions$data, function(y) sapply(y$choices, function(x) x$text))

# Wrap texts for visualization
Questions$TextWrapped <- lapply(Questions$Text, function(x) paste(strwrap(x, width=80), collapse="\n"))
Choices$TextWrapped <- lapply(Choices$Text, function(x) sapply(x, function(y) paste(strwrap(y, width=40), collapse="\n")))

# Save background data for later use
# NOTE: create 'vaalit' subfolder in the working directory if it does not exist yet
save(Questions, Choices, questions, candidates, file="vaalit/Presidentti2012_BackgroundData_20120102.RData")

##############################
## GET ALL USER ANSWER DATA ##
##############################

# We will use the useranswers -query with the timerange-filter
# to get all useranswer data recorded so far
# Note! This takes a lot of time, at least an hour
# Define dates from November 23 to January 14
dates <- c(paste("2011-11", 23:30, sep="-"), paste("2011-12", 1:31, sep="-"), paste("2012-1", 1:14, sep = "-"))
# dates <- dates[1:2] #Oops! This line wasn't supposed to be here! - Commented out 8.1.2012 -Ouzo
per.page <- 10000

dat.list <- list()
for (di in 1:length(dates)) {
  filter <- paste("timerange:",dates[di], sep="")
  message("\n",filter, ", page 1...", appendLF=FALSE)

  # Get results (can download only 10000 at a time)
  dat <- GetPresidentti2012(category = "useranswers", API = API, filter = filter, page = 1, per_page = per.page, show_total = "true")

  # Check if more than 10000 answers given
  ten.ks <- ceiling(dat$pagination$total / 10000)
  if (ten.ks > 1) { #Fixed from '>=' to '>' 8.1.2012 -Ouzo
    # Get remaining results, 10000 at a time
    for (t in 2:ten.ks) {
       message("page ", t, "... ", appendLF=FALSE)
      temp.dat <- GetPresidentti2012(category="useranswers", API=API, filter=filter, page=t, per_page=per.page, show_total="true")
      dat$data <- c(dat$data, temp.dat$dat)
    }
  }
  dat.list[[di]] <- dat
}
names(dat.list) <- dates

# Save raw data
save(dat.list, file="vaalit/Presidentti2012_RawData_20120104.RData")

# Construct a data frame
Presidentti2012.df <- c()
for (di in 1:length(dat.list)) {
  print(paste("Collecting the data", 100*di/length(dat.list), " percent.."))

  # Get respondent information
  info <- unlist(lapply(dat.list[[di]]$data, function(x) as.character(x[1:9])))
  info.mat <- matrix(info, ncol=9, byrow=T)
  colnames(info.mat) <- names(dat.list[[di]]$data[[1]])[1:9]

  # Accept only those users who have answered to all questions
  # Get answers (not for Q14/ID70, because it is a multiple choice question)
  missing <- which(sapply(dat.list[[di]]$data, function(x) length(x$answers)) < 25)
  answer.list <- lapply(dat.list[[di]]$data[-missing], function(x) matrix(as.character(unlist(x$answers[-14])), ncol=2, byrow=T)[,2])
  answer.mat <- matrix(unlist(answer.list), nrow=length(answer.list), ncol=24, byrow=T)
  colnames(answer.mat) <- paste("Q", Questions$ID[-14], sep="")

  # Join the matrices
  date.df <- cbind(as.data.frame(info.mat[-missing,]), as.data.frame(answer.mat))
  Presidentti2012.df <- rbind(Presidentti2012.df, date.df)
  
}

################################################

# Translate variable names and fix some of them
names(Presidentti2012.df)[1:8] <- c("Paivamaara", "Koulutustaso", "Sukupuoli", "Tulot", "Ykkosehdokas", "Puolue", "Ika", "Asuinpaikka")
levels(Presidentti2012.df$Sukupuoli) <- c("NULL", "Nainen", "Mies")[match(levels(Presidentti2012.df$Sukupuoli), c("NULL", "f", "m"))]
Presidentti2012.df$Paivamaara <- as.Date(Presidentti2012.df$Paivamaara)

# Match candidate IDs and names
candidate <- sapply(candidates$data, function(x) x$lastname)  # candidate name
names(candidate) <- sapply(candidates$data, function(x) x$id) # candidate ID
levels(Presidentti2012.df$Ykkosehdokas) <- candidate[levels(Presidentti2012.df$Ykkosehdokas)]
#sort(table(Presidentti2012.df$Ykkosehdokas))/nrow(Presidentti2012.df)

# Reorder factor levels, some by abundance, some in the natural way
# 'attach' lets us use the factors without repeating the data frame name every time
attach(Presidentti2012.df)
Presidentti2012.df$Koulutustaso <- reorder(Koulutustaso, id, length)
Presidentti2012.df$Ykkosehdokas <- reorder(Ykkosehdokas, id, length)
Presidentti2012.df$Puolue <- reorder(Puolue, id, length)
Presidentti2012.df$Asuinpaikka <- reorder(Asuinpaikka, id, length)
Presidentti2012.df$Tulot <- factor(Tulot, levels=levels(Tulot)[c(1,9,2,3,5:8,10:12,4)])
detach(Presidentti2012.df)

#####################################################

# Save preprocessed data frame
save(Presidentti2012.df, file="vaalit/Presidentti2012_DataNov-Dec_20120102.RData")

####################
## VISUALIZATIONS ##
####################

# Need a few packages for the visualizations
# Run install.packages("package") if needed
library(ggplot2)
library(gridExtra)
library(RColorBrewer)
theme_set(theme_bw())

# Load processed data
load("vaalit/Presidentti2012_BackgroundData_20120102.RData")
load("vaalit/Presidentti2012_DataNov-Dec_20120102.RData")

# Let's first plot statistics about the user backgrounds
# Create plots for each variable
stats.plots <- list()
for (pi in 1:7) {
  temp.df <- Presidentti2012.df
  temp.df$X <- temp.df[[pi+1]]
  temp.df <- droplevels(subset(temp.df, X != "NULL"))
  p <- ggplot(temp.df, aes(x=X))
  p <- p + geom_bar(fill="red") + coord_flip()
  p <- p  + labs(x=names(temp.df)[pi+1], y="Määrä")
  stats.plots[[pi]] <- p
}

# Join the plots into one grob and create a pdf
png("vaalit/Presidentti2012_Vastaajatilastoja_20120106.png")
stats.grob <- do.call(arrangeGrob, c(stats.plots, list(ncol=2)))
#ggsave("vaalit/Presidentti2012_Vastaajatilastoja_20120106.png", plot=stats.grob, width=11, height=11)
dev.off()

# We can also study how users supporting different parties have answered to specific questions
# First remove all users without specific party information
Pressa.df <- droplevels(subset(Presidentti2012.df, !(Puolue %in% c("NULL", "Muu", "En äänestänyt"))))
Pressa.df$Puolue <- reorder(Pressa.df$Puolue, Pressa.df$id, function(x) length(x))

# Compute total support for the parties
party.support <- aggregate(Pressa.df$id, by=list(Pressa.df$Puolue), length)
names(party.support) <- c("Puolue", "Kokonaismaara")

# Function for plotting results for a question by party
PlotQuestionParty <- function(Qid) {

  # Get indices
  q.ind1 <- which(names(Pressa.df)==paste("Q", Qid, sep=""))
  q.ind2 <- which(Questions$ID==Qid)

  # Compute results for the given question
  Q.df <- aggregate(Pressa.df[[q.ind1]], by=list(Pressa.df$Puolue, Pressa.df[[q.ind1]]), length)
  names(Q.df) <- c("Puolue", "Vastausvaihtoehto", "Maara")
  levels(Q.df$Vastausvaihtoehto) <- Choices$TextWrapped[[q.ind2]]

  # Compute percentages
  Q.df <- merge(Q.df, party.support, by="Puolue")
  Q.df$Prosentti <- 100*round(Q.df$Maara / Q.df$Kokonaismaara, digits=3)

  # Reorder parties based on 1st choice percentages
  c1.inds <- which(Q.df$Vastausvaihtoehto==levels(Q.df$Vastausvaihtoehto)[1])
  ordering <- order(Q.df$Prosentti[c1.inds])
  Q.df$Puolue <- factor(Q.df$Puolue, levels=Q.df$Puolue[c1.inds][ordering])

  # Make title
  title <- paste("Kysymys ",q.ind2,":\n",Questions$TextWrapped[q.ind2], paste="")

  # Plot
  p <- ggplot(Q.df, aes(x=Puolue, weight=Prosentti, fill=Vastausvaihtoehto))
  p <- p + geom_bar(position="stack") + coord_flip() + ylab("Vastausprosentti")
  p <- p + scale_fill_brewer(palette="GnBu")
  p <- p + opts(title=title, plot.title=theme_text(size=14, hjust=0))
  p <- p + opts(legend.key.height = unit(2, "cm"))
  return(p)
}

# We can create separate plots for interesting questions
png("vaalit/Presidentti2012_Kysymys12_20120106.png")
Q12.plot <- PlotQuestionParty(Questions$ID[12])
dev.off()
#ggsave("vaalit/Presidentti2012_Kysymys12_20120106.png", plot=Q12.plot)

# Or we can plot all questions at once
pdf("vaalit/Presidentti2012_Kysymykset_Puolueittain_20120106.pdf")
Qids <- Questions$ID[-14]
Q.plots <- list()
for (qi in 1:length(Qids))
  Q.plots[[qi]] <- PlotQuestionParty(Qids[qi])
Q.grob <- do.call(arrangeGrob, c(Q.plots, list(ncol=1)))
dev.off()
#ggsave("vaalit/Presidentti2012_Kysymykset_Puolueittain_20120106.pdf", plot=Q.grob, width=8, height=6*length(Q.plots))

