# Datavaalit esimerkki
* Esimerkki datavaalit-datan visualisoinnista R:llä




---

# Äänestysprosentit kunnittatin


```r
# Read election data from datavaalit.fi
election.data <- ReadDatavaalit("election.data")

# Read municipality data from datavaalit.fi
municipality.data <- ReadDatavaalit("municipality.data")

# Extract voting percentages and corresponding municipality ids
vp <- election.data$objects[[2]]$voting_percentage
voting.percentages <- as.numeric(sapply(vp, function(x) {
    x$value
}))
municipality.id <- sapply(strsplit(sapply(vp, function(x) {
    x$municipality
}), "/"), function(x) {
    x[[5]]
})
# FIXME: create direct way to pick the ID, either from municipality.data
# or election.data (these can be matched directly based on api ids)

# Load Maanmittauslaitos data (C) Maanmittauslaitos 2011
LoadData("MML")
sp <- MML[["1_milj_Shape_etrs_shape"]]$kunta1_p

# Match MML and Datavaalit info
mml.id <- as.numeric(as.character(as.data.frame(sp)$Kunta))
sp$voting.percentage <- voting.percentages[match(mml.id, municipality.id)]

# Visualize voting percentages across Finland
tmp <- PlotShape(sp, "voting.percentage")
```

![plot of chunk voting](http://i.imgur.com/APj0M.png) 



---
# Helsinkin valtuuston kokoonpano

```r
# Load the city council of Helsinki
helsinki.council <- ReadDatavaalit("hel.council.members")

# Get stats on different parties
print(GetParties(helsinki.council))
```

```
## 
##    KD Kesk.  Kok.    PS   RKP   SDP   SKP  Vas. Vihr. 
##     1     4    26     4     5    16     1     7    21
```


