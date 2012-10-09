print("Kunnallisvaalit 2008")

tabs2008 <- list()
for (id in election.districts) {
  tabs2008[[id]] <- GetElectedCandidates(2008, "municipal", election.district = id, verbose = TRUE) 
}

candidates2008 <- do.call(rbind, tabs2008)

save(candidates2008, file = "candidates2008.RData", compress = "xz")

#################################################

print("Kunnallisvaalit 2012")

candidates2012 <- ReadAllCandidates()

save(candidates2012, file = "candidates2012.RData", compress = "xz")

###################################################

# Fields in the same order, and discrepant fields in the end
coms <- sort(intersect(colnames(candidates2004), colnames(candidates2012)))
candidates2004 <- candidates2004[, c(coms, c("Äänestysalue", "Alue"), sort(setdiff(colnames(candidates2004), c(coms, c("Äänestysalue", "Alue")))))]
candidates2008 <- candidates2008[, c(coms, c("Äänestysalue", "Alue"), sort(setdiff(colnames(candidates2008), c(coms, c("Äänestysalue", "Alue")))))]
candidates2012 <- candidates2012[, c(coms, sort(setdiff(colnames(candidates2012), coms)))]

###################################################

# Dump into a csv file
write.table(candidates2004, "municipal_elections_candidates_2004_finland.csv", sep=";", quote=FALSE, fileEncoding="iso-8859-1", row.names = FALSE)

# Dump into a csv file
write.table(candidates2008, "municipal_elections_candidates_2008_finland.csv", sep=";", quote=FALSE, fileEncoding="iso-8859-1", row.names = FALSE)

# Dump into a csv file
write.table(candidates2012, "municipal_elections_candidates_2012_finland.csv", sep=";", quote=FALSE, fileEncoding="iso-8859-1", row.names = FALSE)


