  px <- read.px(url)
  df <- as.data.frame(px)

  df <- split(df, df$Ehdokas)[1:10]

  # Convert into more compact table format
  df <- lapply(df, function(dff) {m <- melt(dff, c("Ehdokas", "Äänestysalue", "Äänestystiedot"), "dat"); mc <- cast(m, Ehdokas + Äänestysalue ~ Äänestystiedot); mc <- mc[!mc[["Ehdokkaan numero"]] == 0, ];  })

  df <- do.call(rbind, df)

  # Preprocess candidate field
  ehd <- do.call(rbind, strsplit(as.character(df$Ehdokas), " / "))
  df[["Ehdokkaan nimi"]] <- ehd[, 1]
  df[["Puolue"]] <- ehd[, 2]
  rm(ehd)

  df$Sukunimi <- sapply(strsplit(df[["Ehdokkaan nimi"]], " "), function (x) {x[[1]]})
  df$Etunimet <- sapply(strsplit(df[["Ehdokkaan nimi"]], " "), function (x) {paste(x[-1], collapse = " ")})

  alue <- do.call(rbind, strsplit(as.character(df[["Äänestysalue"]]), " / "))
  df$Kunta <- alue[, 1]
  df$Alue <- alue[, 2]

  df

