# Convert certain key data files from MML, obtained through Kapsi, into RData format.
# From http://kartat.kapsi.fi/
# Later represent this in clearer CSV or JSON format

url.list <- list(

  # Yleiskartta (Shape files)
      # Yleiskartta 1000; 45M; 
      # Yleiskartta 4500; 2.8M; 
  yleiskartta = list("1000" = "http://kartat.kapsi.fi/files/yleiskartta_1000k/kaikki/etrs89/shape/1_milj_Shape_etrs_shape.zip",
  	             "4500" = "http://kartat.kapsi.fi/files/yleiskartta_4500k/kaikki/etrs89/shape/4_5_milj_shape_etrs-tm35fin.zip"),

  # Karttanimet (TXT files); 
      # Paikannimitaulukot + avain-PDF:t
      # Karttanimet 25; 49M; 
      # Karttanimet 100; 13M; 
      # Karttanimet 250; 4.9M; 
      # Karttanimet 500; 1.3M; 
  karttanimet = list("25" = "http://kartat.kapsi.fi/files/nimisto/karttanimet_25/etrs89/txt/KNR20_2012_01.ZIP",
  	             "100" = "http://kartat.kapsi.fi/files/nimisto/karttanimet_100/etrs89/txt/KNR100_2012_01.ZIP",
  		     "250" = "http://kartat.kapsi.fi/files/nimisto/karttanimet_250/etrs89/txt/KNR250_2012_01.ZIP",
  		     "500" = "http://kartat.kapsi.fi/files/nimisto/karttanimet_500/etrs89/txt/KNR500_2012_01.ZIP"),

  # Paikannimet (TXT files)
    # Paikannimet kaikki; 41M; 
    # Paikannimet kaikki; 28M; 
    # Paikat; 44M; 
    # Is there any different btw. 1 and 2?
  paikannimet = list("kaikki_1" = "http://kartat.kapsi.fi/files/nimisto/paikannimet_kaikki/etrs89/gml/paikannimet_2012_10.zip",
  	             "kaikki_2" = "http://kartat.kapsi.fi/files/nimisto/paikannimet_kaikki/etrs89/txt/PNR_2012_01.ZIP",
  		     "paikat" = "http://kartat.kapsi.fi/files/nimisto/paikat/etrs89/gml/paikat_2012_10.zip"),

  # Maastotietokanta (Shape files)
    # Maastotietokanta kaikki; 9.9G; 
    # Maastotietokanta tiestö osoitteilla 1; 14K; 
    # Maastotietokanta tiestö osoitteilla 2; 1.3M; 
  maastotietokanta = list("kaikki" = "http://kartat.kapsi.fi/files/maastotietokanta/kaikki/etrs89/shp.zip",
  		          "tiesto_1" = "http://kartat.kapsi.fi/files/maastotietokanta/tiesto_osoitteilla/etrs89/shp/N61.shp.zip",
  			  "tiesto_2" = "http://kartat.kapsi.fi/files/maastotietokanta/tiesto_osoitteilla/etrs89/shp/N62.shp.zip"),

  # Maastokartta (Shape)
  # several subfolders; check in more detail how could be used
  maastokartta = list("100" = "http://kartat.kapsi.fi/files/maastokartta_100k/kaikki/etrs89/shp/",
  	              "250" = "http://kartat.kapsi.fi/files/maastokartta_250k/kaikki/etrs89/shp/"),

  
  # Kuntajako (XML); 
  # some problems in R; municipality borders also available through yleiskartta shapefiles so skip for now.
    #Kuntajako 10; 22M; 
    #Kuntajako 100; 1.7M; 
    #Kuntajako 250; 1.0M; 
    #Kuntajako 1000; 642k; 
    #Kuntajako 4500; 259k; 

    kuntajako = list("10"  = "http://kartat.kapsi.fi/files/kuntajako/kuntajako_10k/etrs89/gml/TietoaKuntajaosta_2013_10k.zip",
    	             "100"  = "http://kartat.kapsi.fi/files/kuntajako/kuntajako_100k/etrs89/gml/TietoaKuntajaosta_2013_100k.zip",
    		     "250" = "http://kartat.kapsi.fi/files/kuntajako/kuntajako_250k/etrs89/gml/TietoaKuntajaosta_2013_250k.zip",
    		     "1000" = "http://kartat.kapsi.fi/files/kuntajako/kuntajako_1000k/etrs89/gml/TietoaKuntajaosta_2013_1000k.zip",
    		     "4500" = "http://kartat.kapsi.fi/files/kuntajako/kuntajako_4500k/etrs89/gml/TietoaKuntajaosta_2013_4500k.zip")

)



