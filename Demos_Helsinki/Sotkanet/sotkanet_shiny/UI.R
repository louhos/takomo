# Shiny script

# This script is part of the Louhos-project (http://louhos.github.com/)

# Copyright (C) 2012-2013 Juuso Parkkinen and Leo Lahti.
# Contact: <http://louhos.github.com/contact>. 
# All rights reserved.

# This program is open source software; you can redistribute it and/or modify
# it under the terms of the FreeBSD License (keep this notice):
# http://en.wikipedia.org/wiki/BSD_licenses

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

library(shiny)

shinyUI(pageWithSidebar(
  # Application title
  headerPanel("Sotkanet: Alueellinen hyvinvointi"),
  
  sidebarPanel(
    helpText(h4("Käyttöhjeet:")),
    helpText("Tällä visualisoinnilla voit tutkia Sotkanetin indikaattoridataa."),
    helpText(h4("Motion chartin asetukset:")),
    
    radioButtons("region.category", "Aluekategoria",
                 c("Kunta" = "kunta",
                   "Maakunta" = "maakunta",
                   "Seutukunta" = "seutukunta")),
    checkboxGroupInput("indicator.category","Muuttujakategoria",
                       c("Talous"="TALOUS",
                         "Tilastollinen hyvintointi"="TILASTOLLINEN HYVINVOINTI",
                         "Tuloerot"="TULOEROT",
                         "Työelämä"="TYÖELÄMÄ",
                         "Väestönmuutos"="VÄESTÖNMUUTOS")),
    
    helpText(h4("Kartan asetukset:")),
    selectInput("map.indicator", "Muuttuja", 
                choices=c(
                  "[TALOUS] Keskiasteen koulutuksen saaneet, % 15 vuotta täyttäneistä",
                  "[TALOUS] Korkea-asteen koulutuksen saaneet, % 15 vuotta täyttäneistä",
                  "[TALOUS] Koulutuksen ulkopuolelle jääneet 17 - 24-vuotiaat, % vastaavanikaisesta väestöstä",
                  "[TALOUS] Koulutustasomittain",
                  "[TALOUS] Lainakanta, euroa / asukas",
                  "[TALOUS] Suhteellinen velkaantuneisuus, %",
                  "[TALOUS] Verotulot, euroa / asukas",
                  "[TILASTOLLINEN HYVINVOINTI] Depressiolääkkeistä korvausta saaneet 18-64-vuotiaat / 1000 vastaavanikäistä",
                  "[TILASTOLLINEN HYVINVOINTI] Hoitopäivät perusterveydenhuollossa 75 vuotta täyttäneillä / 1000 vastaavanikäistä",
                  "[TILASTOLLINEN HYVINVOINTI] Mielenterveyden häirioihin sairaalahoitoa saaneet 0 - 17-vuotiaat / 1000 vastaavanikaista",
                  "[TILASTOLLINEN HYVINVOINTI] Sairauspäivärahaa saaneet 16 - 64-vuotiaat / 1000 vastaavanikäistä",
                  "[TILASTOLLINEN HYVINVOINTI] Toimeentulotukea saaneet henkilöt vuoden aikana, % asukkaista",
                  "[TILASTOLLINEN HYVINVOINTI] Toimeentulotuki, euroa / asukas",
                  "[TULOEROT] Äänestysaktiivisuus kuntavaaleissa, %",
                  "[TULOEROT] Gini-kerroin, käytettävissä olevat tulot",
                  "[TULOEROT] Kunnan yleinen pienituloisuusaste",
                  "[TYÖELÄMÄ] Ammatillinen, tieteellinen ja tekninen toiminta; Hallinto ja tukipalvelutoiminta, % työllisistä",
                  "[TYÖELÄMÄ] Informaatio ja viestintä, % työllisistä",
                  "[TYÖELÄMÄ] Julkinen hallinto ja maanpuolustus sekä Pakollinen sosiaalivakuutus; Koulutus; Sosiaali- ja terveyspalvelut, % työllisistä",
                  "[TYÖELÄMÄ] Julkiset ja muut palvelut, % työllisistä (-2007)",
                  "[TYÖELÄMÄ] Kaivostoiminta; Sähkö-, kaasu ja lämpöhuolto; Vesi-, viemäri- ja jätehuolto, % työllisistä",
                  "[TYÖELÄMÄ] Kauppa, majoitus- ja ravitsemistoiminta, % työllisistä (-2007)",
                  "[TYÖELÄMÄ] Kiinteistöalan toiminta, % työllisistä",
                  "[TYÖELÄMÄ] Liikenne, % työllisistä (-2007)",
                  "[TYÖELÄMÄ] Maa- ja metsätalous, % työllisistä (-2007)",
                  "[TYÖELÄMÄ] Maa-, metsä- ja kalatalous, % työllisistä",
                  "[TYÖELÄMÄ] Muut palvelut, % työllisistä",
                  "[TYÖELÄMÄ] Rahoitus ja vakuutustoiminta, % työllisistä",
                  "[TYÖELÄMÄ] Rahoitus-, vakuutus- ja liike-elämää palveleva toiminta, % työllisistä (-2007)",
                  "[TYÖELÄMÄ] Rakentaminen, % työllisistä",
                  "[TYÖELÄMÄ] Rakentaminen, % työllisistä (-2007)",
                  "[TYÖELÄMÄ] Teollisuus, % työllisistä",
                  "[TYÖELÄMÄ] Teollisuus, % työllisistä (-2007)",
                  "[TYÖELÄMÄ] Tukku- ja vähittäiskauppa; Kuljetus ja varastointi; Majoitus- ja ravitsemistoiminta , % työllisistä",
                  "[TYÖELÄMÄ] Työlliset, % väestöstä",
                  "[TYÖELÄMÄ] Työttömät, % työvoimasta",
                  "[TYÖELÄMÄ] Vaikeasti työllistyvät, % 15 - 64-vuotiaista",
                  "[VÄESTÖNMUUTOS] 16-64 -vuotiaat, % väestöstä",
                  "[VÄESTÖNMUUTOS] 16-64 -vuotiaat, % väestöstä, väestöennuste 2040",
                  "[VÄESTÖNMUUTOS] 65 vuotta täyttäneet, % väestöstä, väestöennuste 2040",
                  "[VÄESTÖNMUUTOS] 65-74 -vuotiaat, % väestöstä",
                  "[VÄESTÖNMUUTOS] 7-15 -vuotiaat, % väestöstä, väestöennuste 2040",
                  "[VÄESTÖNMUUTOS] Huoltosuhde, demografinen",
                  "[VÄESTÖNMUUTOS] Kuntien välinen nettomuutto / 1 000 asukasta",
                  "[VÄESTÖNMUUTOS] Lähtömuuttajat, lkm",
                  "[VÄESTÖNMUUTOS] Muu kuin suomi, ruotsi tai saame äidinkielenä / 1000 asukasta",
                  "[VÄESTÖNMUUTOS] Tulomuuttajat, lkm",
                  "[VÄESTÖNMUUTOS] Väestöennuste 2020",
                  "[VÄESTÖNMUUTOS] Väestöennuste 2030",
                  "[VÄESTÖNMUUTOS] Väestöennuste 2040",
                  "[YLEISET] Väestö 31.12.")),
    selectInput("map.year", "Vuosi", choices=1990:2011),
    
    submitButton(text="Päivitä"),
    helpText(a("Lähdekoodi githubissa", href="https://github.com/louhos/takomo/tree/master/Demos_Helsinki/Sotkanet", target="_blank"))),
  
  mainPanel(
    tabsetPanel(
      tabPanel("Motion chart",h4("Vertaile indikaattoreita alueittain"),htmlOutput("motionchart")),
      tabPanel("Kartta1",h4("Tarkastele indikaattoreita kartalla"),h4(textOutput("map.status")), htmlOutput("map.gvis")),
      tabPanel("Kartta2",h4("Tarkastele indikaattoreita kartalla vuosittain"),plotOutput("map.ggplot", width="100%"))
      
    )
    
  )
))