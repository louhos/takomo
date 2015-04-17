library(glmnet)
library(plyr)
library(dplyr)
library(reshape2)
library(ggplot2)
library(RColorBrewer)
library(corrplot)

## Mäppäyksiä
f2num<-function(x){as.numeric(levels(x)[x])}

map.vaalipiiri <-function(x) revalue(x,c("01 Helsingin vaalipiiri"="01 Helsinki",
                                     "02 Uudenmaan vaalipiiri"="02 Uusimaa",
                                     "03 Varsinais-Suomen vaalipiiri"="03 Varsinais-Suomi",
                                     "04 Satakunnan vaalipiiri"="04 Satakunta",
                                     "05 Ahvenanmaan maakunnan vaalipiiri"="05 Ahvenanmaa",
                                     "06 Hämeen vaalipiiri"="06 Häme",
                                     "07 Pirkanmaan vaalipiiri"="07 Pirkanmaa",
                                     "08 Kaakkois-Suomen vaalipiiri"="08 Kaakkois-Suomi",
                                     "09 Savo-Karjalan vaalipiiri",
                                     "10 Vaasan vaalipiiri"="10 Vaasa",
                                     "11 Keski-Suomen vaalipiiri"="11 Keski-Suomi",
                                     "12 Oulun vaalipiiri"="12 Oulu",
                                     "13 Lapin vaalipiiri"="13 Lappi")) 
                                     
map.puolue<-function(x) revalue(x,c(
"Perussuomalaiset"="PS", 
"Vihreä liitto"="VIHR",
"Vasemmistoliitto"="VAS", 
"Suomen Kommunistinen Puolue"="SKP",
"Suomen Kristillisdemokraatit (KD)"="KD",
"Suomen Työväenpuolue STP"="STP",
"Suomen ruotsalainen kansanpuolue"="RKP", 
"Itsenäisyyspuolue"="IP",
"Kansallinen Kokoomus"="KOK",
"Köyhien Asialla"="KA",
"Piraattipuolue"="PIR",                    
"Suomen Keskusta"="KESK",
"Muutos 2011"="M11",           
"Kommunistinen Työväenpuolue - Rauhan ja Sosialismin puolesta"="KTP",
"Pirkanmaan Sitoutumattomat yhteislista"="PSYL",
"Suomen Sosialidemokraattinen Puolue"="SDP"))
  
yle.map<-function(x){
  as.numeric(mapvalues(as.character(x),
                       c("","jokseenkin eri mieltä","jokseenkin samaa mieltä","ohita kysymys","täysin eri mieltä","täysin samaa mieltä","Ei",
                         "Kyllä","Tyhjä"),
                       c(NA,-0.5,0.5,0,-1,1,-1,1,0)))
}

## Lataa data (muokattu etukäteen otsikoita vähän ja poistettu vapaat vastaukset)

yle.orig<-read.csv("http://data.yle.fi/dokumentit/Eduskuntavaalit2015/vastaukset_avoimena_datana.csv",sep=";")

mukaan<-c("vaalipiiri",
"id",
"sukunimi",
"etunimi",
"puolue",
"ikä",
"sukupuoli",
"Toimin.tällä.hetkellä.kansanedustajana.",
"sitoutumaton",
"kotikunta",
"ehdokasnumero",
"Äidinkieli",
"Lapsia",
"Työnantaja",
"Koulutus",
"Kielitaito",
"Uskonnollinen.yhteisö",
"Käytän.vaaleihin.rahaa",
"Ulkopuolisen.rahoituksen.osuus",
"Tärkein.ulkopuolinen.rahoituslähde",
"Vuositulot",
"Sijoitusten.arvo..esim..osakkeet...rahastot.",
"X127.Suomessa.on.liian.helppo.elää.sosiaaliturvan.varassa",
"X128.Kaupan.ja.muiden.liikkeiden.aukioloajat.on.vapautettava.",
"X129.Suomessa.on.siirryttävä.perustuloon.joka.korvaisi.nykyisen.sosiaaliturvan.vähimmäistason.",
"X130.Työntekijälle.on.turvattava.lailla.minimityöaika.",
"X131.Ansiosidonnaisen.työttömyysturvan.kestoa.pitää.lyhentää.",
"X132.Euron.ulkopuolella.Suomi.pärjäisi.paremmin.",
"X133.Ruoan.verotusta.on.varaa.kiristää.",
"X134.Valtion.ja.kuntien.taloutta.on.tasapainotettava.ensisijaisesti.leikkaamalla.menoja.",
"X135.Lapsilisiä.on.korotettava.ja.laitettava.verolle.",
"X136.Suomella.ei.ole.varaa.nykyisen.laajuisiin.sosiaali..ja.terveyspalveluihin.",
"X137.Nato.jäsenyys.vahvistaisi.Suomen.turvallisuuspoliittista.asemaa.",
"X138.Suomeen.tarvitaan.enemmän.poliiseja.",
"X139.Maahanmuuttoa.Suomeen.on.rajoitettava.terrorismin.uhan.vuoksi.",
"X140.Venäjän.etupiiripolitiikka.on.uhka.Suomelle.",
"X141.Verkkovalvonnassa.valtion.turvallisuus.on.tärkeämpää.kuin.kansalaisten.yksityisyyden.suoja.",
"X142.Suomen.on.osallistuttava.Isisin.vastaiseen.taisteluun.kouluttamalla.Irakin.hallituksen.joukkoja.",
"X143.Parantumattomasti.sairaalla.on.oltava.oikeus.avustettuun.kuolemaan.",
"X144.Terveys..ja.sosiaalipalvelut.on.tuotettava.ensijaisesti.julkisina.palveluina.",
"X145.Viranomaisten.pitää.puuttua.lapsiperheiden.ongelmiin.nykyistä.herkemmin.",
"X146.Vanhuksen.ja.hänen.omaistensa.vastuuta.hoitokustannuksista.on.lisättävä.",
"X147.Kansalaisten.oikeus.terveyspalveluihin.on.tärkeämpää.kuin.kuntien.itsehallinto.",
"X148.Ilmastonmuutoksen.hillitseminen.pitää.asettaa.teollisuuden.kilpailukyvyn.edelle.",
"X149.Geenimuunneltu.ruoka.on.turvallista.ihmiselle.ja.ympäristölle.",
"X150.Suomen.pitää.ottaa.suurempi.vastuu.EU.n.alueelle.tulevista.turvapaikanhakijoista.",
"X151.On.aika.luopua.ajatuksesta..että.koko.Suomi.on.pidettävä.asuttuna.",
"X152.Peruskoulun.opetusryhmien.koko.on.rajattava.lailla.esimerkiksi.20.oppilaaseen.",
"X201.Suomen.Nato.jäsenyydestä.on.järjestettävä.kansanäänestys.",
"X244.Hyväksytään.periaatepäätös.uuden.ydinvoimalaitosyksikön.rakentamisesta.",
"X245.Tuloveroa.alennetaan.tasaisesti.kaikissa.tuloluokissa.talouden.elvyttämiseksi.",
"X246.Edellisen.eduskunnan.hyväksymä.lainmuutos.samaa.sukupuolta.olevien.avioliiton.sallimisesta.peruutetaan.",
"X247.Mietojen.viinien.ja.vahvojen.oluiden.myynti.ruokakaupassa.sallitaan.",
"X248.Ruotsin.kielen.opiskelu.muutetaan.vapaaehtoiseksi.")

yle<-select(yle.orig,one_of(mukaan))
yle<-mutate_each(yle,funs(yle.map(.)),starts_with("X"))
yle<-filter(yle,!(select(yle,starts_with("X")) %>% mapply(is.na,.) %>% apply(1,all)))
names(yle)<-gsub("\\.+"," ",names(yle))

## Muunnoksia ja siivousta
yle<-group_by(yle,puolue) %>% mutate(N=n()) %>% filter(N>5) %>% select(-N) %>% ungroup
yle<-mutate(yle,nimi=paste(etunimi,sukunimi),
         vaalipiiri=map.vaalipiiri(vaalipiiri),
         nainen=ifelse(sukupuoli=="F",1,0),
         lapset=ifelse(Lapsia=="kyllä ",1,0),
         lapset.miss=ifelse(Lapsia=="",1,0),
         ika=ifelse(ikä<18,45,ikä),
         puolue=factor(puolue),
         puolue.lyh=map.puolue(puolue),
         puolue.lyh=factor(puolue.lyh,level=sort(levels(puolue.lyh))),
         nimi=paste(etunimi,sukunimi),
         one=1)

yle<-mutate(yle, tyo=mapvalues(Työnantaja,
                       c("",
                         "ei työelämässä",
                         "julkinen ",
                         "yksityinen "),
                       c("tyo.NA",
                         "tyo.eityoelam",
                         "tyo.julkinen",
                         "tyo.yksityinen")))

## Grafiikkaa (ei blogissa)
ggplot(data=filter(yle,tyo!="tyo.NA",tyo!="tyo.eityoelam"),aes(x=puolue.lyh,fill=tyo))+geom_bar(position="fill")
ggplot(data=filter(yle,tyo!="tyo.NA"),aes(x=puolue.lyh,fill=tyo))+geom_bar(position="fill")

yle<-mutate(yle,kieli=mapvalues(Äidinkieli,
                       c("",
                         "suomi ",                         
                         "ruotsi ",
                          "muu"),
                       c("kieli.NA",
                         "kieli.suomi",                                                  
                         "kieli.ruotsi",
                         "kieli.muu")))

## Grafiikkaa (ei blogissa)
ggplot(data=filter(yle,kieli!="kieli.NA"),aes(x=puolue.lyh,fill=kieli))+geom_bar(position="fill")

yle<-mutate(yle,uskontokunta=mapvalues(`Uskonnollinen yhteisö`,
                           c("",
                             "ei kuulu kirkkoon tai muuhun uskonnolliseen yhteisöön",
                             "evankelis-luterilainen kirkko ",
                             "muu kristillinen kirkko tai yhteisö ",
                             "ortodoksinen kirkko ",
                             "muu uskonnollinen yhteisö "),
                           c("usk.NA",
                             "usk.ei",
                             "usk.ev.lut",
                             "usk.muu.krist",
                             "usk.ort",
                             "usk.muu")),
            uskontokunta=factor(uskontokunta,levels= c("usk.NA",
                                                       "usk.ei",
                                                       "usk.ev.lut",
                                                       "usk.muu.krist",
                                                       "usk.ort",
                                                       "usk.muu")))

## Grafiikkaa (ei blogissa)
ggplot(data=filter(yle,uskontokunta!="usk.NA"),aes(x=puolue.lyh,fill=uskontokunta))+geom_bar(position="fill")
ggplot(data=yle,aes(x=puolue.lyh,fill=uskontokunta))+geom_bar(position="fill")

yle<-mutate(yle,rahoitus=mapvalues(`Käytän vaaleihin rahaa`,
                            c("",
                              "alle 1 000 euroa ",
                              "1 000-5 000 euroa ",
                              "5 000-10 000 euroa ",
                              "10 000-20 000 euroa ",
                              "20 000-50 000 euroa ",
                              "yli 50 000 euroa"),
                            c("rah.NA",
                              "rah.alle.1t",
                              "rah.1t.5t",
                              "rah.5t.10t",
                              "rah.10t.20t",
                              "rah.20t.50t",
                              "rah.yli.50t")),
            rahoitus=factor(rahoitus,levels=c("rah.NA",
                                              "rah.alle.1t",
                                              "rah.1t.5t",
                                              "rah.5t.10t",
                                              "rah.10t.20t",
                                              "rah.20t.50t",
                                              "rah.yli.50t"))
            )

yle<-mutate(yle,rahoitus.c=mapvalues(`Käytän vaaleihin rahaa`,
                            c("",
                              "alle 1 000 euroa ",
                              "1 000-5 000 euroa ",
                              "5 000-10 000 euroa ",
                              "10 000-20 000 euroa ",
                              "20 000-50 000 euroa ",
                              "yli 50 000 euroa"),
                            c(0,
                              500,
                              3000,
                              7500,
                              15000,
                              35000,
                              60000)),
         rahoitus.c=as.numeric(levels(rahoitus.c)[rahoitus.c]),
         rahoitus.miss=ifelse(rahoitus=="rah.NA",1,0))

yle<-mutate(yle,ulkop.rah=mapvalues(`Ulkopuolisen rahoituksen osuus`,
                             c("",
                               "0",
                               "1-20 % ",
                               "21-50% ",
                               "51-80 % ",
                               "81-100%"),
                             c("spons.NA",
                               "spons.0",
                               "spons.0.20",
                               "spons.21.50",
                               "spons.51.80",
                               "spons.81.100")),
            ulkop.rah.c=mapvalues(`Ulkopuolisen rahoituksen osuus`,
                             c("",
                               "0 %",
                               "1-20 % ",
                               "21-50% ",
                               "51-80 % ",
                               "81-100%"),
                             c(0,
                               0,
                               10,
                               35,
                               75,
                               90)),
         ulkop.rah.c=as.numeric(levels(ulkop.rah.c)[ulkop.rah.c]),
         ulkop.rah.miss=ifelse(ulkop.rah=="spons.NA",1,0))
         
yle<-mutate(yle,koulutus=mapvalues(Koulutus,
                            c("",
                              "ammattitutkinto ",
                              "joku muu",
                              "korkeakoulututkinto ",
                              "peruskoulu ",
                              "ylioppilas "
                              ),
                            c("koul.NA",
                              "koul.ammatti",
                              "koul.muu",
                              "koul.akat",
                              "koul.perus",
                              "koul.yo")))
         
yle<-mutate(yle,tulot=mapvalues(Vuositulot,
                            c("",
                              "30 000-50 000 euroa ",
                              "20 000-30 000 euroa ",
                              "50 000-70 000 euroa ",
                              "alle 20 000 euroa ",
                              "70 000-100 000 euroa ",
                              "yli 100 000 euroa"
                            ),
                            c("tulo.NA",
                              "tulo.30t.50t",
                              "tulo.20t.30t",
                              "tulo.50t.70t",
                              "tulo.0t.20t",
                              "tulo.70t.100t",
                              "tulo.yli.100t")),
         tulot.c=mapvalues(Vuositulot,
                         c("",
                           "30 000-50 000 euroa ",
                           "20 000-30 000 euroa ",
                           "50 000-70 000 euroa ",
                           "alle 20 000 euroa ",
                           "70 000-100 000 euroa ",
                           "yli 100 000 euroa"
                         ),
                         c(0,
                           40000,
                           25000,
                           60000,
                           10000,
                           85000,
                           120000)),
         tulot.c=as.numeric(levels(tulot.c)[tulot.c]),
         tulot.miss=ifelse(tulot=="tulo.NA",1,0))

## Grafiikkaa tuloista jne...
filter(yle,rahoitus.miss==0,ulkop.rah.miss==0,tulot.miss==0) %>% group_by(puolue.lyh) %>% 
  summarise(rahoitus=mean(rahoitus.c),ulkop=mean(ulkop.rah.c),tulot=mean(tulot.c)) %>% 
  qplot(data=.,x=rahoitus,y=ulkop,label=puolue.lyh,size=tulot)+geom_text(size=5,hjust=.5,vjust=-.5)

yle<-mutate(yle,sijoitus=mapvalues(`Sijoitusten arvo esim osakkeet rahastot `,
                            c("",
                              "ei sijoituksia ",
                              "0-10 000 euroa ",
                              "10 000-50 000 euroa ",
                              "50 000-200 000 euroa ",
                              "200 000-500 000 euroa ",
                              "yli 500 000 euroa"
                              ),
                            c("sijoitus.NA",
                              "sijoitus.ei",
                              "sijoitus.0t.50t",
                              "sijoitus.0t.50t",
                              "sijoitus.yli.50t",
                              "sijoitus.yli.50t",
                              "sijoitus.yli.50t")),

         sijoitus.c=mapvalues(`Sijoitusten arvo esim osakkeet rahastot `,
                            c("",
                              "ei sijoituksia ",
                              "0-10 000 euroa ",
                              "10 000-50 000 euroa ",
                              "50 000-200 000 euroa ",
                              "200 000-500 000 euroa ",
                              "yli 500 000 euroa"
                            ),
                            c(0,
                              0,
                              5000,
                              30000,
                              125000,
                              350000,
                              500000)),
         sijoitus.c=as.numeric(levels(sijoitus.c)[sijoitus.c]),
         sijoitus.miss=ifelse(sijoitus=="sijoitus.NA",1,0))
     
yle<-left_join(yle,dcast(select(yle,id,tyo),id~tyo,fun=length),by="id") %>%  
left_join(.,dcast(select(yle,id,koulutus),id~koulutus,fun=length),by="id") %>%
left_join(.,dcast(select(yle,id,uskontokunta),id~uskontokunta,fun=length),by="id") %>%
left_join(.,dcast(select(yle,id,tulot),id~tulot,fun=length),by="id") %>% 
left_join(.,dcast(select(yle,id,ulkop.rah),id~ulkop.rah,fun=length),by="id") %>% 
left_join(.,dcast(select(yle,id,rahoitus),id~rahoitus,fun=length),by="id") %>% 
left_join(.,dcast(select(yle,id,kieli),id~kieli,fun=length),by="id")

# "laimeus" / "jyrkkyys"
yle$rad.euc<-select(yle,starts_with("X")) %>% mutate_each(funs(.^2)) %>% rowSums(.,na.rm=TRUE) %>% sqrt

# radikaalisuusplotti
qplot(data=yle,y=rad.euc,x=puolue.lyh)+geom_boxplot()

## Puolueiden frekvenssit tolppadiagrammina
puolue<-c("IP","KD","KESK","KOK","KTP","M11","PIR","PS","PSYL","RKP","SDP","SKP","STP","VAS","VIHR")
puolue.vari=c(brewer.pal(name="Paired",n=12),"black","gray40","brown")[
  c(1,10,3,2,5,14,13,9,12,11,6,15,7,8,4)]
names(puolue.vari)<-puolue

ggplot(data=yle,aes(x=puolue.lyh,fill=puolue.lyh))+
  geom_bar(position="stack")+ scale_fill_manual(values=puolue.vari)+
  theme(legend.position="bottom")

# x: piirrevektorit, y: luokka

# Pelkkä demografia
#x=select(yle,starts_with("usk."),
#         starts_with("tulo."),
#         starts_with("kieli."),
#         starts_with("koulutus."),
#         starts_with("sijoitus."),
#         ika,
#         nainen,
#         lapset,
#         lapset.miss) %>% as.matrix; row.names(x)<-yle$id

# Pelkät mielipiteet

x=select(yle,starts_with("X"),rad.euc) %>% as.matrix; row.names(x)<-yle$id

x[is.na(x)]<-0
y=factor(yle$puolue.lyh)

# Luokittelu ja datat
classifier<-cv.glmnet(x,y,family="multinomial",type.measure="class",standardize=TRUE,intercept=TRUE, alpha=1)

posterior <- data.frame(id=yle$id, 
                        puolue=yle$puolue.lyh, 
                        drop(predict(classifier,x,type="response")),
                        puolue.e=tolower(drop(predict(classifier,x,type="class"))))

posterior <-mutate(posterior,correct=ifelse(puolue==toupper(puolue.e),1,0))

## Sekaannusmatriisi
p<-table(posterior$puolue,posterior$puolue.e)
p.miss<-setdiff(rownames(p),toupper(colnames(p)))
d.p<-dim(p)
p<-cbind(p,matrix(0,d.p[1],length(p.miss))); colnames(p)[(d.p[2]+1):d.p[1]]<-tolower(p.miss)

p<-p[order(rownames(p)),]
p<-p[match(colnames(p),tolower(rownames(p))),]

corrplot(prop.table(p,1),method="shade",is.corr=FALSE,addCoef.col=2,
         addCoefasPercent=TRUE,col=colorRampPalette(c("white","white","black"),1)(80))

## Tällä sataa datat tiedostoon
ehdokkaat<-left_join(select(yle,sukunimi,etunimi,id,vaalipiiri),posterior,by="id")
write.table(file="ehdokkaat.yle.csv",ehdokkaat,row.names=FALSE,dec=".",sep=";",quote=FALSE)

#Kertoimet luettavaan muotoon ja graafiksi (toimii vain kysymysten perusteella tehdylle luokittimelle)
j<-data.frame(matrix(0,34,15));names(j)<-puolue
for (i in puolue) j[i]<-coef(classifier)[[i]] %>% as.numeric
j$question<-""
j$question[1]<-"intercept"
j$question[2:33]<-yle.long.names
j$question[34]<-"euc"

k<-melt(j) %>%
  filter(question!="intercept") %>% 
  rename(puolue=variable,kysymys=question) %>%
  mutate(txt=ifelse(value==0,"0",sprintf("%1.2f", value)))

k<-arrange(k,kysymys) 

ggplot(data=k,aes(y=kysymys, x=puolue,fill = value, label = txt)) + 
  geom_tile() + geom_text(size=4, colour = "black") +
  scale_fill_gradient2(low = "#c51b7d", high = "#4d9221",mid="#fdfdfd",  midpoint=0) +
  ylim(rev(sort(unique(k$kysymys))))









