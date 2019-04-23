###Malo ponavljanje###

#Pravljenje data frame

pol <- c(rep("musko",3),rep("zensko",3))

#sa rep, kazemo R-u da 3 puta ponovi musko, to jest 3 puta zensko
vis_glasa <- c(233,204,242,130,112,142)

my.df <- data.frame(pol, vis_glasa)#napravi mi data frame
#Ajde da vidimo kako izgleda nas data frame

my.df

# Sada cemo da napravimo prvi linearan model

lm1<- lm(vis_glasa ~ pol, data = my.df)

summary(lm1) #Daj mi rezultate


########Tumacenje koeficijenata##########

# Kako tumacim intercept kad imam kategorijalni prediktor
# u dobijenom modelu intercept je 226.33, to je aritmeticka sredina
# referentne kategorije, u ovom slucaju srednja visina glasa za muski pol

# Kako tumacim nagib?

# u ovom slucaju nagib je -98, to znaci, da je zenski pol u proseku ima 
# nizu frekvenciju glasa za -98 Hz, to jest ima u proseku 226-99= 128

# Mozemo da proverimo proseke 

with (my.df, tapply (vis_glasa, pol, mean))


####Hajde sada da vidimo na realnim podacima


# Prvo cemo da ucitamo podatke

# pre toga cemo da podesimo da nam je radni direktorijum na dekstopu
# u folderu "LMER_2019"

setwd("C:/Users/User/Desktop/LMER_2019") #kod mene posle users ide lenovo,
# a kod vas je mozda drugacije

dat2 <- read.table ("cas3.podaci.txt", sep="\t", header = T)

# proverimo dimenzije (broj redova i broj kolona) data frame#a

dim(dat2)

# vidimo da postoji 100 redova i 6 kolona (to mozemo da vidimo i u Environment)

# da vidimo naslove kolona

colnames(dat2)

## Napravimo mali uvid u prvih nekoliko redova

head(dat2)

# vidimo da se u prvoj koloni nalaze sifre ispitanika, u drugoj podatak o pripadnosti nivou grupe u okviru varijable Manipulacija, u trecoj isto to za varijablu Rukost, u cetvrtoj Nacitanost, u petoj IQ, u sestoj RT


## Mozemo da proverimo tip podataka u kolonama

str(dat2)

#Ispitanik, Manipulacija i Rukost su kodirani kao faktori
#Nacitanost, IQ i RT kao integeri (celi brojevi)

## A mozemo i vizuelnim putem:

plot(dat2)

## Da vizualizujemo jednu varijablu
# napravimo prvo frekvencijsku tabelu pomocu table(), a onda tu tabelu prikazemo graficki

barplot(table(dat2$IQ))

## Dodamo nazive osa, naslov i promenimo boju

barplot(table(dat2$IQ), 
        xlab="IQ", # naziv x ose
        ylab="Frekvencija", # naziv y ose
        main="Distribucija IQ u uzorku", # naslov na vrhu grafikona
        col="red") # boja stubica

plot(density(dat2$IQ)) #gustina distribucije
qqnorm(dat2$IQ)


plot(density(dat2$RT)) #gustina distribucije
qqnorm(dat2$RT)




###Pravljenje klasicnog - obicnog linarnog modela

# dodajemo jedan prediktor - jedan fiksni efekat

lm1 <- lm (RT~ IQ, data=dat2)
summary(lm1) #model je znacajan

# dodajemo jos jedan fiksni efekat - nacitanost

lm2<- lm (RT~ IQ + Nacitanost, data=dat2)
summary(lm2) #i ovaj model je znacajan

lm3<- lm (RT~ IQ + Nacitanost + Rukost, data=dat2)
summary(lm3) #rukost nije znacajna, pa je izbacujemo iz modela


lm4 <- lm (RT~ IQ + Nacitanost + Manipulacija, data = dat2)
summary(lm4) #Manipulacija nije znacajna


# Dakle, na RT uticu IQ i Nacitanost, hajde da vidimo sta znace koeficijenti

summary(lm2)

# Provera pretpostavki modela

# provera linearnosti prediktora

fitted(lm2)
sort(fitted(lm2))
sort(residuals(lm2))

plot(fitted(lm2),residuals(lm2), xlab="fitovane vrednost",ylab="reziduali", xlim=c(1000,2200), ylim=c(-150,108), abline(0,0))

# Ispitamo kolinearnost medu prediktorima
# za pocetak proverimo jednostavne korelacije

cor (dat2$IQ,dat2$Nacitanost) #viosoka korelacija izmedu prediktora
cor (dat2$IQ, dat2$RT)
cor (dat2$Nacitanost, dat2$RT)

#Provera normalnosti reziduala
hist(residuals(lm2))

qqnorm(residuals(lm2))
qqline(residuals(lm2))


#provera uticajnosti tacaka

dat2=dat2[dat2$RT>299,] #nema RT-ova koje treba izbaciti



## Scatterplot za RT i IQ

plot(dat2$IQ, dat2$RT) 
# prvi argument: sta se mapira na x osu, drugi argument: sta se mapira na y osu


# izracunamo koeficijent korelacije izmedu dve varijable

x=cor(dat2$IQ, dat2$RT) 
x= round(x,2) # zaokruzimo na dve decimale


## dodamo regresionu pravu

plot(dat2$IQ, dat2$RT,
     type="n") # podesimo tip tacaka na "none" ## "ne dodaji nista"

plot(dat2$IQ, dat2$RT,ylab="RT",xlab="IQ") # Kazemo mu da nam x IQ, a y da je RT

lm_dat1 <- lm(RT ~ IQ, data=dat2) # prvo napravimo linearni model
# dobijemo objekat koji sadrzi regresione koeficijente
coef(lm_dat1) # da vidimo koeficijente

abline(coef(lm_dat1)) # ovo je isto kao komanda u gornjem redu, samo krace

abline(coef(lm_dat1), lwd = 3) # podebljamo liniju

abline(coef(lm_dat1), lwd = 3, col="red") # linija da bude crvena

## Scatterplot za Nacitanost i RT

plot(dat2$Nacitanost, dat2$RT) 
# prvi argument: sta se mapira na x osu, drugi argument: sta se mapira na y osu


# izracunamo koeficijent korelacije izmedu dve varijable

x=cor(dat2$Nacitanost, dat2$RT) 
x= round(x,2) # zaokruzimo na dve decimale


## dodamo regresionu pravu

plot(dat2$Nacitanost, dat2$RT,
     type="n") # podesimo tip tacaka na "none" ## "ne dodaji nista"

plot(dat2$Nacitanost, dat2$RT,
     type="n",ylab="RT",xlab="Nacitanost") # Kazemo mu da nam x IQ, a y da je RT

lm_dat <- lm(RT ~ Nacitanost, data=dat2) # prvo napravimo linearni model
# dobijemo objekat koji sadrzi regresione koeficijente
coef(lm_dat) # da vidimo koeficijente

abline(coef(lm_dat)) # ovo je isto kao komanda u gornjem redu, samo krace

abline(coef(lm_dat), lwd = 3) # podebljamo liniju

abline(coef(lm_dat), lwd = 3, col="red") # linija da bude crvena


# Kako tumacimo koeficijent kod kontinuiranog prediktora?


summary(lm1)

#Dakle, moramo da normalizujemo prediktor da bi koeficijent imao smisao...iako ce p vrednosti ostati iste

dat2$IQ.n= scale(dat2$IQ) #normalizovanje

#Da vidimo model

lm1.n <- lm (RT~ IQ.n, data=dat2)

summary(lm1.n) #model je znacajan

#scatterplot za IQ.n i RT

x=cor(dat2$IQ.n, dat2$RT) 
x= round(x,2) # zaokruzimo na dve decimale
plot(dat2$IQ.n, dat2$RT,
     type="n",ylab="RT",xlab="IQ") # podesimo tip tacaka na "none" ## "ne dodaji nista"

lm_dat1.n <- lm(RT ~ IQ.n, data=dat2) # prvo napravimo linearni model
# dobijemo objekat koji sadrzi regresione koeficijente

abline(coef(lm_dat1.n), lwd = 3, col="red") # linija da bude crvena

# vidimo da je za prosecan IQ, prosecno vreme reakcije 1605, a nagib znaci
# da kada se pomerimo za jednu standardnu devijaciju to jest za jednu jedinicu, RT se
# povecava za 135 ms

dat2$Nacitanost.n= scale(dat2$Nacitanost) #normalizovanje

lm4<- lm (RT~ IQ.n * Nacitanost.n, data=dat2)
summary(lm4) #rukost nije znacajna, pa je izbacujemo iz modela

# za koliko treba korigovati nagib za RT ako se na skali IQ
# pomerimo za jedno mesto, odnosno, za koliko treba 
# korigovati nabib za RT,ako se na skali  Nacitanosti pomerimo za jedno mesto.

