#ovde pistemo skript, koji kada sacuvamo, posle samo pokrenemo i dobijemo output, slicno
#kao kod SPSS sintakse

#zagrevanje prstiju

# 1) R kao kalkulator

# 2) Logicki operatori - TRUE & FALSE

3<2

"pravo" = "krivo" #stringove stavljamo pod navodnike

"pravo" == "krivo" #jednakost oznacavamo sa ==

"pravo" == "Pravo"

"pravo" == "pravo"

# 3) Pravljenje data frame-a

myvar <- c(1,2,3,4,5) #objekat dodeljujemo sa strelicom ili sa =

myvar

myvar = 1:4 #napravi mi niz od jedan do 4

myvar

myvar= 1:10

myvar = sample(1:100, 10) #uzorkuj mi iz opsega od 1 do 100, na slucajan nacin 10 brojeva

mybinary = sample(1:2, 10, replace=T)

mydf = data.frame(myvar,mybinary) #spoj mi u data frame myvar i mybinary

mydf

colnames (mydf) = c("tezina", "pol")

mydf

# 3) Tipovi podataka

class(2) #provera pojedinacnog podatka, ili varijable

class (2.55)

class(2L)

class ("musko")

class (TRUE)

class ("TRUE") #sa navodnicima smo mu rekli da je karakter, to jest string varijabla

str (mydf) #provera vrste podataka za citav data frame

# promena vrste podatka

mydf$pol = as.factor(mydf$pol)

class(mydf$pol)
str(mydf)


# 4) Deskriptivna statistika

mean(mydf$tezina)

sd(mydf$tezina)

hist(mydf$tezina)

tapply (mydf$pol, mydf$tezina, mean)

with (mydf, tapply (pol, tezina, mean))

t.test (tezina ~ pol, data=mydf)

# 5) cuvanje data frame-a na disku

# Ne zaboravite, podaci ce vam se sacuvati u radnom direktorijumu - getwd() - je komanda da vidite gde je radni
# direktorijum, a - setwd -  je da se podesi zeljeni radni direktorijum
# na primer, podesicemo ga da bude na desktopu

setwd("c:/Users/user/Desktop")

write.csv(mydf, "podaci vezba1.csv")

write.table(mydf, file="podaci vezba1.txt", append = FALSE, sep = "\t", eol="\n", col.names=TRUE, row.names=FALSE)

##### UCITAVANJE PODATAKA ####

# tradicionalno - preko R konzole
# Ne zaboravite da podesite work direktorijum tamo gde vam se nalaze podaci, pa tek onda moze da 
# radi komanda read.table

read.table ("podaci vezba1.txt", sep="\t", header = T)

dat = read.table ("podaci vezba1.txt", sep="\t", header = T) #sa dat, nazovemo matricu

# preko R studija, idemo na import data set


#### SREDJIVANJE PODATAKA ####


