# Pravljenje novog data frame a kako bismo kreirali novi LM:

# visina tona ~ pol + uctivost  +  ??

#Imamo 2 fiksna efekta: pol i uctivost, pri cemu se uctivost ponavlja po ispitanicima

subj<-c(rep(1:6,8))
pol<-c(rep("m",24),rep("z",24))
uctivost<-c(rep(c(rep(c("uctiv","neuctiv"),times=c(6,6))), 4))
vis_glasa<-sample(160:260,48, replace=T)
mydf3<-data.frame(subj, pol, uctivost, vis_glasa)
mydf3

# subj pol uctivost vis_glasa
# 1     1   m    uctiv       243
# 2     2   m    uctiv       175
# 3     3   m    uctiv       258
# 4     4   m    uctiv       256
# 5     5   m    uctiv       172
# 6     6   m    uctiv       182
# 7     1   m  neuctiv       210
# 8     2   m  neuctiv       239
# 9     3   m  neuctiv       255
# 10    4   m  neuctiv       180
# 11    5   m  neuctiv       190
# 12    6   m  neuctiv       165

### KORISNO ### Brz uvid u strukturu podataka ####
#funkcija dim - ispitivanje dimenzionalnosti matrice
#funkcije str - uvid u podatke
#funkcija head - prvih 6 redova matrice
#funkcija tail - poslednjih 6 redova matrice

dim(mydf3)

#[1] 48  4

str(mydf3)

# 'data.frame':	48 obs. of  4 variables:
#   $ subj     : int  1 2 3 4 5 6 1 2 3 4 ...
# $ pol      : Factor w/ 2 levels "m","z": 1 1 1 1 1 1 1 1 1 1 ...
# $ uctivost : Factor w/ 2 levels "neuctiv","uctiv": 2 2 2 2 2 2 1 1 1 1 ...
# $ vis_glasa: int  243 175 258 256 172 182 210 239 255 180 ...

head(mydf3)

# subj pol uctivost vis_glasa
# 1    1   m    uctiv       243
# 2    2   m    uctiv       175
# 3    3   m    uctiv       258
# 4    4   m    uctiv       256
# 5    5   m    uctiv       172
# 6    6   m    uctiv       182 

tail(mydf3)

# subj pol uctivost vis_glasa
# 43    1   z  neuctiv       181
# 44    2   z  neuctiv       204
# 45    3   z  neuctiv       171
# 46    4   z  neuctiv       211
# 47    5   z  neuctiv       165
# 48    6   z  neuctiv       191


# Potrebno je da subjekte tretiramo kao faktor, kako bismo mogli da prikazemo variranja po ispitanicima

mydf3$subj<-as.factor(mydf3$subj)
is.factor(mydf3$subj)

#[1] TRUE

##Hajde da vidimo sta se desava sa variranjima slucajnih efekata (slika 17).
# Plotovanje variranja aritmetickih sredina visine glasa medu ispitanicima

plot(mydf3$subj,mydf3$vis_glasa, xlab="Broj ispitanika", ylab="Visina glasa (sredina)")


# Dodajemo u data frame i variranja koja poticu od razlicitih ajtema, kojih u ovom izmisljenom primeru ima 12, po 6 za svaku situaciju uctivosti.

item<-c(rep(1:12,4))
mydf4<-data.frame(subj, pol,uctivost, item, vis_glasa)
mydf4

# Plotujemo variranja sredina visine glasa po ajtemima (slika 18).

plot(mydf4$item,mydf4$vis_glasa, xlab="Broj ajtema", ylab="Visina glasa (sredina)")
