#### Kod prilagoden za radionicu u Banja Luci, april, 2019 ####

#####LEPmler2018_cas3: Uvod u analizu mesovitih efekata### ovo je naziv orginalnog skripta

# Za pocetak, ucitacemo potrebne pakete


library(lme4) # da bismo pravili modele
library(lmerTest) # da nam budu prikazane p vrednosti
library(ggplot2) # za grafikone
library(gridExtra) # za uredjivanje visestrukih grafikona
library(languageR) # za razne stvari
library(lattice) # za grafikone

# I ucitacemo jedan dataframe

dat.im=read.table("vezbe 5.txt",sep="\t",T)

# proverimo dimenzije
dim(dat.im)


# mali uvid u podatke
head(dat.im)

# napravimo uvid u strukturu podataka
str(dat.im)

# zadrzimo u data frame-u samo reci

dat.im = dat.im[dat.im$Leksikalnost == "word",]

# zadrzimo u data frame-u samo tacne odgovore

dat.im = dat.im[dat.im$Error_code == "C",]

# proverimo da li je ZV normalno distribuirana

#g1 = ggplot(dat.im, aes(RT)) + geom_density()
#g2 = ggplot(dat.im, aes(sample=RT)) +
#  stat_qq() + stat_qq_line()

qqnorm(dat.im$RT)

# za sada cemo primeniti log transformaciju RT


dat.im$RT = log(dat.im$RT)

# opet proverimo distribuciju
qqnorm(dat.im$RT)


# transformisemo frekvenciju reci, jer znamo da stoji u log odnosu
# sa RT (SETITE SE USLOVA O LINEARNOSTI)
# kasnije cemo i formalno proveravati da li je odnos linearan

dat.im$frekv = log(dat.im$Frekvencija)

qqnorm(dat.im$Frekvencija)

qqnorm(dat.im$frekv)


# pored toga, kontinuirane prediktore treba centrirati na nulu
# zbog smislenosti intercepta
# a jos je bolje normalizovati vrednosti:

dat.im$frekv.sirovo = dat.im$frekv #da bismo sacuvali sirove frekvencije u data frameu
dat.im$frekv = scale(dat.im$frekv) #normalizovanje

# da vidimo sta smo uradili sa frekv:

qqnorm(dat.im$frekv.sirovo)
qqnorm(dat.im$frekv) #sveli na (0,1)


mean(dat.im$frekv.sirovo)

exp(mean(dat.im$frekv.sirovo))
round(mean(dat.im$frekv),5)

# skaliramo i broj znacenja
dat.im$NoS = scale(dat.im$NoS)

# gradenje modela sa slucajnim efektima
# pocinjemo sa random interceptom i variranjem ispitanika, jer da nissta ne znamo 
# o nacrtu, znamo da se ispitanici razliku medusobno po vecini svojstava

lmer1 = lmer( RT ~ 1 + (1|Subject), data = dat.im)


# pored toga sto ocekujemo da se ispitanici razlikuju po brzini,
# ocekujemo i da vreme reagovanja nece biti isto za sve reci

# napravimo model koji informisemo o tome da ocekujemo razlicit intercept za
# svaku rec, tj. ocekujemo da se reci razlikuju medjusobno po
# brzini kojom se reaguje na njih

# posto nismo prikazali sve reci srpskog jezika, a dodatno,
# zelimo da svoje nalaze generalizujemo na citavu populaciju reci naseg jezika
# ni reci ne mozemo tretirati kao fiksne efekte, vec kao slucajne

lmer2 = lmer( RT ~ 1 + (1|Rec), data = dat.im)


# mozemo da napravimo model koji istovremeno informisemo da ocekujemo 
# i razlike izmedju ispitanika i razlike izmedje reci

lmer3 = lmer( RT ~ 1 + (1|Subject) + (1|Rec), data = dat.im)

summary(lmer3)


# mozemo i da proverimo da li je opravdano ukljuciti svaki od 
# ova dva slucajna efekta
# setite se - ovo je "data driven" pristup


# uporedimo model koji sadrzi samo ispitanike i 
# model koji sadrzi i ispitanike i stimuluse
# da bismo proverili da li je opravdano ukljuciti stimuluse kao random efekat

anova(lmer1, lmer3)

# jeste

# uporedimo model koji sadrzi samo stimuluse i 
# model koji sadrzi i ispitanike i stimuluse
# da bismo proverili da li je opravdano ukljuciti ispitanike kao random efekat

anova(lmer2, lmer3)

# jeste

# dakle, model sa oba izvora slucajnih efekata je opravdan 
# i dizajnom (dva izvora zavisnosti merenja: ispitanici i reci)
# i podacima

# KAKO DODAJEMO FIKSNE EFEKTE?

# na isti nacin kao u obicnim linearnim modelima:

lmer4 = lmer( RT ~ frekv + (1|Subject) + (1|Rec), data = dat.im)

# Da li dodavanje frekvencije kao fiksnog efekta cini da model 
# bolje opisuje podatke? Da li je opravdan podacima ili nepotrebno usloznjava model?
# Nekontrolisano dodavanje prediktora moze da dovede do tzv. overfitting-a
summary(lmer4)
anova(lmer3, lmer4)

# vidimo da je opravdano ukljuciti frekvenciju
# model koji nju sadrzi ima manji AIC, manji BIC i veci loglikelyhood

# Tek kad utvrdimo da dodavanje prediktora čini model opravdano boljim
# gledamo koeficijente iz modela

summary(lmer4)

# STA DOBIJAMO KAD PRIKAZEMO REZIME MODELA

# Prve linije daju osnovne podatke o algoritmu, formuli koju smo primenili i podacima
# Potom dobijamo REML (Restricted Maximum Likelyhood) kriterijum konvergiranja 
# (koji moze da posluzi kao indeks za goodness of fit te i za poredjenje modela)

# Dobijamo osnovne podatke o distribuciji reziduala 
# (za sada se cini da je simetricna, kasnije cemo to dalje proveravati)

# Dolazimo do dela ispisa u kom su prikazani parametri za slucajne efekte
# Rekli smo da se za njih procenjuje varijansa/standardna devijacija
# Vidimo procenu za slucajni intercept za reci, procenu za slucajni intercept za
# ispitanike i rezidual
# Rezidual je ono sto smo u obicnom linearnom modelu oznacavali kao gresku
# (ono ciju strukturu ne razumemo).
# Mozemo da kazemo i da smo gresku iz lm razdvojili 
# na deo ciju strukturu razumemo (razlicite prosecne brzine ispitanika i reci)
# i deo ciju strukturu ne razumemo (gresku)

# Na kraju, prikazani su koeficijenti za FIKSNE EFEKTE.

# Mi imamo jedan kontinuirani prediktor.
# To znaci da nam intercept kaze koju vrednost ima ZV kada je vrednost NV jednaka nuli.
# Da bi ovo bilo smisleno, centrirali smo prediktor na nulu, 
# Sto znaci da nula sada oznacava prosecnu frekvencu, 
# te dobijamo podatak o vrednosti ZV (tj. RT) za prosecnu vrednost NV (tj. frekvence).

# Drugi koeficijent odnosi se na prediktor i govori nam za koliko se promeni vrednost ZV,
# kada se vrednost NV poveca za jedan. 
# Vidimo da je povecanje frekvence za jedno mesto na skali praceno 
# skracenjem vremena reakcije za 0.029, kao i da je ova promena statisticki znacajna

# parcijalni fiksni efekat prediktora mozemo ovako da dobijemo:

plotLMER.fnc(lmer4)
fitted(lmer4)


# ovako trazimo vrednosti fiksnih koeficijenata (tj. koeficijenata za fiksne efekte)

#fixef(lmer4)

# ovako trazimo vrednost random koeficijenata, koji se jos zovu i 
# BLUPs (Best Linear Unbiased Predictors)

#ranef(lmer4)

# da dobijemo vrednosti za koje treba korigovati intercept za svakog ispitanika:
#ranef(lmer4)$Subject

# da dobijemo vrednosti za koje treba korigovati intercept za svaku rec:
#ranef(lmer4)$Rec

# Mozemo i graficki da ih prikazemo

#print(dotplot(ranef(lmer4, condVar = TRUE))$Subject)	
#print(dotplot(ranef(lmer4, condVar = TRUE))$Rec)	


# isto to moze i ovako, da se malo prisetite prvog casa :)
#print(dotplot(ranef(lmer4, condVar = TRUE))[[2]])	
#print(dotplot(ranef(lmer4, condVar = TRUE))[[1]])	

# DA IZVEDEMO PREDIKCIJU ZA ISPITANIKA S9 ZA REČ ZVONO:

# Ovo su njihovi BLUPs:
# s9   0.129583215
# ZVONO  -0.0285428733



dat.im$RT.fitted = fitted(lmer4)

## Demonstracija slucajnog nagiba

# Medjutim, pored toga sto informisemo model o tome da ocekuje razlicita prosecna 
# vremena reagovanja od razlicitih ispitanika i za razlicite reci
# mozemo da se zapitamo i da li je neki fiksni efekat bas isti za sve ispitanike.

# da vidimo kako izgleda efekat frekvencije odvojeno za svakog ispitanika

ggplot(dat.im, aes(x=frekv, y=RT)) +  
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  facet_wrap(~Subject) 

# Dakle, rec je o slucajnim efketima koji poticu od 
# Mozemo da informisemo model o tome da ocekujemo razlicit efekat frekvencije 
# za razlicite ispitanike

lmer5 = lmer( RT ~ frekv + (1 + frekv|Subject) + (1|Rec), data = dat.im)

# proverimo da li je ovo opravdano podacima

anova(lmer4, lmer5)

# zapravo nije potreban slucajni nagib...

# Da pogledamo kako izgledaju brojke

summary(lmer5)

# Mozemo da primetimo dve stvari:
# 1) variranje za nagib frekvence je mnogo manje nego variranje za intercept 
#    za ispitanike ili reci
# 2) korelacija izmedju intercepta za ispitanike i nagiba efekta frekence jednaka je -1
#    Sto znaci da su ispitanici koji su bili brzi bili istovremeno i osetljiviji na
#    frekvenciju. Medjutim, to sto je korelacija ovako visoka je cesto znak da smo 
#    ukljucili nepotrebne parametre u model, 
#    Sto nam, uostalom, poredjenje dva modela i sugerise.

# da vidimo korelaciju izmedju intercepta za ispitanika i nabiga po ispitaniku:
plot(ranef(lmer5)$Subject)


# Medjutim, postoji glediste po kom variranje nagiba frekvence po ispitaniku treba ostaviti 
# u modelu, jer je opravdano nacrtom (cak i ako nije opravdano podacima), te doprinosi
# razresavanju problema zavisnih merenja.

# Ako se odlucimo da ostavimo ovu tzv. "slucajnu interakciju", mozemo da pokusamo 
# da iskljucimo koralaciju izmedju intercepta za ispitanika i nabiga po ispitaniku:


lmer6 = lmer( RT ~ frekv + (1 + frekv||Subject) + (1|Rec), data = dat.im)

# to smo postigli sa dve vertikalne linije, a mogli smo isto to i ovako:

lmer6 = lmer( RT ~ frekv + (1 |Subject) + (0 + frekv|Subject) + 
                (1|Rec), data = dat.im)


anova(lmer5, lmer6)

# vidimo da ni ovo nije opravdano podacima
# a kad pogledamo ispis, vidimo da je variarnje nagiba zaista blisko nuli

summary(lmer6)


# za fiksne efekte treba da proverimo i da li postoji nelinearna komponenta

lmer6n = lmer( RT ~ poly(frekv,degree=2,raw=T) + (1 + frekv||Subject) + (1|Rec), data = dat.im)

anova(lmer6, lmer6n)

# vidimo da ni ovo nije opravdano podacima
# model sa nelinearnim efektom Cak ima neSto loSiji fit

# pogledacemo ispis, tek da vidimo kako se izlazi na kraj sa nelinearnostima
# u linearnom modelu

# primetite da za prediktor frekv sada postoje dva koeficijenta
# prvi se odnosi na linearnu komponentu
# drugi se odnosi na kvadratnu kompenentu

summary(lmer6n)


# Na slican nacin na koji smo se pitali da li postoji variranje nagiba efekta
# frekvencije po ispitanicima, mozemo da se zapitamo i da li postoji analogno 
# variranje po recima.
# Medjutim, frekvencija nije ponovljena po recima, tj. jedna rec je uvek iste frekvencije
# pa bi ovo pitanje bilo besmisleno, tj. ne bi bilo opravdano nacrtom.
# To mozemo da ucinimo za neki prediktor koji je ponovljen po recima.
# U ovom slucaju, u te svrhe moze da nam posluzi varijabla 
# (koju sam napravila za potrebe demonstracije)
# koja se zove Brzina.ispitanika
# To je kategorijalna varijabla koja je napravljena tako sto su ispitanici podeljeni
# u dve grupe (brzi, spori) na osnovu medijane varijable SubjSpeed 
# što je prosecno vreme reakcije ispitanika u eksperimentu

# da pogledamo prvo da li su brzi ispitanici brzi na svim recima, kao i da li su
# podjednako brzi na razlicitim recima:

#ggplot(dat.im, aes(x=Brzina.ispitanika, y=RT)) +
# geom_point() +
# geom_smooth(method = "lm", se = TRUE) +
# facet_wrap(~Rec) 

# napravimo model u koji unesemo informaciju o tome da očekujemo
# razlike u odnosima između brzih i sporih ispitanika za različite reči
# ovo je dozvoljeno, pošto su svaku reč videli i brzi i spori ispitanici
# odnosno, faktor Brzina.ispitanika je ponovljen po rečima


lmer7 = lmer( RT ~ frekv + (1 + frekv||Subject) + 
                (1 + Brzina.ispitanika|Rec), 
                data = dat.im)

anova(lmer6, lmer7)

# ponovo, vidimo da ovo nije opravdano podacima
# a kad pogledamo rezime modela, vidimo i da je variranje vrlo nisko
# a korelacija ponovo veoma visoka:

summary(lmer7)

# ponovo iskljucimo korelaciju:

lmer8 = lmer( RT ~ frekv + (1 + frekv||Subject) + 
                (1 + Brzina.ispitanika||Rec), 
              data = dat.im)
anova(lmer6, lmer8)

# ponovo, vidimo da ovo nije opravdano podacima
# a kad pogledamo rezime modela, vidimo i da je variranje vrlo nisko

summary(lmer8)

# ovo je svakako bilo u svrhu ilustracije
# vracamo se na model lmer6

# na ovaj nacin mozemo da dodajemo nove prediktore
# za njih testiramo nelinearnosti, fiksne i slucajne interakcije
# opravdanost podacima

# medjutim, interpretacija fiksnih efekata u lmer-u je ista kao interpretacija
# u lm-u, kojom smo se bavili na pocetku
# stoga necemo ukljucivati nove prediktore u ovu analizu




# Za koeficijente iz modela mozemo da procenimo 95% intervale poverenja
# Vidimo da se nasi efekti uvek nalaze sa iste strane nule
confint(lmer6, method="Wald")


# Sada cemo da proverimo da li su prekrseni neki od preduslova za
# primenu linernog modela:

# napravimo kolonu sa predvidjenim vrednostima ZV:
dat.im$RT.fitted = predict(lmer6)

# ako nas zanima procenat objasnjene varijanse:

cor(dat.im$RT, dat.im$RT.fitted)^2


# napravimo kolonu sa rezidualima:
dat.im$RT.res = residuals(lmer6)

# plotujemo korelaciju izmedju fitovanih vrednosti i reziduala
# da proverimo da li postoji homogenost varijanse
# ovo treba da bude jedno lepo "jaje"
# klasican prikaz
plot(predict(lmer6),residuals(lmer6), xlab="reziduali",ylab="fitovane vrednosti",
     abline(0,0))
#prikaz u ggplotu
ggplot(dat.im, aes(x=RT.fitted, y=RT.res)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) 

# da proverimo da li se reziduali normalno distribuiraju
# ovo treba da bude što sličnije ravnoj liniji:
#klasican prikaz
qqnorm(residuals(lmer6))
qqline(residuals(lmer6)) #dodaje liniju na qq plot


# isto to moze i ovako, pa prikaze i skater i noramalnost :)
par(mfcol=c(1,2))
qqnorm(resid(lmer6))
plot(fitted(lmer6), resid(lmer6))
par(mfcol=c(1,1))



# sad cemo da izbacimo tacke sa velikim rezidualima
# da proverimo da li uticu previse na model
# refitujemo model na podskupu tacaka


lmer6t = lmer( RT ~ frekv + (1 + frekv||Subject) + (1|Rec), 
              data = dat.im, subset=abs(scale(resid(lmer6)))<2.5)

summary(lmer6t)


par(mfcol=c(1,2))
qqnorm(resid(lmer6t))
plot(fitted(lmer6t), resid(lmer6t))
par(mfcol=c(1,1))

# vidimo da sad reziduali izgledaju mnogo bolje
# efekti su opstali i kad smo se otarasili strcaka













