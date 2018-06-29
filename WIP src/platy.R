setwd("/home/peto/Documents/rprediction")
getwd()
#install.packages("xgboost")
library(xgboost)
#install.packages("Matrix")
library("Matrix")
library("tm")
library(dplyr)
#install.packages("tidyverse")
library(tidyverse)
library(tidytext)
library(xgboost)
library(tidyr)
library("Matrix")
library(xgboost)
library(tidyverse)
library(RWeka)
library("tm")
library("textcat")

#tymto si nacitas treningovy dataset, vybral som ten s potrebnymi features
data <- read.csv(file.path("job-salary-prediction", "Train_rev1.csv"), nrows=200)
#vypomoc pri ziskavani skutocne nahodne zvolenych riadkov
set.seed(1234)
#premennu uchovavajucu train prepisujeme, odteraz udrziava sample z datasetu
data <- data[sample(1:nrow(data)), ]
#vyber features/textu pre n-gramizaciu
this<-data.frame(cbind(data$Title,data$Category,data$FullDescription,data$SalaryNormalized))
#na to aby som oklamal niektore funkcie a aby som nemusel cast-ovat
write.csv(this, "this.csv")
#musim si ho ale nacitat, som vo svojom Work. directory, neuvadzam cestu
this <- read.csv("this.csv")
this
len <- NROW(this)
len
linecounts <- data.frame(file_name = c("this.csv"),
                         line_count = c(len))
library(pander)
pandoc.table(linecounts, style = 'rmarkdown')

library(devtools)
library(slam)
library(textcat)
#pozri sa v kolkych jazykoch je nas treningovy dataset
table(textcat(this))

#funkcia vracia len ukazovatel na funkciu, volaj ju cez TrigramTokenizer()
TrigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))
#funkcia vracia len ukazovatel na funkciu, volaj ju cez BigramTokenizer()
BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
#stvorgramy
QuatroTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 4, max = 4))
#tu su trigramy
trigramy <- TrigramTokenizer(this)
#tu su bigramy
bigramy <- BigramTokenizer(this)
#tu su stvorgramy
stvorgramy <- QuatroTokenizer(this)
library(tokenizers)
library(dplyr)
#data.frame kvoli lahsej manipulacii s obsahom a kvoli buducemu
#pouzitiu v argumentoch!
trigramcounts <- data.frame(table((trigramy)))
trigramcounts$Freq #pocetnosti vyskytov jednotlivych trigramov
trigramcounts$Var1 #VSETKY trigramy

bigramcounts <- data.frame(table((bigramy)))
bigramcounts$Var1 #VSETKY bigramy
bigramcounts$Freq #pocetnosti vyskytov jednotlivych bigramov

stvorgramcounts <- data.frame(table((stvorgramy)))
stvorgramcounts$Var1 #VSETKY stvorgramy
stvorgramcounts$Freq #pocetnosti vyskytov jednotlivych stvorgramov
#vidis, ze niektore stvorice slov sa v texte nachadzaju viacerokrat

#najdime si trojice slov (trigramy) tykajuce sa platu
findsalary_in_3<-grep("salary",trigramcounts$Var1,value=TRUE,invert=FALSE,ignore.case=TRUE)
findsalary_in_3 #vsetky trigramy obsahujuce slovo salary
#najdime si dvojice slov (bigramy) tykajuce sa platu
find_salary_in_2<-grep("salary",bigramcounts$Var1,value=TRUE,invert=FALSE,ignore.case=TRUE)
find_salary_in_2 #vsetky bigramy obsahujuce slovo salary
#najdime si stvorice slov (stvorgramy) obsahujuce slovo salary
find_salary_in_4<-grep("salary",stvorgramcounts$Var1,value=TRUE,invert=FALSE,ignore.case=TRUE)
find_salary_in_4 #vsetky stvorgramy obsahujuce slovo salary
library(quanteda)
library(tau)
library(plyr)
library(ggplot2)
library(cowplot)
#toto nam vypocita kolko krat sa nachadza kazdy trigram
#obsahujuci slovo salary v texte
trigram_freq <- textcnt(findsalary_in_3, n=3L, method = "string")
trigram_freq
#prvy index je pomocny, druhy index znaci s akym n-gramom narabame
#je tu 3-ka, teda s trigramami
x13 <- data.frame(poc_vyskytu = unclass(trigram_freq), poc_znakov = nchar(names(trigram_freq)))
#prvy index je 2, pretoze som v druhom kroku
x23 <- data.frame(trigramy = rownames(x13), poc_vyskytu = x13$poc_vyskytu, poc_znakov = x13$poc_znakov)
x33 <- arrange(x23, desc(poc_vyskytu)) #necham si ich zoradit, lebo chcem barplot
#najcastejsie sa vyskytujucich trigramov so slovom salary
x33 #tabulka ukazujuca trigramy obsahujuce slovo salary
#plus ich pocetnost v texte a pocet znakov, z ktorych sa tieto
#tri slova skladaju
#do konzoly si necham vypisat len cast data.frame-u
head(x33)
trigramov_10 <- head(x33, 10) #pozor uz su zoradene! Vidime najpocetnejsich 10
g3 <- ggplot(trigramov_10 , aes(x = reorder(trigramy, -poc_vyskytu), y = poc_vyskytu, fill = poc_znakov))
g33 <- g3 + geom_bar(stat = "identity") + xlab("Trigramy") + ylab("Pocetnost vyskytu") + ggtitle("10 najcastejsich trigramov") + theme(axis.text.x = element_text(angle = 45, hjust = 1))
library(gridExtra)
grid.arrange(g33, ncol=1)

#vytvorim si frekvencnu tabulku stvorgramov obsahujucich slovo salary
#chcem v nej vidiet pocetnost takych stvorgramov v texte a pocet znakov 
#z ktorych sa skladaju TAKETO stvorgramy!
stvorgram_freq <- textcnt(find_salary_in_4, n = 4L, method = "string")
x14 <- data.frame(poc_vyskytu = unclass(stvorgram_freq), poc_znakov = nchar(names(stvorgram_freq)))
x24 <- data.frame(stvorgramy = rownames(x14), poc_vyskytu = x14$poc_vyskytu, poc_znakov = x14$poc_znakov)
x34 <- arrange(x24, desc(poc_vyskytu))
head(x34) #uz su zoradene, uvidis najpocetnejsie!

stvorgramov_10 <- head(x34, 10)
stvorgramov_10

g4 <- ggplot(stvorgramov_10 , aes(x = reorder(stvorgramy, -poc_vyskytu), y = poc_vyskytu, fill = poc_znakov))
g44 <- g4 + geom_bar(stat = "identity") + xlab("Stvorgramy") + ylab("Pocetnost") + ggtitle("10 najpocetnejsich stvorgramov") + theme(axis.text.x = element_text(angle = 45, hjust = 1))
library(gridExtra)
grid.arrange(g44, ncol=1)

#robim to iste co vyssie len s bigramami
bigram_freq <- textcnt(find_salary_in_2, n = 2L, method = "string")
x12 <- data.frame(poc_vyskytu = unclass(bigram_freq), poc_znakov = nchar(names(bigram_freq)))
#vsimaj si, ze dodavam trigramy ako novy stlpec, aby sme videli co pocitame
x22 <- data.frame(bigramy = rownames(x12), poc_vyskytu = x12$poc_vyskytu, poc_znakov = x12$poc_znakov)
#kvoli citatelnejsiemu vypisu predchadzajuceho data.frame-u
x32 <- arrange(x22, desc(poc_vyskytu))
#do konzoly si necham vypisat len cast data.frame-u
head(x32)

bigram_10 <- head(x32, 10)
bigram_10

g2 <- ggplot(bigram_10 , aes(x = reorder(bigramy, -poc_vyskytu), y = poc_vyskytu, fill = poc_znakov))
g22 <- g2 + geom_bar(stat = "identity") + xlab("Bigramy") + ylab("Pocetnost") + ggtitle("10 najpocetnejsich bigramov") + theme(axis.text.x = element_text(angle = 45, hjust = 1))
library(gridExtra)
grid.arrange(g22, ncol=1)


#ideme si vypocitat korelacie medzi nasimi bigramami,
#trigramami a stvorgramami OBSAHUJUCIMI SLOVO SALARY!
library(plyr)
library(dplyr)
#bigramy v trigramoch
poc_stobigramovsosalary <- x32$poc_vyskytu[1:100]
x32$bigramy
poc_stobigramovsosalary 
data.frame(x32$bigramy, x32$poc_vyskytu) #pocetnosti vyskytu bigramov so slovom salary v texte
list_bigramov_so_salary <- as.character(x32$bigramy[1:100])
list_bigramov_so_salary
zasobnik <- array()
zasobnik[1]
i=1

for(i in 1:100){
  #x33 teda hladame bigramy v trigramoch obsahujucich slovo salary
  #POZOR! Text na gramizaciu bol vybrany!
  ten_bigram <- filter(x33, grepl(paste("^", list_bigramov_so_salary[i],sep =""), x33$trigramy))
  ten_bigram
  #ten_bigram nam vrati vsetky trigramy obsahujuce bigram s indexom [i]
  #obsahujuci slovo salary
  #znak ^ je pre znackovanie hladanych bigramov
  zasobnik[i] <- c(sum(ten_bigram$poc_vyskytu))
  as.integer(zasobnik[i])
  #ten_bigram s indexom [i] sa nachadza v trigramoch celkovo 
  #poc_vyskytu krat
  #vyhladaj si vsetkych 100 bigramov v trigramoch
}  

bigramy_v_trigramoch_frame <- data.frame(poc_vyskytu_bigr = x32$poc_vyskytu[1:100],list_bigramov = list_bigramov_so_salary, v_trigramoch = zasobnik)
bigramy_v_trigramoch_frame #samozrejme ze si h


library(plyr)
library(dplyr)
#trigramy v stvorgramoch
poc_stotrigramovsosalary <- x33$poc_vyskytu[1:100]
data.frame(x33$trigramy, x33$poc_vyskytu)
poc_stotrigramovsosalary #pocetnosti vyskytu trigramov so slovom salary v texte
list_trigramov_so_salary <- as.character(x33$trigramy[1:100])
list_trigramov_so_salary
zasobnik <- array()
for(i in 1:100){
  ten_trigram_x <- filter(x34, grepl(paste("^", list_trigramov_so_salary[i],sep =""), x34$stvorgramy))
  zasobnik[i] <- c(sum(ten_trigram_x$poc_vyskytu))
}  

trigramy_v_stvorgramoch_frame <- data.frame(poc_vysk_trigramov = x33$poc_vyskytu[1:100], trigramy = list_trigramov_so_salary, trigram_v_stvorgrame = zasobnik)
trigramy_v_stvorgramoch_frame


par(mfrow = c(2,2))
plot(bigramy_v_trigramoch_frame$poc_vyskytu_bigr, bigramy_v_trigramoch_frame$v_trigramoch,
     pch=16, cex=1.3, col="blue",
     xlab ="Pocet bigramov so salary v texte", ylab ="suma poctu vyskytov vybraneho bigramu v trigramoch")

plot(trigramy_v_stvorgramoch_frame$poc_vysk_trigramov, trigramy_v_stvorgramoch_frame$trigram_v_stvorgrame,
     pch=16, cex=1.3, col="blue",
     xlab ="Pocet trigramov so salary v texte", ylab ="suma poctu vyskytov vybraneho trigramu v stvorgramoch")

library(rgp)
options(digits = 5)
stepsGenerations <- 1000
initialPopulation <- 500
Steps <- c(10)
y<-as.double(bigramy_v_trigramoch_frame$v_trigramoch)
x<-as.double(bigramy_v_trigramoch_frame$poc_vyskytu_bigr)
model_df <- data.frame(y, x)
newFuncSet <- functionSet("+","-","*", "/","sqrt", "log", "exp") # ,, )
gpresult <- symbolicRegression(y ~ x, 
                               data=model_df, functionSet=newFuncSet,
                               populationSize=initialPopulation,
                               stopCondition=makeStepsStopCondition(stepsGenerations))




bf <- gpresult$population[[which.min(sapply(gpresult$population, gpresult$fitnessFunction))]]
wf <- gpresult$population[[which.max(sapply(gpresult$population, gpresult$fitnessFunction))]]

bf1 <- gpresult$population[[which.min((gpresult$fitnessValues))]]
plot(x,y)
lines(x, bf(x), type = "l", col="blue", lwd=3)

library(rgp)
options(digits = 5)
stepsGenerations <- 1000
initialPopulation <- 500
Steps <- c(10)
y<-as.double(trigramy_v_stvorgramoch_frame$poc_trigramov)
x<-as.double(trigramy_v_stvorgramoch_frame$suma_poctu_vyskytov)
model_df <- data.frame(y, x)
newFuncSet <- functionSet("+","-","*", "/","sqrt", "log", "exp") # ,, )
gpresult <- symbolicRegression(y ~ x, 
                               data=model_df, functionSet=newFuncSet,
                               populationSize=initialPopulation,
                               stopCondition=makeStepsStopCondition(stepsGenerations))




bf <- gpresult$population[[which.min(sapply(gpresult$population, gpresult$fitnessFunction))]]
wf <- gpresult$population[[which.max(sapply(gpresult$population, gpresult$fitnessFunction))]]

bf1 <- gpresult$population[[which.min((gpresult$fitnessValues))]]
plot(x,y)
lines(x, bf(x), type = "l", col="blue", lwd=3)


phrase <- function() {
    s <- readline(prompt="Enter a phrase: ")
    if(!grepl("^[A-B]+$",s))
    {
      return(phrase())
    }
    
    return(phrase(s))
  }
  
  print(phrase())
# phrase= "My salary will be"
phrase_length <- lengths(gregexpr("\\W+", phrase)) + 1
phrase_length
zasoba <- array()
zasoba[1]
i=1 #iba pre krokovanie cyklu
ConditionalTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = phrase_length, max = phrase_length))
dictionary <- ConditionalTokenizer(this)
dictionary <- data.frame(dictionary)
dictionary$dictionary
nasloSa <- array()
nasloSa[1]
#skuska
#citame vetu odzadu!
for (i in phrase_length:1) {
zasoba[i] <- paste0(word(phrase, ((phrase_length - 1):phrase_length)), collapse = " ")
zasoba[i] #posledna dvojica slov zadana pouzivatelom
nasloSa[i] <- filter(dictionary,grep("[[:digit:]]",dictionary$dictionary, value=TRUE), dictionary)
nasloSa[i] #vsetky ngramy dlzky "pocet slov zadanych do konzoly" 
#obsahujucich posledne dve slova zadane pouzivatelom
?xgb.DMatrix
?matrix
?textcnt
class(nasloSa[i])
datovy_vektor <- textcnt(paste( unlist(nasloSa[i]), collapse=''),method="string") #konecne, fu
datovy_vektor
#matrix vyzaduje optional data vector
frekvencie <- matrix(datovy_vektor, nrow = NROW(datovy_vektor), ncol=1,byrow=FALSE)
class(frekvencie)
#xgb.DMatrix vyzaduje zase xgboost
#prvy param 'data' musi byt dgCMatrix alebo matrix
frekvencie <- lapply(frekvencie, as.numeric)
class(frekvencie)
frekvencie <- unlist(frekvencie)
class(frekvencie) #konecne numeric! Boha ca!
zlepsenie_bind <- log(frekvencie, base=exp(1)) #zlepsenie modelu cez log
specialna_matica <- xgb.DMatrix(as.matrix(cbind(frekvencie)),label=zlepsenie_bind) #este raz a porazi ma
specialna_matica
bstDMatrix <- xgboost(data = specialna_matica, max.depth = 2, eta = 1, nthread = 2, nround = 2, objective = "reg:linear")
?xgboost

}



