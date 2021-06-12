?load
load("data/wszystkieOferty080621.RData")
nrow(wszystkieOferty080621)
View(wszystkieOferty080621)

#wyswietlamy statystyki
summary(wszystkieOferty080621)
str(wszystkieOferty080621)

#czysczenie danych - zmiana na liczby, kategoryzacja i usunięcie uszkodzonych danych
library(tidyverse)

wszystkieOferty <- wszystkieOferty080621

#MUTATE
#nowy dataframe z uzyciem pipe - wymieniamy znaki wyrazeniem regularnym i zamieniamy na numeric
#wszystkieOferty <- wszystkieOferty080621%>%mutate(cena=   as.numeric(cena%>%str_replace_all("[^\\d,]","")%>%str_replace_all(",",".")) )
wszystkieOferty <- wszystkieOferty%>%mutate(cena=   as.numeric(cena%>%str_replace_all("[^\\d,]", "")%>%str_replace_all(",",".")) )

# z odwróconymi apostrofami???
wszystkieOferty$'Marka pojazdu'
View(wszystkieOferty)
str(wszystkieOferty)

#zmienic: przebieg, pojemnosc, moc, rok produkcji
wszystkieOferty <- wszystkieOferty%>%mutate(Przebieg=   as.numeric(Przebieg%>%str_replace_all("[^\\d,]", "")%>%str_replace_all(",",".")) )
wszystkieOferty <- wszystkieOferty%>%mutate(`Pojemność skokowa`=   as.numeric(`Pojemność skokowa`%>%str_replace_all("[^\\d,]", "")%>%str_replace_all(",",".")) )
wszystkieOferty <- wszystkieOferty%>%mutate(Moc=   as.numeric(Moc%>%str_replace_all("[^\\d,]", "")%>%str_replace_all(",",".")) )
wszystkieOferty <- wszystkieOferty%>%mutate(`Rok produkcji`=   as.numeric(`Rok produkcji`%>%str_replace_all("[^\\d,]", "")%>%str_replace_all(",",".")))

#FILTER - przecinek zastepuje i oraz & | i !
#trzeba dodac is.na() bo inaczej wyrzuci wszystkie ogloszenia z NA
#wszystkieOfertyTest<- wszystkieOferty%>%filter(waluta=="PLN", (Bezwypadkowy=='Tak'))
#nrow(wszystkieOfertyTest)
wszystkieOferty<- wszystkieOferty%>%filter(waluta=="PLN", (Bezwypadkowy=='Tak') | is.na(Bezwypadkowy))
nrow(wszystkieOferty)

wszystkieOferty<- wszystkieOferty%>%filter(waluta=="PLN", (Uszkodzony!='Tak') | is.na(Uszkodzony))
nrow(wszystkieOferty)

#SELECT - wybieramy tylko kolumny, ktore nas interesuja
summary(wszystkieOferty)
#wszystkieOferty<-wszystkieOferty%>%select(cena,`Marka pojazdu`,`Model pojazdu`,`Rok produkcji`,Przebieg,`Pojemność skokowa`,`Rodzaj paliwa`,Moc,`Skrzynia biegów`,Typ)
wszystkieOferty<-wszystkieOferty%>%select(-waluta,-Kategoria)
wszystkieOferty<-wszystkieOferty%>%select(cena:Kolor,Stan)
wszystkieOferty<-wszystkieOferty%>%select(-`Liczba drzwi`)
wszystkieOferty<-wszystkieOferty%>%select(-`Liczba miejsc`,-Napęd)
summary(wszystkieOferty)

#zamien nazwy kolumn ze spacjami na podloga _
names(wszystkieOferty) <- gsub(" ", "_", names(wszystkieOferty))

#Powinnismy jeszcze przed odpaleniem funkcji wybrac markę. Bo została zmienna factor z modelami i ma dużo leveli
wszystkieOferty%>%group_by(`Marka_pojazdu`)%>%summarise(n=n())%>%arrange(desc(n))

#wybieramy jedna marke Volkswagen
wszystkieOfertyVW<- wszystkieOferty%>%filter(Marka_pojazdu=='Volkswagen')
wszystkieOfertyVW<-wszystkieOfertyVW%>%select(-`Marka_pojazdu`)%>%droplevels()
nrow(wszystkieOfertyVW)
str(wszystkieOfertyVW)

#install.packages("VIM")
library(VIM)
aggr(wszystkieOfertyVW, numbers=TRUE,
     sortVars=TRUE,
     labels=names(data),
     cex.axis=.7)

#install.packages("mice")
library(mice)
md.pattern(wszystkieOfertyVW)




#funkcja ze stackoverflow
#install.packages("rcompanion")
require(rcompanion)
library(rcompanion)
#mixed_assoc(wszystkieOfertyVW)
View(mixed_assoc(wszystkieOfertyVW))


#install.packages("kknn")
#install.packages("caret")
#install.packages("mlr")

library(caTools)
library(randomForest)
library(rpart)
library(e1071)
library(kknn)
library(caret)
#library(mlr)


set.seed(123)
sample<-sample.split(Y=wszystkieOfertyVW,SplitRatio = .75)
trains<-subset(wszystkieOfertyVW,sample=TRUE)
tests<-subset(wszystkieOfertyVW,sample=FALSE)
?randomForest
#tworzymy po wszystkich zmiennych w zbiorze
regrRF<-randomForest(cena~.,data=trains)



# Calculate a pairwise association between all variables in a data-frame. In particular nominal vs nominal with Chi-square, numeric vs numeric with Pearson correlation, and nominal vs numeric with ANOVA.
# Adopted from https://stackoverflow.com/a/52557631/590437
mixed_assoc = function(df, cor_method="spearman", adjust_cramersv_bias=TRUE){
  df_comb = expand.grid(names(df), names(df),  stringsAsFactors = F) %>% set_names("X1", "X2")
  
  is_nominal = function(x) class(x) %in% c("factor", "character")
  # https://community.rstudio.com/t/why-is-purr-is-numeric-deprecated/3559
  # https://github.com/r-lib/rlang/issues/781
  is_numeric <- function(x) { is.integer(x) || is_double(x)}
  
  f = function(xName,yName) {
    x =  pull(df, xName)
    y =  pull(df, yName)
    
    result = if(is_nominal(x) && is_nominal(y)){
      # use bias corrected cramersV as described in https://rdrr.io/cran/rcompanion/man/cramerV.html
      cv = cramerV(as.character(x), as.character(y), bias.correct = adjust_cramersv_bias)
      data.frame(xName, yName, assoc=cv, type="cramersV")
      
    }else if(is_numeric(x) && is_numeric(y)){
      correlation = cor(x, y, method=cor_method, use="complete.obs")
      data.frame(xName, yName, assoc=correlation, type="correlation")
      
    }else if(is_numeric(x) && is_nominal(y)){
      # from https://stats.stackexchange.com/questions/119835/correlation-between-a-nominal-iv-and-a-continuous-dv-variable/124618#124618
      r_squared = summary(lm(x ~ y))$r.squared
      data.frame(xName, yName, assoc=sqrt(r_squared), type="anova")
      
    }else if(is_nominal(x) && is_numeric(y)){
      r_squared = summary(lm(y ~x))$r.squared
      data.frame(xName, yName, assoc=sqrt(r_squared), type="anova")
      
    }else {
      warning(paste("unmatched column type combination: ", class(x), class(y)))
    }
    
    # finally add complete obs number and ratio to table
    result %>% mutate(complete_obs_pairs=sum(!is.na(x) & !is.na(y)), complete_obs_ratio=complete_obs_pairs/length(x)) %>% rename(x=xName, y=yName)
  }
  
  # apply function to each variable combination
  map2_df(df_comb$X1, df_comb$X2, f)
}

