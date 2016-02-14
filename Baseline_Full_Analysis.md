# MBRP - BaseLine - Full Analysis
Leonardo Martins  
11 de fevereiro de 2016  

This is a public document with all scripts used  manuscript: 


All files used here are availible in a public repository licensed under MIT Licences and accessible by the following url:

https://github.com/crepeia/MBRP


#Preparing new analysis

#Loading required packages

```r
require(foreign) # Read data stored SPSS
require(car) #Recode Variables
require(psych) #Psychometrics
require(lavaan) #Confirmatory and SEM
require(semPlot) # Plots for SEM
require(semTools) # Comparing SEM models
require(ggplot2) # Plots
require(Hmisc)
```


```r
#Setting Directory
setwd("~/MBRP_R")

#Importing SPSS file .sav
base.dat <- read.spss("Base.sav", to.data.frame = T, use.missings = T)

#Recode Missing Data 888
for (i in c(1:350)) {
base.dat[,c(i)]<-sub("888", "NA", base.dat[,c(i)], ignore.case = FALSE, perl = FALSE, fixed = F, useBytes = FALSE)
}

#Recode Missing Data 777
for (i in c(1:350)) {
base.dat[,c(i)]<-sub("777", "NA", base.dat[,c(i)], ignore.case = FALSE, perl = FALSE, fixed = F, useBytes = FALSE)
}

#Creating a subset for analysis without cases excluded in our baseline
MBRP <- base.dat[grep("CORRI", base.dat$ETAPA), ]

#Selecting variable for this work 
MBRP_baseline <- MBRP[ ,c(9,11,12,19,24,245:252,265,266,295:298,299:311,325,326,347:350)]

MBRP_baseline$FFMQTOTAL<-as.numeric(MBRP_baseline$FFMQTOTAL)

#Removing NA from FFMQ Total
MBRP_baseline <- subset(MBRP_baseline, !is.na(MBRP_baseline$FFMQTOTAL))

#Summary 
summary(MBRP_baseline$FFMQTOTAL)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##      86     114     121     121     128     151
```

```r
#Recode Mindfulness FFMQ into High and Low
MBRP_baseline$FFMQgroup[MBRP_baseline$FFMQTOTAL< 121] <- "Baixo"
MBRP_baseline$FFMQgroup[MBRP_baseline$FFMQTOTAL>= 121] <- "Altos"
```

#Descriptive

```r
#Summary 
summary(MBRP_baseline$FFMQTOTAL)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##      86     114     121     121     128     151
```

```r
#FFMQ Groups (High and Low based on median cut off)
as.factor(MBRP_baseline$FFMQgroup)
```

```
##  [1] Baixo Altos Baixo Altos Altos Altos Altos Altos Baixo Altos Altos
## [12] Baixo Altos Baixo Altos Altos Baixo Altos Altos Baixo Baixo Altos
## [23] Baixo Altos Altos Altos Altos Baixo Baixo Altos Baixo Altos Altos
## [34] Altos Baixo Altos Altos Baixo Altos Altos Altos Baixo Altos Baixo
## [45] Altos Altos Baixo Baixo Altos Baixo Baixo Altos Altos Altos Altos
## [56] Baixo Altos Altos Altos Baixo Baixo Altos Baixo Baixo Baixo Baixo
## [67] Altos Altos Baixo Baixo Baixo Baixo Baixo Altos Baixo Altos Altos
## [78] Baixo Baixo Baixo Altos Altos Baixo Altos Altos Baixo Baixo Baixo
## [89] Baixo Baixo Baixo Baixo Altos Baixo Baixo Altos Altos Baixo
## Levels: Altos Baixo
```

```r
describe(MBRP_baseline$FFMQgroup)
```

```
## MBRP_baseline$FFMQgroup 
##       n missing  unique 
##      98       0       2 
## 
## Altos (51, 52%), Baixo (47, 48%)
```

```r
##Gender
as.factor(MBRP_baseline$X.1Gênero)
```

```
##  [1] Feminino  Feminino  Feminino  Feminino  Feminino  Feminino  Feminino 
##  [8] Feminino  Feminino  Masculino Feminino  Feminino  Feminino  Feminino 
## [15] Feminino  Masculino Feminino  Masculino Masculino Masculino Feminino 
## [22] Feminino  Feminino  Feminino  Masculino Feminino  Feminino  Feminino 
## [29] Feminino  Feminino  Masculino Feminino  Masculino Masculino Feminino 
## [36] Feminino  Feminino  Feminino  Feminino  Feminino  Feminino  Masculino
## [43] Masculino Feminino  Feminino  Masculino Feminino  Feminino  Feminino 
## [50] Masculino Masculino Feminino  Feminino  Feminino  Feminino  Masculino
## [57] Masculino Feminino  Feminino  Feminino  Feminino  Feminino  Feminino 
## [64] Feminino  Feminino  Masculino Feminino  Feminino  Feminino  Feminino 
## [71] Feminino  Feminino  Feminino  Feminino  Feminino  Feminino  Feminino 
## [78] Feminino  Feminino  Masculino Feminino  Feminino  Feminino  Feminino 
## [85] Feminino  Feminino  Feminino  Feminino  Feminino  Feminino  Feminino 
## [92] Masculino Feminino  Feminino  Feminino  Masculino Feminino  Feminino 
## Levels: Feminino  Masculino
```

```r
describe(MBRP_baseline$X.1Gênero)
```

```
## MBRP_baseline$X.1Gênero 
##       n missing  unique 
##      98       0       2 
## 
## Feminino  (78, 80%), Masculino (20, 20%)
```

```r
##Educational Study
as.factor(MBRP_baseline$X.7.1RECODEEscolaridade)
```

```
##  [1] 5 ANOS  DE ESTUDO ATÉ 8 ANOS    9 ANOS DE ESTUDO ATÉ 11 ANOS   
##  [3] 5 ANOS  DE ESTUDO ATÉ 8 ANOS    5 ANOS  DE ESTUDO ATÉ 8 ANOS   
##  [5] 9 ANOS DE ESTUDO ATÉ 11 ANOS    5 ANOS  DE ESTUDO ATÉ 8 ANOS   
##  [7] 5 ANOS  DE ESTUDO ATÉ 8 ANOS    NA                             
##  [9] SUPERIOR INCOMPLETO OU COMPLETO 9 ANOS DE ESTUDO ATÉ 11 ANOS   
## [11] 9 ANOS DE ESTUDO ATÉ 11 ANOS    0 ANOS DE ESTUDO ATÉ 4 ANOS    
## [13] SUPERIOR INCOMPLETO OU COMPLETO 9 ANOS DE ESTUDO ATÉ 11 ANOS   
## [15] 9 ANOS DE ESTUDO ATÉ 11 ANOS    SUPERIOR INCOMPLETO OU COMPLETO
## [17] 0 ANOS DE ESTUDO ATÉ 4 ANOS     0 ANOS DE ESTUDO ATÉ 4 ANOS    
## [19] 5 ANOS  DE ESTUDO ATÉ 8 ANOS    0 ANOS DE ESTUDO ATÉ 4 ANOS    
## [21] 9 ANOS DE ESTUDO ATÉ 11 ANOS    9 ANOS DE ESTUDO ATÉ 11 ANOS   
## [23] 0 ANOS DE ESTUDO ATÉ 4 ANOS     9 ANOS DE ESTUDO ATÉ 11 ANOS   
## [25] 9 ANOS DE ESTUDO ATÉ 11 ANOS    9 ANOS DE ESTUDO ATÉ 11 ANOS   
## [27] 9 ANOS DE ESTUDO ATÉ 11 ANOS    9 ANOS DE ESTUDO ATÉ 11 ANOS   
## [29] 9 ANOS DE ESTUDO ATÉ 11 ANOS    9 ANOS DE ESTUDO ATÉ 11 ANOS   
## [31] 0 ANOS DE ESTUDO ATÉ 4 ANOS     9 ANOS DE ESTUDO ATÉ 11 ANOS   
## [33] SUPERIOR INCOMPLETO OU COMPLETO 5 ANOS  DE ESTUDO ATÉ 8 ANOS   
## [35] 0 ANOS DE ESTUDO ATÉ 4 ANOS     SUPERIOR INCOMPLETO OU COMPLETO
## [37] 0 ANOS DE ESTUDO ATÉ 4 ANOS     0 ANOS DE ESTUDO ATÉ 4 ANOS    
## [39] 9 ANOS DE ESTUDO ATÉ 11 ANOS    5 ANOS  DE ESTUDO ATÉ 8 ANOS   
## [41] 9 ANOS DE ESTUDO ATÉ 11 ANOS    9 ANOS DE ESTUDO ATÉ 11 ANOS   
## [43] 0 ANOS DE ESTUDO ATÉ 4 ANOS     0 ANOS DE ESTUDO ATÉ 4 ANOS    
## [45] 9 ANOS DE ESTUDO ATÉ 11 ANOS    0 ANOS DE ESTUDO ATÉ 4 ANOS    
## [47] 0 ANOS DE ESTUDO ATÉ 4 ANOS     SUPERIOR INCOMPLETO OU COMPLETO
## [49] 5 ANOS  DE ESTUDO ATÉ 8 ANOS    9 ANOS DE ESTUDO ATÉ 11 ANOS   
## [51] 9 ANOS DE ESTUDO ATÉ 11 ANOS    0 ANOS DE ESTUDO ATÉ 4 ANOS    
## [53] 0 ANOS DE ESTUDO ATÉ 4 ANOS     9 ANOS DE ESTUDO ATÉ 11 ANOS   
## [55] SUPERIOR INCOMPLETO OU COMPLETO 9 ANOS DE ESTUDO ATÉ 11 ANOS   
## [57] SUPERIOR INCOMPLETO OU COMPLETO 9 ANOS DE ESTUDO ATÉ 11 ANOS   
## [59] SUPERIOR INCOMPLETO OU COMPLETO 0 ANOS DE ESTUDO ATÉ 4 ANOS    
## [61] 9 ANOS DE ESTUDO ATÉ 11 ANOS    9 ANOS DE ESTUDO ATÉ 11 ANOS   
## [63] SUPERIOR INCOMPLETO OU COMPLETO 9 ANOS DE ESTUDO ATÉ 11 ANOS   
## [65] 5 ANOS  DE ESTUDO ATÉ 8 ANOS    5 ANOS  DE ESTUDO ATÉ 8 ANOS   
## [67] SUPERIOR INCOMPLETO OU COMPLETO SUPERIOR INCOMPLETO OU COMPLETO
## [69] 5 ANOS  DE ESTUDO ATÉ 8 ANOS    SUPERIOR INCOMPLETO OU COMPLETO
## [71] 9 ANOS DE ESTUDO ATÉ 11 ANOS    5 ANOS  DE ESTUDO ATÉ 8 ANOS   
## [73] 0 ANOS DE ESTUDO ATÉ 4 ANOS     9 ANOS DE ESTUDO ATÉ 11 ANOS   
## [75] 9 ANOS DE ESTUDO ATÉ 11 ANOS    9 ANOS DE ESTUDO ATÉ 11 ANOS   
## [77] SUPERIOR INCOMPLETO OU COMPLETO 0 ANOS DE ESTUDO ATÉ 4 ANOS    
## [79] 9 ANOS DE ESTUDO ATÉ 11 ANOS    5 ANOS  DE ESTUDO ATÉ 8 ANOS   
## [81] SUPERIOR INCOMPLETO OU COMPLETO 0 ANOS DE ESTUDO ATÉ 4 ANOS    
## [83] 9 ANOS DE ESTUDO ATÉ 11 ANOS    0 ANOS DE ESTUDO ATÉ 4 ANOS    
## [85] 5 ANOS  DE ESTUDO ATÉ 8 ANOS    SUPERIOR INCOMPLETO OU COMPLETO
## [87] SUPERIOR INCOMPLETO OU COMPLETO 9 ANOS DE ESTUDO ATÉ 11 ANOS   
## [89] 9 ANOS DE ESTUDO ATÉ 11 ANOS    9 ANOS DE ESTUDO ATÉ 11 ANOS   
## [91] 9 ANOS DE ESTUDO ATÉ 11 ANOS    0 ANOS DE ESTUDO ATÉ 4 ANOS    
## [93] 9 ANOS DE ESTUDO ATÉ 11 ANOS    9 ANOS DE ESTUDO ATÉ 11 ANOS   
## [95] 9 ANOS DE ESTUDO ATÉ 11 ANOS    SUPERIOR INCOMPLETO OU COMPLETO
## [97] 9 ANOS DE ESTUDO ATÉ 11 ANOS    9 ANOS DE ESTUDO ATÉ 11 ANOS   
## 5 Levels: 0 ANOS DE ESTUDO ATÉ 4 ANOS     ...
```

```r
describe(MBRP_baseline$X.7.1RECODEEscolaridade)
```

```
## MBRP_baseline$X.7.1RECODEEscolaridade 
##       n missing  unique 
##      98       0       5 
## 
## 0 ANOS DE ESTUDO ATÉ 4 ANOS     (21, 21%) 
## 5 ANOS  DE ESTUDO ATÉ 8 ANOS    (15, 15%) 
## 9 ANOS DE ESTUDO ATÉ 11 ANOS    (43, 44%) 
## NA                             (1, 1%) 
## SUPERIOR INCOMPLETO OU COMPLETO (18, 18%)
```

```r
##Meditation
as.factor(MBRP_baseline$X.11.1Vocêpraticameditação)
```

```
##  [1] Não  Não  Não  Não  Não  Não  Não  Não  Sim  Não  Não  Não  Não  Não 
## [15] Não  Não  Não  NA   Não  Não  Não  Não  Não  Não  Não  Não  Não  Não 
## [29] Não  Sim  Não  Não  Não  Não  Não  Não  Não  Não  Não  Não  Não  Não 
## [43] Não  Não  Não  Não  Não  Não  Não  Não  Não  Não  Não  Não  Não  Não 
## [57] Não  Não  Não  Não  Não  Sim  Não  Não  Não  Não  Sim  Não  Não  Não 
## [71] Não  Não  Não  Não  Não  Não  Não  Não  Não  Não  Não  Não  Não  Não 
## [85] Não  Não  Não  Não  Não  Não  Não  Não  Não  Não  Não  Não  Não  Não 
## Levels: NA  Não  Sim
```

```r
describe(MBRP_baseline$X.11.1Vocêpraticameditação)
```

```
## MBRP_baseline$X.11.1Vocêpraticameditação 
##       n missing  unique 
##      98       0       3 
## 
## NA  (1, 1%), Não  (93, 95%), Sim  (4, 4%)
```

```r
#Age
MBRP_baseline$X.2Idade<-as.numeric(MBRP_baseline$X.2Idade)
summary(MBRP_baseline$X.2Idade)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   19.00   42.00   49.50   48.77   56.00   71.00
```

```r
sd(MBRP_baseline$X.2Idade)
```

```
## [1] 11.2761
```


```r
#Dataframe
MBRP_base <- MBRP_baseline[,c(39,1,14,33,34,16,17,20,21,22:32,35:38,6:13)]

#As dataframe
MBRP_base<-as.data.frame(MBRP_base)

#As factor
MBRP_base[,c(1)]<-as.factor(MBRP_base[,c(1)])

#As numeric
for (i in c(2:32)) {
MBRP_base[,c(i)]<-as.numeric(MBRP_base[,c(i)])
}

# x for correlation matrix
x<-MBRP_base[32]
```


#Comparing Means - Multiple t-tests 

#Tobacco Variables

```r
#Descriptive
lapply(MBRP_base[,c(2,3)], function(x) describeBy(x, group =MBRP_base$FFMQgroup))
```

```
## $Escore
## group: Altos
##   vars  n  mean   sd median trimmed  mad min max range skew kurtosis   se
## 1    1 50 14.86 6.77     14   14.47 4.45   3  33    30 0.55    -0.02 0.96
## -------------------------------------------------------- 
## group: Baixo
##   vars  n  mean   sd median trimmed  mad min max range skew kurtosis   se
## 1    1 47 15.98 5.66     16   16.05 4.45   5  27    22 -0.1    -0.55 0.83
## 
## $FAGERTRONTOTAL
## group: Altos
##   vars  n mean   sd median trimmed  mad min max range  skew kurtosis   se
## 1    1 51 6.06 2.35      6    6.22 2.97   0  10    10 -0.48    -0.66 0.33
## -------------------------------------------------------- 
## group: Baixo
##   vars  n mean  sd median trimmed  mad min max range  skew kurtosis   se
## 1    1 47 6.87 1.9      7    6.97 1.48   2  10     8 -0.48    -0.05 0.28
```

```r
#Test T
lapply(MBRP_base[,c(2,3)], function(x) t.test(x ~ MBRP_base$FFMQgroup))
```

```
## $Escore
## 
## 	Welch Two Sample t-test
## 
## data:  x by MBRP_base$FFMQgroup
## t = -0.88449, df = 93.748, p-value = 0.3787
## alternative hypothesis: true difference in means is not equal to 0
## 95 percent confidence interval:
##  -3.630156  1.392709
## sample estimates:
## mean in group Altos mean in group Baixo 
##            14.86000            15.97872 
## 
## 
## $FAGERTRONTOTAL
## 
## 	Welch Two Sample t-test
## 
## data:  x by MBRP_base$FFMQgroup
## t = -1.8915, df = 94.352, p-value = 0.06163
## alternative hypothesis: true difference in means is not equal to 0
## 95 percent confidence interval:
##  -1.6674460  0.0404122
## sample estimates:
## mean in group Altos mean in group Baixo 
##            6.058824            6.872340
```

```r
#Boxplot Order
paste(names(MBRP_base[,c(2,3)]), sep=",")
```

```
## [1] "Escore"         "FAGERTRONTOTAL"
```

```r
lapply(MBRP_base[,c(2,3)], function(x) boxplot(x~FFMQgroup, data=MBRP_base, main="Boxplot", xlab="FFMQ Group", ylab="Check Order Above"))
```

![](Baseline_Full_Analysis_files/figure-html/unnamed-chunk-5-1.png)<!-- -->![](Baseline_Full_Analysis_files/figure-html/unnamed-chunk-5-2.png)<!-- -->

```
## $Escore
## $Escore$stats
##      [,1] [,2]
## [1,]    3    5
## [2,]   11   13
## [3,]   14   16
## [4,]   17   19
## [5,]   26   27
## 
## $Escore$n
## [1] 50 47
## 
## $Escore$conf
##          [,1]    [,2]
## [1,] 12.65933 14.6172
## [2,] 15.34067 17.3828
## 
## $Escore$out
## [1] 33 28 29 27
## 
## $Escore$group
## [1] 1 1 1 1
## 
## $Escore$names
## [1] "Altos" "Baixo"
## 
## 
## $FAGERTRONTOTAL
## $FAGERTRONTOTAL$stats
##      [,1] [,2]
## [1,]  0.0    4
## [2,]  4.5    6
## [3,]  6.0    7
## [4,]  8.0    8
## [5,] 10.0   10
## 
## $FAGERTRONTOTAL$n
## [1] 51 47
## 
## $FAGERTRONTOTAL$conf
##          [,1]     [,2]
## [1,] 5.225645 6.539067
## [2,] 6.774355 7.460933
## 
## $FAGERTRONTOTAL$out
## [1] 2 2
## 
## $FAGERTRONTOTAL$group
## [1] 2 2
## 
## $FAGERTRONTOTAL$names
## [1] "Altos" "Baixo"
```

```r
#Correlation
print(corr.test(x,MBRP_base[2:3]), short=F)
```

```
## Call:corr.test(x = x, y = MBRP_base[2:3])
## Correlation matrix 
##           Escore FAGERTRONTOTAL
## FFMQTOTAL  -0.08          -0.31
## Sample Size 
##           Escore FAGERTRONTOTAL
## FFMQTOTAL     97             98
## Probability values  adjusted for multiple tests. 
##           Escore FAGERTRONTOTAL
## FFMQTOTAL   0.46              0
## 
##  To see confidence intervals of the correlations, print with the short=FALSE option
## 
##  Confidence intervals based upon normal theory.  To get bootstrapped values, try cor.ci
##             lower     r upper    p
## FFMQT-Escor -0.27 -0.08  0.13 0.46
## FFMQT-FAGER -0.48 -0.31 -0.12 0.00
```

#QSU - Urge Variables 

```r
#Descriptive
lapply(MBRP_base[,c(4,5)], function(x) describeBy(x, group =MBRP_base$FFMQgroup))
```

```
## $QSU1
## group: Altos
##   vars  n  mean    sd median trimmed   mad min max range skew kurtosis
## 1    1 47 64.02 25.11     65   62.92 26.69  21 119    98 0.33    -0.72
##     se
## 1 3.66
## -------------------------------------------------------- 
## group: Baixo
##   vars  n  mean    sd median trimmed   mad min max range skew kurtosis
## 1    1 44 69.52 22.22     68   68.92 22.98  30 119    89  0.2    -0.69
##     se
## 1 3.35
## 
## $QSU2
## group: Altos
##   vars  n  mean    sd median trimmed   mad min max range  skew kurtosis
## 1    1 48 54.62 16.44     56    54.8 14.08  19  91    72 -0.15    -0.11
##     se
## 1 2.37
## -------------------------------------------------------- 
## group: Baixo
##   vars  n  mean    sd median trimmed   mad min max range  skew kurtosis
## 1    1 45 62.07 14.95     65   62.68 13.34  27  91    64 -0.41    -0.33
##     se
## 1 2.23
```

```r
#Test T
lapply(MBRP_base[,c(4,5)], function(x) t.test(x ~ MBRP_base$FFMQgroup))
```

```
## $QSU1
## 
## 	Welch Two Sample t-test
## 
## data:  x by MBRP_base$FFMQgroup
## t = -1.1084, df = 88.73, p-value = 0.2707
## alternative hypothesis: true difference in means is not equal to 0
## 95 percent confidence interval:
##  -15.363889   4.360988
## sample estimates:
## mean in group Altos mean in group Baixo 
##            64.02128            69.52273 
## 
## 
## $QSU2
## 
## 	Welch Two Sample t-test
## 
## data:  x by MBRP_base$FFMQgroup
## t = -2.2858, df = 90.921, p-value = 0.02459
## alternative hypothesis: true difference in means is not equal to 0
## 95 percent confidence interval:
##  -13.9086361  -0.9746973
## sample estimates:
## mean in group Altos mean in group Baixo 
##            54.62500            62.06667
```

```r
#Boxplot Order
paste(names(MBRP_base[,c(4,5)]), sep=",")
```

```
## [1] "QSU1" "QSU2"
```

```r
lapply(MBRP_base[,c(4,5)], function(x) boxplot(x~FFMQgroup, data=MBRP_base, main="Boxplot", xlab="FFMQ Group", ylab="Check Order Above"))
```

![](Baseline_Full_Analysis_files/figure-html/unnamed-chunk-6-1.png)<!-- -->![](Baseline_Full_Analysis_files/figure-html/unnamed-chunk-6-2.png)<!-- -->

```
## $QSU1
## $QSU1$stats
##       [,1]  [,2]
## [1,]  21.0  30.0
## [2,]  42.0  51.5
## [3,]  65.0  68.0
## [4,]  78.5  83.0
## [5,] 119.0 119.0
## 
## $QSU1$n
## [1] 47 44
## 
## $QSU1$conf
##          [,1]     [,2]
## [1,] 56.58797 60.49689
## [2,] 73.41203 75.50311
## 
## $QSU1$out
## numeric(0)
## 
## $QSU1$group
## numeric(0)
## 
## $QSU1$names
## [1] "Altos" "Baixo"
## 
## 
## $QSU2
## $QSU2$stats
##      [,1] [,2]
## [1,] 19.0   34
## [2,] 44.5   55
## [3,] 56.0   65
## [4,] 64.0   69
## [5,] 91.0   85
## 
## $QSU2$n
## [1] 48 45
## 
## $QSU2$conf
##          [,1]     [,2]
## [1,] 51.55296 61.70255
## [2,] 60.44704 68.29745
## 
## $QSU2$out
## [1] 27 31 91
## 
## $QSU2$group
## [1] 2 2 2
## 
## $QSU2$names
## [1] "Altos" "Baixo"
```

```r
#Correlation
print(corr.test(x,MBRP_base[4:5]), short=F)
```

```
## Call:corr.test(x = x, y = MBRP_base[4:5])
## Correlation matrix 
##            QSU1 QSU2
## FFMQTOTAL -0.17 -0.3
## Sample Size 
##           QSU1 QSU2
## FFMQTOTAL   91   93
## Probability values  adjusted for multiple tests. 
##           QSU1 QSU2
## FFMQTOTAL  0.1 0.01
## 
##  To see confidence intervals of the correlations, print with the short=FALSE option
## 
##  Confidence intervals based upon normal theory.  To get bootstrapped values, try cor.ci
##            lower     r upper    p
## FFMQT-QSU1 -0.37 -0.17  0.03 0.10
## FFMQT-QSU2 -0.47 -0.30 -0.10 0.01
```

#HAD e HAS

```r
#Descriptive
lapply(MBRP_base[,c(6,7)], function(x) describeBy(x, group =MBRP_base$FFMQgroup))
```

```
## $HADansiedade
## group: Altos
##   vars  n  mean   sd median trimmed  mad min max range skew kurtosis   se
## 1    1 51 10.73 4.01     11   10.68 4.45   3  19    16 0.08    -0.86 0.56
## -------------------------------------------------------- 
## group: Baixo
##   vars  n  mean   sd median trimmed  mad min max range  skew kurtosis   se
## 1    1 47 12.19 3.77     12   12.18 4.45   4  20    16 -0.03    -0.73 0.55
## 
## $HADdepressao
## group: Altos
##   vars  n mean  sd median trimmed  mad min max range skew kurtosis   se
## 1    1 51 7.47 3.8      7     7.1 4.45   2  18    16 0.74     0.01 0.53
## -------------------------------------------------------- 
## group: Baixo
##   vars  n mean   sd median trimmed  mad min max range skew kurtosis   se
## 1    1 47 9.98 4.07      9    9.97 2.97   1  21    20 0.16     0.26 0.59
```

```r
#Teste T
lapply(MBRP_base[,c(6,7)], function(x) t.test(x ~ MBRP_base$FFMQgroup))
```

```
## $HADansiedade
## 
## 	Welch Two Sample t-test
## 
## data:  x by MBRP_base$FFMQgroup
## t = -1.8667, df = 95.955, p-value = 0.06499
## alternative hypothesis: true difference in means is not equal to 0
## 95 percent confidence interval:
##  -3.02486502  0.09286669
## sample estimates:
## mean in group Altos mean in group Baixo 
##            10.72549            12.19149 
## 
## 
## $HADdepressao
## 
## 	Welch Two Sample t-test
## 
## data:  x by MBRP_base$FFMQgroup
## t = -3.1462, df = 93.822, p-value = 0.002217
## alternative hypothesis: true difference in means is not equal to 0
## 95 percent confidence interval:
##  -4.0910412 -0.9252291
## sample estimates:
## mean in group Altos mean in group Baixo 
##            7.470588            9.978723
```

```r
#Boxplot Order
paste(names(MBRP_base[,c(6,7)]), sep=",")
```

```
## [1] "HADansiedade" "HADdepressao"
```

```r
lapply(MBRP_base[,c(6,7)], function(x) boxplot(x~FFMQgroup, data=MBRP_base, main="Boxplot", xlab="FFMQ Group", ylab="Check Order Above"))
```

![](Baseline_Full_Analysis_files/figure-html/unnamed-chunk-7-1.png)<!-- -->![](Baseline_Full_Analysis_files/figure-html/unnamed-chunk-7-2.png)<!-- -->

```
## $HADansiedade
## $HADansiedade$stats
##      [,1] [,2]
## [1,]    3  4.0
## [2,]    8  9.5
## [3,]   11 12.0
## [4,]   14 15.0
## [5,]   19 20.0
## 
## $HADansiedade$n
## [1] 51 47
## 
## $HADansiedade$conf
##           [,1]     [,2]
## [1,]  9.672534 10.73243
## [2,] 12.327466 13.26757
## 
## $HADansiedade$out
## numeric(0)
## 
## $HADansiedade$group
## numeric(0)
## 
## $HADansiedade$names
## [1] "Altos" "Baixo"
## 
## 
## $HADdepressao
## $HADdepressao$stats
##      [,1] [,2]
## [1,]    2    2
## [2,]    4    8
## [3,]    7    9
## [4,]   10   12
## [5,]   18   18
## 
## $HADdepressao$n
## [1] 51 47
## 
## $HADdepressao$conf
##          [,1]     [,2]
## [1,] 5.672534 8.078133
## [2,] 8.327466 9.921867
## 
## $HADdepressao$out
## [1] 21  1
## 
## $HADdepressao$group
## [1] 2 2
## 
## $HADdepressao$names
## [1] "Altos" "Baixo"
```

```r
#Correlation
print(corr.test(x,MBRP_base[6:7]), short=F)
```

```
## Call:corr.test(x = x, y = MBRP_base[6:7])
## Correlation matrix 
##           HADansiedade HADdepressao
## FFMQTOTAL        -0.32        -0.45
## Sample Size 
## [1] 98
## Probability values  adjusted for multiple tests. 
##           HADansiedade HADdepressao
## FFMQTOTAL            0            0
## 
##  To see confidence intervals of the correlations, print with the short=FALSE option
## 
##  Confidence intervals based upon normal theory.  To get bootstrapped values, try cor.ci
##             lower     r upper p
## FFMQT-HADns -0.49 -0.32 -0.13 0
## FFMQT-HADdp -0.60 -0.45 -0.28 0
```


#PANAS - Positive and Negative

```r
#Descriptive
lapply(MBRP_base[,c(8,9)], function(x) describeBy(x, group =MBRP_base$FFMQgroup))
```

```
## $AfetoPositivo
## group: Altos
##   vars  n  mean   sd median trimmed  mad min max range  skew kurtosis   se
## 1    1 49 26.98 6.65     28   27.02 7.41  14  41    27 -0.19    -0.96 0.95
## -------------------------------------------------------- 
## group: Baixo
##   vars  n  mean   sd median trimmed  mad min max range skew kurtosis   se
## 1    1 45 23.33 6.77     23   23.19 7.41  12  39    27 0.17    -0.79 1.01
## 
## $AfetoNegativo
## group: Altos
##   vars  n  mean   sd median trimmed  mad min max range skew kurtosis   se
## 1    1 50 19.52 6.69     18   19.18 7.41   9  34    25 0.45    -0.88 0.95
## -------------------------------------------------------- 
## group: Baixo
##   vars  n  mean   sd median trimmed  mad min max range skew kurtosis   se
## 1    1 44 22.34 5.89     21   22.22 5.93  13  33    20 0.31    -1.04 0.89
```

```r
#Teste T
lapply(MBRP_base[,c(8,9)], function(x) t.test(x ~ MBRP_base$FFMQgroup))
```

```
## $AfetoPositivo
## 
## 	Welch Two Sample t-test
## 
## data:  x by MBRP_base$FFMQgroup
## t = 2.6303, df = 91.029, p-value = 0.01002
## alternative hypothesis: true difference in means is not equal to 0
## 95 percent confidence interval:
##  0.8926763 6.3998407
## sample estimates:
## mean in group Altos mean in group Baixo 
##            26.97959            23.33333 
## 
## 
## $AfetoNegativo
## 
## 	Welch Two Sample t-test
## 
## data:  x by MBRP_base$FFMQgroup
## t = -2.1749, df = 92, p-value = 0.03221
## alternative hypothesis: true difference in means is not equal to 0
## 95 percent confidence interval:
##  -5.3969758 -0.2448424
## sample estimates:
## mean in group Altos mean in group Baixo 
##            19.52000            22.34091
```

```r
#Boxplot Order
paste(names(MBRP_base[,c(8,9)]), sep=",")
```

```
## [1] "AfetoPositivo" "AfetoNegativo"
```

```r
lapply(MBRP_base[,c(8,9)], function(x) boxplot(x~FFMQgroup, data=MBRP_base, main="Boxplot", xlab="FFMQ Group", ylab="Check Order Above"))
```

![](Baseline_Full_Analysis_files/figure-html/unnamed-chunk-8-1.png)<!-- -->![](Baseline_Full_Analysis_files/figure-html/unnamed-chunk-8-2.png)<!-- -->

```
## $AfetoPositivo
## $AfetoPositivo$stats
##      [,1] [,2]
## [1,]   14   12
## [2,]   20   18
## [3,]   28   23
## [4,]   32   29
## [5,]   41   39
## 
## $AfetoPositivo$n
## [1] 49 45
## 
## $AfetoPositivo$conf
##          [,1]     [,2]
## [1,] 25.29143 20.40914
## [2,] 30.70857 25.59086
## 
## $AfetoPositivo$out
## numeric(0)
## 
## $AfetoPositivo$group
## numeric(0)
## 
## $AfetoPositivo$names
## [1] "Altos" "Baixo"
## 
## 
## $AfetoNegativo
## $AfetoNegativo$stats
##      [,1] [,2]
## [1,]    9   13
## [2,]   15   18
## [3,]   18   21
## [4,]   24   28
## [5,]   34   33
## 
## $AfetoNegativo$n
## [1] 50 44
## 
## $AfetoNegativo$conf
##          [,1]     [,2]
## [1,] 15.98899 18.61806
## [2,] 20.01101 23.38194
## 
## $AfetoNegativo$out
## numeric(0)
## 
## $AfetoNegativo$group
## numeric(0)
## 
## $AfetoNegativo$names
## [1] "Altos" "Baixo"
```

```r
#Correlation
print(corr.test(x,MBRP_base[8:9]), short=F)
```

```
## Call:corr.test(x = x, y = MBRP_base[8:9])
## Correlation matrix 
##           AfetoPositivo AfetoNegativo
## FFMQTOTAL          0.51         -0.35
## Sample Size 
## [1] 94
## Probability values  adjusted for multiple tests. 
##           AfetoPositivo AfetoNegativo
## FFMQTOTAL             0             0
## 
##  To see confidence intervals of the correlations, print with the short=FALSE option
## 
##  Confidence intervals based upon normal theory.  To get bootstrapped values, try cor.ci
##             lower     r upper p
## FFMQT-AftPs  0.34  0.51  0.64 0
## FFMQT-AftNg -0.52 -0.35 -0.16 0
```

#PANAS - Sub-escales

```r
#Descriptive
lapply(MBRP_base[,c(10:20)], function(x) describeBy(x, group =MBRP_base$FFMQgroup))
```

```
## $Medo
## group: Altos
##   vars  n  mean   sd median trimmed  mad min max range skew kurtosis   se
## 1    1 51 10.61 3.62     10   10.39 4.45   5  20    15 0.51    -0.59 0.51
## -------------------------------------------------------- 
## group: Baixo
##   vars  n  mean  sd median trimmed  mad min max range skew kurtosis   se
## 1    1 43 12.12 3.7     11   11.91 2.97   6  21    15 0.56    -0.56 0.56
## 
## $Hostilidade
## group: Altos
##   vars  n mean   sd median trimmed  mad min max range skew kurtosis   se
## 1    1 50 12.5 4.75     11    11.9 4.45   7  26    19 0.94     0.05 0.67
## -------------------------------------------------------- 
## group: Baixo
##   vars  n  mean   sd median trimmed  mad min max range skew kurtosis   se
## 1    1 43 14.91 4.66     14   14.57 4.45   7  27    20 0.52    -0.35 0.71
## 
## $Culpa
## group: Altos
##   vars  n mean   sd median trimmed  mad min max range skew kurtosis   se
## 1    1 51 9.53 3.92      8    9.17 4.45   5  19    14  0.6    -0.77 0.55
## -------------------------------------------------------- 
## group: Baixo
##   vars  n mean   sd median trimmed  mad min max range skew kurtosis   se
## 1    1 43 10.7 4.17     10   10.34 4.45   5  21    16 0.65    -0.52 0.64
## 
## $Tristeza
## group: Altos
##   vars  n  mean   sd median trimmed  mad min max range skew kurtosis   se
## 1    1 51 10.71 4.63     10   10.29 4.45   5  24    19 0.63    -0.21 0.65
## -------------------------------------------------------- 
## group: Baixo
##   vars  n  mean   sd median trimmed  mad min max range skew kurtosis   se
## 1    1 46 12.39 5.74   11.5   12.11 6.67   5  25    20 0.46    -1.04 0.85
## 
## $Jovialidade
## group: Altos
##   vars  n  mean   sd median trimmed  mad min max range  skew kurtosis   se
## 1    1 49 21.67 6.42     23   21.73 7.41   9  34    25 -0.12    -0.89 0.92
## -------------------------------------------------------- 
## group: Baixo
##   vars  n  mean   sd median trimmed  mad min max range skew kurtosis   se
## 1    1 47 18.34 6.54     19   18.15 7.41   8  32    24 0.08    -0.95 0.95
## 
## $AutoAfirmacao
## group: Altos
##   vars  n  mean   sd median trimmed  mad min max range skew kurtosis   se
## 1    1 48 14.81 4.53     15   14.72 4.45   6  27    21  0.2    -0.19 0.65
## -------------------------------------------------------- 
## group: Baixo
##   vars  n mean   sd median trimmed  mad min max range skew kurtosis   se
## 1    1 44 13.7 3.74     14   13.56 2.97   7  22    15 0.23    -0.68 0.56
## 
## $Atentividade
## group: Altos
##   vars  n  mean sd median trimmed  mad min max range  skew kurtosis   se
## 1    1 50 12.64  3     13    12.7 2.97   7  18    11 -0.26    -0.77 0.42
## -------------------------------------------------------- 
## group: Baixo
##   vars  n  mean   sd median trimmed  mad min max range skew kurtosis   se
## 1    1 46 10.57 3.52     10   10.47 2.97   4  19    15 0.41    -0.36 0.52
## 
## $Timidez
## group: Altos
##   vars  n mean   sd median trimmed  mad min max range skew kurtosis  se
## 1    1 49 7.35 2.79      7    7.07 2.97   4  14    10 0.83    -0.07 0.4
## -------------------------------------------------------- 
## group: Baixo
##   vars  n mean   sd median trimmed  mad min max range skew kurtosis   se
## 1    1 44 8.25 3.26      8    7.83 2.97   4  17    13 1.18     0.99 0.49
## 
## $Fadiga
## group: Altos
##   vars  n mean   sd median trimmed  mad min max range skew kurtosis   se
## 1    1 51 9.49 3.61      9    9.32 4.45   4  18    14 0.35    -0.76 0.51
## -------------------------------------------------------- 
## group: Baixo
##   vars  n  mean   sd median trimmed  mad min max range  skew kurtosis  se
## 1    1 46 10.39 3.41   10.5   10.42 2.97   4  17    13 -0.16    -0.76 0.5
## 
## $Serenidade
## group: Altos
##   vars  n mean   sd median trimmed  mad min max range  skew kurtosis   se
## 1    1 48 7.46 2.42      7    7.47 2.97   3  12     9 -0.05    -1.11 0.35
## -------------------------------------------------------- 
## group: Baixo
##   vars  n mean   sd median trimmed  mad min max range skew kurtosis   se
## 1    1 46  6.3 1.92      6    6.16 1.48   3  11     8 0.51    -0.18 0.28
## 
## $Supresa
## group: Altos
##   vars  n mean   sd median trimmed  mad min max range skew kurtosis   se
## 1    1 51 5.31 2.38      4       5 1.48   3  12     9 0.93     0.24 0.33
## -------------------------------------------------------- 
## group: Baixo
##   vars  n mean  sd median trimmed  mad min max range skew kurtosis   se
## 1    1 43 5.49 2.1      5    5.29 1.48   3  11     8 0.76    -0.26 0.32
```

```r
#Teste T
lapply(MBRP_base[,c(10:20)], function(x) t.test(x ~ MBRP_base$FFMQgroup))
```

```
## $Medo
## 
## 	Welch Two Sample t-test
## 
## data:  x by MBRP_base$FFMQgroup
## t = -1.99, df = 88.637, p-value = 0.04968
## alternative hypothesis: true difference in means is not equal to 0
## 95 percent confidence interval:
##  -3.014689184 -0.002182681
## sample estimates:
## mean in group Altos mean in group Baixo 
##            10.60784            12.11628 
## 
## 
## $Hostilidade
## 
## 	Welch Two Sample t-test
## 
## data:  x by MBRP_base$FFMQgroup
## t = -2.4623, df = 89.4, p-value = 0.01572
## alternative hypothesis: true difference in means is not equal to 0
## 95 percent confidence interval:
##  -4.3492176 -0.4647359
## sample estimates:
## mean in group Altos mean in group Baixo 
##            12.50000            14.90698 
## 
## 
## $Culpa
## 
## 	Welch Two Sample t-test
## 
## data:  x by MBRP_base$FFMQgroup
## t = -1.392, df = 87.22, p-value = 0.1675
## alternative hypothesis: true difference in means is not equal to 0
## 95 percent confidence interval:
##  -2.8363291  0.4998038
## sample estimates:
## mean in group Altos mean in group Baixo 
##            9.529412           10.697674 
## 
## 
## $Tristeza
## 
## 	Welch Two Sample t-test
## 
## data:  x by MBRP_base$FFMQgroup
## t = -1.5805, df = 86.439, p-value = 0.1176
## alternative hypothesis: true difference in means is not equal to 0
## 95 percent confidence interval:
##  -3.8052012  0.4343572
## sample estimates:
## mean in group Altos mean in group Baixo 
##            10.70588            12.39130 
## 
## 
## $Jovialidade
## 
## 	Welch Two Sample t-test
## 
## data:  x by MBRP_base$FFMQgroup
## t = 2.5188, df = 93.648, p-value = 0.01347
## alternative hypothesis: true difference in means is not equal to 0
## 95 percent confidence interval:
##  0.7055717 5.9605160
## sample estimates:
## mean in group Altos mean in group Baixo 
##            21.67347            18.34043 
## 
## 
## $AutoAfirmacao
## 
## 	Welch Two Sample t-test
## 
## data:  x by MBRP_base$FFMQgroup
## t = 1.2839, df = 89.067, p-value = 0.2025
## alternative hypothesis: true difference in means is not equal to 0
## 95 percent confidence interval:
##  -0.6067542  2.8226633
## sample estimates:
## mean in group Altos mean in group Baixo 
##            14.81250            13.70455 
## 
## 
## $Atentividade
## 
## 	Welch Two Sample t-test
## 
## data:  x by MBRP_base$FFMQgroup
## t = 3.0948, df = 88.847, p-value = 0.002634
## alternative hypothesis: true difference in means is not equal to 0
## 95 percent confidence interval:
##  0.7426727 3.4068925
## sample estimates:
## mean in group Altos mean in group Baixo 
##            12.64000            10.56522 
## 
## 
## $Timidez
## 
## 	Welch Two Sample t-test
## 
## data:  x by MBRP_base$FFMQgroup
## t = -1.4264, df = 85.078, p-value = 0.1574
## alternative hypothesis: true difference in means is not equal to 0
## 95 percent confidence interval:
##  -2.1618488  0.3557264
## sample estimates:
## mean in group Altos mean in group Baixo 
##            7.346939            8.250000 
## 
## 
## $Fadiga
## 
## 	Welch Two Sample t-test
## 
## data:  x by MBRP_base$FFMQgroup
## t = -1.2645, df = 94.785, p-value = 0.2092
## alternative hypothesis: true difference in means is not equal to 0
## 95 percent confidence interval:
##  -2.3158833  0.5136668
## sample estimates:
## mean in group Altos mean in group Baixo 
##            9.490196           10.391304 
## 
## 
## $Serenidade
## 
## 	Welch Two Sample t-test
## 
## data:  x by MBRP_base$FFMQgroup
## t = 2.5652, df = 88.891, p-value = 0.01199
## alternative hypothesis: true difference in means is not equal to 0
## 95 percent confidence interval:
##  0.2601155 2.0478555
## sample estimates:
## mean in group Altos mean in group Baixo 
##            7.458333            6.304348 
## 
## 
## $Supresa
## 
## 	Welch Two Sample t-test
## 
## data:  x by MBRP_base$FFMQgroup
## t = -0.37818, df = 91.802, p-value = 0.7062
## alternative hypothesis: true difference in means is not equal to 0
## 95 percent confidence interval:
##  -1.0918651  0.7425719
## sample estimates:
## mean in group Altos mean in group Baixo 
##            5.313725            5.488372
```

```r
#Boxplot Order
paste(names(MBRP_base[,c(10:20)]), sep=",")
```

```
##  [1] "Medo"          "Hostilidade"   "Culpa"         "Tristeza"     
##  [5] "Jovialidade"   "AutoAfirmacao" "Atentividade"  "Timidez"      
##  [9] "Fadiga"        "Serenidade"    "Supresa"
```

```r
lapply(MBRP_base[,c(10:20)], function(x) boxplot(x~FFMQgroup, data=MBRP_base, main="Boxplot", xlab="FFMQ Group", ylab="Check Order Above"))
```

![](Baseline_Full_Analysis_files/figure-html/unnamed-chunk-9-1.png)<!-- -->![](Baseline_Full_Analysis_files/figure-html/unnamed-chunk-9-2.png)<!-- -->![](Baseline_Full_Analysis_files/figure-html/unnamed-chunk-9-3.png)<!-- -->![](Baseline_Full_Analysis_files/figure-html/unnamed-chunk-9-4.png)<!-- -->![](Baseline_Full_Analysis_files/figure-html/unnamed-chunk-9-5.png)<!-- -->![](Baseline_Full_Analysis_files/figure-html/unnamed-chunk-9-6.png)<!-- -->![](Baseline_Full_Analysis_files/figure-html/unnamed-chunk-9-7.png)<!-- -->![](Baseline_Full_Analysis_files/figure-html/unnamed-chunk-9-8.png)<!-- -->![](Baseline_Full_Analysis_files/figure-html/unnamed-chunk-9-9.png)<!-- -->![](Baseline_Full_Analysis_files/figure-html/unnamed-chunk-9-10.png)<!-- -->![](Baseline_Full_Analysis_files/figure-html/unnamed-chunk-9-11.png)<!-- -->

```
## $Medo
## $Medo$stats
##      [,1] [,2]
## [1,]  5.0    6
## [2,]  7.5   10
## [3,] 10.0   11
## [4,] 13.0   15
## [5,] 20.0   21
## 
## $Medo$n
## [1] 51 43
## 
## $Medo$conf
##           [,1]      [,2]
## [1,]  8.783157  9.795261
## [2,] 11.216843 12.204739
## 
## $Medo$out
## numeric(0)
## 
## $Medo$group
## numeric(0)
## 
## $Medo$names
## [1] "Altos" "Baixo"
## 
## 
## $Hostilidade
## $Hostilidade$stats
##      [,1] [,2]
## [1,]    7    7
## [2,]    9   12
## [3,]   11   14
## [4,]   15   18
## [5,]   23   27
## 
## $Hostilidade$n
## [1] 50 43
## 
## $Hostilidade$conf
##           [,1]     [,2]
## [1,]  9.659326 12.55431
## [2,] 12.340674 15.44569
## 
## $Hostilidade$out
## [1] 26
## 
## $Hostilidade$group
## [1] 1
## 
## $Hostilidade$names
## [1] "Altos" "Baixo"
## 
## 
## $Culpa
## $Culpa$stats
##      [,1] [,2]
## [1,]  5.0    5
## [2,]  6.5    7
## [3,]  8.0   10
## [4,] 12.5   13
## [5,] 19.0   21
## 
## $Culpa$n
## [1] 51 43
## 
## $Culpa$conf
##          [,1]      [,2]
## [1,] 6.672534  8.554314
## [2,] 9.327466 11.445686
## 
## $Culpa$out
## numeric(0)
## 
## $Culpa$group
## numeric(0)
## 
## $Culpa$names
## [1] "Altos" "Baixo"
## 
## 
## $Tristeza
## $Tristeza$stats
##      [,1] [,2]
## [1,]  5.0  5.0
## [2,]  6.5  8.0
## [3,] 10.0 11.5
## [4,] 13.0 17.0
## [5,] 20.0 25.0
## 
## $Tristeza$n
## [1] 51 46
## 
## $Tristeza$conf
##           [,1]      [,2]
## [1,]  8.561912  9.403375
## [2,] 11.438088 13.596625
## 
## $Tristeza$out
## [1] 24
## 
## $Tristeza$group
## [1] 1
## 
## $Tristeza$names
## [1] "Altos" "Baixo"
## 
## 
## $Jovialidade
## $Jovialidade$stats
##      [,1] [,2]
## [1,]    9  8.0
## [2,]   17 12.5
## [3,]   23 19.0
## [4,]   26 22.5
## [5,]   34 32.0
## 
## $Jovialidade$n
## [1] 49 47
## 
## $Jovialidade$conf
##          [,1]     [,2]
## [1,] 20.96857 16.69533
## [2,] 25.03143 21.30467
## 
## $Jovialidade$out
## numeric(0)
## 
## $Jovialidade$group
## numeric(0)
## 
## $Jovialidade$names
## [1] "Altos" "Baixo"
## 
## 
## $AutoAfirmacao
## $AutoAfirmacao$stats
##      [,1] [,2]
## [1,]  6.0    7
## [2,] 11.5   11
## [3,] 15.0   14
## [4,] 18.0   16
## [5,] 27.0   22
## 
## $AutoAfirmacao$n
## [1] 48 44
## 
## $AutoAfirmacao$conf
##          [,1]     [,2]
## [1,] 13.51765 12.80903
## [2,] 16.48235 15.19097
## 
## $AutoAfirmacao$out
## numeric(0)
## 
## $AutoAfirmacao$group
## numeric(0)
## 
## $AutoAfirmacao$names
## [1] "Altos" "Baixo"
## 
## 
## $Atentividade
## $Atentividade$stats
##      [,1] [,2]
## [1,]    7    5
## [2,]   11    9
## [3,]   13   10
## [4,]   15   12
## [5,]   18   16
## 
## $Atentividade$n
## [1] 50 46
## 
## $Atentividade$conf
##          [,1]      [,2]
## [1,] 12.10622  9.301125
## [2,] 13.89378 10.698875
## 
## $Atentividade$out
## [1] 19 18  4  4
## 
## $Atentividade$group
## [1] 2 2 2 2
## 
## $Atentividade$names
## [1] "Altos" "Baixo"
## 
## 
## $Timidez
## $Timidez$stats
##      [,1] [,2]
## [1,]    4    4
## [2,]    5    6
## [3,]    7    8
## [4,]    8    9
## [5,]   12   13
## 
## $Timidez$n
## [1] 49 44
## 
## $Timidez$conf
##          [,1]     [,2]
## [1,] 6.322857 7.285418
## [2,] 7.677143 8.714582
## 
## $Timidez$out
## [1] 13 13 14 14 16 17 15 17
## 
## $Timidez$group
## [1] 1 1 1 1 2 2 2 2
## 
## $Timidez$names
## [1] "Altos" "Baixo"
## 
## 
## $Fadiga
## $Fadiga$stats
##      [,1] [,2]
## [1,]    4  4.0
## [2,]    7  9.0
## [3,]    9 10.5
## [4,]   12 13.0
## [5,]   18 17.0
## 
## $Fadiga$n
## [1] 51 46
## 
## $Fadiga$conf
##           [,1]      [,2]
## [1,]  7.893779  9.568167
## [2,] 10.106221 11.431833
## 
## $Fadiga$out
## numeric(0)
## 
## $Fadiga$group
## numeric(0)
## 
## $Fadiga$names
## [1] "Altos" "Baixo"
## 
## 
## $Serenidade
## $Serenidade$stats
##      [,1] [,2]
## [1,]    3    3
## [2,]    6    5
## [3,]    7    6
## [4,]   10    7
## [5,]   12   10
## 
## $Serenidade$n
## [1] 48 46
## 
## $Serenidade$conf
##          [,1]     [,2]
## [1,] 6.087787 5.534083
## [2,] 7.912213 6.465917
## 
## $Serenidade$out
## [1] 11 11
## 
## $Serenidade$group
## [1] 2 2
## 
## $Serenidade$names
## [1] "Altos" "Baixo"
## 
## 
## $Supresa
## $Supresa$stats
##      [,1] [,2]
## [1,]    3    3
## [2,]    3    4
## [3,]    4    5
## [4,]    7    7
## [5,]   12   11
## 
## $Supresa$n
## [1] 51 43
## 
## $Supresa$conf
##          [,1]     [,2]
## [1,] 3.115023 4.277157
## [2,] 4.884977 5.722843
## 
## $Supresa$out
## numeric(0)
## 
## $Supresa$group
## numeric(0)
## 
## $Supresa$names
## [1] "Altos" "Baixo"
```

```r
#Correlation
print(corr.test(x,MBRP_base[10:20]), short=F)
```

```
## Call:corr.test(x = x, y = MBRP_base[10:20])
## Correlation matrix 
##            Medo Hostilidade Culpa Tristeza Jovialidade AutoAfirmacao
## FFMQTOTAL -0.34       -0.42 -0.29    -0.36        0.47          0.25
##           Atentividade Timidez Fadiga Serenidade Supresa
## FFMQTOTAL         0.48   -0.26  -0.25       0.39   -0.14
## Sample Size 
##           Medo Hostilidade Culpa Tristeza Jovialidade AutoAfirmacao
## FFMQTOTAL   94          93    94       97          96            92
##           Atentividade Timidez Fadiga Serenidade Supresa
## FFMQTOTAL           96      93     97         94      94
## Probability values  adjusted for multiple tests. 
##           Medo Hostilidade Culpa Tristeza Jovialidade AutoAfirmacao
## FFMQTOTAL    0           0  0.02        0           0          0.04
##           Atentividade Timidez Fadiga Serenidade Supresa
## FFMQTOTAL            0    0.04   0.04          0    0.19
## 
##  To see confidence intervals of the correlations, print with the short=FALSE option
## 
##  Confidence intervals based upon normal theory.  To get bootstrapped values, try cor.ci
##             lower     r upper    p
## FFMQT-Medo  -0.51 -0.34 -0.15 0.00
## FFMQT-Hstld -0.58 -0.42 -0.24 0.00
## FFMQT-Culpa -0.46 -0.29 -0.09 0.02
## FFMQT-Trstz -0.52 -0.36 -0.17 0.00
## FFMQT-Jvldd  0.29  0.47  0.61 0.00
## FFMQT-AtAfr  0.05  0.25  0.44 0.04
## FFMQT-Atntv  0.31  0.48  0.62 0.00
## FFMQT-Timdz -0.44 -0.26 -0.06 0.04
## FFMQT-Fadig -0.43 -0.25 -0.05 0.04
## FFMQT-Srndd  0.20  0.39  0.55 0.00
## FFMQT-Suprs -0.33 -0.14  0.07 0.19
```

#CESD

```r
#Descriptive
lapply(MBRP_base[,c(21:24)], function(x) describeBy(x, group =MBRP_base$FFMQgroup))
```

```
## $CESD1
## group: Altos
##   vars  n mean   sd median trimmed  mad min max range skew kurtosis   se
## 1    1 51 6.04 4.18      6    5.83 4.45   0  15    15 0.32    -0.81 0.59
## -------------------------------------------------------- 
## group: Baixo
##   vars  n mean   sd median trimmed  mad min max range  skew kurtosis  se
## 1    1 45 7.53 4.68      9    7.57 5.93   0  15    15 -0.15     -1.4 0.7
## 
## $CESD2
## group: Altos
##   vars  n mean   sd median trimmed  mad min max range skew kurtosis   se
## 1    1 48 4.65 3.61      4    4.45 4.45   0  13    13 0.39    -0.94 0.52
## -------------------------------------------------------- 
## group: Baixo
##   vars  n mean   sd median trimmed  mad min max range skew kurtosis   se
## 1    1 47 7.04 5.09      6    6.85 5.93   0  17    17 0.34    -1.32 0.74
## 
## $CESD3
## group: Altos
##   vars  n mean  sd median trimmed  mad min max range skew kurtosis  se
## 1    1 49 4.86 2.8      5    4.73 2.97   0  11    11 0.33    -0.83 0.4
## -------------------------------------------------------- 
## group: Baixo
##   vars  n mean   sd median trimmed  mad min max range  skew kurtosis   se
## 1    1 47 6.36 3.62      7    6.38 4.45   0  12    12 -0.16    -1.19 0.53
## 
## $CESD4
## group: Altos
##   vars  n mean   sd median trimmed  mad min max range  skew kurtosis   se
## 1    1 50 5.56 2.43    5.5    5.65 2.22   0   9     9 -0.23    -0.89 0.34
## -------------------------------------------------------- 
## group: Baixo
##   vars  n mean   sd median trimmed  mad min max range skew kurtosis   se
## 1    1 44 3.95 2.48      4    3.92 2.97   0   9     9 0.16     -1.3 0.37
```

```r
#Teste T
lapply(MBRP_base[,c(21:24)], function(x) t.test(x ~ MBRP_base$FFMQgroup))
```

```
## $CESD1
## 
## 	Welch Two Sample t-test
## 
## data:  x by MBRP_base$FFMQgroup
## t = -1.6408, df = 88.974, p-value = 0.1044
## alternative hypothesis: true difference in means is not equal to 0
## 95 percent confidence interval:
##  -3.3034532  0.3152179
## sample estimates:
## mean in group Altos mean in group Baixo 
##            6.039216            7.533333 
## 
## 
## $CESD2
## 
## 	Welch Two Sample t-test
## 
## data:  x by MBRP_base$FFMQgroup
## t = -2.6404, df = 82.787, p-value = 0.009895
## alternative hypothesis: true difference in means is not equal to 0
## 95 percent confidence interval:
##  -4.2021875 -0.5912523
## sample estimates:
## mean in group Altos mean in group Baixo 
##            4.645833            7.042553 
## 
## 
## $CESD3
## 
## 	Welch Two Sample t-test
## 
## data:  x by MBRP_base$FFMQgroup
## t = -2.2716, df = 86.59, p-value = 0.02559
## alternative hypothesis: true difference in means is not equal to 0
## 95 percent confidence interval:
##  -2.821134 -0.187985
## sample estimates:
## mean in group Altos mean in group Baixo 
##            4.857143            6.361702 
## 
## 
## $CESD4
## 
## 	Welch Two Sample t-test
## 
## data:  x by MBRP_base$FFMQgroup
## t = 3.1608, df = 90.057, p-value = 0.002143
## alternative hypothesis: true difference in means is not equal to 0
## 95 percent confidence interval:
##  0.5963875 2.6145216
## sample estimates:
## mean in group Altos mean in group Baixo 
##            5.560000            3.954545
```

```r
#Boxplot Order
paste(names(MBRP_base[,c(21:24)]), sep=",")
```

```
## [1] "CESD1" "CESD2" "CESD3" "CESD4"
```

```r
lapply(MBRP_base[,c(21:24)], function(x) boxplot(x~FFMQgroup, data=MBRP_base, main="Boxplot", xlab="FFMQ Group", ylab="Check Order Above"))
```

![](Baseline_Full_Analysis_files/figure-html/unnamed-chunk-10-1.png)<!-- -->![](Baseline_Full_Analysis_files/figure-html/unnamed-chunk-10-2.png)<!-- -->![](Baseline_Full_Analysis_files/figure-html/unnamed-chunk-10-3.png)<!-- -->![](Baseline_Full_Analysis_files/figure-html/unnamed-chunk-10-4.png)<!-- -->

```
## $CESD1
## $CESD1$stats
##      [,1] [,2]
## [1,]  0.0    0
## [2,]  2.5    3
## [3,]  6.0    9
## [4,]  9.0   11
## [5,] 15.0   15
## 
## $CESD1$n
## [1] 51 45
## 
## $CESD1$conf
##          [,1]     [,2]
## [1,] 4.561912  7.11574
## [2,] 7.438088 10.88426
## 
## $CESD1$out
## numeric(0)
## 
## $CESD1$group
## numeric(0)
## 
## $CESD1$names
## [1] "Altos" "Baixo"
## 
## 
## $CESD2
## $CESD2$stats
##      [,1] [,2]
## [1,]    0    0
## [2,]    1    2
## [3,]    4    6
## [4,]    8   11
## [5,]   13   17
## 
## $CESD2$n
## [1] 48 47
## 
## $CESD2$conf
##          [,1]   [,2]
## [1,] 2.403627 3.9258
## [2,] 5.596373 8.0742
## 
## $CESD2$out
## numeric(0)
## 
## $CESD2$group
## numeric(0)
## 
## $CESD2$names
## [1] "Altos" "Baixo"
## 
## 
## $CESD3
## $CESD3$stats
##      [,1] [,2]
## [1,]    0    0
## [2,]    2    4
## [3,]    5    7
## [4,]    7    9
## [5,]   11   12
## 
## $CESD3$n
## [1] 49 47
## 
## $CESD3$conf
##          [,1]     [,2]
## [1,] 3.871429 5.847667
## [2,] 6.128571 8.152333
## 
## $CESD3$out
## numeric(0)
## 
## $CESD3$group
## numeric(0)
## 
## $CESD3$names
## [1] "Altos" "Baixo"
## 
## 
## $CESD4
## $CESD4$stats
##      [,1] [,2]
## [1,]  0.0    0
## [2,]  4.0    2
## [3,]  5.5    4
## [4,]  8.0    6
## [5,]  9.0    9
## 
## $CESD4$n
## [1] 50 44
## 
## $CESD4$conf
##          [,1]     [,2]
## [1,] 4.606217 3.047224
## [2,] 6.393783 4.952776
## 
## $CESD4$out
## numeric(0)
## 
## $CESD4$group
## numeric(0)
## 
## $CESD4$names
## [1] "Altos" "Baixo"
```

```r
#Correlation
print(corr.test(x,MBRP_base[21:24]), short=F)
```

```
## Call:corr.test(x = x, y = MBRP_base[21:24])
## Correlation matrix 
##           CESD1 CESD2 CESD3 CESD4
## FFMQTOTAL -0.31 -0.39  -0.3  0.48
## Sample Size 
##           CESD1 CESD2 CESD3 CESD4
## FFMQTOTAL    96    95    96    94
## Probability values  adjusted for multiple tests. 
##           CESD1 CESD2 CESD3 CESD4
## FFMQTOTAL     0     0     0     0
## 
##  To see confidence intervals of the correlations, print with the short=FALSE option
## 
##  Confidence intervals based upon normal theory.  To get bootstrapped values, try cor.ci
##             lower     r upper p
## FFMQT-CESD1 -0.48 -0.31 -0.11 0
## FFMQT-CESD2 -0.55 -0.39 -0.20 0
## FFMQT-CESD3 -0.47 -0.30 -0.10 0
## FFMQT-CESD4  0.31  0.48  0.62 0
```

#Correlation

```r
print(corr.test(x,MBRP_base[2:24]), short=F)
```

```
## Call:corr.test(x = x, y = MBRP_base[2:24])
## Correlation matrix 
##           Escore FAGERTRONTOTAL  QSU1 QSU2 HADansiedade HADdepressao
## FFMQTOTAL  -0.08          -0.31 -0.17 -0.3        -0.32        -0.45
##           AfetoPositivo AfetoNegativo  Medo Hostilidade Culpa Tristeza
## FFMQTOTAL          0.51         -0.35 -0.34       -0.42 -0.29    -0.36
##           Jovialidade AutoAfirmacao Atentividade Timidez Fadiga Serenidade
## FFMQTOTAL        0.47          0.25         0.48   -0.26  -0.25       0.39
##           Supresa CESD1 CESD2 CESD3 CESD4
## FFMQTOTAL   -0.14 -0.31 -0.39  -0.3  0.48
## Sample Size 
##           Escore FAGERTRONTOTAL QSU1 QSU2 HADansiedade HADdepressao
## FFMQTOTAL     97             98   91   93           98           98
##           AfetoPositivo AfetoNegativo Medo Hostilidade Culpa Tristeza
## FFMQTOTAL            94            94   94          93    94       97
##           Jovialidade AutoAfirmacao Atentividade Timidez Fadiga Serenidade
## FFMQTOTAL          96            92           96      93     97         94
##           Supresa CESD1 CESD2 CESD3 CESD4
## FFMQTOTAL      94    96    95    96    94
## Probability values  adjusted for multiple tests. 
##           Escore FAGERTRONTOTAL QSU1 QSU2 HADansiedade HADdepressao
## FFMQTOTAL   0.46           0.02  0.3 0.03         0.01            0
##           AfetoPositivo AfetoNegativo Medo Hostilidade Culpa Tristeza
## FFMQTOTAL             0          0.01 0.01           0  0.03        0
##           Jovialidade AutoAfirmacao Atentividade Timidez Fadiga Serenidade
## FFMQTOTAL           0          0.07            0    0.06   0.07          0
##           Supresa CESD1 CESD2 CESD3 CESD4
## FFMQTOTAL    0.38  0.02     0  0.03     0
## 
##  To see confidence intervals of the correlations, print with the short=FALSE option
## 
##  Confidence intervals based upon normal theory.  To get bootstrapped values, try cor.ci
##             lower     r upper    p
## FFMQT-Escor -0.27 -0.08  0.13 0.46
## FFMQT-FAGER -0.48 -0.31 -0.12 0.02
## FFMQT-QSU1  -0.37 -0.17  0.03 0.30
## FFMQT-QSU2  -0.47 -0.30 -0.10 0.03
## FFMQT-HADns -0.49 -0.32 -0.13 0.01
## FFMQT-HADdp -0.60 -0.45 -0.28 0.00
## FFMQT-AftPs  0.34  0.51  0.64 0.00
## FFMQT-AftNg -0.52 -0.35 -0.16 0.01
## FFMQT-Medo  -0.51 -0.34 -0.15 0.01
## FFMQT-Hstld -0.58 -0.42 -0.24 0.00
## FFMQT-Culpa -0.46 -0.29 -0.09 0.03
## FFMQT-Trstz -0.52 -0.36 -0.17 0.00
## FFMQT-Jvldd  0.29  0.47  0.61 0.00
## FFMQT-AtAfr  0.05  0.25  0.44 0.07
## FFMQT-Atntv  0.31  0.48  0.62 0.00
## FFMQT-Timdz -0.44 -0.26 -0.06 0.06
## FFMQT-Fadig -0.43 -0.25 -0.05 0.07
## FFMQT-Srndd  0.20  0.39  0.55 0.00
## FFMQT-Suprs -0.33 -0.14  0.07 0.38
## FFMQT-CESD1 -0.48 -0.31 -0.11 0.02
## FFMQT-CESD2 -0.55 -0.39 -0.20 0.00
## FFMQT-CESD3 -0.47 -0.30 -0.10 0.03
## FFMQT-CESD4  0.31  0.48  0.62 0.00
```

