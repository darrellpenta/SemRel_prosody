# DP 3/5 Testing model

rm(list=ls())
library(languageR)
library(lme4)
library(lmerTest)

# ------------------------------SET UP INPUT FILE(S)----------------------------
d.base <- read.csv("data/d_prosody.csv")
as.factor(d.base$subject)
as.factor(d.base$item)
#------------------------------SET UP OUTPUT FILE------------------------------
sink("output/SemRel Prosody Analyses.txt")


cat(" ", "\n")
cat("SEMREL PROSODY ANALYSES RUN ON: ", format(Sys.time(), "%b. %d, %Y at %T"), sep = "", fill= 70)
cat(" ", "\n")




# -------------------CREATE SUBSETS--------------------------------------------
d.d1 <- subset(d.base, word.type == "D1")
d.n1 <- subset(d.base, word.type == "N1")
d.p1 <- subset(d.base, word.type == "P1")
d.d2 <- subset(d.base, word.type == "D2")
d.a1 <- subset(d.base, word.type == "A1")
d.n2 <- subset(d.base, word.type == "N2")

#--------------------PREPARE PREDICTOR VARIABLES FOR MODEL---------------------
# Below, the separate measures for each of the predictors are combined, centered, and scaled to minimize conlinearity where relevent (i.e., all but plausibiltiy and association), and values are added to new columns

d.d1$relat    <- scale(d.d1$related, center=TRUE, scale=TRUE)
d.d1$integ    <- scale(d.d1$integrated, center=TRUE, scale=TRUE)
d.d1$plaus    <- scale(d.d1$plausibility, center=TRUE, scale=TRUE)
d.d1$assoc    <- d.d1$association

d.n1$relat    <- scale(d.n1$related, center=TRUE, scale=TRUE)
d.n1$integ    <- scale(d.n1$integrated, center=TRUE, scale=TRUE)
d.n1$plaus    <- scale(d.n1$plausibility, center=TRUE, scale=TRUE)
d.n1$assoc    <- d.n1$association

d.p1$relat    <- scale(d.p1$related, center=TRUE, scale=TRUE)
d.p1$integ    <- scale(d.p1$integrated, center=TRUE, scale=TRUE)
d.p1$plaus    <- scale(d.p1$plausibility, center=TRUE, scale=TRUE)
d.p1$assoc    <- d.p1$association

d.d2$relat    <- scale(d.d2$related, center=TRUE, scale=TRUE)
d.d2$integ    <- scale(d.d2$integrated, center=TRUE, scale=TRUE)
d.d2$plaus    <- scale(d.d2$plausibility, center=TRUE, scale=TRUE)
d.d2$assoc    <- d.d2$association

d.a1$relat    <- scale(d.a1$related, center=TRUE, scale=TRUE)
d.a1$integ    <- scale(d.a1$integrated, center=TRUE, scale=TRUE)
d.a1$plaus    <- scale(d.a1$plausibility, center=TRUE, scale=TRUE)
d.a1$assoc    <- d.a1$association

d.n2$relat    <- scale(d.n2$related, center=TRUE, scale=TRUE)
d.n2$integ    <- scale(d.n2$integrated, center=TRUE, scale=TRUE)
d.n2$plaus    <- scale(d.n2$plausibility, center=TRUE, scale=TRUE)
d.n2$assoc    <- d.n2$association
# -----------------------------------------------------------------------------
# ---------------------------CREATE AND COMPARE MODELS--------------------------
# -------------------------------------------------------------------------------


# D1 -------------------
cat(rep(c("-"), times=40, quote=F),"\n")
cat("SUBSET D1", sep = "", fill = 60)
cat(rep(c("-"), times=40, quote=F), "\n")

pros.d1 <- lmer(seconds ~  relat + integ  + assoc + plaus + (1|subject) + (1|item), data = d.d1, REML=FALSE)
print(summary(pros.d1))

# N1 -------------------

cat(rep(c("-"), times=40, quote=F),"\n")
cat("SUBSET N1", sep = "", fill = 60)
cat(rep(c("-"), times=40, quote=F), "\n")
pros.n1 <- lmer(seconds ~  relat + integ  + assoc + plaus + (1|subject) + (1|item), data = d.n1, REML=FALSE)
print(summary(pros.n1))

# P1 -------------------

cat(rep(c("-"), times=40, quote=F),"\n")
cat("SUBSET P1", sep = "", fill = 60)
cat(rep(c("-"), times=40, quote=F), "\n")
pros.p1 <- lmer(seconds ~  relat + integ  + assoc + plaus + (1|subject) + (1|item), data = d.p1, REML=FALSE)
print(summary(pros.p1))

# D2 -------------------

cat(rep(c("-"), times=40, quote=F),"\n")
cat("SUBSET D2", sep = "", fill = 60)
cat(rep(c("-"), times=40, quote=F), "\n")
pros.d2 <- lmer(seconds ~  relat + integ  + assoc + plaus + (1|subject) + (1|item), data = d.d2, REML=FALSE)
print(summary(pros.d2))

# A1 -------------------

cat(rep(c("-"), times=40, quote=F),"\n")
cat("SUBSET A1", sep = "", fill = 60)
cat(rep(c("-"), times=40, quote=F), "\n")
pros.a1 <- lmer(seconds ~  relat + integ  + assoc + plaus + (1|subject) + (1|item), data = d.a1, REML=FALSE)
print(summary(pros.a1))

# N2 -------------------

cat(rep(c("-"), times=40, quote=F),"\n")
cat("SUBSET N2", sep = "", fill = 60)
cat(rep(c("-"), times=40, quote=F), "\n")
pros.n2 <- lmer(seconds ~  relat + integ  + assoc + plaus + (1|subject) + (1|item), data = d.n2, REML=FALSE)
print(summary(pros.n2))

sink()



View(d.base)

library(dplyr)
library(reshape2)



d <- read.csv("output/d_prosody.csv")
View(d)
d.val <- read.csv("data/prosody_items_original.csv")
View(d.val)
subtlex <- read.csv("data/subtlex.csv")
View(subtlex)
d.backup<-d


# ----------------------------------------------------
d.val$LengthPhon.Det <-2
d.len.phon <- d.val[,c("subitem","LengthPhon.Det","LengthPhon.Head","LengthPhon.Prep","LengthPhon.Adj","LengthPhon.Det","LengthPhon.Noun")] #SUBSET OF MAIN SHEET
d.len.phon<-melt(d.len.phon, id.vars=1) #create melted version of data
View(d.len.phon)
d.len.phon$test<-NA
d.len.phon$test<-rep(c("D1","N1","P1","A1","D2","N2"), each = 698)
d.len.phon$unique.id <- paste(d.len.phon[,1],d.len.phon[,4],sep="-") #concatenate string
d <-left_join(d,d.len.phon, by="unique.id") #join b to ad
# ----------------------------------------------------
d.val$Length.Syl.Det <-1
d.len.syll <- d.val[,c(1,68,24:25,68,26:27)] #SUBSET OF MAIN SHEET
View(d.len.syll)
d.len.syll<-melt(d.len.syll, id.vars=1) #create melted version of data
View(d.len.syll)
d.len.syll$test<-NA
d.len.syll$test<-rep(c("D1","N1","P1","A1","D2","N2"), each = 698)
d.len.syll$unique.id <- paste(d.len.syll[,1],d.len.syll[,4],sep="-") #concatenate string
names(d)[names(d)=="value"] <- "len.syll"
d.len.syll$test = NULL
d.len.syll$variable = NULL
d <-left_join(d,d.len.syll, by="unique.id") #join b to ad

library(dplyr)
library(reshape2)
d <- read.csv("output/d_prosody.csv")

d.val <- read.csv("data/prosody_items_original.csv")
d.int <- d.val[,c(1,6)]
d.int<-melt(d.int, id.vars=1)
merge(d.base,d.int, by="subitem")
View(d.base)
d.phon <- d.val[,c(1,20:24)]
d.phon$LengthSylHead = NULL
d.phonm <-melt(d.phon, id.vars=1)

d$word <- sapply(d$word, as.character)
d$word[is.na(d$word)] <- " "
as.data.frame(d$word)
d$word<-tolower(d$word)

# ---populates column with freq. values----
d$freq<- "NA"
d$freq <- sapply(d$freq, as.character)
d$freq[is.na(d$freq)] <- " "
as.data.frame(d$freq)

freq <-
  for (i in d$word) {
    if(length(subtlex[subtlex$Word == i, 7]) == 0) {
      d[d$word == i, 9] <- 0
    }  else {
      d[d$word == i, 9] <- subtlex[subtlex$Word ==i, 7]
    }
  }

# ------

d$len.char <- NA
d$len.char <- nchar(d$word)

# -----
dys <-d.val[,c("subitem","dysfluency")]
d<-merge(d,dys, by = "subitem")



d$len.phon <- "NA"
d$len.phon <- sapply(d$len.phon, as.character)
d$len.phon[is.na(d$len.phon)] <- " "
as.data.frame(d$len.phon)

freq <-
  for (i in d$word) {
    if(length(subtlex[subtlex$Word == i, 7]) == 0) {
      d[d$word == i, 9] <- 0
    }  else {
      d[d$word == i, 9] <- subtlex[subtlex$Word ==i, 7]
    }
  }
