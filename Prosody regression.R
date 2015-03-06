# DP 3/5 Testing model

rm(list=ls())
library(languageR)
library(lme4)
library(lmerTest)

# ------------------------------SET UP INPUT FILE(S)----------------------------
d.base <- read.csv("data/d_prosody.csv")
as.factor(d.base$subject)
as.factor(d.base$item)

# For meeting with Neal
# View(d.base)
# d.val <- read.csv("data/prosody_items_original.csv")
# View(d.val)

#------------------------------SET UP OUTPUT FILE------------------------------
sink("output/SemRel Prosody Analyses.txt")
cat(" ", "\n")
cat("SEMREL PROSODY ANALYSES RUN ON: ", format(Sys.time(), "%b. %d, %Y at %T"), sep = "", fill= 70)
cat(" ", "\n")




# ------------------------------CREATE SUBSETS-----------------------------------

d.d1 <- subset(d.base, word.type == "D1")
d.n1 <- subset(d.base, word.type == "N1")
d.p1 <- subset(d.base, word.type == "P1")
d.d2 <- subset(d.base, word.type == "D2")
d.a1 <- subset(d.base, word.type == "A1")
d.n2 <- subset(d.base, word.type == "N2")

# --------------------PREPARE PREDICTOR VARIABLES FOR MODEL---------------------

# D(et) 1 PREDICTORS
d.d1$relat    <- scale(d.d1$related, center=TRUE, scale=TRUE)
d.d1$integ    <- scale(d.d1$integrated, center=TRUE, scale=TRUE)
d.d1$plaus    <- scale(d.d1$plausibility, center=TRUE, scale=TRUE)
d.d1$assoc    <- d.d1$association

# N(oun) 1 PREDICTORS
d.n1$freq     <- d.n1$freq
d.n1$len.char <- scale(d.n1$len.char, center=TRUE, scale=TRUE)
d.n1$len.phon <- scale(d.n1$len.phon, center=TRUE, scale=TRUE)
d.n1$len.syll <- scale(d.n1$len.syll, center=TRUE, scale=TRUE)
d.n1$relat    <- scale(d.n1$related, center=TRUE, scale=TRUE)
d.n1$integ    <- scale(d.n1$integrated, center=TRUE, scale=TRUE)
d.n1$plaus    <- scale(d.n1$plausibility, center=TRUE, scale=TRUE)
d.n1$assoc    <- d.n1$association

# P(rep) 1 PREDICTORS
d.p1$freq     <- d.p1$freq
d.p1$len.char <- scale(d.p1$len.char, center=TRUE, scale=TRUE)
d.p1$len.phon <- scale(d.p1$len.phon, center=TRUE, scale=TRUE)
d.p1$relat    <- scale(d.p1$related, center=TRUE, scale=TRUE)
d.p1$integ    <- scale(d.p1$integrated, center=TRUE, scale=TRUE)
d.p1$plaus    <- scale(d.p1$plausibility, center=TRUE, scale=TRUE)
d.p1$assoc    <- d.p1$association

# D(et) 2 PREDICTORS
d.d2$relat    <- scale(d.d2$related, center=TRUE, scale=TRUE)
d.d2$integ    <- scale(d.d2$integrated, center=TRUE, scale=TRUE)
d.d2$plaus    <- scale(d.d2$plausibility, center=TRUE, scale=TRUE)
d.d2$assoc    <- d.d2$association

# A(dj) 1 PREDICTORS
d.a1$freq     <- d.a1$freq
d.a1$len.char <- scale(d.a1$len.char, center=TRUE, scale=TRUE)
d.a1$len.phon <- scale(d.a1$len.phon, center=TRUE, scale=TRUE)
d.a1$len.syll <- scale(d.a1$len.syll, center=TRUE, scale=TRUE)
d.a1$relat    <- scale(d.a1$related, center=TRUE, scale=TRUE)
d.a1$integ    <- scale(d.a1$integrated, center=TRUE, scale=TRUE)
d.a1$plaus    <- scale(d.a1$plausibility, center=TRUE, scale=TRUE)
d.a1$assoc    <- d.a1$association

# N(oun) 2 PREDICTORS
d.n2$freq     <- d.n2$freq
d.n2$len.char <- scale(d.n2$len.char, center=TRUE, scale=TRUE)
d.n2$len.phon <- scale(d.n2$len.phon, center=TRUE, scale=TRUE)
d.n2$len.syll <- scale(d.n2$len.syll, center=TRUE, scale=TRUE)
d.n2$relat    <- scale(d.n2$related, center=TRUE, scale=TRUE)
d.n2$integ    <- scale(d.n2$integrated, center=TRUE, scale=TRUE)
d.n2$plaus    <- scale(d.n2$plausibility, center=TRUE, scale=TRUE)
d.n2$assoc    <- d.n2$association


# ------------------------------------------------------------------------------------------------------------------
# ---------------------------CREATE AND COMPARE MODELS--------------------------------------------------------------
# ------------------------------------------------------------------------------------------------------------------


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
pros.n1 <- lmer(seconds ~  freq + len.char + len.phon + len.syll + relat + integ  + assoc + plaus + (1|subject) + (1|item), data = d.n1, REML=FALSE)
print(summary(pros.n1))


# P1 -------------------
cat(rep(c("-"), times=40, quote=F),"\n")
cat("SUBSET P1", sep = "", fill = 60)
cat(rep(c("-"), times=40, quote=F), "\n")
pros.p1 <- lmer(seconds ~ freq + len.char + len.phon +  relat + integ  + assoc + plaus + (1|subject) + (1|item), data = d.p1, REML=FALSE)
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
pros.a1 <- lmer(seconds ~ freq + len.char + len.phon + len.syll + relat + integ  + assoc + plaus + (1|subject) + (1|item), data = d.a1, REML=FALSE)
print(summary(pros.a1))

# N2 -------------------
cat(rep(c("-"), times=40, quote=F),"\n")
cat("SUBSET N2", sep = "", fill = 60)
cat(rep(c("-"), times=40, quote=F), "\n")
pros.n2 <- lmer(seconds ~  freq + len.char + len.phon + len.syll + relat + integ  + assoc + plaus + (1|subject) + (1|item), data = d.n2, REML=FALSE)
print(summary(pros.n2))

# N2 -------------------
cat(rep(c("-"), times=40, quote=F),"\n")
cat("CORRELATION MATRIX", sep = "", fill = 60)
cat(rep(c("-"), times=40, quote=F), "\n")
d.cor<-d.base[,c(5,9:16)]
d.cor<-subset(d.cor, word.type =="D1" | word.type == "N1" | word.type == "P1" | word.type == "D2" | word.type == "A1"| word.type=="N2")
d.cor$word.type = NULL
cor.mat <-cor(d.cor)
print(cor.mat, digits=4)
sink()

