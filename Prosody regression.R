# DP 3/5 Testing model

rm(list=ls())
library(languageR)
library(lme4)
library(lmerTest)


#------------------------------SET UP OUTPUT FILE------------------------------
sink("output/SemRel Prosody Analyses.txt")


cat(" ", "\n")
cat("SEMREL PROSODY ANALYSES RUN ON: ", format(Sys.time(), "%b. %d, %Y at %T"), sep = "", fill= 70)
cat(" ", "\n")

# ------------------------------SET UP INPUT FILE(S)----------------------------
d.base <- read.csv("data/d_prosody.csv")
as.factor(d.base$subject)
as.factor(d.base$item)


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


# D1-------------------
cat(rep(c("-"), times=40, quote=F),"\n")
cat("SUBSET D1", sep = "", fill = 60)
cat(rep(c("-"), times=40, quote=F), "\n")

pros.d1 <- lmer(seconds ~  relat + integ  + assoc + plaus + (1|subject) + (1|item), data = d.d1, REML=FALSE)
print(summary(pros.d1))

# N1-------------------

cat(rep(c("-"), times=40, quote=F),"\n")
cat("SUBSET N1", sep = "", fill = 60)
cat(rep(c("-"), times=40, quote=F), "\n")
pros.n1 <- lmer(seconds ~  relat + integ  + assoc + plaus + (1|subject) + (1|item), data = d.n1, REML=FALSE)
print(summary(pros.n1))
sink()



