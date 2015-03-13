
rm(list=ls())
#install.packages("corrplot")
library(corrplot)
library(languageR)
library(lme4)
library(lmerTest)


# ------------------------------SET UP INPUT FILE(S)----------------------------
d.base <- read.csv("data/d_prosody.csv",colClasses="character")
d.base$subject   <- as.factor(d.base$subject)
d.base$item      <- as.factor(d.base$item)
#d.base$word.type <- as.factor(d.base$word.type)
d.base$seconds   <- as.numeric(d.base$seconds)
# scaling freq or not makes no difference in whether models run, but DOES affect results (a bit)
d.base$freq      <- as.numeric(d.base$freq)
#d.base$freq      <- scale(as.numeric(d.base$freq), center = TRUE, scale = TRUE)
d.base$len.char  <- scale(as.numeric(d.base$len.char), center=TRUE, scale=TRUE)
d.base$len.phon  <- scale(as.numeric(d.base$len.phon), center=TRUE, scale=TRUE)
d.base$len.syll  <- scale(as.numeric(d.base$len.syll), center=TRUE, scale=TRUE)
d.base$relat     <- scale(as.numeric(d.base$related), center=TRUE, scale=TRUE)
d.base$integ     <- scale(as.numeric(d.base$integrated), center=TRUE, scale=TRUE)
d.base$plaus     <- scale(as.numeric(d.base$plausibility), center=TRUE, scale=TRUE)
d.base$assoc     <- scale(as.numeric(d.base$association), center = TRUE, scale = TRUE)
d.base$n1.len    <- scale(as.numeric(d.base$n1.len), center = TRUE, scale = TRUE)
d.base$p1.len    <- scale(as.numeric(d.base$p1.len), center = TRUE, scale = TRUE)
d.base$a1.len    <- scale(as.numeric(d.base$a1.len), center = TRUE, scale = TRUE)
d.base$n2.len    <- scale(as.numeric(d.base$n2.len), center = TRUE, scale = TRUE)

# scaling fr.pre.wd or not makes no difference
d.base$fr.pre.wd <- as.numeric(d.base$pre.word.freq)
#d.base$fr.pre.wd <- scale(as.numeric(d.base$pre.word.freq), center = TRUE, scale = TRUE)

# scaling fr.pos.wd causes computation of residuals from 5 preamble positions to throw a warning
#d.base$fr.pos.wd <- scale(as.numeric(d.base$post.word.freq), center = TRUE, scale = TRUE)
d.base$fr.pos.wd <- as.numeric(d.base$post.word.freq)

# View(d.base)


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



# ------------------------------------------------------------------------------------------------------------------
# ---------------------------CREATE AND COMPARE MODELS--------------------------------------------------------------
# ------------------------------------------------------------------------------------------------------------------


# D1 -------------------
cat(rep(c("-"), times=40, quote=F),"\n")
cat("SUBSET D1 (with Related)", sep = "", fill = 60)
cat(rep(c("-"), times=40, quote=F), "\n")
pros.d1.r <- lmer(seconds ~  relat + integ  + plaus + n1.len + p1.len + a1.len + n2.len + fr.pos.wd + (1|subject) + (1|item), data = d.d1, REML=TRUE)
print(summary(pros.d1.r))

cat(rep(c("-"), times=40, quote=F),"\n")
cat("SUBSET D1 (with Association)", sep = "", fill = 60)
cat(rep(c("-"), times=40, quote=F), "\n")
pros.d1.a <- lmer(seconds ~  assoc + integ  + plaus + n1.len + p1.len + a1.len + n2.len + fr.pos.wd + (1|subject) + (1|item), data = d.d1, REML=TRUE)
print(summary(pros.d1.a))


# N1 -------------------
cat(rep(c("\n"), times = 2), rep(c("-"), times=40, quote=F),"\n")
cat("SUBSET N1 (with Related)", sep = "", fill = 60)
cat(rep(c("-"), times=40, quote=F), "\n")
pros.n1.r <- lmer(seconds ~  relat + integ + plaus + freq + len.char + len.phon + len.syll + p1.len + a1.len + n2.len + fr.pos.wd + (1|subject) + (1|item), data = d.n1, REML=TRUE)
print(summary(pros.n1.r))

cat(rep(c("-"), times=40, quote=F),"\n")
cat("SUBSET N1 (with Association)", sep = "", fill = 60)
cat(rep(c("-"), times=40, quote=F), "\n")
pros.n1.a <- lmer(seconds ~  assoc + integ + plaus + freq + len.char + len.phon + len.syll + p1.len + a1.len + n2.len + fr.pos.wd + (1|subject) + (1|item), data = d.n1, REML=TRUE)
print(summary(pros.n1.a))


# P1 -------------------
cat(rep(c("\n"), times = 2), rep(c("-"), times=40, quote=F),"\n")
cat("SUBSET P1 (with Related)", sep = "", fill = 60)
cat(rep(c("-"), times=40, quote=F), "\n")
pros.p1.r <- lmer(seconds ~ relat + integ  + plaus + freq + len.char + len.phon + n1.len + a1.len + n2.len + fr.pre.wd + (1|subject) + (1|item), data = d.p1, REML=TRUE)
print(summary(pros.p1.r))

cat(rep(c("-"), times=40, quote=F),"\n")
cat("SUBSET P1 (with Association)", sep = "", fill = 60)
cat(rep(c("-"), times=40, quote=F), "\n")
pros.p1.a <- lmer(seconds ~ assoc + integ  + plaus + freq + len.char + len.phon + n1.len + a1.len + n2.len + fr.pre.wd + (1|subject) + (1|item), data = d.p1, REML=TRUE)
print(summary(pros.p1.a))

# D2 -------------------
cat(rep(c("\n"), times = 2), rep(c("-"), times=40, quote=F),"\n")
cat("SUBSET D2 (with Related)", sep = "", fill = 60)
cat(rep(c("-"), times=40, quote=F), "\n")
pros.d2.r <- lmer(seconds ~  relat + integ + plaus + n1.len + p1.len + a1.len + n2.len + fr.pre.wd + fr.pos.wd + (1|subject) + (1|item), data = d.d2, REML=TRUE)
print(summary(pros.d2.r))

cat(rep(c("-"), times=40, quote=F),"\n")
cat("SUBSET D2 (with Association)", sep = "", fill = 60)
cat(rep(c("-"), times=40, quote=F), "\n")
pros.d2.a <- lmer(seconds ~ assoc + integ + plaus + n1.len + p1.len + a1.len + n2.len + fr.pre.wd + fr.pos.wd + (1|subject) + (1|item), data = d.d2, REML=TRUE)
print(summary(pros.d2.a))

# A1 -------------------

cat(rep(c("\n"), times = 2), rep(c("-"), times=40, quote=F),"\n")
cat("SUBSET A1 (with Related)", sep = "", fill = 60)
cat(rep(c("-"), times=40, quote=F), "\n")
pros.a1.r <- lmer(seconds ~ relat + integ  + plaus + freq + len.char + len.phon + len.syll + n1.len + p1.len + n2.len + fr.pos.wd + (1|subject) + (1|item), data = d.a1, REML=TRUE)
print(summary(pros.a1.r))

cat(rep(c("-"), times=40, quote=F),"\n")
cat("SUBSET A1 (with Association)", sep = "", fill = 60)
cat(rep(c("-"), times=40, quote=F), "\n")
pros.a1.a <- lmer(seconds ~ assoc + integ  + plaus + freq + len.char + len.phon + len.syll  + n1.len + p1.len + n2.len + fr.pos.wd + (1|subject) + (1|item), data = d.a1, REML=TRUE)
print(summary(pros.a1.a))

# N2 -------------------
cat(rep(c("\n"), times = 2), rep(c("-"), times=40, quote=F),"\n")
cat("SUBSET N2 (with Related)", sep = "", fill = 60)
cat(rep(c("-"), times=40, quote=F), "\n")
pros.n2.r <- lmer(seconds ~  relat + integ  + plaus + freq + len.char + len.phon + len.syll + n1.len + p1.len + a1.len + fr.pre.wd + fr.pos.wd + (1|subject) + (1|item), data = d.n2, REML=TRUE)
print(summary(pros.n2.r))

cat(rep(c("-"), times=40, quote=F),"\n")
cat("SUBSET N2 (with Association)", sep = "", fill = 60)
cat(rep(c("-"), times=40, quote=F), "\n")
pros.n2.a <- lmer(seconds ~  assoc + integ  + plaus + freq + len.char + len.phon + len.syll  + n1.len + p1.len + a1.len + fr.pre.wd + fr.pos.wd + (1|subject) + (1|item), data = d.n2, REML=TRUE)
print(summary(pros.n2.a))

# CORRELATION MATRIX -------------------
cat(rep(c("\n"), times = 2), rep(c("-"), times=40, quote=F),"\n")
cat("CORRELATION MATRIX", sep = "", fill = 60)
cat(rep(c("-"), times=40, quote=F), "\n")
d.cor <-d.base[,c(5, 9:12, 19:22,25:30)]
d.cor <-subset(d.cor, word.type =="D1" | word.type == "N1" | word.type == "P1" | word.type == "D2" | word.type == "A1"| word.type=="N2")
d.cor$word.type = NULL

cor.mat <-cor(d.cor, use="complete")
print(cor.mat, digits=4)
png(file="figures/correlation_matrix.png")
plot.title = paste("Semrel Prosody Correlation Matrix")
#corrplot(cor.mat, method = "circle", main = plot.title, mar=c(1,1,2,1))
corrplot.mixed(cor.mat, main = plot.title, mar=c(1,1,2,1))
dev.off()


sink()

#===================  COMPUTING & USING RESIDUALS from here on ================

sink("output/SemRel Prosody Analyses-Residuals.txt")
cat(" ", "\n")
cat("SEMREL PROSODY ANALYSES with RESIDUALS RUN ON: ", format(Sys.time(), "%b. %d, %Y at %T"), sep = "", fill= 70)
cat(" ", "\n")


d.preamble.6pos <- subset(d.base, word.type == "D1" | word.type == "N1" | word.type == "P1" | word.type == "D2" | word.type == "A1"| word.type == "N2")
pros.preamble.frln <- lmer( seconds ~ freq + len.phon  + (1 + freq + len.phon |subject) + (1 + freq + len.phon |item), data = d.preamble.6pos, REML=TRUE)
d.preamble.6pos$resid.frln <- residuals(pros.preamble.frln)
cat(rep(c("-"), times=40, quote=F),"\n")
cat("6pos Freq. & Length", sep = "", fill = 60)
cat(rep(c("-"), times=40, quote=F),"\n")
print(summary(pros.preamble.frln))

pros.preamble.fpos <- lmer( resid.frln ~ fr.pos.wd + (1 + fr.pos.wd|subject) + (1 + fr.pos.wd|item), data = d.preamble.6pos, REML=TRUE)
d.preamble.6pos$resid.frln.fpos <- residuals(pros.preamble.fpos)
cat(rep(c("-"), times=40, quote=F),"\n")
cat("6pos Freq. Following Word", sep = "", fill = 60)
cat(rep(c("-"), times=40, quote=F),"\n")
print(summary(pros.preamble.fpos))

d.preamble.5pos <- subset(d.preamble.6pos, word.type != "D1")
pros.preamble.fpre <- lmer( resid.frln.fpos ~ fr.pre.wd + (1 + fr.pre.wd|subject) + (1 + fr.pre.wd|item), data = d.preamble.5pos, REML=TRUE)
d.preamble.5pos$resid.fpre <- residuals(pros.preamble.fpre)
cat(rep(c("-"), times=40, quote=F),"\n")
cat("6pos Freq. Preceding Word", sep = "", fill = 60)
cat(rep(c("-"), times=40, quote=F),"\n")
print(summary(pros.preamble.fpre))


# ---------------------- REGRESSION CREATE SUBSETS-----------------------------------

d.d1 <- subset(d.preamble.6pos, word.type == "D1")
d.n1 <- subset(d.preamble.5pos, word.type == "N1")
d.p1 <- subset(d.preamble.5pos, word.type == "P1")
d.d2 <- subset(d.preamble.5pos, word.type == "D2")
d.a1 <- subset(d.preamble.5pos, word.type == "A1")
d.n2 <- subset(d.preamble.5pos, word.type == "N2")

# ------------------------------------------------------------------------------------------------------------------
# ---------------------------CREATE AND COMPARE RESIDUALS MODELS -------------------------------------------------
# ------------------------------------------------------------------------------------------------------------------


# D1 (w/Resids)-------------------
cat(rep(c("\n"), times = 2), rep(c("-"), times=40, quote=F),"\n")
cat("D1", sep = "", fill = 60)
cat(rep(c("-"), times=40, quote=F),"\n")
pros.preamble.d1 <- lmer( resid.frln.fpos ~ plaus + (1 + plaus|subject) + (1 + plaus|item), data = d.d1, REML=TRUE)
d.d1$resid <- residuals(pros.preamble.d1)
cat(" ", "\n")
cat("D1:: Plaus", sep = "", fill = 60)
cat(rep(c("-"), times=20, quote=F),"\n")
print(summary(pros.preamble.d1))

pros.preamble.d1 <- lmer( resid ~ n1.len + p1.len + (1 + n1.len + p1.len|subject) + (1 + n1.len + p1.len|item), data = d.d1, REML=TRUE)
d.d1$resid <- residuals(pros.preamble.d1)
cat(" ", "\n")
cat("D1:: N1 & P1 Length", sep = "", fill = 60)
cat(rep(c("-"), times=20, quote=F),"\n")
print(summary(pros.preamble.d1))

pros.preamble.d1 <- lmer( resid ~ n2.len + (1 + n2.len|subject) + (1 + n2.len|item), data = d.d1, REML=TRUE)
d.d1$resid <- residuals(pros.preamble.d1)
cat(" ", "\n")
cat("D1:: (s/b A1) &  N2 Length", sep = "", fill = 60)
cat(rep(c("-"), times=20, quote=F),"\n")
print(summary(pros.preamble.d1))


cat(rep(c("-"), times=40, quote=F),"\n")
cat("SUBSET D1 (with Related) residuals", sep = "", fill = 60)
cat(rep(c("-"), times=40, quote=F), "\n")
pros.d1.r <- lmer(resid ~ relat + integ + (1|subject) + (1|item), data = d.d1, REML=TRUE)
print(summary(pros.d1.r))

cat(rep(c("-"), times=40, quote=F),"\n")
cat("SUBSET D1 (with Association) residuals", sep = "", fill = 60)
cat(rep(c("-"), times=40, quote=F), "\n")
pros.d1.a <- lmer(resid ~ assoc + integ + (1|subject) + (1|item), data = d.d1, REML=TRUE)
print(summary(pros.d1.a))



# N1 (w/ Resids)-------------------
cat(rep(c("\n"), times = 2), rep(c("-"), times=40, quote=F),"\n")
cat("N1", sep = "", fill = 60)
cat(rep(c("-"), times=40, quote=F),"\n")
pros.preamble.n1 <- lmer( resid.frln.fpos ~ plaus + (1 + plaus|subject) + (1 + plaus|item), data = d.n1, REML=TRUE)
d.n1$resid <- residuals(pros.preamble.n1)
cat(" ", "\n")
cat("N1:: Plaus", sep = "", fill = 60)
cat(rep(c("-"), times=20, quote=F),"\n")
print(summary(pros.preamble.n1))

pros.preamble.n1 <- lmer( resid ~  p1.len + a1.len + (1 + p1.len + a1.len|subject) + (1 + p1.len + a1.len|item), data = d.n1, REML=TRUE)
d.n1$resid <- residuals(pros.preamble.n1)
cat(" ", "\n")
cat("N1:: P1 & A1 Length", sep = "", fill = 60)
cat(rep(c("-"), times=20, quote=F),"\n")
print(summary(pros.preamble.n1))

pros.preamble.n1 <- lmer( resid ~ n2.len + (1 + n2.len|subject) + (1 + n2.len|item), data = d.n1, REML=TRUE)
d.n1$resid <- residuals(pros.preamble.n1)
cat(" ", "\n")
cat("N1:: N2 Length", sep = "", fill = 60)
cat(rep(c("-"), times=20, quote=F),"\n")
print(summary(pros.preamble.n1))


cat(rep(c("-"), times=40, quote=F),"\n")
cat("SUBSET N1 (with Related) residuals", sep = "", fill = 60)
cat(rep(c("-"), times=40, quote=F), "\n")
pros.n1.r <- lmer(resid ~  relat + integ + (1|subject) + (1|item), data = d.n1, REML=TRUE)
print(summary(pros.n1.r))

cat(rep(c("-"), times=40, quote=F),"\n")
cat("SUBSET N1 (with Association) residuals", sep = "", fill = 60)
cat(rep(c("-"), times=40, quote=F), "\n")
pros.n1.a <- lmer(resid ~  assoc + integ  + (1|subject) + (1|item), data = d.n1, REML=TRUE)
print(summary(pros.n1.a))


# P1 (w/ Resids)-------------------

cat(rep(c("\n"), times = 2), rep(c("-"), times=40, quote=F),"\n")
cat("P1", sep = "", fill = 60)
cat(rep(c("-"), times=40, quote=F),"\n")
pros.preamble.p1 <- lmer( resid.frln.fpos ~ plaus + (1 + plaus|subject) + (1 + plaus|item), data = d.p1, REML=TRUE)
d.p1$resid <- residuals(pros.preamble.p1)
cat(" ", "\n")
cat("P1:: Plaus", sep = "", fill = 60)
cat(rep(c("-"), times=20, quote=F),"\n")
print(summary(pros.preamble.p1))

pros.preamble.p1 <- lmer( resid ~  n1.len + p1.len + (1 + n1.len + p1.len|subject) + (1 + n1.len + p1.len|item), data = d.p1, REML=TRUE)
d.p1$resid <- residuals(pros.preamble.p1)
cat(" ", "\n")
cat("P1:: N1 & P1 Length", sep = "", fill = 60)
cat(rep(c("-"), times=20, quote=F),"\n")
print(summary(pros.preamble.p1))

pros.preamble.p1 <- lmer( resid ~ a1.len + n2.len + (1 + a1.len + n2.len|subject) + (1 + a1.len + n2.len|item), data = d.p1, REML=TRUE)
d.p1$resid <- residuals(pros.preamble.p1)
cat(" ", "\n")
cat("P1:: A1 &  N2 Length", sep = "", fill = 60)
cat(rep(c("-"), times=20, quote=F),"\n")
print(summary(pros.preamble.p1))

cat(rep(c("-"), times=40, quote=F),"\n")
cat("SUBSET P1 (with Related) residuals", sep = "", fill = 60)
cat(rep(c("-"), times=40, quote=F), "\n")
pros.p1.r <- lmer(resid ~ relat + integ + (1|subject) + (1|item), data = d.p1, REML=TRUE)
print(summary(pros.p1.r))

cat(rep(c("-"), times=40, quote=F),"\n")
cat("SUBSET P1 (with Association) residuals", sep = "", fill = 60)
cat(rep(c("-"), times=40, quote=F), "\n")
pros.p1.a <- lmer(resid ~ assoc + integ + (1|subject) + (1|item), data = d.p1, REML=TRUE)
print(summary(pros.p1.a))

# D2 (w/ Resids) -------------------
cat(rep(c("\n"), times = 2), rep(c("-"), times=40, quote=F),"\n")
cat("D2", sep = "", fill = 60)
cat(rep(c("-"), times=40, quote=F),"\n")
pros.preamble.d2 <- lmer( resid.frln.fpos ~ plaus + (1 + plaus|subject) + (1 + plaus|item), data = d.d2, REML=TRUE)
d.d2$resid <- residuals(pros.preamble.d2)
cat(" ", "\n")
cat("D2:: Plaus", sep = "", fill = 60)
cat(rep(c("-"), times=20, quote=F),"\n")
print(summary(pros.preamble.d2))

pros.preamble.d2 <- lmer( resid ~  n1.len + a1.len + (1 + n1.len + a1.len|subject) + (1 + n1.len + a1.len|item), data = d.d2, REML=TRUE)
d.d2$resid <- residuals(pros.preamble.d2)
cat(" ", "\n")
cat("D2:: N1 & A1 Length", sep = "", fill = 60)
cat(rep(c("-"), times=20, quote=F),"\n")
print(summary(pros.preamble.d2))

pros.preamble.d2 <- lmer( resid ~ n2.len + (1 + n2.len|subject) + (1 + n2.len|item), data = d.d2, REML=TRUE)
d.d2$resid <- residuals(pros.preamble.d2)
cat(" ", "\n")
cat("D2:: N2 Length", sep = "", fill = 60)
cat(rep(c("-"), times=20, quote=F),"\n")
print(summary(pros.preamble.d2))

cat(rep(c("-"), times=40, quote=F),"\n")
cat("SUBSET D2 (with Related) residuals", sep = "", fill = 60)
cat(rep(c("-"), times=40, quote=F), "\n")
pros.d2.r <- lmer(resid ~  relat + integ + (1|subject) + (1|item), data = d.d2, REML=TRUE)
print(summary(pros.d2.r))

cat(rep(c("-"), times=40, quote=F),"\n")
cat("SUBSET D2 (with Association) residuals", sep = "", fill = 60)
cat(rep(c("-"), times=40, quote=F), "\n")
pros.d2.a <- lmer(resid ~ assoc + integ + (1|subject) + (1|item), data = d.d2, REML=TRUE)
print(summary(pros.d2.a))

# A1 (w/ Resids)-------------------
cat(rep(c("\n"), times = 2), rep(c("-"), times=40, quote=F),"\n")
cat("A1", sep = "", fill = 60)
cat(rep(c("-"), times=40, quote=F),"\n")
pros.preamble.a1 <- lmer( resid.frln.fpos ~ plaus + (1 + plaus|subject) + (1 + plaus|item), data = d.a1, REML=TRUE)
d.a1$resid <- residuals(pros.preamble.a1)
cat(" ", "\n")
cat("A1:: Plaus", sep = "", fill = 60)
cat(rep(c("-"), times=20, quote=F),"\n")
print(summary(pros.preamble.a1))

pros.preamble.a1 <- lmer( resid ~  n1.len + p1.len + (1 + n1.len + p1.len|subject) + (1 + n1.len + p1.len|item), data = d.a1, REML=TRUE)
d.a1$resid <- residuals(pros.preamble.a1)
cat(" ", "\n")
cat("A1:: N1 & P1 Length", sep = "", fill = 60)
cat(rep(c("-"), times=20, quote=F),"\n")
print(summary(pros.preamble.a1))

pros.preamble.a1 <- lmer( resid ~ n2.len + (1 + n2.len|subject) + (1 + n2.len|item), data = d.a1, REML=TRUE)
d.a1$resid <- residuals(pros.preamble.a1)
cat(" ", "\n")
cat("A1:: N2 Length", sep = "", fill = 60)
cat(rep(c("-"), times=20, quote=F),"\n")
print(summary(pros.preamble.a1))

cat(rep(c("\n"), times = 2), rep(c("-"), times=40, quote=F),"\n")
cat(rep(c("-"), times=40, quote=F),"\n")
cat("SUBSET A1 (with Related) residuals", sep = "", fill = 60)
cat(rep(c("-"), times=40, quote=F), "\n")
pros.a1.r <- lmer(resid ~ relat + integ + (1|subject) + (1|item), data = d.a1, REML=TRUE)
print(summary(pros.a1.r))

cat(rep(c("-"), times=40, quote=F),"\n")
cat("SUBSET A1 (with Association) residuals", sep = "", fill = 60)
cat(rep(c("-"), times=40, quote=F), "\n")
pros.a1.a <- lmer(resid ~ assoc + integ + (1|subject) + (1|item), data = d.a1, REML=TRUE)
print(summary(pros.a1.a))

# N2 (w/ Resids)-------------------

cat(rep(c("\n"), times = 2), rep(c("-"), times=40, quote=F),"\n")
cat("N2", sep = "", fill = 60)
cat(rep(c("-"), times=40, quote=F),"\n")
pros.preamble.n2 <- lmer( resid.frln.fpos ~ plaus + (1 + plaus|subject) + (1 + plaus|item), data = d.n2, REML=TRUE)
d.n2$resid <- residuals(pros.preamble.n2)
cat(" ", "\n")
cat("N2:: Plaus", sep = "", fill = 60)
cat(rep(c("-"), times=20, quote=F),"\n")
print(summary(pros.preamble.n2))

pros.preamble.n2 <- lmer( resid ~  n1.len + p1.len + (1 + n1.len + p1.len|subject) + (1 + n1.len + p1.len|item), data = d.n2, REML=TRUE)
d.n2$resid <- residuals(pros.preamble.n2)
cat(" ", "\n")
cat("N2:: N1 & P1 Length", sep = "", fill = 60)
cat(rep(c("-"), times=20, quote=F),"\n")
print(summary(pros.preamble.n2))

pros.preamble.n2 <- lmer( resid ~ a1.len + (1 + a1.len|subject) + (1 + a1.len|item), data = d.n2, REML=TRUE)
d.n2$resid <- residuals(pros.preamble.n2)
cat(" ", "\n")
cat("N2:: A1 Length", sep = "", fill = 60)
cat(rep(c("-"), times=20, quote=F),"\n")
print(summary(pros.preamble.n2))


cat(rep(c("-"), times=40, quote=F),"\n")
cat("SUBSET N2 (with Related) residuals", sep = "", fill = 60)
cat(rep(c("-"), times=40, quote=F), "\n")
pros.n2.r <- lmer(resid ~  relat + integ + (1|subject) + (1|item), data = d.n2, REML=TRUE)
print(summary(pros.n2.r))

cat(rep(c("-"), times=40, quote=F),"\n")
cat("SUBSET N2 (with Association) residuals", sep = "", fill = 60)
cat(rep(c("-"), times=40, quote=F), "\n")
pros.n2.a <- lmer(resid ~  assoc + integ + (1|subject) + (1|item), data = d.n2, REML=TRUE)
print(summary(pros.n2.a))



sink()
