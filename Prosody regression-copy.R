
rm(list=ls())
#install.packages("corrplot")
library(corrplot)
library(languageR)
library(lme4)
library(lmerTest)


# ------------------------------SET UP INPUT FILE(S)----------------------------
d.base <- read.csv("data/d_prosody.csv")
d.base$subject   <- as.factor(d.base$subject)
d.base$item      <- as.factor(d.base$item)
d.base$freq      <- d.base$freq
d.base$len.char  <- scale(d.base$len.char, center=TRUE, scale=TRUE)
d.base$len.phon  <- scale(d.base$len.phon, center=TRUE, scale=TRUE)
d.base$len.syll  <- scale(d.base$len.syll, center=TRUE, scale=TRUE)
d.base$relat     <- scale(d.base$related, center=TRUE, scale=TRUE)
d.base$integ     <- scale(d.base$integrated, center=TRUE, scale=TRUE)
d.base$plaus     <- scale(d.base$plausibility, center=TRUE, scale=TRUE)
d.base$assoc     <- d.base$association
d.base$n1.len    <- scale(d.base$n1.len, center = TRUE, scale = TRUE)
d.base$p1.len    <- scale(d.base$p1.len, center = TRUE, scale = TRUE)
d.base$a1.len    <- scale(d.base$a1.len, center = TRUE, scale = TRUE)
d.base$n2.len    <- scale(d.base$n2.len, center = TRUE, scale = TRUE)
d.base$fr.pre.wd <- (as.numeric(d.base$pre.word.freq))
d.base$fr.pos.wd <- (as.numeric(d.base$post.word.freq))

# For meeting with Neal
 View(d.base)
#  d.val <- read.csv("data/prosody_items_original.csv")
#  View(d.val)
#
# #------------------------------SET UP OUTPUT FILE------------------------------
# sink("output/SemRel Prosody Analyses.txt")
# cat(" ", "\n")
# cat("SEMREL PROSODY ANALYSES RUN ON: ", format(Sys.time(), "%b. %d, %Y at %T"), sep = "", fill= 70)
# cat(" ", "\n")
#
#
#
#
# # ------------------------------CREATE SUBSETS-----------------------------------
#
# d.d1 <- subset(d.base, word.type == "D1")
# d.n1 <- subset(d.base, word.type == "N1")
# d.p1 <- subset(d.base, word.type == "P1")
# d.d2 <- subset(d.base, word.type == "D2")
# d.a1 <- subset(d.base, word.type == "A1")
# d.n2 <- subset(d.base, word.type == "N2")
#
#
#
# # ------------------------------------------------------------------------------------------------------------------
# # ---------------------------CREATE AND COMPARE MODELS--------------------------------------------------------------
# # ------------------------------------------------------------------------------------------------------------------
#
#
# # D1 -------------------
# cat(rep(c("-"), times=40, quote=F),"\n")
# cat("SUBSET D1 (with Related)", sep = "", fill = 60)
# cat(rep(c("-"), times=40, quote=F), "\n")
# pros.d1.r <- lmer(seconds ~  relat + integ  + plaus + n1.len + p1.len + a1.len + n2.len + fr.pos.wd + (1|subject) + (1|item), data = d.d1, REML=TRUE)
# print(summary(pros.d1.r))
#
# cat(rep(c("-"), times=40, quote=F),"\n")
# cat("SUBSET D1 (with Association)", sep = "", fill = 60)
# cat(rep(c("-"), times=40, quote=F), "\n")
# pros.d1.a <- lmer(seconds ~  assoc + integ  + plaus + n1.len + p1.len + a1.len + n2.len + fr.pos.wd + (1|subject) + (1|item), data = d.d1, REML=TRUE)
# print(summary(pros.d1.a))
#
#
# # N1 -------------------
# cat(rep(c("-"), times=40, quote=F),"\n")
# cat("SUBSET N1 (with Related)", sep = "", fill = 60)
# cat(rep(c("-"), times=40, quote=F), "\n")
# pros.n1.r <- lmer(seconds ~  relat + integ + plaus + freq + len.char + len.phon + len.syll + p1.len + a1.len + n2.len + fr.pos.wd + (1|subject) + (1|item), data = d.n1, REML=TRUE)
# print(summary(pros.n1.r))
#
# cat(rep(c("-"), times=40, quote=F),"\n")
# cat("SUBSET N1 (with Association)", sep = "", fill = 60)
# cat(rep(c("-"), times=40, quote=F), "\n")
# pros.n1.a <- lmer(seconds ~  assoc + integ + plaus + freq + len.char + len.phon + len.syll + p1.len + a1.len + n2.len + fr.pos.wd + (1|subject) + (1|item), data = d.n1, REML=TRUE)
# print(summary(pros.n1.a))
#
#
# # P1 -------------------
# cat(rep(c("-"), times=40, quote=F),"\n")
# cat("SUBSET P1 (with Related)", sep = "", fill = 60)
# cat(rep(c("-"), times=40, quote=F), "\n")
# pros.p1.r <- lmer(seconds ~ relat + integ  + plaus + freq + len.char + len.phon + n1.len + a1.len + n2.len + fr.pre.wd + (1|subject) + (1|item), data = d.p1, REML=TRUE)
# print(summary(pros.p1.r))
#
# cat(rep(c("-"), times=40, quote=F),"\n")
# cat("SUBSET P1 (with Association)", sep = "", fill = 60)
# cat(rep(c("-"), times=40, quote=F), "\n")
# pros.p1.a <- lmer(seconds ~ assoc + integ  + plaus + freq + len.char + len.phon + n1.len + a1.len + n2.len + fr.pre.wd + (1|subject) + (1|item), data = d.p1, REML=TRUE)
# print(summary(pros.p1.a))
#
# # D2 -------------------
# cat(rep(c("-"), times=40, quote=F),"\n")
# cat("SUBSET D2 (with Related)", sep = "", fill = 60)
# cat(rep(c("-"), times=40, quote=F), "\n")
# pros.d2.r <- lmer(seconds ~  relat + integ + plaus + n1.len + p1.len + a1.len + n2.len + fr.pre.wd + fr.pos.wd + (1|subject) + (1|item), data = d.d2, REML=TRUE)
# print(summary(pros.d2.r))
#
# cat(rep(c("-"), times=40, quote=F),"\n")
# cat("SUBSET D2 (with Association)", sep = "", fill = 60)
# cat(rep(c("-"), times=40, quote=F), "\n")
# pros.d2.a <- lmer(seconds ~ assoc + integ + plaus + n1.len + p1.len + a1.len + n2.len + fr.pre.wd + fr.pos.wd + (1|subject) + (1|item), data = d.d2, REML=TRUE)
# print(summary(pros.d2.a))
#
# # A1 -------------------
#
# cat(rep(c("-"), times=40, quote=F),"\n")
# cat("SUBSET A1 (with Related)", sep = "", fill = 60)
# cat(rep(c("-"), times=40, quote=F), "\n")
# pros.a1.r <- lmer(seconds ~ relat + integ  + plaus + freq + len.char + len.phon + len.syll + n1.len + p1.len + n2.len + fr.pos.wd + (1|subject) + (1|item), data = d.a1, REML=TRUE)
# print(summary(pros.a1.r))
#
# cat(rep(c("-"), times=40, quote=F),"\n")
# cat("SUBSET A1 (with Association)", sep = "", fill = 60)
# cat(rep(c("-"), times=40, quote=F), "\n")
# pros.a1.a <- lmer(seconds ~ assoc + integ  + plaus + freq + len.char + len.phon + len.syll  + n1.len + p1.len + n2.len + fr.pos.wd + (1|subject) + (1|item), data = d.a1, REML=TRUE)
# print(summary(pros.a1.a))
#
# # N2 -------------------
# cat(rep(c("-"), times=40, quote=F),"\n")
# cat("SUBSET N2 (with Related)", sep = "", fill = 60)
# cat(rep(c("-"), times=40, quote=F), "\n")
# pros.n2.r <- lmer(seconds ~  relat + integ  + plaus + freq + len.char + len.phon + len.syll + n1.len + p1.len + a1.len + fr.pre.wd + fr.pos.wd + (1|subject) + (1|item), data = d.n2, REML=TRUE)
# print(summary(pros.n2.r))
#
# cat(rep(c("-"), times=40, quote=F),"\n")
# cat("SUBSET N2 (with Association)", sep = "", fill = 60)
# cat(rep(c("-"), times=40, quote=F), "\n")
# pros.n2.a <- lmer(seconds ~  assoc + integ  + plaus + freq + len.char + len.phon + len.syll  + n1.len + p1.len + a1.len + fr.pre.wd + fr.pos.wd + (1|subject) + (1|item), data = d.n2, REML=TRUE)
# print(summary(pros.n2.a))
#
# # CORRELATION MATRIX -------------------
# cat(rep(c("-"), times=40, quote=F),"\n")
# cat("CORRELATION MATRIX", sep = "", fill = 60)
# cat(rep(c("-"), times=40, quote=F), "\n")
# d.cor<-d.base[,c(5, 9:16, 19:22,29:30)]
# d.cor<-subset(d.cor, word.type =="D1" | word.type == "N1" | word.type == "P1" | word.type == "D2" | word.type == "A1"| word.type=="N2")
# d.cor$word.type = NULL
# cor.mat <-cor(d.cor, use="complete")
# print(cor.mat, digits=4)
#
# jpeg(file="figures/correlation_matrix.jpg")
# plot.title = paste("Semrel Prosody Correlation Matrix")
# #corrplot(cor.mat, method = "circle", main = plot.title, mar=c(1,1,2,1))
# corrplot.mixed(cor.mat, main = plot.title, mar=c(1,1,2,1))
#
# sink()
#===================  COMPUTING and USING RESIDUALS from here on ================

sink("output/SemRel Prosody Analyses-Regressions.txt")
cat(" ", "\n")
cat("SEMREL PROSODY ANALYSES with REGRESSIONS RUN ON: ", format(Sys.time(), "%b. %d, %Y at %T"), sep = "", fill= 70)
cat(" ", "\n")



# d.preamble.6pos <- subset(d.base, word.type == "D1" | word.type == "N1" | word.type == "P1" | word.type == "D2" | word.type == "A1"| word.type == "N2")
# pros.preamble.frln <- lmer( seconds ~ freq + len.phon + (1 + freq + len.phon|subject) + (1 + freq + len.phon|item), data = d.preamble.6pos, REML=TRUE)
# d.preamble.6pos$resid.frln <- residuals(pros.preamble.frln)
# #summary(pros.preamble.frln)
# pros.preamble.fpos <- lmer( resid.frln ~ fr.pos.wd + (1 + fr.pos.wd|subject) + (1 + fr.pos.wd|item), data = d.preamble.6pos, REML=TRUE)
# d.preamble.6pos$resid.frln.fpos <- residuals(pros.preamble.fpos)
# summary(pros.preamble.fpos)
#
#
# d.preamble.5pos <- subset(d.preamble.6pos, word.type != "D1")
# pros.preamble.fpre <- lmer( resid.frln.fpos ~ fr.pre.wd + (1 + fr.pre.wd|subject) + (1 + fr.pre.wd|item), data = d.preamble.5pos, REML=TRUE)
# d.preamble.5pos$resid.fpre <- residuals(pros.preamble.fpre)
# summary(pros.preamble.fpre)
#
# # # ------------------------------CREATE SUBSETS-----------------------------------
#
# d.d1 <- subset(d.preamble.6pos, word.type == "D1")
# d.n1 <- subset(d.preamble.5pos, word.type == "N1")
# d.p1 <- subset(d.preamble.5pos, word.type == "P1")
# d.d2 <- subset(d.preamble.5pos, word.type == "D2")
# d.a1 <- subset(d.preamble.5pos, word.type == "A1")
# d.n2 <- subset(d.preamble.5pos, word.type == "N2")
#
#
# # ------------------------------------------------------------------------------------------------------------------
# # ---------------------------CREATE AND COMPARE MODELS on residuals-------------------------------------------------
# # ------------------------------------------------------------------------------------------------------------------
#
#
# # D1 -------------------
# cat(rep(c("-"), times=40, quote=F),"\n")
# cat("SUBSET D1 (with Related) residuals", sep = "", fill = 60)
# cat(rep(c("-"), times=40, quote=F), "\n")
# pros.d1.r <- lmer(resid.frln.fpos ~  relat + integ  + plaus + n1.len + p1.len + a1.len + n2.len + (1|subject) + (1|item), data = d.d1, REML=TRUE)
# print(summary(pros.d1.r))
#
# cat(rep(c("-"), times=40, quote=F),"\n")
# cat("SUBSET D1 (with Association) residuals", sep = "", fill = 60)
# cat(rep(c("-"), times=40, quote=F), "\n")
# pros.d1.a <- lmer(resid.frln.fpos ~  assoc + integ  + plaus + n1.len + p1.len + a1.len + n2.len + (1|subject) + (1|item), data = d.d1, REML=TRUE)
# print(summary(pros.d1.a))
#
#
# # N1 -------------------
# cat(rep(c("-"), times=40, quote=F),"\n")
# cat("SUBSET N1 (with Related) residuals", sep = "", fill = 60)
# cat(rep(c("-"), times=40, quote=F), "\n")
# pros.n1.r <- lmer(resid.fpre ~  relat + integ + plaus + p1.len + a1.len + n2.len + (1|subject) + (1|item), data = d.n1, REML=TRUE)
# print(summary(pros.n1.r))
#
# cat(rep(c("-"), times=40, quote=F),"\n")
# cat("SUBSET N1 (with Association) residuals", sep = "", fill = 60)
# cat(rep(c("-"), times=40, quote=F), "\n")
# pros.n1.a <- lmer(resid.fpre ~  assoc + integ + plaus + p1.len + a1.len + n2.len + (1|subject) + (1|item), data = d.n1, REML=TRUE)
# print(summary(pros.n1.a))
#
#
# # P1 -------------------
# cat(rep(c("-"), times=40, quote=F),"\n")
# cat("SUBSET P1 (with Related) residuals", sep = "", fill = 60)
# cat(rep(c("-"), times=40, quote=F), "\n")
# pros.p1.r <- lmer(resid.fpre ~ relat + integ  + plaus + n1.len + a1.len + n2.len + (1|subject) + (1|item), data = d.p1, REML=TRUE)
# print(summary(pros.p1.r))
#
# cat(rep(c("-"), times=40, quote=F),"\n")
# cat("SUBSET P1 (with Association) residuals", sep = "", fill = 60)
# cat(rep(c("-"), times=40, quote=F), "\n")
# pros.p1.a <- lmer(resid.fpre ~ assoc + integ  + plaus + n1.len + a1.len + n2.len + (1|subject) + (1|item), data = d.p1, REML=TRUE)
# print(summary(pros.p1.a))
#
# # D2 -------------------
# cat(rep(c("-"), times=40, quote=F),"\n")
# cat("SUBSET D2 (with Related) residuals", sep = "", fill = 60)
# cat(rep(c("-"), times=40, quote=F), "\n")
# pros.d2.r <- lmer(resid.fpre ~  relat + integ + plaus + n1.len + p1.len + a1.len + n2.len + (1|subject) + (1|item), data = d.d2, REML=TRUE)
# print(summary(pros.d2.r))
#
# cat(rep(c("-"), times=40, quote=F),"\n")
# cat("SUBSET D2 (with Association) residuals", sep = "", fill = 60)
# cat(rep(c("-"), times=40, quote=F), "\n")
# pros.d2.a <- lmer(resid.fpre ~ assoc + integ + plaus + n1.len + p1.len + a1.len + n2.len + (1|subject) + (1|item), data = d.d2, REML=TRUE)
# print(summary(pros.d2.a))
#
# # A1 -------------------
#
# cat(rep(c("-"), times=40, quote=F),"\n")
# cat("SUBSET A1 (with Related) residuals", sep = "", fill = 60)
# cat(rep(c("-"), times=40, quote=F), "\n")
# pros.a1.r <- lmer(resid.fpre ~ relat + integ  + plaus + n1.len + p1.len + n2.len + (1|subject) + (1|item), data = d.a1, REML=TRUE)
# print(summary(pros.a1.r))
#
# cat(rep(c("-"), times=40, quote=F),"\n")
# cat("SUBSET A1 (with Association) residuals", sep = "", fill = 60)
# cat(rep(c("-"), times=40, quote=F), "\n")
# pros.a1.a <- lmer(resid.fpre ~ assoc + integ  + plaus + n1.len + p1.len + n2.len + (1|subject) + (1|item), data = d.a1, REML=TRUE)
# print(summary(pros.a1.a))
#
# # N2 -------------------
# cat(rep(c("-"), times=40, quote=F),"\n")
# cat("SUBSET N2 (with Related) residuals", sep = "", fill = 60)
# cat(rep(c("-"), times=40, quote=F), "\n")
# pros.n2.r <- lmer(resid.fpre ~  relat + integ  + plaus + n1.len + p1.len + a1.len + (1|subject) + (1|item), data = d.n2, REML=TRUE)
# print(summary(pros.n2.r))
#
# cat(rep(c("-"), times=40, quote=F),"\n")
# cat("SUBSET N2 (with Association) residuals", sep = "", fill = 60)
# cat(rep(c("-"), times=40, quote=F), "\n")
# pros.n2.a <- lmer(resid.fpre ~  assoc + integ  + plaus + n1.len + p1.len + a1.len + (1|subject) + (1|item), data = d.n2, REML=TRUE)
# print(summary(pros.n2.a))

# ======================================================================================
# ======================================================================================
# ======================================================================================
# ======================================================================================

#===================  COMPUTING and USING RESIDUALS from here on ================

sink("output/SemRel Prosody Analyses-Regressions.txt")
cat(" ", "\n")
cat("SEMREL PROSODY ANALYSES with REGRESSIONS RUN ON: ", format(Sys.time(), "%b. %d, %Y at %T"), sep = "", fill= 70)
cat(" ", "\n")



d.preamble.6pos <- subset(d.base, word.type == "D1" | word.type == "N1" | word.type == "P1" | word.type == "D2" | word.type == "A1"| word.type == "N2")
pros.preamble.frln <- lmer( seconds ~ freq + len.phon  + (1 + freq + len.phon |subject) + (1 + freq + len.phon |item), data = d.preamble.6pos, REML=TRUE)
d.preamble.6pos$resid.frln <- residuals(pros.preamble.frln)

pros.preamble.plaus <- lmer( resid.frln ~ plaus + (1 + plaus |subject) + (1 + plaus |item), data = d.preamble.6pos, REML=TRUE)
d.preamble.6pos$resid.frlnpl <- residuals(pros.preamble.plaus)

#summary(pros.preamble.frln)
pros.preamble.fpos <- lmer( resid.frlnpl ~ fr.pos.wd + (1 + fr.pos.wd|subject) + (1 + fr.pos.wd|item), data = d.preamble.6pos, REML=TRUE)
d.preamble.6pos$resid.frlnpl.fpos <- residuals(pros.preamble.fpos)
summary(pros.preamble.fpos)

#
# d.preamble.5pos <- subset(d.preamble.6pos, word.type != "D1")
# pros.preamble.fpre <- lmer( resid.frln.fpos ~ fr.pre.wd + (1 + fr.pre.wd|subject) + (1 + fr.pre.wd|item), data = d.preamble.5pos, REML=TRUE)
# d.preamble.5pos$resid.fpre <- residuals(pros.preamble.fpre)
# summary(pros.preamble.fpre)

# # ------------------------------CREATE SUBSETS-----------------------------------

d.d1 <- subset(d.preamble.6pos, word.type == "D1")
# d.n1 <- subset(d.preamble.5pos, word.type == "N1")
# d.p1 <- subset(d.preamble.5pos, word.type == "P1")
# d.d2 <- subset(d.preamble.5pos, word.type == "D2")
# d.a1 <- subset(d.preamble.5pos, word.type == "A1")
# d.n2 <- subset(d.preamble.5pos, word.type == "N2")


# ------------------------------------------------------------------------------------------------------------------
# ---------------------------CREATE AND COMPARE MODELS on residuals-------------------------------------------------
# ------------------------------------------------------------------------------------------------------------------


# D1 -------------------
cat(rep(c("-"), times=40, quote=F),"\n")
cat("SUBSET D1 (with Related) residuals", sep = "", fill = 60)
cat(rep(c("-"), times=40, quote=F), "\n")
pros.d1.r <- lmer(resid.frlnpl.fpos ~  relat + integ  + n1.len + p1.len + a1.len + n2.len + (1|subject) + (1|item), data = d.d1, REML=TRUE)
print(summary(pros.d1.r))

cat(rep(c("-"), times=40, quote=F),"\n")
cat("SUBSET D1 (with Association) residuals", sep = "", fill = 60)
cat(rep(c("-"), times=40, quote=F), "\n")
pros.d1.a <- lmer(resid.frlnpl.fpos ~  assoc + integ  + n1.len + p1.len + a1.len + n2.len + (1|subject) + (1|item), data = d.d1, REML=TRUE)
print(summary(pros.d1.a))

sink()
#
# # N1 -------------------
# cat(rep(c("-"), times=40, quote=F),"\n")
# cat("SUBSET N1 (with Related) residuals", sep = "", fill = 60)
# cat(rep(c("-"), times=40, quote=F), "\n")
# pros.n1.r <- lmer(resid.fpre ~  relat + integ + plaus + p1.len + a1.len + n2.len + (1|subject) + (1|item), data = d.n1, REML=TRUE)
# print(summary(pros.n1.r))
#
# cat(rep(c("-"), times=40, quote=F),"\n")
# cat("SUBSET N1 (with Association) residuals", sep = "", fill = 60)
# cat(rep(c("-"), times=40, quote=F), "\n")
# pros.n1.a <- lmer(resid.fpre ~  assoc + integ + plaus + p1.len + a1.len + n2.len + (1|subject) + (1|item), data = d.n1, REML=TRUE)
# print(summary(pros.n1.a))
#
#
# # P1 -------------------
# cat(rep(c("-"), times=40, quote=F),"\n")
# cat("SUBSET P1 (with Related) residuals", sep = "", fill = 60)
# cat(rep(c("-"), times=40, quote=F), "\n")
# pros.p1.r <- lmer(resid.fpre ~ relat + integ  + plaus + n1.len + a1.len + n2.len + (1|subject) + (1|item), data = d.p1, REML=TRUE)
# print(summary(pros.p1.r))
#
# cat(rep(c("-"), times=40, quote=F),"\n")
# cat("SUBSET P1 (with Association) residuals", sep = "", fill = 60)
# cat(rep(c("-"), times=40, quote=F), "\n")
# pros.p1.a <- lmer(resid.fpre ~ assoc + integ  + plaus + n1.len + a1.len + n2.len + (1|subject) + (1|item), data = d.p1, REML=TRUE)
# print(summary(pros.p1.a))
#
# # D2 -------------------
# cat(rep(c("-"), times=40, quote=F),"\n")
# cat("SUBSET D2 (with Related) residuals", sep = "", fill = 60)
# cat(rep(c("-"), times=40, quote=F), "\n")
# pros.d2.r <- lmer(resid.fpre ~  relat + integ + plaus + n1.len + p1.len + a1.len + n2.len + (1|subject) + (1|item), data = d.d2, REML=TRUE)
# print(summary(pros.d2.r))
#
# cat(rep(c("-"), times=40, quote=F),"\n")
# cat("SUBSET D2 (with Association) residuals", sep = "", fill = 60)
# cat(rep(c("-"), times=40, quote=F), "\n")
# pros.d2.a <- lmer(resid.fpre ~ assoc + integ + plaus + n1.len + p1.len + a1.len + n2.len + (1|subject) + (1|item), data = d.d2, REML=TRUE)
# print(summary(pros.d2.a))
#
# # A1 -------------------
#
# cat(rep(c("-"), times=40, quote=F),"\n")
# cat("SUBSET A1 (with Related) residuals", sep = "", fill = 60)
# cat(rep(c("-"), times=40, quote=F), "\n")
# pros.a1.r <- lmer(resid.fpre ~ relat + integ  + plaus + n1.len + p1.len + n2.len + (1|subject) + (1|item), data = d.a1, REML=TRUE)
# print(summary(pros.a1.r))
#
# cat(rep(c("-"), times=40, quote=F),"\n")
# cat("SUBSET A1 (with Association) residuals", sep = "", fill = 60)
# cat(rep(c("-"), times=40, quote=F), "\n")
# pros.a1.a <- lmer(resid.fpre ~ assoc + integ  + plaus + n1.len + p1.len + n2.len + (1|subject) + (1|item), data = d.a1, REML=TRUE)
# print(summary(pros.a1.a))
#
# # N2 -------------------
# cat(rep(c("-"), times=40, quote=F),"\n")
# cat("SUBSET N2 (with Related) residuals", sep = "", fill = 60)
# cat(rep(c("-"), times=40, quote=F), "\n")
# pros.n2.r <- lmer(resid.fpre ~  relat + integ  + plaus + n1.len + p1.len + a1.len + (1|subject) + (1|item), data = d.n2, REML=TRUE)
# print(summary(pros.n2.r))
#
# cat(rep(c("-"), times=40, quote=F),"\n")
# cat("SUBSET N2 (with Association) residuals", sep = "", fill = 60)
# cat(rep(c("-"), times=40, quote=F), "\n")
# pros.n2.a <- lmer(resid.fpre ~  assoc + integ  + plaus + n1.len + p1.len + a1.len + (1|subject) + (1|item), data = d.n2, REML=TRUE)
# print(summary(pros.n2.a))
#
#



sink()
