library(limma)
library(Glimma)
library(GGally)
library(ggplot2)
library(edgeR)
library(DESeq2)

################# Create data and setup design
rm(list=ls())
set.seed(1)
counts <- as.data.frame(matrix(rpois(24000, lambda = 500), ncol = 48))
colnames(counts) <- c(paste0("NB", ".", c(1:6)), paste0("NC", ".", c(1:6)), paste0("ND", ".", c(1:6)), paste0("NE", ".", c(1:6)), paste0("VB", ".", c(1:6)), paste0("VC", ".", c(1:6)), paste0("VD", ".", c(1:6)), paste0("VE", ".", c(1:6)))
x <- DGEList(counts=counts)

# Simulate possible confounding variables
exVars <- data.frame("Sample" = colnames(counts), "Lane" = sample(c("L1", "L2"), 48, replace=T), "Day" = sample(c("1", "2"), 48, replace=T), "Mortality" = runif(48,0,1), "LogVirus1" = runif(48,2,7), "LogVirus2" = runif(48,2,7), "rnaConc" = runif(48,50,400), "RIN" = runif(48,7,10))

x$samples$group <- as.factor(unlist(lapply(as.character(exVars$Sample), function(x) substring(unlist(strsplit(x, "[.]"))[1],1))))
x$samples$virus <- as.factor(unlist(lapply(as.character(exVars$Sample), function(x) substring(unlist(strsplit(x, "[.]"))[1],1,1))))
x$samples$diet <- as.factor(unlist(lapply(as.character(exVars$Sample), function(x) substring(unlist(strsplit(x, "[.]"))[1],2,2))))
x$samples$lane <- exVars$Lane
x$samples$day <- exVars$Day
x$samples$mortality <- exVars$Mortality
x$samples$lv1 <- exVars$LogVirus1
x$samples$lv2 <- exVars$LogVirus2
x$samples$rnaConc <- exVars$rnaConc
x$samples$rin <- exVars$RIN

# Filter out genes that have low read counts (doesn't filter out any genes in this MWE, but does filter out genes in my real data)
keep.exprs <- which(rowSums(x[[1]]>10)>24)
x <- x[keep.exprs,, keep.lib.sizes=FALSE]
counts <- x[[1]]
design <- model.matrix(~0 + x$samples$group)
colnames(design) <- levels(x$samples$group)
cont_matrix <- makeContrasts(NBvsNC = NB-NC, NBvsND = NB-ND, NBvsNE = NB-NE, NBvsVB = NB-VB, NBvsVC = NB-VC, NBvsVD = NB-VD, NBvsVE = NB-VE, NCvsND = NC-ND, NCvsNE = NC-NE, NCvsVB = NC-VB, NCvsVC = NC-VC, NCvsVD = NC-VD, NCvsVE = NC-VE, NDvsNE = ND-NE, NDvsVB = ND-VB, NDvsVC = ND-VC, NDvsVD = ND-VD, NDvsVE = ND-VE, NEvsVB = NE-VB, NEvsVC = NE-VC, NEvsVD = NE-VD, NEvsVE = NE-VE,   VBvsVC = VB-VC, VBvsVD = VB-VD, VBvsVE = VB-VE, VCvsVD = VC-VD, VCvsVE = VC-VE, VDvsVE = VD-VE, levels = colnames(design))

################# No blocking
# Creates sigGenes0, genePval0)
y <- DGEList(counts)
y <- calcNormFactors(y, method = "TMM")
v <- voom(y, design)
fit <- lmFit(v, design)
fit2 <- contrasts.fit(fit, cont_matrix)
fit2 <- eBayes(fit2)

pairNames <- colnames(cont_matrix)
sigGenes0 <- list()
genePval0 <- list()
for (i in 1:length(pairNames)) {
  temp <- topTreat(fit2, coef=i, n=Inf)
  sigRows <- which(temp$adj.P.Val<0.05)
  sigGenes0[[ pairNames[i] ]] <- nrow(temp[sigRows,])
  genePval0[[ pairNames[i] ]] <- temp$adj.P.Val
}

################# Double voom (voom runs twice)
# Creates laneResults, dayResults, mortalityResults, lv1Results, lv2Results, rnaConcResults, rinResults
doCorr <- function(variable){
  y <- DGEList(counts)
  y <- calcNormFactors(y, method = "TMM")
  v <- voom(y, design)
  corfit <- duplicateCorrelation(v, design, block = x$samples[[variable]])
  v <- voom(y, design, block = x$samples[[variable]], correlation = corfit$consensus)
  fit <- lmFit(v, design, block = x$samples[[variable]], correlation = corfit$consensus)
  fit2 <- contrasts.fit(fit, cont_matrix)
  fit2 <- eBayes(fit2)
  
  pairNames <- colnames(cont_matrix)
  sigGenes2 <- list()
  genePval2 <- list()
  corr2 <- list()
  for (i in 1:length(pairNames)) {
    temp <- topTreat(fit2, coef=i, n=Inf)
    sigRows <- which(temp$adj.P.Val<0.05)
    sigGenes2[[ pairNames[i] ]] <- nrow(temp[sigRows,])
    genePval2[[ pairNames[i] ]] <- temp$adj.P.Val
    corr2[[pairNames[i]]] <- cor(genePval0[[pairNames[i]]], genePval2[[pairNames[i]]])
  }
  return(list(overallCorr = corfit$consensus, corrPairs = corr2, sigPairs = sigGenes2))
}

laneResults <- doCorr("lane")
dayResults <- doCorr("day")
mortalityResults <- doCorr("mortality") #Error
lv1Results <- doCorr("LogVirus1")
lv2Results <- doCorr("LogVirus2")
rnaConcResults <- doCorr("rnaConc") #Error
rinResults <- doCorr("rin") #Error

################# Overall summary
# Create blockDEG, blockCorrPairs, blockOverallCorr
blockDEG = data.frame()
for (i in 1:length(pairNames)) {
  row <- c(laneResults$sigPairs[[pairNames[i]]], dayResults$sigPairs[[pairNames[i]]], lv1Results$sigPairs[[pairNames[i]]], lv2Results$sigPairs[[pairNames[i]]])
  blockDEG <- rbind(blockDEG, row)
}
rownames(blockDEG) <- pairNames
colnames(blockDEG) <- c("lane", "day", "lv1", "lv2")

blockCorrPairs = data.frame()
for (i in 1:length(pairNames)) {
  row <- c(laneResults$corrPairs[[pairNames[i]]], dayResults$corrPairs[[pairNames[i]]], lv1Results$corrPairs[[pairNames[i]]], lv2Results$corrPairs[[pairNames[i]]])
  blockCorrPairs <- rbind(blockCorrPairs, row)
}
rownames(blockCorrPairs) <- pairNames
colnames(blockCorrPairs) <- c("lane", "day", "lv1", "lv2")

blockOverallCorr <- data.frame()
row <- c(laneResults$overallCorr, dayResults$overallCorr, lv1Results$overallCorr, lv2Results$overallCorr)
blockOverallCorr <- rbind(blockOverallCorr, row)
colnames(blockOverallCorr) <- c("lane", "day", "lv1", "lv2")
