library(limma)
library(Glimma)
library(GGally)
library(ggplot2)
library(edgeR)
library(DESeq2)

# https://support.bioconductor.org/p/59700/

# This script creates three files - blockDEG, blockCorrPairs, and blockOverallCorr. The blockDEG represents the number of DEGs that remain in all 28 pairs when a given possible confounding variable is blocked. The blockOverallCorr represents what is returned by duplicateCorrelation (corfit$consensus) for all 28 pairs. The blockCorrPairs gives the correlation between p-values for all genes between the unblocked model and the model where a given extraneous variable is blocked.

# Note: IAPV extraneous variable returned NANs

rm(list=ls())
thisPath <- "/Users/lindz/bigPint"
beeCounts <- read.delim(file=paste0(thisPath, "/AllLaneCount.txt"), row.names=1, stringsAsFactors = FALSE)
colnames(beeCounts) <- c("NC.1", "NC.2", "NR.1", "VR.1", "NS.1", "VP.1", "NS.2", "VR.2", "NP.1", "VP.2", "VC.1", "NP.2", "VP.3", "NP.3", "VS.1", "VS.2", "VC.2", "NC.3", "VP.4", "NC.4", "NR.2", "VC.3", "VC.4", "NP.4", "VR.3", "NC.5", "VS.3", "NP.5", "VC.5", "VS.4", "NS.3", "VS.5", "VP.5", "NR.3", "NR.4", "VC.6", "NS.4", "NC.6", "NP.6", "VR.4", "NR.5", "NR.6", "NS.5", "VP.6", "NS.6", "VR.5", "VR.6", "VS.6")
beeCounts <- beeCounts[ , order(names(beeCounts))]
x <- DGEList(counts=beeCounts)

exVars <- read.csv("/Users/lindz/bigPint/tblshoot/CheckAllVars/extraVarClean.csv")
x$samples$group <- as.factor(unlist(lapply(colnames(x), function(x) substring(unlist(strsplit(x, "[.]"))[1],1))))
x$samples$virus <- as.factor(unlist(lapply(colnames(x), function(x) substring(unlist(strsplit(x, "[.]"))[1],1,1))))
x$samples$diet <- as.factor(unlist(lapply(colnames(x), function(x) substring(unlist(strsplit(x, "[.]"))[1],2,2))))
x$samples$lane <- exVars$Lane
x$samples$day <- exVars$Day
x$samples$mortality <- exVars$Mortality
x$samples$sbv <- exVars$SBV
x$samples$iapv <- exVars$IAPV
x$samples$rnaConc <- exVars$rnaConc
x$samples$rin <- exVars$RIN

##################### Make quantitative extraneous variables categorical
#x$samples$day <- as.factor(x$samples$day)
#x$samples$mortality <- cut(x$samples$mortality,3,labels=c('Low','Medium','High'))
#x$samples$sbv <- cut(x$samples$sbv,3,labels=c('Low','Medium','High'))
#x$samples$iapv <- cut(x$samples$iapv,3,labels=c('Low','Medium','High'))
#x$samples$rnaConc <- cut(x$samples$rnaConc,3,labels=c('Low','Medium','High'))
#x$samples$rin <- cut(x$samples$rin,3,labels=c('Low','Medium','High'))

##################### Filter then normalize (don't log since just for visuals)
# rule of thumb is to keep rows that have at least 10 counts for several samples
keep.exprs <- which(rowSums(x[[1]]>10)>8)
x <- x[keep.exprs,, keep.lib.sizes=FALSE] # 15,314 to 10,221
counts <- x[[1]]
design <- model.matrix(~0 + x$samples$group)
colnames(design) <- levels(x$samples$group)
cont_matrix <- makeContrasts(NCvsNP = NC-NP, NCvsNR = NC-NR, NCvsNS = NC-NS, NCvsVC = NC-VC, NCvsVP = NC-VP, NCvsVR = NC-VR, NCvsVS = NC-VS, NPvsNR = NP-NR, NPvsNS = NP-NS, NPvsVC = NP-VC, NPvsVP = NP-VP, NPvsVR = NP-VR, NPvsVS = NP-VS, NRvsNS = NR-NS, NRvsVC = NR-VC, NRvsVP = NR-VP, NRvsVR = NR-VR, NRvsVS = NR-VS, NSvsVC = NS-VC, NSvsVP = NS-VP, NSvsVR = NS-VR, NSvsVS = NS-VS, VCvsVP = VC-VP, VCvsVR = VC-VR, VCvsVS = VC-VS, VPvsVR = VP-VR, VPvsVS = VP-VS, VRvsVS = VR-VS, levels = colnames(design))

################# No blocking 
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
mortalityResults <- doCorr("mortality")
sbvResults <- doCorr("sbv")
#iapvResults <- doCorr("iapv") # Returned all NA values
rnaConcResults <- doCorr("rnaConc")
rinResults <- doCorr("rin")

blockDEG = data.frame()
for (i in 1:length(pairNames)) {
  row <- c(laneResults$sigPairs[[pairNames[i]]], dayResults$sigPairs[[pairNames[i]]], mortalityResults$sigPairs[[pairNames[i]]], sbvResults$sigPairs[[pairNames[i]]], rnaConcResults$sigPairs[[pairNames[i]]], rinResults$sigPairs[[pairNames[i]]])
  blockDEG <- rbind(blockDEG, row)
}
rownames(blockDEG) <- pairNames
colnames(blockDEG) <- c("lane", "day", "mortality", "sbv", "rnaConc", "rin")

blockCorrPairs = data.frame()
for (i in 1:length(pairNames)) {
  row <- c(laneResults$corrPairs[[pairNames[i]]], dayResults$corrPairs[[pairNames[i]]], mortalityResults$corrPairs[[pairNames[i]]], sbvResults$corrPairs[[pairNames[i]]], rnaConcResults$corrPairs[[pairNames[i]]], rinResults$corrPairs[[pairNames[i]]])
  blockCorrPairs <- rbind(blockCorrPairs, row)
}
rownames(blockCorrPairs) <- pairNames
colnames(blockCorrPairs) <- c("lane", "day", "mortality", "sbv", "rnaConc", "rin")

blockOverallCorr <- data.frame()
row <- c(laneResults$overallCorr, dayResults$overallCorr, mortalityResults$overallCorr, sbvResults$overallCorr, rnaConcResults$overallCorr, rinResults$overallCorr)
blockOverallCorr <- rbind(blockOverallCorr, row)
colnames(blockOverallCorr) <- c("lane", "day", "mortality", "sbv", "rnaConc", "rin")
