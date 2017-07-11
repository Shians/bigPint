library(limma)
library(Glimma)
library(GGally)
library(ggplot2)
library(edgeR)
library(DESeq2)

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

# Single voom (only runs voom once)
y <- DGEList(counts)
y <- calcNormFactors(y, method = "TMM")
v <- voom(y, design)
corfit <- duplicateCorrelation(v, design, block = x$samples$lane)
cont_matrix <- makeContrasts(NCvsNP = NC-NP, NCvsNR = NC-NR, NCvsNS = NC-NS, NCvsVC = NC-VC, NCvsVP = NC-VP, NCvsVR = NC-VR, NCvsVS = NC-VS, NPvsNR = NP-NR, NPvsNS = NP-NS, NPvsVC = NP-VC, NPvsVP = NP-VP, NPvsVR = NP-VR, NPvsVS = NP-VS, NRvsNS = NR-NS, NRvsVC = NR-VC, NRvsVP = NR-VP, NRvsVR = NR-VR, NRvsVS = NR-VS, NSvsVC = NS-VC, NSvsVP = NS-VP, NSvsVR = NS-VR, NSvsVS = NS-VS, VCvsVP = VC-VP, VCvsVR = VC-VR, VCvsVS = VC-VS, VPvsVR = VP-VR, VPvsVS = VP-VS, VRvsVS = VR-VS, levels = colnames(design))

fit <- lmFit(v, design, block = x$samples$lane, correlation = corfit$consensus)
fit2 <- contrasts.fit(fit, cont_matrix)
fit2 <- eBayes(fit2)

pairNames <- colnames(cont_matrix)
topGenes1 <- list()
genePval2 <- list()
for (i in 1:length(pairNames)) {
  temp <- topTreat(fit2, coef=i, n=Inf)
  sigRows <- which(temp$adj.P.Val<0.05)
  topGenes1[[ pairNames[i] ]] <- nrow(temp[sigRows,])
  genePval2[[ pairNames[i] ]] <- temp$adj.P.Val
}

