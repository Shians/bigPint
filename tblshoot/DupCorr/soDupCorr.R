library(limma)
library(Glimma)
library(GGally)
library(ggplot2)
library(edgeR)
library(DESeq2)

set.seed(1)
counts <- as.data.frame(matrix(rpois(24000, lambda = 500), ncol = 48))
colnames(counts) <- c(rep("N.A",6), rep("N.B",6), rep("N.C",6), rep("N.D",6), rep("V.A",6), rep("V.B",6), rep("V.C",6), rep("V.D",6))
x <- DGEList(counts=beeCounts)

exVars <- data.frame("Sample" = colnames(counts), "Lane" = sample(c("L1", "L2"), 48, replace=T), "Day" = sample(c("1", "2"), 48, replace=T), "Mortality" = runif(48,0,1), "LogVirus1" = runif(48,2,7), "LogVirus2" = runif(48,2,7), "rnaConc" = runif(48,50,400), "RIN" = runif(48,7,10))

Sample = colnames(counts)
Lane = sample(c("L1", "L2"), 48, replace=T)
Day = sample(c("1", "2"), 48, replace=T)
Mortality = runif(48,0,1)
LogVirus1 = runif(48,2,7)
LogVirus2 = runif(48,2,7)
rnaConc = runif(48,50,400)
RIN = runif(48,7,10)

