# Simulate gene expression data for 100 probes and 6 microarrays
# Microarray are in two groups
# First two probes are more highly expressed in second group
# Std deviations vary between genes with prior df=4
sd <- 0.3*sqrt(4/rchisq(100,df=4))
y <- matrix(rnorm(100*6,sd=sd),100,6)
rownames(y) <- paste("Gene",1:100)
y[1:2,4:6] <- y[1:2,4:6] + 2
design <- cbind(Grp1=1,Grp2vs1=c(0,0,0,1,1,1))
options(digits=3)

# Fit with correlated arrays
# Suppose each pair of arrays is a block
block <- c(1,1,2,2,3,3)
dupcor <- duplicateCorrelation(y,design,block=block)
dupcor$consensus.correlation
fit1 <- lmFit(y,design,block=block,correlation=dupcor$consensus)
fit1 <- eBayes(fit1)
topTable(fit1,coef=2)


######################################################################
library("limma")
library("edgeR")

# 15 treatment groups (3 levels A/B/C and 5 levels 1/2/3/4/5) each with 6 replicates
# Simulate data
set.seed(123)
counts <- matrix(rpois(9000, lambda = 500), ncol = 90)
rownames(counts) <- paste0("g_", 1:nrow(counts))
anno <- data.frame(block = rep(paste0("block", 1:6), each = 15), fac1 = rep(c("A", "B", "C"), 30), fac2 = rep(paste0("treat", 1:5), 18))
combined_fac <- factor(paste(anno$fac1, anno$fac2, sep = "."))
design <- model.matrix(~0 + combined_fac)
colnames(design) <- levels(combined_fac)

# Single voom
y <- DGEList(counts)
y <- calcNormFactors(y)
v <- voom(y, design)
corfit <- duplicateCorrelation(v, design, block = anno$block)
fit <- lmFit(v, design, block = anno$block, correlation = corfit$consensus)
cont_matrix <- makeContrasts(AvB_1 = A.treat1 - B.treat1, AvB_2 = A.treat2 - B.treat2, levels = design)
fit2 <- contrasts.fit(fit, cont_matrix)
fit2 <- eBayes(fit2)
result_single <- topTable(fit2, number = nrow(counts), sort.by = "none")

# Double voom
y <- DGEList(counts)
y <- calcNormFactors(y)
v <- voom(y, design)
corfit <- duplicateCorrelation(v, design, block = anno$block)
v <- voom(y, design, block = anno$block, correlation = corfit$consensus)
fit <- lmFit(v, design, block = anno$block, correlation = corfit$consensus)
cont_matrix <- makeContrasts(AvB_1 = A.treat1 - B.treat1, AvB_2 = A.treat2 - B.treat2, levels = design)
fit2 <- contrasts.fit(fit, cont_matrix)
fit2 <- eBayes(fit2)
result_double <- topTable(fit2, number = nrow(counts), sort.by = "none")

# No blocking
y <- DGEList(counts)
y <- calcNormFactors(y)
v <- voom(y, design)
fit <- lmFit(v, design)
cont_matrix <- makeContrasts(AvB_1 = A.treat1 - B.treat1, AvB_2 = A.treat2 - B.treat2, levels = design)
fit2 <- contrasts.fit(fit, cont_matrix)
fit2 <- eBayes(fit2)
result_no_block <- topTable(fit2, number = nrow(counts), sort.by = "none")

# Comparison - all correlations > 0.99
cor(result_single$adj.P.Val, result_double$adj.P.Val)
cor(result_single$adj.P.Val, result_no_block$adj.P.Val)
cor(result_double$adj.P.Val, result_no_block$adj.P.Val)
