library(sva)
library(bladderbatch)
library(pamr)
library(limma)

data(bladderdata)

# The sva package assumes there are two types of variables that are being considered: (1) adjustment variables (extraneous variables) and (2) variables of interest. Two model matrices must be made: the "full model" and the "null model". The null model is a model matrix that includes terms for all of the adjustment variables but not the variables of interest. The full model includes terms for both the adjustment variables and the variables of interest.

pheno = pData(bladderEset)
edata = exprs(bladderEset)

# Next we create the full model matrix - including both the adjustment variables and the variable of interest (cancer status). In this case we only have the variable of interest. Since cancer status has multiple levels, we treat it as a factor variable.
mod = model.matrix(~as.factor(cancer), data=pheno)
# The null model contains only the adjustment variables. Since we are not adjusting for any other variables in this analysis, only an intercept is included in the model.
mod0 = model.matrix(~1,data=pheno)

# Estimate number of latent variables that need to be estimated
n.sv = num.sv(edata,mod,method="leek")

# Apply the sva function to estimate the surrogate variables. svobject contains four components, sv, pprob.gam, pprob.b, n.sv. sv is a matrix whose columns correspond to the estimated surrogate variables. They can be used in downstream analyses. pprob.gam is the posterior probability that each gene is associated with one or more latent variables. pprob.b is the posterior probability that each gene is associated with the variables of interest.
svobj = sva(edata,mod,mod0,n.sv=n.sv)

# The f.pvalue function can be used to calculate parametric F-test p-values for each row of a data matrix. In the case of the bladder study, this would correspond to calculating a parametric F-test p- value for each of the 22,283 rows of the matrix. The F-test compares the models mod and mod0. They must be nested models, so all of the variables in mod0 must appear in mod.

# First we can calculate the F-test p-values for differential expression with respect to cancer status, without adjusting for surrogate variables, adjust them for multiple testing, and calculate the number that are significant with a Q-value less than 0.05.
pValues = f.pvalue(edata,mod,mod0)
qValues = p.adjust(pValues,method="BH")
length(which(qValues<0.05))/length(qValues)

# Note that nearly 70% of the genes are strongly differentially expressed at an FDR of less than 5% between groups. This number seems artificially high, even for a strong phenotype like cancer. Now we can perform the same analysis, but adjusting for surrogate variables. The first step is to include the surrogate variables in both the null and full models. The reason is that we want to adjust for the surrogate variables, so we treat them as adjustment variables that must be included in both models.
modSv = cbind(mod,svobj$sv)
mod0Sv = cbind(mod0,svobj$sv)
pValuesSv = f.pvalue(edata,modSv,mod0Sv)
qValuesSv = p.adjust(pValuesSv,method="BH")
length(which(qValuesSv<0.05))/length(qValuesSv)

# The limma package is one of the most commonly used packages for differential expression analysis. The sva package can easily be used in conjunction with the limma package to perform adjusted differential expression analysis. The first step in this process is to fit the linear model with the surrogate variables included
fit = lmFit(edata,modSv)

# From here, you can use the limma functions to perform the usual analyses. As an example, suppose we wanted to calculate differential expression with respect to cancer. To do that we first compute the contrasts between the pairs of cancer/normal terms. We do not include the surrogate variables in the contrasts, since they are only being used to adjust the analysis.
contrast.matrix <- cbind("C1"=c(-1,1,0,rep(0,svobj$n.sv)),"C2"=c(0,-1,1,rep(0,svobj$n.sv)),"C3"=c(-1,0,1,rep(0,svobj$n.sv)))
fitContrasts = contrasts.fit(fit,contrast.matrix)

eb = eBayes(fitContrasts)
topTableF(eb, adjust="BH")

#########################################################################################
# The ComBat function adjusts for known batches using an empirical Bayesian framework. In order to use the function, you must have a known batch variable in your dataset.
batch = pheno$batch

# Just as with sva, we then need to create a model matrix for the adjustment variables, including the variable of interest. Note that you do not include batch in creating this model matrix - it will be included later in the ComBat function. In this case there are no other adjustment variables so we simply fit an intercept term.
modcombat = model.matrix(~1, data=pheno)

# We now apply the ComBat function to the data, using parametric empirical Bayesian adjustments. This returns an expression matrix, with the same dimensions as your original dataset. This new expression matrix has been adjusted for batch.

# There are a few additional options for the ComBat function. By default, it performs parametric empirical Bayesian adjustments. If you would like to use nonparametric empirical Bayesian adjustments, use the par.prior=FALSE option (this will take longer). Additionally, use the prior.plots=TRUE option to give prior plots with black as a kernel estimate of the empirical batch effect density and red as the parametric estimate. For example, you might chose to use the parametric Bayesian adjustments for your data, but then can check the plots to ensure that the estimates were reasonable.

combat_edata = ComBat(dat=edata, batch=batch, mod=modcombat, par.prior=TRUE, prior.plots=FALSE)

# Significance analysis can then be performed directly on the adjusted data using the model matrix and null model matrix as described before. These P-values and Q-values now account for the known batch effects included in the batch variable.
pValuesComBat = f.pvalue(combat_edata,mod,mod0)
qValuesComBat = p.adjust(pValuesComBat,method="BH")

# Removing known batch effects with a linear model
# Direct adjustment for batch effects can also be performed using the f.pvalue function. In the bladder cancer example, one of the known variables is a batch variable. This variable can be included as an adjustment variable in both mod and mod0. Then the f.pvalue function can be used to detect differential expression. This approach is a simplified version of ComBat.
modBatch = model.matrix(~as.factor(cancer) + as.factor(batch),data=pheno)
mod0Batch = model.matrix(~as.factor(batch),data=pheno)
pValuesBatch = f.pvalue(edata,modBatch,mod0Batch)
qValuesBatch = p.adjust(pValuesBatch,method="BH")









