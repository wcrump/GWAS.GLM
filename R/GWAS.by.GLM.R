#' Perform GWA by GLM using PCs and/or Other Covariates
#'
#' @param gen.data A numeric genotype matrix/data.frame (rows = taxa, columns = markers).
#' @param phen.data A phenotype matrix/data.frame (rows = taxa, column = trait scores). Only one trait is permitted.
#' @param gen.map A matrix/data.frame of SNP information (rows = SNPs, columns = c(SNP_ID, Chromosome, Position))
#' @param covs A matrix/data.frame of covariates (rows = taxa, columns = covariates)
#' @param corr.thresh A numerical value (0-1) indicating the maximum allowed correlation between any one covariate and any one PC
#' @param PCs A numerical value indicating the number of PCs to be included in the GLM equation as additional covariates to any supplied by the user
#' @return A vector of p values. Each p value corresponding to one SNP tested.

GWAS.by.GLM <- function(gen.data, phen.data, gen.map, covs, corr.thresh, PCs) {

	#perform PCA
	PCA <- make.pca(gen.data)

	#filter PCs
	filtered.PCA <- filter.pca(PCA, covs, corr.thresh)

	#perform gwas by glm method and get p values for all markers
	gwas.results <- GWAS(gen.data, phen.data, covs, filtered.PCA, PCs)

	return(gwas.results)
}
