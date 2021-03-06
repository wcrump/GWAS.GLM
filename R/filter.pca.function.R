#' Filter out PCs colinear with other covariates.
#'
#' @param PCA The output list from the prcomp() function.
#' @param covs A matrix/data.frame of covariates (rows = individuals, columns = covariates)
#' @param threshold A numerical value (0-1) indicating the maximum allowed correlation between any one covariate and any one PC
#' @return A matrix of filtered PCs (not correlated with any covariates supplied by user)


filter.pca <- function(PCA, covs, threshold=0.2){
	library(tidyverse)

	# ensure the first column is the first covariate
	if (class(covs[,1]) != "integer" & class(covs[,1]) != "numeric"){
		covs <- column_to_rownames(covs, var = colnames(covs[1]))
	}

	correlation.matrix <- cor(PCA$x, covs)
	indices <- apply(correlation.matrix, 2, function(x){x>=threshold})
	index.vect <- apply(indices, 2, function(x){which(x)})

	PCA$x <- PCA$x[,-index.vect]

	return(PCA)
}



