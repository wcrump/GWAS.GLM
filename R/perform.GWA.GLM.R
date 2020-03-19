#' Perform GWA by GLM
#'
#'
#'
#' @param gen.data A numeric genotype matrix/data.frame (rows = taxa, columns = markers).
#' @param phen.data A phenotype matrix/data.frame (rows = taxa, column = trait scores). Only one trait is permitted.
#' @param covs A matrix/data.frame of covariates (rows = taxa, columns = covariates)
#' @param PCA The output list from the prcomp() function
#' @param PCs A numerical value indicating the number of PCs to be included in the GLM equation as additional covariates to any supplied by the user
#' @return A vector of p values. Each p value corresponding to one SNP tested.

GWAS<- function(gen.data, phen.data, covs=NULL, PCA, PCs=3){
	library(tidyverse)

	# ensure the first column is the first covariate
	if (class(phen.data[,1]) != "integer" & class(phen.data[,1]) != "numeric"){
		phen.data <- column_to_rownames(phen.data, var = colnames(phen.data[1]))
	}

	#loop through genome
	G=gen.data[,-1]
	y=as.matrix(phen.data)
	n=nrow(G)
	m=ncol(G)
	P=matrix(NA,1,m)
	for (i in 1:m){
		x=G[,i]
		if(max(x)==min(x)){
			p=1}else{
				X=as.matrix(cbind(1, covs[,-1],PCA$x[,1:PCs],x))
				LHS=t(X)%*%X
				C=solve(LHS)
				RHS=t(X)%*%y
				b=C%*%RHS
				yb=X%*%b
				e=y-yb
				n=length(y)
				ve=sum(e^2)/(n-1)
				vt=C*ve
				t=b/sqrt(diag(vt))
				p=2*(1-pt(abs(t),n-2))
			} #end of testing variation
		P[i]=p[length(p)]
	} #end of looping for markers

	return(P)
}



