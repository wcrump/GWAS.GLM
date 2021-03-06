% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pca.function.R
\name{make.pca}
\alias{make.pca}
\title{Perform Principle Component Analysis}
\usage{
make.pca(gen.data)
}
\arguments{
\item{gen.data}{A numeric genotype matrix (rows = taxa, columns = markers).}
}
\value{
A large prcomp ($sdev, $rotation, $center, $scale, $x)
}
\description{
Perform Principle Component Analysis
}
