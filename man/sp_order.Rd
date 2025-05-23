% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/reefspawn-data.R
\docType{data}
\name{sp_order}
\alias{sp_order}
\title{Load and preprocess coral fecundity and size data}
\format{
An object of class \code{character} of length 11.
}
\usage{
sp_order
}
\value{
Two data frames:
\itemize{
\item \code{fecundity}: colony area, width, and reproductive status
\item \code{coralsize}: species, width, and assigned growthform
}
}
\description{
Loads \code{fecundity.csv} and \code{size_structure.csv} from the \code{inst/extdata} directory,
computes derived width from area, and assigns species-level growthforms.
}
\details{
\itemize{
\item \code{fecundity} includes colony-level reproductive data.
\item \code{coralsize} contains colony size structure by species, with growthform classification.
\item Width is computed as \code{sqrt(area_cm2 / pi) * 2}.
\item Growthforms are assigned using \code{fct_recode()} based on species identity.
}
}
\examples{
\dontrun{
head(fecundity)
head(coralsize)
}
}
\keyword{datasets}
