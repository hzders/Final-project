\name{getci}
\alias{getci}
\alias{moreci}
\alias{barci}


\title{
Chemotaxis Index
}
\description{
The function will make sure the data inputs are all positive and perform the formula for chemotaxis index (CI) = (regionA# - regionB#)/total#. the function will return a table with the CI from each row added in the last column}
\usage{
getci(a)
}
\arguments{
  \item{a}{
}
}
\details{
This table needs to have four coloumns that are named "Chemicals", "RegionA", "RegionB", and "Origin". Each row are named by the chemical testes.
}
\value{

}
\references{

\author{
Ziduan Han
}

\examples{
example <-data.frame("Chemicals" = c("NaCl", "O2"), "Region.A" = c(20,30), 
"Region.B" = c(15,13), "origin" = c(71,32) )
getci(example)
moreci(example)
barci(example)
}


