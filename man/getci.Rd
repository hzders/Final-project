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
moreci(a)
barci(a, stderror)
}
\arguments{
\item{a}{
a is the input dataframe with coloumns as "Chemicals", "Region    .A", "Region.B" and "origin"(also see the example)
}
\item{stderror}{
a logical value if it is TRUE, use standard error in the barplot. Otherwise, use standard deviation.
}
}
\details{
This table needs to have four coloumns that are named "Chemicals", "RegionA", "RegionB", and "Origin". Each row are named by the chemical testes.
}
\value{
getci returns the dataframe with one more coloum for CI values. moreci returns mean, SD and SE based on each category of chemicals. barci return a barplot of mean
}
\author{
Ziduan Han
}
\examples{
example <-data.frame("Chemicals" = c("NaCl","NaCl","NaCl", "O2","O2"), "Region.A" = c(20,30,15,13,50), 
"Region.B" = c(15,13,5,3,4), "origin" = c(71,32,32,12,12) )
getci(example)
moreci(example)
barci(example, stderror = TRUE)
}


