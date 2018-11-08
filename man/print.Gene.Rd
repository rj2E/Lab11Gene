\name{print.Gene}
\alias{print.Gene}

\title{
Print out a Gene Object
}
\description{
This methods prints some descriptive information about a Gene object.
}
\usage{
\method{print}{Gene}(x, ...)
}

\arguments{
  \item{x}{
A Gene object
}
\item{...}{
Other arguments (none implemented).
}
}
\details{
The gene ID, symbol, and length in amino acids are printed.
}
\value{
Information is printed to the console but no object is returned.
}


\examples{
mygene <- Gene("Abc000011", "XYZ", c(8020, 112, 900),
               "ATGAAATAG")
print(mygene)
}

