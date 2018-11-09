\name{Gene function}
\alias{Gene}
\alias{print.Gene}
\alias{proteinLength}
\alias{proteinLength.Gene}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Lab 11, Methods to Return Information about a Sequence File
}
\description{
The first method will return the length of amino acids from your sequence file.  The second method will return Gene ID, Symbol ID, and amino acid length of your sequence file.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
Gene(ID, symbol, ontology, CDS)

proteinLength(out, ...)

\method{proteinLength}{Gene}(out, ...)

\method{print}{Gene}(x, ...)

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ID}{
  Gene id
  }
  \item{symbol}{
  gene symbol
  }
  \item{ontology}{
  gene ontology
  }
  \item{CDS}{
  gene CDS
  }
  \item{out}{
  output
  }
  \item{x}{
  item x
  }
  \item{...}{
  ... item
  }
   The file contains gene ID, gene symbol, a vector of ontology terms, and the protein-coding portion of the DNA sequence (the CDS).
%%     ~~Describe \code{SNPmat} here~~
}

%%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
\code{proteinLength} Returns a value for the length of the sequence in amono acids, so it divides the nucleotide length by 3.
\code{print_info.Gene} Returns the gene ID, Symbol ID, and length in amino acids four your sequence file. 
\code{proteinLength.Gene} Returns a value for the length of the sequence in amono acids for the 'Gene' class.
}
%%\references{
%% ~put references to the literature/web site here ~
%}
\author{
Lucas Roberts & RJ Twohey III
}
\note{
Most of this code was provided by Lindsay Clark
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%}
\examples{}
%%##---- Should be DIRECTLY executable !! ----
%%##-- ==>  Define data, use random,
%%##--	or do  help(data=index)  for the standard data sets.

%%## The function is currently defined as
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%%\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
%%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
