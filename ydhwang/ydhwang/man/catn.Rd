\name{catn}
\alias{catn}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Concatenate and Print function with line break added
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
catn(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
Concatenate, print and add a line break
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
YD Hwang
}
\note{
simple function
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (x) 
{
    cat(x, "\n")
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{catn}% use one of  RShowDoc("KEYWORDS")
\keyword{print}% __ONLY ONE__ keyword per line
