\name{pick.day}
\alias{pick.day}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
pick.day
}
\description{
this is to pick the closest data points to the target value of y
and then it gives the value of z corresponding to the picked data points
this is (sort of) useful to align multiple time series 
grid is made over y. (arbitrary) threshold 5 is used. in the future, threshold should be one of the argument
}
\usage{
pick.day(x, y, z)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
(time) index
}
  \item{y}{
reference (time) index
}
  \item{z}{
target values
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
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
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
function (x, y, z) 
{
    out <- z[min(which.min(abs(x - y)))]
    if (min(abs(x - y)) >= 5) {
        out <- NA
    }
    return(out)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.

