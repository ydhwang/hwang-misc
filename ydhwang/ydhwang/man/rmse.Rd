\name{rmse}
\alias{rmse}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
calculating RMSE
}
\description{
a function to calculate the RMSE
}
\usage{
rmse(x, y)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
yhat (prediction or forecast)
}
  \item{y}{
y. 
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
function (x, y) 
{
    if (any(dim(x) != dim(y))) {
        stop()
    }
    sqrt(mean((x - y)^2))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.

 
