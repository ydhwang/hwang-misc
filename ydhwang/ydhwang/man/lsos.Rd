\name{lsos}
\alias{lsos}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
list all the items in the working space sorted by the object size
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
lsos(..., n = 10)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
}
  \item{n}{
number of items listed
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
http://stackoverflow.com/questions/1358003/tricks-to-manage-the-available-memory-in-an-r-session
}
\author{
YD Hwang
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
function (..., n = 10) 
{
    .ls.objects(..., order.by = "Size", decreasing = TRUE, head = TRUE, 
        n = n)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.

