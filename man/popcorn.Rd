\name{popcorn}


\alias{popcorn}


\title{Simulation of stovetop popcorn cooking}


\description{A fixed number of popcorn kernels are cooked are heated at the same temperature. This simulation returns the number of
kernels that are edible when the pot is left on the stove for a given number of \code{T} seconds.\cr}


\usage{popcorn(T=120)}




\arguments{

\item{T}{
  the number of seconds that the pot is left on the stove. The default amount, if not provided, is 120 seconds.
}
}


\details{This function simulates the number of edible popcorn kernels that are made when cooking a fixed number of kernels at the same heat setting
for a given amount of \code{T} seconds.\cr
Time durations less than 77 seconds are not supported. A vector (list) of time values is not permitted, since the goal is to perform sequential
experimentation to determine the optimum time, with the fewest number of function calls.
}


\value{Returns the number of edible popcorn kernels. Random noise is added for some realism.}


\references{Please see Chapter 5 of the following book: Kevin Dunn, 2010 to 2026, \emph{Process Improvement using Data}, \url{https://learnche.org/pid}
}


\author{Kevin Dunn, <kgdunn@gmail.com>}



\seealso{\code{\link{grocery}}, \code{\link{manufacture}}}

\examples{
# Cooking for a very short duration is not supported.
# For example, popcorn(T=50) will fail

# Cooking from 77 seconds onwards is supported
popcorn(T=120)

# What happens if we leave the pot on the stove for too long?
popcorn(T=500)

# Can you find the optimum time to cook on the stove
# using the fewest number of function calls?
}


\concept{ design of experiments }
