\name{manufacture}


\alias{manufacture}


\title{Simulation of a manufacturing facility's profit when varying two factors}


\description{The hourly profit made when selling the product at price \code{P} and the product is produced
   at a throughput rate \code{T} [parts per hour].}


\usage{manufacture(P=0.75, T=325)}




\arguments{

\item{P}{
  the selling price of the product. The default amount, if unspecified, is $0.75.
}
\item{T}{
  the production rate (throughput), measured in parts per hour. The default value, if unspecified, is 325 parts per hour.
}
}


\details{This function simulates the hourly profit in a manufacturing facility.\cr
Two factors can be adjusted by the user to determine the optimum:
\itemize{
	\item   \code{P}: The selling price of the product (must be a positive value).
	\item   \code{T}: The production rate, also called throughput (must be a positive value).
}

Can you determine the best combination of conditions, using a systematic method (i.e. not by trial and error)? More defects are created
when production rates are too high.
}


\value{Returns the hourly profit made. Random noise is added for some realism.}


\references{Please see Chapter 5 of the following book: Kevin Dunn, 2010 to 2026, \emph{Process Improvement using Data}, \url{https://learnche.org/pid/}
}


\author{Kevin Dunn, <kgdunn@gmail.com>}



\seealso{\code{\link{popcorn}}, \code{\link{grocery}}}

\examples{
# Producing at the default settings of price ($0.75)
# and throughput of 325 parts per hour:
manufacture()

# Let's try selling for a higher price, $1.05,
# and a slower throughput of 298 parts per hour:
manufacture(P=1.05, T=298)

# What happens if the product is sold too cheaply
# at high production rates?
manufacture(P=0.52, T=417)

# Can you find the optimum combination of settings to
# maximize the profit, but using only a few experiments?
}



\concept{ design of experiments }
