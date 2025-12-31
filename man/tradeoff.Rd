

\name{tradeoff}
\alias{tradeoff}
\alias{tradeOff}
\title{Trade-offs for a specified fractional factorial design}
\description{Fractional factorial designs are a trade-off of information learned \emph{vs.} the amount of work and cost (experiments) invested. This function shows which factors are confounded (confused; aliased) with each other when running a fractional factorial. The output shows the design's resolution and how to generate the fractional factorial.}

\usage{tradeoff(runs=8, factors=7, display=TRUE)}

\arguments{

\item{runs}{
  the number of (unreplicated) experimental runs in the fractional factorial design. Must be a power of 2 that is 8 or higher (e.g. 8, 16, 32, 64, ...)
}
\item{factors}{
  the number of factors being investigated in the fractional factorial design.
}
\item{display}{
  will by default print the results to the screen and also returns a list containing the same information; setting this to \code{FALSE} will suppress the screen output.
}
}


\details{Fractional factorial designs require a sacrifice in the clarity of the information learned, at the benefit of performing fewer
experimental runs; thereby decreasing cost and time to run the full set of factorial experiments. See \code{\link{tradeOffTable}} for a
visual display of this concept.

For example, when running 8 experiments with 7 factors, the design resolution (clarity of estimated effects) is going to be low. This function's
output indicates how the factors are aliased (confounded) with each other, so you can evaluate the use of the design before actually performing it.

The function provides 3 main sources of information: the resolution, the generators and the aliasing structure. Each one of these is described in
the reference.

Aliasing is only reported to the level of the main effects and two-factor interaction (2fi, \code{fi2}). Higher level interactions are of-course
present in many fractional factorial designs, and may be calculated from the defining relationship. Future versions of this function will
return the defining relationship to assist with this.
}


\value{The function currently returns a list with 3 entries: the resolution, the generator(s) and the aliasing structure.
}


\references{Please see Chapter 5 of the following book: \cr
Kevin Dunn, 2010 to 2026, \emph{Process Improvement using Data}, \url{https://learnche.org/pid}

Please see this paper to gain an understanding of how these trade-off tables are constructed:\cr
Arthur Fries and William G. Hunter, (1980) Minimum Aberration \eqn{2^{k-p}} Designs, \emph{Technometrics}, \bold{22}(4), pp. 601-608, \url{https://www.jstor.org/stable/1268198}
}



\author{Kevin Dunn, <kgdunn@gmail.com>}

\seealso{\code{\link{tradeOffTable}} for a visual representation of this information.}
\examples{

# Running 8 experiments? What are the trade-offs with 4, 5, 6, or 7 factors?
\dontrun{tradeoff(runs=8, factors=4)}
\dontrun{tradeoff(runs=8, factors=5)}
\dontrun{tradeoff(runs=8, factors=6)}
\dontrun{tradeoff(runs=8, factors=7)}

# Running 16 experiments? What are the trade-offs ?
\dontrun{tradeoff(runs=16, factors=5)}
\dontrun{tradeoff(runs=16, factors=6)}
\dontrun{tradeoff(runs=16, factors=7)}
\dontrun{tradeoff(runs=16, factors=8)}
\dontrun{tradeoff(runs=16, factors=9)}
}


\concept{ design of experiments }
