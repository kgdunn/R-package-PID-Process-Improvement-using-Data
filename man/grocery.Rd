\encoding{UTF-8}
\name{grocery}


\alias{grocery}


\title{Simulation of grocery store profits for a single product}


\description{The hourly profit made when selling the product at price \code{P} and the product is displayed at height \code{H} [cm] on the shelf.}


\usage{grocery(P=3.46, H=150)}




\arguments{

\item{P}{
  the selling price of the product. The default amount, if unspecified, is €3.46.
}
\item{H}{
  the display height of the product, as measured from the ground up. The default value, if unspecified, is 150cm.
}
}


\details{This function simulates the hourly profit in a grocery store of selling a particular product.\cr
Two factors can be adjusted by the user to determine the optimum:
\itemize{
	\item   \code{P}: The selling price of the product (must be a positive value).
	\item   \code{H}: The height that the product is displayed at (must be a positive value).
}

Can you determine the best combination of conditions, using a systematic method (i.e. not by trial and error)?
}


\value{Returns the hourly profit made. Random noise is added for some realism.}


\references{Please see Chapter 5 of the following book: Kevin Dunn, 2010 to 2026, \emph{Process Improvement using Data}, \url{https://learnche.org/pid/}
}


\author{Kevin Dunn, <kgdunn@gmail.com>}



\seealso{\code{\link{popcorn}}, \code{\link{manufacture}}}

\examples{
# Selling at the default settings of price (€3.46)
# and shelf height (150cm):
grocery()

# Let's try selling for a low price, €2.75,
# and put the product 120 cm off the ground
grocery(P=2.75, H=120)

# What happens if the product is too high
# for the average person?
grocery(P=1.25, H=200)

# Can you find the optimum combination of settings to
# maximize the profit, but using only a few experiments?
}


\concept{ design of experiments }
