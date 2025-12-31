

\name{paretoPlot}


\alias{paretoPlot}


\alias{pareto.plot}

\title{Pareto plot (coefficient plot) for a factorial design model}


\description{Creates a plot that shows the model coefficients from the least squares model as bars.
The absolute value of the coefficients are taken.
The coefficient's sign is represented by the bar colour: grey for negative and black for positive coefficients.}


\usage{paretoPlot(lsmodel, xlab="Effect name", ylab="Magnitude of effect",
                       main="Pareto plot", legendtitle="Sign of coefficients",
                       negative=c("Negative", "grey"),
                       positive=c("Positive", "black"))}




\arguments{

  \item{lsmodel}{
    a linear model object (least squares model) created by the \code{lm(\ldots)} function.
  }
  \item{xlab}{
    label for horizontal axis in the bar plot.
  }
  \item{ylab}{
    label for vertical axis in the bar plot.
  }
  \item{main}{
    label for the plot.
  }
  \item{legendtitle}{
    an alternative to the default legend title.
  }
  \item{negative}{
    a two-element vector, both entries must be strings, the first of which
    provides the name for negative bars in the legend, and the second tells which
    colour to plot negative bars with. The default is: c("Negative", "grey").
  }
  \item{positive}{
    a two-element vector, both entries must be strings, the first of which
    provides the name for positive bars in the legend, and the second tells which
    colour to plot those bars with.
  }
  For example, a general design of experiments with 4 factors: A, B, C, and D can be built using \code{lsmodel <- lm(y ~ A*B*C*D)}, and then the 2^4=16 coefficients visualized with this function using \code{paretoPlot(lsmodel)}. Since the largest magnitude coefficients are mostly of interest, the coefficient bars are shown sorted from largest to smallest absolute magnitude. The sign information is retained though with the bar's colour: grey for negative and black for positive coefficients.

  The coefficients are the exact coefficients from the linear model. When the linear model is in coded units (i.e. -1 for the low level, and +1 for the high level), then the coefficients represent the change in the
  experiment's response variable (y), for a 2-unit (not 1-unit) change in the input variable. This interpretation of course assumes the factors are independent, which is the case in a full factorial.

  Please see the reference for more details.
}


\value{Returns a \code{ggplot2} object, which, by default, is shown before being returned by this function. The \code{ggplot2} object may be further manipulated, if desired.}


\references{A Pareto plot is similar in spirit to the Lenth plot, other than for the fact that absolute coefficients are shown. Please see Chapter 5 of the following book:
Kevin Dunn, 2010 to 2026, \emph{Process Improvement using Data}, \url{https://learnche.org/pid}
}



\author{Kevin Dunn, <kgdunn@gmail.com>}



\examples{
# 2-factor example
T <- c(-1, +1, -1, +1)  # centered and scaled temperature
S <- c(-1, -1, +1, +1)  # centered and scaled speed variable
y <- c(69, 60, 64, 53)  # conversion, is our response variable, y
doe.model <- lm(y ~ T + S + T * S)  # create a model with main effects, and interaction
paretoPlot(doe.model)

# 3-factor example
data(pollutant)
mod.full <- lm(y ~ C*T*S, data=pollutant)
paretoPlot(mod.full)
}




\concept{ design of experiments }
