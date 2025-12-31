

\name{contourPlot}


\alias{contourPlot}



\title{2-dimensional contour plot of a factorial design model}


\description{Creates a two-dimensional contour plot of a factorial design model (linear least squares model). The two specified factors are
plotted on the horizontal and vertical axes. If the model has more than two factors, then the remaining factors are set at their zero level.
It is assumed the model is in coded units.}


\usage{contourPlot(lsmodel,
            xlab=attr(lsmodel$terms,'term.labels')[1],
            ylab=attr(lsmodel$terms,'term.labels')[2],
            main="Contour plot",
            N=25,
            xlim=c(-3.2, 3.2),
            ylim=c(-3.2, 3.2),
            colour.function=terrain.colors)}




\arguments{

\item{lsmodel}{
  a linear model object (least squares model) created by the \code{lm(\ldots)} function.
}
\item{xlab}{
  the label and variable used for horizontal axis in the bar plot, and \bold{must} also match a variable in the \code{lsmodel}; the default will use the first available variable.
}
\item{ylab}{
  the label and variable used for vertical axis in the bar plot, and \bold{must} also match a variable in the \code{lsmodel}; the default will use the second available variable.
}
\item{main}{
  label for the plot.
}
\item{N}{
  the resolution of the plot; higher values have greater resolution.
}
\item{xlim}{
  the limits of the horizontal axis (in coded units); the defaults indicate the model's effect outside the standard factorial range.
}
\item{ylim}{
  the limits of the vertical axis (in coded units); the defaults indicate the model's effect outside the standard factorial range.
}
\item{colour.function}{
  the function used to colour-code the contour plot; see \code{\link{rainbow}} for alternative colour schemes.
}
}


\details{Typical usage is to create a generic linear model with the \code{lm(\dots)} command, and supply that as the input to this function.

For example, a general design of experiments with 4 factors: A, B, C, and D can be built using\cr
\code{lsmodel <- lm(y ~ A*B*C*D)}, and then various 2-D contour plots visualized with\cr
\code{contourPlot(lsmodel, "A", "B")}, \code{contourPlot(lsmodel, "B", "C")}, or whichever combination of factors
are desired. The variables \emph{not} being plotted are set at their zero (0) value.

Future versions of this function will allow specifying the inactive variable levels. In the interim, please see the \code{rsm}
package (the \code{rsm::contour(...)} function) if this functionality is required.
}


\value{Returns a \code{ggplot2} object, which, by default, is shown before being returned by this function.
The \code{ggplot2} object may be further manipulated, if desired.}


\references{Please see Chapter 5 of the following book:
Kevin Dunn, 2010 to 2026, \emph{Process Improvement using Data}, \url{https://learnche.org/pid}
}



\author{Kevin Dunn, <kgdunn@gmail.com>}


\examples{

# 2-factor example
# ----------------
T <- c(-1, +1, -1, +1)  # centered and scaled temperature
S <- c(-1, -1, +1, +1)  # centered and scaled speed variable
y <- c(69, 60, 64, 53)  # conversion, is our response variable, y
doe.model <- lm(y ~ T + S + T * S)  # model with main effects, and interaction
contourPlot(doe.model)

# 3-factor example
# ----------------
data(pollutant)
mod.full <- lm(y ~ C*T*S, data=pollutant)
contourPlot(mod.full, N=50)               # high resolution plot
contourPlot(mod.full, xlab='C', ylab='S',
            main="Strong C:S interaction",
			colour.function=rainbow)

# Central composite design
P <- c(-1,   +1,  -1,  +1,     0, -1.41,     0, +1.41)
T <- c(-1,   -1,  +1,  +1, +1.41,     0, -1.41,     0)
y <- c(715, 713, 733, 725,   738,   717,   721,   710)
mod.CCD <- lm(y ~ P*T + I(P^2) + I(T^2))
contourPlot(mod.CCD, 'P', 'T', xlim=c(-2.2, 2.2), ylim=c(-3,2))
}



\concept{ design of experiments }
