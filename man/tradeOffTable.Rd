

\name{tradeOffTable}

\alias{tradeOffTable}


\title{A trade-off table of fractional factorial designs}

\description{Creates a new plot that shows a trade-off table for fractional factorial designs.}

\usage{tradeOffTable()}


\details{

	Displays the following trade-off table:

	\figure{trade-off-table.png}{options: style="width: 900px"}

	The rows in the table are the number of experiments done in the fractional factorial (\eqn{n}).\cr
	The columns are the number of factors under investigation in the design (\eqn{k}).\cr
	The cell at a particular row/column intersection gives several pieces of information:
	\itemize{
			\item	The top-left entry of the form: \eqn{2^{k-p}=n}. For example, \eqn{p=1} corresponds to half-fractions,
			and \eqn{p=2} corresponds to quarter-fractions.
			\item	The subscript in the top-left entry, written in Roman numerals gives the design resolution. For example, \eqn{IV}
			corresponds to a resolution 4 design, implying 2-factor interactions are at most confounded with other 2-factor interactions.
			\item	The bold entries in the bottom-right tell how to generate the remaining factors in the design. \cr
					A "full" entry indicates a full factorial; while "twice" indicates a twice replicated full factorial.
	}
	Blank entries are impossible fractional factorial combinations.

	A detailed explanation of the table is provided in the book reference.
}

\references{
	Chapter 5 of the following book: Kevin Dunn, 2010 to 2026, \emph{Process Improvement using Data}, \url{https://learnche.org/pid/}

	Please see this paper to gain an understanding of how these trade-off tables are constructed:\cr
	Arthur Fries and William G. Hunter, (1980) Minimum Aberration \eqn{2^{k-p}} Designs, \emph{Technometrics}, \bold{22}(4), pp. 601-608, \url{https://www.jstor.org/stable/1268198}
}

\note{
	Certain blocks are not unique. For example, a \eqn{2^{8-3}} resolution IV design (with 32 runs and 8 factors) is shown as having +/-\bold{F = ABC}, +/-\bold{G=ABD} and +/-\bold{H=ACDE}. But another option is  +/-\bold{H=BCDE}, which you might see in other software, or tables in textbooks.
}

\author{Kevin Dunn, <kgdunn@gmail.com>}


\seealso{
	\code{\link{tradeoff}} which can be used to extend the table out to more factors or more experiments.
}

\examples{
	tradeOffTable()
}

