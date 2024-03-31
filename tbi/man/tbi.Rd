\name{tbi}
\alias{tbi}
\title{Tea Bag Index}
\description{
This function calculates the two parameters of the Tea Bag Index (decomposition rate and litter stabilisation factor) following the methodology described in \href{https://besjournals.onlinelibrary.wiley.com/doi/full/10.1111/2041-210X.12097}{Keuskamp JA, Dingemans BJJ, Lehtinen T, Sarneel JM, Hefting MM. 2013. Tea Bag Index: a novel approach to collect uniform decomposition data across ecosystems. Methods in ecology and evolution, 4: 1070–1075}.
}
\usage{
tbi(g.start,
    r.start,
    g.end,
    r.end,
    incubation.time,
    Hg=0.842,
    Hr=0.552,
    Wbag=0.2494625,
    FcorrGreen=1,
    FcorrRed=1)
}
\arguments{

\item{g.start}{A numeric vector with the mass (g) of the bags of green tea at the start of the experiment (including tea, bag, cord and label)}

\item{r.start}{A numeric vector with the mass (g) of the bags of rooibos tea at the start of the experiment (including tea, bag, cord and label)}

\item{g.end}{A numeric vector with the mass (g) of green tea at the end of the incubation period (only the tea, without the bag, cord and label)}

\item{r.end}{A numeric vector with the mass (g) of rooibos tea at the end of the incubation period (only the tea, without the bag, cord and label)}

\item{incubation.time}{A numeric vector with the incubation time (days)}

\item{Hg}{Hydrolysable fraction of green tea}

\item{Hr}{Hydrolysable fraction of red tea}

\item{Wbag}{Mass (g) of an empty tea bag (including bag, cord and label)}

\item{FcorrGreen}{Correction factor for handling and drying green tea}

\item{FcorrRed}{Correction factor for handling and drying red tea}

}

\examples{
tbi(g.start=2,
    r.start=2,
    g.end=0.630,
    r.end=1.450,
    incubation.time = 90)
}
