\name{skewns}
\alias{skewns}
\docType{data}
\title{
Skew Normal Scores
%%   ~~ data name/kind ... ~~
}
\description{
This defines the class of skew normal scores.  These are optimal scores for
error distributions with a skew normal distribution.  The R package sn, devepoled by Azzalini (2014)
is required to obtain these scoes.
%%  ~~ A concise (1-5 lines) description of the dataset. ~~
}
\usage{data("skewns")}
\format{
  The format is:
Formal class 'scores' [package "Rfit"] with 3 slots
  ..@ phi  :function (u, param)  
  ..@ Dphi :function (u, param)  
  ..@ param: num 1
}
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
}
\references{
Azzalini, A. (2014), The R sn package: The skew-normal and skew-t distributions (version 1.0-0), Universita di Padova, Italia.

Kloke, J. and McKean, J.W. (2014), \emph{Nonparametric statistical methods using R}, Boca Raton, FL: Chapman-Hall.

%%  ~~ possibly secondary sources and usages ~~
}
\examples{
#   skew normal scores for a skew normal distribution with alpha = 7
library(mnormt)
library(sn)
#source("skewns.r")
n <- 100
ps <- (1:n)/(n+1)
salp <- skewns
salp@param <- c(-7)
phialp <- getScores(salp,ps)
}
\keyword{datasets}
