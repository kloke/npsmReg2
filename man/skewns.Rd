\name{skewns}
\alias{skewns}
\docType{data}
\title{ Skew Normal Scores }
\description{
This defines the class of skew normal scores.  These are optimal scores for
error distributions with a skew normal distribution.  The R package sn, devepoled by Azzalini (2014)
is required to obtain these scoes.
}
\usage{data("skewns")}
\format{
  The format is:
Formal class 'scores' [package "Rfit"] with 3 slots
  ..@ phi  :function (u, param)  
  ..@ Dphi :function (u, param)  
  ..@ param: num 1
}
\references{
Azzalini, A. (2014), The R sn package: The skew-normal and skew-t distributions (version 1.0-0), Universita di Padova, Italia.

Kloke, J. and McKean, J.W. (2014), \emph{Nonparametric statistical methods using R}, Boca Raton, FL: Chapman-Hall.
}
