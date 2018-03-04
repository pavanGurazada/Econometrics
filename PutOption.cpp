#include <RcppArmadillo.h>

// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::plugins(cpp11)]]

using Rcpp::NumericVector;
using Rcpp::log;
using Rcpp::pnorm;

/*
using arma::colvec;
using arma::log;
using arma::normcdf;
*/
using std::sqrt;
using std::exp;

// [[Rcpp::export]]
NumericVector put_option_pricer_rcpp(NumericVector s, double k, double r, double y, double t, double sigma) {
  NumericVector d1 = (Rcpp::log(s/k) + (r - y + sigma * sigma/2.0)*t)/(sigma*sqrt(t));
  NumericVector d2 = d1 - sigma * sqrt(t);
  
  NumericVector V = pnorm(-d2) * k * exp(-r * t) - s * exp(-y * t) * pnorm(-d1);
  
  return V;
}

/*
// [[Rcpp::export]]
colvec put_option_pricer_arma(colvec s, double k, double r, double y, double t, double sigma) {
  colvec d1 = (arma::log(s/k) + (r - y + sigma*sigma/2.0)*t)/(sigma * sqrt(t));
  colvec d2 = d1 - sigma * sqrt(t);
  
  colvec V = normcdf(-d2) * k * exp(-r * t) - s * exp(-y * t) % normcdf(-d1);
  
  return V;
}
*/

/*** R
put_option_pricer <- function(s, k, r, y, t, sigma) {
  d1 <- (log(s/k) + (r - y + sigma^2)*t)/(sigma * sqrt(t))
  d2 <- d1 - sigma * sqrt(t)
  
  V <- pnorm(-d2) * k * exp(-r*t) - s* exp(-y*t) * pnorm(-d1)
  
  return(V)
}

s <- matrix(seq(0, 100, by = 0.001), ncol = 1)

rbenchmark::benchmark(R = put_option_pricer(s, 60, 0.01, 0.02, 1, 0.05),
                      # Arma = put_option_pricer_arma(s, 60, 0.01, 0.02, 1, 0.05),
                      Rcpp = put_option_pricer_rcpp(s, 60, 0.01, 0.02, 1, 0.05),
                      order = "relative",
                      replications = 100)[, 1:4]
*/

