#define ARMA_64BIT_WORD
#include <RcppArmadillo.h>

// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::plugins(cpp11)]]

using namespace Rcpp;

/**
 * @title 100 numpy exercises, but in RcppArmadillo
 * @author Pavan Gurazada
 * @licence MIT
 * @summary Replication of the popular 100 problems in numpy. Lets see how it
 *  goes
 *
*/

using arma::vec;

// Create null vector of size 10
vec z1(10, arma::fill::zeros);

// Create null vector of size 10; reset fifth element to 1
vec z2(10, arma::fill::zeros); 
z2[4] = 1;

// Create vector with values ranging from 10 to 49
vec z3 = arma::linspace(10, 49, 39);










