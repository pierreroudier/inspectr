require(RcppArmadillo)
require(inline)

# code <- '
#   arma::mat beta = Rcpp::as<arma::mat>(beta_);
#   int n = beta.n_rows; 
#   int p = beta.n_cols;
#   arma::mat Ip = arma::eye<arma::mat>( p, p );
#   int ii;
#   double S=0;
#   for (ii=0; ii<(n+p); ii++) {
#     S += ii; // dummy calculation
#   }
#   return Rcpp::wrap(S);
#  '
# 
# fun <- cxxfunction(signature(beta_ ="matrix"),
#                        code, plugin="RcppArmadillo")

m <- matrix(1:9,3)
# fun(m)

## distances between a matrix x and a row of values y
code_dist <- '
  arma::mat x = Rcpp::as<arma::mat>(x_);
  arma::rowvec y = Rcpp::as<arma::rowvec>(y_);
  int n = x.n_rows;
  arma::rowvec res(n);
  int i;
  for (i=0; i<n, i++) {
    res.col(i) = i;
  }
  return Rcpp::wrap(res);
'

fun2 <- cxxfunction(signature(x_ ="matrix", y_ = "vector"),
                       code_dist, plugin="RcppArmadillo")

fun2(x = m, y = 1:4)
