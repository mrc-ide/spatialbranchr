// 22-11-2018 Two simple optimisation steps
// to cut down running time.
// 1. Have a cut-off for SI after which it drops to 0
// and multiply upto the cut-off point.
// 2. In the incidence matrix skip multiplication
// if the entire column is 0. Comparison of col to 0
// has to be done in R because apparently there is no
// vector comparison operator in stan.

/* stan does not have an in operator.
// https://discourse.mc-stan.org/t/stan-equivalent-of-rs-in-function/3849/7
*/
functions {
   // if(r_in(3,{1,2,3,4})) will evaluate as 1
  int r_in(int pos, int[] pos_var) {
   
    for (p in 1:(size(pos_var))) {
       if (pos_var[p] == pos) {
       // can return immediately, as soon as find a match
          return 1;
       } 
    }
    return 0;
  }
}

data {
  int <lower = 1> T; // Time points
  int <lower = 1> N; // Number of locations
  int <lower = 0>  I[T, N];
  row_vector[T + 1] SI;
  int <lower = 0> rindex[T, N];
  int num_Rjt ;
  real pmovement[N, N];  
  real prior_mean;
  real prior_std;
  //int zero_incid; // number of countries with zero incidence
  //int zero_idx[zero_incid]; // indices of places with 0 incidence.
}

parameters {
 real <lower = 0> R[num_Rjt];
}

model {
  real row_total;
  real a = ( prior_mean / prior_std)^2;
  real b = prior_mean / ( prior_std ^ 2);
  R ~ gamma(a, b);  

  for(t in 2:T){
    for(j in 1:N){
      real mu = 0;
      // Calculate mu[ t, j]
      for( i in 1:N) {
        real tmp = 0;
	// If i is index of a place with 0 incidence
	// skip this multiplication
	//if(r_in(i, zero_idx)) {
	//  continue;
	//}  

        for( s in 1:t){
	  tmp = tmp + I[ s, i] * SI[ t - s + 1];
	}
        tmp = tmp * pmovement[i, j] * R[ rindex[ t, i]];
	mu = mu + tmp;
      } // end of computing mu[ t, j]
      target += poisson_lpmf(I[ t, j] | mu);
    }
  }
}
