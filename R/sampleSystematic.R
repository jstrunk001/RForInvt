#'@title
#'  take (a) systematic sample(s) from a vector
#'
#'@description
#'  take (a) systematic sample(s) from a vector
#'
#'@details
#'
#'  This program is free software but it is provided WITHOUT WARRANTY
#'  and with ABSOLUTELY NO GUARANTEE of fitness or functionality for any purpose;
#'  you can redistribute it and/or modify it under the terms of the GNU
#'  General Public License as published by the Free Software Foundation;
#'  either version 2 of the License, or (at your option) any later version.
#'
#'\cr
#'Revision History
#' \tabular{ll}{
#'1.0 \tab 7/13/2021 Implemented \cr
#'1.1 \tab date and revisions.. \cr
#'}
#'
#'@author
#'
#'Jacob Strunk <someone@@somewhere.com>
#'
#'
#'@param N vector length
#'@param n length of sample (optionally a vector of samples)
#'@param exact_n should n be matched exactly
#'@param as_idx should a numeric index be return (T) or T/F vector (F)
#'
#'@return
#'  vector of numeric indices or T/F vector of length N
#'  vector of 0's (not sampled) and group ids 1:n for sampled items if length(n) > 1
#'
#'@examples
#'
#'  sampleSystematic(2000, 25)
#'  sampleSystematic(205, n=c(50,50))
#'
#'
#'@export
#
#'@seealso \code{\link{sample}}\cr \code{\link{which}}\cr

#Desired upgrades to this function:
#add nj vector of n values
#

sampleSystematic = function(
                            N
                            , n=NA
                            , exact_n = T
                            , as_idx = T
                            ){

  #handle systematic sample with more than one group
  if(length(n)>1){
    idx = c(rep(0,N))
    n_to = length(n)
    for(j in 1:n_to){
      #take sample
      Nj = sum(idx==0)
      idx_j = sample.systematic( N=Nj , n=n[j] , exact_n=exact_n )

      #translate sample based on remaining unsampled items
      idx[idx==0][idx_j] = j

    }

  }

  #take single systematic sample
  if(length(n) == 1){
   #prepare initial conditions
   k = N/n
   k_min = floor(k)
   k_max = ceiling(k)
   k_mod =  N %% n
   k_max_p = k - k_min
   k_min_p = 1 - k_max_p

   #create random start
   start = sample(1:sample(c(k_min,k_max),1,prob=c(k_min_p,k_max_p)),1)

   #create random distances between observations since
   #interval N/n is rarely an integer
   vec_k = c(start,sample(c(k_min,k_max),n-1,replace=T,prob=c(k_min_p,k_max_p)))
   idx = cumsum(vec_k)

   #get rid of too large indices
   idx=idx[idx<N]

   #force the vector length to be n - otherwise could be off by 1
   #subsample idx to size n, or add missing idx with random supplement
    if(exact_n){
      ln_idx = length(idx)
      n_diff = n - ln_idx
      if(n_diff > 0){
       idx = sort(c(idx, sample((1:N)[-idx],n_diff)))
      }
      if(n_diff < 0){
       idx = sort(sample(idx,n))
      }
    }
    if(!as_idx) idx = 1:N %in% idx

   if(max(idx) > N) browser()
  }

 return(idx)

}


