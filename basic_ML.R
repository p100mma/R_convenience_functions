
unstratified_CV<- function(N, k, shuffle=TRUE, return_list=FALSE){
N %/% k -> f_size
N_f<- f_size*k
true_idx<- 1: N
if (shuffle) perm_idx<- sample(true_idx, N, replace=FALSE) else perm_idx <- true_idx

perm_idx[1:N_f]-> idx_kdiv
split_idx<- split(idx_kdiv,
		 ceiling( seq_along(idx_kdiv)/f_size) #grouping function, i -> CEIL( i / FOLD_SIZE )
		 )
if (N_f < N) {  # if N does not divide cleanly into k, append the rest of perm_idx to the last part of split_idx
 split_idx[[length(split_idx)]][   #access the last element, each has f_size elements
	   (f_size+1): (f_size + (N - N_f) )  #append the remaining N- N_f elems to the end
			       ] = perm_idx[ (N_f +1) : N]
}

#assemble  the train indexes
train_idx<- lapply( 1:k, function(k_i)
			unlist(split_idx[ -k_i] ) # all the indexes without the k_i-th fold
		  )
allFALSE<- rep(FALSE, N)
train_masks<- lapply( train_idx, function(i_idx)
			  { i_mask<- allFALSE; i_mask[ i_idx ] = TRUE; i_mask   } )
if (return_list) return(train_masks) else {
	retmat<- matrix(nrow=k, ncol=N)
	for (i in 1:k) retmat[i,] = train_masks[[i]]
	return(retmat)
}
}

unstratified_bootstrap<- function(N, N_b= N, check_oob=TRUE){

b_idx<- sample(1:N, N_b, replace=TRUE)
if (check_oob){
oob_idx<- c(1:N) [ -b_idx ]
if (!length(oob_idx)) stop("oob check: stopped because there are no oob objects. Consider smaller N_b")
}
return(b_idx)
}

stratified_bootstrap<- function(y, prop_b, check_oob=TRUE){
N<- length(y)
yi<- sort(unique(y)) 
N_yi<- unlist(lapply(yi, function(yj) sum(y==yj) ) )
N_b_yi= floor(prop_b*N_yi)
true_idx_yi<- lapply(yi, function(yj) which(y==yj) )
b_idx_yi <- lapply( seq_along(yi), function(j) 
		    sample(true_idx_yi[[j]], N_b_yi[[j]], replace=TRUE)
		 )
b_idx<- do.call(c, b_idx_yi)
if (check_oob){
oob_idx<- c(1:N) [ -b_idx ]
if (!length(oob_idx)) stop("oob check: stopped because there are no oob objects. Consider smaller N_b")
}
return(b_idx)
}



stratified_CV<- function(y, k, shuffle=TRUE, return_list=FALSE){
N<- length(y)
yi<- sort(unique(y)) 
N_yi<- unlist(lapply(yi, function(yj) sum(y==yj) ) )
N_yi %/% k -> yi_f_size
N_f_yi<- yi_f_size * k


true_idx_yi<- lapply(yi, function(yj) which(y==yj) )
if (shuffle){ 
	perm_idx_yi<-lapply(seq_along(true_idx_yi),
			  function(j)   sample(true_idx_yi[[j]], N_yi[[j]], replace=FALSE)
			   )
	    } else {perm_idx_yi <- true_idx_yi}
idx_kdiv_yi <- lapply(seq_along(perm_idx_yi),
		      function(j) perm_idx_yi[[j]][1: N_f_yi[[j]] ]
		     )

split_idx_yi<- lapply( seq_along(idx_kdiv_yi), function(j)
			split( idx_kdiv_yi[[j]],
				ceiling( seq_along(idx_kdiv_yi[[j]])/ yi_f_size[[j]] )
			     )
		     )

for (j in seq_along(split_idx_yi) ){ #append reminders to k-th fold, for each y_i
if (N_f_yi[[j]] < N_yi[[j]] ) {
split_idx_yi[[j]][[length(split_idx_yi[[j]])]][
	 (yi_f_size[[j]] +1):( yi_f_size[[j]] + ( N_yi[[j]] - N_f_yi[[j]] )  )
					      ] = perm_idx_yi[[j]] [ (N_f_yi[[j]] +1 ) : N_yi[[j]] ]
}
}

#assemble  the train indexes
# get train idx for all y_i
train_idx<- lapply( 1:k, function(k_i) {
			yi_tr<- list()
			for (j in seq_along(yi))
			{
			yi_tr[[j]]<-unlist(split_idx_yi[[j]][ -k_i] ) # all the indexes without the k_i-th fold
			}
			do.call(c, yi_tr)
					}
		  )
allFALSE<- rep(FALSE, N)
train_masks<- lapply( train_idx, function(i_idx)
			  { i_mask<- allFALSE; i_mask[ i_idx ] = TRUE; i_mask   } )
if (return_list) return(train_masks) else {
	retmat<- matrix(nrow=k, ncol=N)
	for (i in 1:k) retmat[i,] = train_masks[[i]]
	return(retmat)
}
}


rep_unstratified_CV<- function( N, k, r, return_list= FALSE){
rep_results<- lapply( 1:r, function(i_r) 
			unstratified_CV(N, k, shuffle=TRUE, return_list=return_list) 
		    )
if (return_list) do.call(c, rep_results) else {
	rep_arr<- array( dim=c(k,N,r) )
	for (i in 1:r) rep_arr[,,i] <- rep_results[[i]]
	return(rep_arr)
					      }
}

rep_unstratified_bootstrap<- function( N, r, N_b=N, check_oob=TRUE){

rep_results<- lapply( 1:r, function(i_r) 
			unstratified_bootstrap(N, N_b,check_oob) 
		    )
return(rep_results)
}

rep_stratified_CV<- function( y, k, r, return_list= FALSE){
rep_results<- lapply( 1:r, function(i_r) 
			stratified_CV(y, k, shuffle=TRUE, return_list=return_list) 
		    )
if (return_list) do.call(c, rep_results) else {
	rep_arr<- array( dim=c(k,N,r) )
	for (i in 1:r) rep_arr[,,i] <- rep_results[[i]]
	return(rep_arr)
					      }
}


rep_stratified_bootstrap<- function( y, r, prop_b, check_oob=TRUE){

rep_results<- lapply( 1:r, function(i_r) 
			stratified_bootstrap(y, prop_b,check_oob) 
		    )
return(rep_results)
}

