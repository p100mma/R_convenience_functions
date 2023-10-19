#depends on general.R

#finds ONE index m of elements of longDataVector, for which sign_function(longDataVector, m)==0
which_bisectSearch<- function( longDataVector, sign_function, surrogateValue=NA,
                    N_MAX= ceil(log2(length(longDataVector)))+1,
                    LDVmin2maxSorted=FALSE,...) {
if (!LDVmin2maxSorted) longDataVector<- min2maxSort(longDataVector)
L <-1 #left
R <-N<- length(longDataVector) #right
n_iter=0
while (n_iter < N_MAX) { n_iter= n_iter +1
M= floor((L+R)/2)
sgnM<- sign_function(longDataVector,M,...)
if (sgnM==0) return(M)
sgnL<- sign_function(longDataVector,L,...)
if (sgnM==sgnL) L <- M else R <- M
}
return(surrogateValue)
}

example_signFunction<- function(LV, m, some_constant, some_array){ #LV should contain nonegative min2max sorted elements, m is in 1:length(LV)
    #find m for which sum_m >= some_constant and sum_pm < some_constant (previous m, that is m -1)
    array_thr.m<- some_array
    array_thr.m[ some_array < LV[[m]] ] = 0
    sum_m <- sum(array_thr.m)
    if (sum_m < some_constant) 
        return(-1)
    if ( (m==1) && ( sum_m >= some_constant) ) #special case, if there is no previous m, condition on m only suffices that m is a solution
        return(0)
    array_thr.pm<- some_array  #pm <- previous m
    array_thr.pm[ some_array < LV[[m-1]] ] = 0
    sum_pm <- sum(array_thr.pm)
    if ( ( sum_m >= some_constant) && (sum_pm >= some_constant) )  
        return(1)
    if ( ( sum_m >= some_constant) && (sum_pm < some_constant) ) #only possible other outcome if all LV are positive nad LV is sorted 
        return(0)                                                # but lets be verbose about it
}
