
# Linear-scaling ..............................................................

L_Norm <- function(A)
{
  points_l = dim(A)[1]
  members_l = dim(A)[2]
  
  L_Mat = matrix(data=0,nrow =points_l, ncol=members_l);
  
  for(j in 1:members_l)
  {
    smax = max(A[,j]);
    smin = min(A[,j]);
    
    for(i in 1:points_l)
    {
      L_Mat[i,j] = (A[i,j]-smin)/(smax-smin);
    }
  }
  return(L_Mat);
} # end of the function

