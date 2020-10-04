
Selected_en <- function(Sel_Boost, A)
{
  points_se = dim(A)[1]
  members_se = dim(A)[2]
  en_count = sum(Sel_Boost[,1]) #row-vector, consists of indices of selected
  
  S_en = matrix(data=0, nrow=points_se, ncol= en_count)
  k=0
  
  for(i in 1:members_se)
  {
    if(Sel_Boost[i,1]==1)
    {
      k = k+1;
      S_en[,k] = A[,i]
    }
  }
  return(S_en);
} # end of the function
