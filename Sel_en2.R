
Sel_en2 <- function(S_Ind, A)
{
  temp_mat = as.matrix(S_Ind)
  count_se2 = dim(temp_mat)[2]  #row-vector
  
  points_se2 = dim(A)[1]
  members_se2 = dim(A)[2]
  
  kk=0;
  S_En2 = matrix(data=0, nrow = points, ncol = count_se2)
  S_En2_Avg = matrix(data=0, nrow = points, ncol = 1)
  
  for(i in 1:members_se2)
  {
    if(is.element(i, temp_mat[1,]))
    {
      kk = kk+1
      S_En2[,kk] = A[,i]
    }
  }
  for(i in 1:points_se2)
  {
    S_En2_Avg[i,1] = mean(S_En2[i,])
  }
  return(S_En2_Avg);
} # end of function
