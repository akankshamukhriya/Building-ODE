
# Z SCORE NORMALIZATION .......................................................

Z_Norm <- function(A)
{
  points_z = dim(A)[1]
  members_z = dim(A)[2]
  
  Z_Mat = matrix(data=0, nrow =points_z, ncol=members_z);
  
  for(j in 1:members_z)
  {
    M_A = mean(A[,j]);
    STD_A = sd(A[,j]);
    
    for(i in 1:points_z)
    {
      temp1_z = A[i,j]-M_A;
      temp2_z = STD_A;
      Z_Mat[i,j]= temp1_z/temp2_z;
      Z_Mat[i,j] = pnorm(Z_Mat[i,j]);
    }
  }
  return(Z_Mat);
} # end of the funtion 
