
Current_prediction <- function(EE_NORM_cp)
{
  points_cp = dim(EE_NORM_cp)[1];
  members_cp = dim(EE_NORM_cp)[2];
  
  cp = matrix(, nrow= points_cp, ncol= 1);
  
  for(i in 1:points_cp)
  {
    cp[i,1] = 0;
    for(j in 1:members_cp)
    {
      cp[i,1] = cp[i,1] + EE_NORM_cp[i,j];
    }
    cp[i,1] = cp[i,1] / members_cp;
  }
  return(cp);
} # end of the function

