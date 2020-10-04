
# Boosting-Function  .....................................................

# Input: 
# Target_pass - Target set
# W_Boost_pass - Weight vector
# t_pass - t: threshold percentage for deciding |top-k|
# d_pass - d: drop rate
# boost_j - Newly added ensemble member
# SCORE_LIST_SET_NORM - Set of Z-normalized outlier score lists of all candidate detectors


# Output:
# W-Boost_pass - Updated weight vector

Boosting <- function(Target_Boost2, W_Boost2, t2, d2, boost_j, SCORE_LIST_SET_NORM)
{
  
  points_b = dim(W_Boost2)[1]
  outliers_included = matrix(data=0, nrow =points_b, ncol=1)
  temp_b = matrix(data=0, nrow =points_b, ncol=1)
  
  temp_b = as.matrix(SCORE_LIST_SET_NORM[,boost_j])
  outliers_included = Convert_binary(temp_b,t2)
  
  for(i in 1:points_b)
  {
    if((Target_Boost2[i,1]==1)&&(outliers_included[i,1]==1))
    { 
      W_Boost2[i,1] = W_Boost2[i,1]*d2
    }
  }
  return(W_Boost2)
} # END OF THE FUNCTION ........................................................

