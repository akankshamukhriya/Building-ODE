
Convert_binary <- function(target_c1, t_c)
{
  points_c = dim(target_c1)[1]
  top_c = floor(points_c*t_c)
  target_c2 = matrix(data=0, nrow =points_c, ncol=1)
  
  temp_c_IND = Sort_Descending(target_c1)
  
  for(i in 1:top_c)
  {
    target_c2[temp_c_IND[i, 1],1] = 1
  }
  
  return(target_c2);
} # end of the function  
