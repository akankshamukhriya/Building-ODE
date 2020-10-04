
DIV_Scores <- function(SCORE_LIST_SET_NORM, SCORE_LIST_SET_IND, k, t)
{
  points = dim(SCORE_LIST_SET_NORM)[1]
  members = dim(SCORE_LIST_SET_NORM)[2]
  
  DIV_Scores_Mat = matrix(data=0, nrow = members, ncol=4)
  library(weights);
  
  # DIVERSITY_GREEDY  ...............................................................................
  
  corr1 = matrix(data=0, nrow = 1, ncol = 4);
  temp1 = matrix(data=0, nrow = members, ncol=members)
  temp1_sum = matrix(data=0, nrow = members, ncol=1)
  temp1_hetero_sum = matrix(data=0, nrow = members, ncol=1)
  
  for(i in 1:members)
  {
    for(j in 1:members)
    {
      if(i!=j)
      {
        corr1[1,] = wtd.cor(SCORE_LIST_SET_NORM[,i],SCORE_LIST_SET_NORM[,j], W[,1]);
        temp1[i,j] = corr1[1,1]
        temp1_sum[i,1] = temp1[i,j] + temp1_sum[i,1]
      }
    }
  }
  # DIVERSITY_GENERAL  ...............................................................................
  
  corr1 = matrix(data=0, nrow = 1, ncol = 4);
  temp_gen = matrix(data=0, nrow = members, ncol=members)
  temp_gen_sum = matrix(data=0, nrow = members, ncol=1)
  temp1_gen_hetero_sum = matrix(data=0, nrow = members, ncol=1)
  
  for(i in 1:members)
  {
    for(j in 1:members)
    {
      if(i!=j)
      {
        corr1[1,] = cor(SCORE_LIST_SET_NORM[,i],SCORE_LIST_SET_NORM[,j], method = "pearson");
        temp_gen[i,j] = corr1[1,1]
        temp_gen_sum[i,1] = temp_gen[i,j] + temp_gen_sum[i,1]
      }
    }
  }
  
  # DIVERSITY_VERTICAL  ...............................................................................
  
  corr3 = matrix(data=0, nrow = 1, ncol = 4);
  temp3 = matrix(data=0, nrow = members, ncol=members)
  temp3_sum = matrix(data=0, nrow = members, ncol=1)
  
  for(i in 1:members)
  {
    for(j in 1:members)
    {
      if(i!=j)
      {
        corr3[1,] = wtd.cor(SCORE_LIST_SET_NORM[,i],SCORE_LIST_SET_NORM[,j], W_V[,1]);
        temp3[i,j] = corr3[1,1]
        temp3_sum[i,1] = temp3[i,j] + temp3_sum[i,1]
      }
    }
  }
  
  # DIVERSITY_BOOST_SEL  ...............................................................................
  
  corr4 = matrix(data=0, nrow = 1, ncol = 4);
  temp4 = matrix(data=0, nrow = members, ncol=members)
  temp4_sum = matrix(data=0, nrow = members, ncol=1)
  
  for(i in 1:members)
  {
    for(j in 1:members)
    {
      if(i!=j)
      {
        corr4[1,] = wtd.cor(SCORE_LIST_SET_NORM[,i], SCORE_LIST_SET_NORM[,j], W3[,1]);
        temp4[i,j] = corr4[1,1]
        temp4_sum[i,1] = temp4[i,j] + temp4_sum[i,1]
      }
    }
  }
  
  tm = members-1
  for(i in 1:members)
  {
    DIV_Scores_Mat[i,1] = 1-(temp1_sum[i,1]/tm)
    DIV_Scores_Mat[i,2] = 1-(temp3_sum[i,1]/tm)
    DIV_Scores_Mat[i,3] = 1-(temp4_sum[i,1]/tm)
    DIV_Scores_Mat[i,4] = 1-(temp_gen_sum[i,1]/tm)
  }
  return(DIV_Scores_Mat);
  
} # end of the function
