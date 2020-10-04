

Boost_Select <- function(SCORE_LIST_SET_NORM, SCORE_LIST_SET_IND, t, d)
{
  points = dim(SCORE_LIST_SET)[1]
  members = dim(SCORE_LIST_SET)[2]
  
  temp_target = matrix(data=0, nrow = points, ncol=1)
  temp_target_SORT = matrix(data=0, nrow = points, ncol=1)
  temp_target_IND = matrix(data=0, nrow = points, ncol=1)
  
  for(i in 1:points)
  {
    temp_target[i,1] = mean(SCORE_LIST_SET_NORM[i,])  
  }
  
  top_nt = floor(points*t)
  
  tempp = sort(temp_target[,1], decreasing= TRUE, index.return = TRUE);
  temp_target_SORT[,1]= as.matrix(tempp$x);
  temp_target_IND[,1] = as.matrix(tempp$ix);
  
  Target_Boost = Convert_binary(temp_target, t)
  
  W_Boost = matrix(data =0, nrow = points, ncol = 1);
  
  wout = 1/(2*top_nt);
  win = 1/(2*(points-top_nt));
  
  for(i in 1:points)
  {
    if(Target_Boost[i,1] == 1)
    { 
      W_Boost[i,1] = wout;    
    }
    else
    { 
      W_Boost[i,1] = win;  
    }
  }
  
  library(weights)
  library(wCorr)
  
  corr = matrix(, nrow = members, ncol = 4);
  for(i in 1:members)
  {
    corr[i,] = wtd.cor(SCORE_LIST_SET_NORM[,i],Target_Boost, W_Boost[,1]);
    
  } 
  
  corr_sort = sort(corr[,1], decreasing = TRUE, index.return = TRUE);
  corr_ind <- as.matrix(corr_sort$ix);
  corr_sort_mat <- as.matrix(corr_sort$x);
  
  Selected_Boost = matrix(data =0, nrow = members, ncol = 1); 
  considered3 = matrix(data =0, nrow = members, ncol =1);
  
  kk = 1;
  
  E = matrix(, nrow = points);
  E_NORM = matrix(, nrow = points);
  p = matrix(, nrow = points);
  
  E[,kk] = SCORE_LIST_SET[,corr_ind[1,1]];
  E_NORM[,kk] = SCORE_LIST_SET_NORM[,corr_ind[1,1]];
  
  p[,1] = E_NORM[,kk];
  
  Selected_Boost[corr_ind[1,1],1] = 1;
  considered3[corr_ind[1,1],1] = 1;
  
  # sorting by Weighted Pearson Correlation to p
  corr_p = matrix(, nrow = members, ncol = 4);
  rem=0;
  
  for(i in 1:members)
  {
    if(considered3[i,1] == 0)
    {
      corr_p[i,] = wtd.cor(SCORE_LIST_SET_NORM[,i],p[,1], W_Boost[,1]);
      rem = rem +1;
    }else{
      corr_p[i,1] = 1000;
      corr_p[i,2] = 20;
      corr_p[i,3] = 20
      corr_p[i,4] = 20;
    }
  } 
  
  corrp_sort = sort(corr_p[,1], decreasing = FALSE, index.return = TRUE);
  corrp_ind <- as.matrix(corrp_sort$ix);
  p_v = Current_prediction(E_NORM);
  
  count =members-1;
  ns =1;
  
  while(count >0)
  {
    ns =1;
    j = corrp_ind[ns,1];
    fc=0;
    
    while((considered3[j,1] == 1)&&(ns<members))
    {
      if(considered3[j,1] == 1)
      { 
        ns = ns +1;
        j = corrp_ind[ns,1];
        fc=fc+1;
      } 
    }
    
    considered3[j,1] = 1;
    d = dim(E);
    d1 = d[2];
    E1 = matrix(, nrow = points, ncol = d1+1);
    E1_NORM = matrix(, nrow = points, ncol = d1+1);
    E1[,1:d1] = E[,1:d1];
    E1_NORM[,1:d1] = E_NORM[,1:d1];
    E1[,d1+1] = SCORE_LIST_SET[,j];
    E1_NORM[,d1+1] = SCORE_LIST_SET_NORM[,j];
    x =d1+1;
    
    E1_UNI = matrix(data =0, nrow =points, ncol = 1);
    E1_UNI = Current_prediction(E1_NORM);
    
    p_v = Current_prediction(E_NORM);
    
    wp_ejv = wtd.cor(E1_UNI[,1],Target_Boost[,1], W_Boost[,1]); #wPC of ensemble to the Target, if detector-j is included in ensemble
    
    wp_ev = wtd.cor(p_v[,1],Target_Boost[,1], W_Boost[,1]); #wPC of ensemble to the Target, if detector-j is not included in ensemble
    
    if(wp_ejv[1,1] > wp_ev[1,1]) # detector-j is included in the ensemble, if TRUE
    {
      Selected_Boost[j,1] =1;
      boost_j =j;
      
      k1 = dim(E)[2];
      E_NEW = matrix(, nrow = points, ncol = k1+1);
      E_NEW_NORM = matrix(, nrow = points, ncol = k1+1);
      
      E_NEW[,1:k1] = E[,];
      E_NEW_NORM[,1:k1] = E_NORM[,];
      E_NEW[,k1+1] = SCORE_LIST_SET[,j];
      E_NEW_NORM[,k1+1] = SCORE_LIST_SET_NORM[,j];
      E = E_NEW;
      E_NORM = E_NEW_NORM;
      
      #p = Current_prediction(E,E_NORM); # Prediction (Average score) of currently selected ensemble
      p = Current_prediction(E_NORM);
      
      for(m in 1:members)
      {
        if(considered3[m,1] ==0)
          rem = rem+1;
      }
      
      # sorting by Wpearson correaltion to p
      corr_p = matrix(, nrow = members, ncol = 4);
      for(i in 1:members)
      {
        if(considered3[i,1] == 0)
        {
          corr_p[i,] = wtd.cor(SCORE_LIST_SET_NORM[,i],p[,1], W_Boost[,1])
        }
        else
        {
          corr_p[i,1] = 1000;
          corr_p[i,2] = 20;
          corr_p[i,3] = 20
          corr_p[i,4] = 20;
        }
      }
      corrp_sort = sort(corr_p[,1], decreasing = FALSE, index.return = TRUE);
      corrp_ind <- as.matrix(corrp_sort$ix);
      
      d = 0.25
      W_Boost = Boosting(Target_Boost, W_Boost, t, d, boost_j, SCORE_LIST_SET_NORM) # Boosting-step
    }  # end if ..........................................................
    count = count-1;
    
  }# end while .........................................
  
  return(Selected_Boost);
} # end of the function


