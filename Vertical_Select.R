

Vertical_Select <- function(SCORE_LIST_SET_NORM, SCORE_LIST_SET_IND)
{
   points = dim(SCORE_LIST_SET)[1]
   members = dim(SCORE_LIST_SET)[2]
  
   target2 = matrix(data=0, nrow = points, ncol=1)
   
   target2 = Current_prediction(SCORE_LIST_SET_NORM)
   
   target2_SORT = matrix(data=0, nrow = points, ncol=1)
   target2_IND = matrix(data=0, nrow = points, ncol=1)
   rnk = matrix(data =0, nrow = points, ncol = 1);
   W2 = matrix(data =0, nrow = points, ncol = 1);
   
   temp = sort(target2[,1], decreasing= TRUE, index.return = TRUE);
   target2_SORT[,1] = as.matrix(temp$x);
   target2_IND[,1] = as.matrix(temp$ix);
   
   for(i in 1:points)
   {
     rnk[target2_IND[i,1],1] = i;
   }
   
   for(i in 1:points)
   {
     W2[i,1] = 1/(rnk[i,1])
   }
   
   library(weights)
   library(wCorr)
   
   corr2 = matrix(, nrow = members, ncol = 4);
   for(i in 1:members)
   {
     corr2[i,] = wtd.cor(SCORE_LIST_SET_NORM[,i],target2, W2[,1]);
   }
   corr2_sort = sort(corr2[,1], decreasing = TRUE, index.return = TRUE);
   corr2_ind <- as.matrix(corr2_sort$ix);
   corr2_sort_mat <- as.matrix(corr2_sort$x);
   
   Selected_Vertical = matrix(data =0, nrow = members, ncol = 1); 
   considered2 = matrix(data =0, nrow =members, ncol =1);
   
   kkk = 1;
   
   E2 = matrix(, nrow = points);
   E2_NORM = matrix(, nrow = points);
   p2 = matrix(, nrow = points);
   
   E2[,kkk] = SCORE_LIST_SET[,corr2_ind[1,1]];
   E2_NORM[,kkk] = SCORE_LIST_SET_NORM[,corr2_ind[1,1]];
   p2[,1] = E2_NORM[,kkk];
   
   Selected_Vertical[corr2_ind[1,1],1] = 1;
   considered2[corr2_ind[1,1],1] = 1;
   
   # sorting by Wpearson correaltion to p
   
   corr_p2 = matrix(, nrow = members, ncol = 4);
   rem=0;
   
   for(i in 1:members)
   {
     if(considered2[i,1] == 0)
     {
       corr_p2[i,] = wtd.cor(SCORE_LIST_SET_NORM[,i],p2[,1], W2[,1]);
       rem = rem +1;
     }
     else
     {
       corr_p2[i,1] = 1000;
       corr_p2[i,2] = 20;
       corr_p2[i,3] = 20
       corr_p2[i,4] = 20;
     }
   } 
   
   corrp2_sort = sort(corr_p2[,1], decreasing = TRUE, index.return = TRUE);
   corrp2_ind <- as.matrix(corrp2_sort$ix);
   
   p2_v = Current_prediction(E2_NORM);
   
   count =members-1;
   ns =1;
   
   while(count >0)
   {
     ns=1;
     j = corrp2_ind[ns,1];
     fc=0;
     
     while((considered2[j,1] == 1)&&(ns<members))
     {
       if(considered2[j,1] == 1)
       { 
         ns = ns +1;
         j = corrp2_ind[ns,1];
         fc=fc+1;
       } 
     }
    
     considered2[j,1] = 1;
     d = dim(E2);
     d1 = d[2];
     E12 = matrix(, nrow = points, ncol = d1+1);
     E12_NORM = matrix(, nrow = points, ncol = d1+1);
     
     E12[,1:d1] = E2[,1:d1];
     E12_NORM[,1:d1] = E2_NORM[,1:d1];
     
     E12[,d1+1] = SCORE_LIST_SET[,j];
     E12_NORM[,d1+1] = SCORE_LIST_SET_NORM[,j];
     
     x =d1+1;
     
     E12_UNI = matrix(data =0, nrow =points, ncol = 1);
     E12_UNI_NORM = matrix(data =0, nrow =points, ncol = 1);
     
     E12_UNI = Current_prediction(E12_NORM);
     
     p2_v = Current_prediction(E2_NORM);
     
     wp2_ejv = wtd.cor(E12_UNI[,1],target2[,1], W2[,1]);
     
     wp2_ev = wtd.cor(p2_v[,1],target2[,1], W2[,1]);
     
     if((wp2_ejv[1,1] > wp2_ev[1,1]))  
     {
       Selected_Vertical[j,1] =1;
       
       k1 = dim(E2)[2];
       E2_NEW = matrix(, nrow = points, ncol = k1+1);
       E2_NEW_NORM = matrix(, nrow = points, ncol = k1+1);
       
       E2_NEW[,1:k1] = E2[,];
       E2_NEW_NORM[,1:k1] = E2_NORM[,];
       
       
       E2_NEW[,k1+1] = SCORE_LIST_SET[,j];
       E2_NEW_NORM[,k1+1] = SCORE_LIST_SET_NORM[,j];
       
       E2 = E2_NEW;
       E2_NORM = E2_NEW_NORM;
       
       p2 = Current_prediction(E2_NORM);
       
       for(m in 1:members)
       {
         if(considered2[m,1] ==0)
           rem = rem+1;
       }
       corr2_p = matrix(, nrow = members, ncol = 4);
       
       for(i in 1:members)
       {
         if(considered2[i,1] == 0)
         {
           corr2_p[i,] = wtd.cor(SCORE_LIST_SET_NORM[,i],p2[,1], W2[,1])
         }else{
           corr2_p[i,1] = 1000;
           corr2_p[i,2] = 20;
           corr2_p[i,3] = 20
           corr2_p[i,4] = 20;
         }
       }
       corrp2_sort = sort(corr2_p[,1], decreasing = TRUE, index.return = TRUE);
       corrp2_ind <- as.matrix(corrp2_sort$ix);
     }
     count = count-1;
   } # end of outer-while loop

  return(Selected_Vertical);
}

