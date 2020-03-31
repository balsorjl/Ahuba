##### == Determine number of important basis vectors  == #####

cum_var <- function(pca.eig.3, thresh){
  
  pca.eig.3 = pca.eig.3
  thresh = thresh
  
  while(T){
    
    cum.sum.counter <- 0
    
    for (i in 1:length(pca.eig.3)) {
      
      if(pca.eig.3[i] >= thresh){ ## You can change the value of 80 
        ## to whatever minimum variance threshold you desire.
        
        cum.sum.counter <- i
        
        break
        
      } else {
        
        next
        
      }
      
    }
    
    break
    
  }
  
  return(cum.sum.counter)
  
}
