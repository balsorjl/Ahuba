##### == Creating feature matrix == #####

feature_matrix <- function(Corr.scores.bfpval,
                           Corr.scores.Rval,
                           thresh) {
  
  library(tidyverse)
  
  Corr.scores.bfpval2 <- Corr.scores.bfpval
  Corr.scores.bfpval2[Corr.scores.bfpval2 > (thresh)] <- NA 
  
  while (T) {
    
    for (i in 1:nrow(Corr.scores.bfpval)){
      
      for (j in 1:ncol(Corr.scores.bfpval)) {
        
        if (is.na(Corr.scores.bfpval2[i,j]) == TRUE ) { 
          
          Corr.scores.Rval[i,j] <- NA
          
        } else {
          
          Corr.scores.Rval[i,j] <- round(Corr.scores.Rval[i,j],3)
          
        }
        
      }
      
    }
    
    break
    
  }
  
  Corr.scores.Rval <- as.data.frame(Corr.scores.Rval)
  
  Corr.scores.Rval$dim <- rownames(Corr.scores.Rval)
  
  Corr.scores.Rval2 <- gather(Corr.scores.Rval, feature, value, -dim)
  
  names(Corr.scores.Rval2) <- c("Dimension", "variable", "value")
  
  index.ord <- colnames(Corr.scores.Rval)[-ncol(Corr.scores.Rval)]
  
  ggplot(Corr.scores.Rval2,
         aes(x = variable, 
             y = Dimension)) +
    geom_tile(data = Corr.scores.Rval2,
              aes(fill = value,
                  width = 0.95,
                  height = 0.95))+ 
    scale_fill_continuous(low = "red",
                          high = "darkgreen",
                          limits = c(-1, 1), 
                          breaks = c(-1,0,1),
                          labels = c(-1,0,1),
                          na.value = "grey",
                          name = "Correlation")+
    theme(panel.background = element_rect(fill = 'gray95'),
          axis.text.x = element_text(angle = 65,hjust = 1),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank())+
    scale_x_discrete(expand = c(0,0),
                     name = "Protein Indices" ,
                     limits = index.ord
                     )+
    scale_y_discrete(expand = c(0,0),
                     limits = rev(unique(sort(Corr.scores.Rval2$Dimension)))
                     )+
    coord_fixed(ratio = 1) +
    geom_text(aes(label = #round(value, 5)
                    value
    ))+
    theme(axis.text = element_text(size = 14),
          axis.title = element_text(size = 14,
                                    face = "bold"),
          legend.title = element_text(size = 16),
          legend.text = element_text(size = 14),
          plot.margin = margin(0, 0, 0, 0, "cm"))
  
}
