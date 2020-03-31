##### == Plotting basis vectors == #####

amplitude_plots <- function(cum.var,pca.var.coord){
  
  library(FactoMineR)  
  library(factoextra)
  library(data.table)
  
  cum.var = cum.var
  
  pca.var.coord = pca.var.coord
  
  while (T) {
    
    for (i in 1:cum.var){
      
      name <- paste0("Amplitude (Basis Vector ",i,")")
      
      VarCoordDim1<-data.frame(pca.var.coord[,i])
      
      setDT(VarCoordDim1, keep.rownames = TRUE)[]
      
      #print(c(VarCoordDim1, name))
      
      xyz <- ggplot(data=VarCoordDim1,
                    aes(rn,pca.var.coord...i.))+
        geom_col(colour="black")+
        scale_y_continuous(expand =c(0,0),name=name)+
        scale_x_discrete(limits=VarCoordDim1$rn)+
        theme(axis.line.y=element_line(),
              axis.line.x=element_line(),
              panel.grid=element_blank(),
              axis.text.x = element_text(angle=45,hjust =1,size=12),
              axis.text.y = element_text(angle=0,vjust=0.5,size=12),
              axis.title.x = element_text(size=14,face="bold"),
              axis.title.y = element_text(size=14,face="bold"),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank())+
        xlab("Proteins")
      
      print(xyz)
      
    }
    
    break
    
  }
  
}
