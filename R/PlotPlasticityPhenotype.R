##### == Creating plasticity phenotype == #####

plasticity_phenotype <- function(means = means,
                                 maxes = maxes,
                                 mins = mins,
                                 grouping_mean_column = grouping_mean_column, # 1
                                 first_mean_sums_column = first_mean_sums_column, # 2 
                                 last_mean_sums_column = last_mean_sums_column, # 4
                                 first_mean_indices_column =  first_mean_indices_column, # 5 
                                 first_max_sums_column = first_max_sums_column,
                                 last_max_sums_column = last_max_sums_column, # 3
                                 grouping_var = grouping_var){
  
  
  while (T) {
    
    library(tidyverse)
    
    feat.cols <- list()
    phenotype.cols <- data.frame(matrix(ncol = length(means[,grouping_mean_column]),
                                        nrow = ncol(means[,2:ncol(means)])))
    
    rownames(phenotype.cols) <- colnames(means[,
                                               2:ncol(means)])
    
    colnames(phenotype.cols) <- means[,grouping_mean_column]
    
    for (i in 1:length(colnames(means[,
                                      first_mean_indices_column:ncol(means)]))) {
      
      feat.min<-mins[last_max_sums_column+i] 
      feat.max<-maxes[last_max_sums_column+i]
      #feat.range<-(abs(feat.min)+feat.max)/2 
      feat.range<-(feat.max - feat.min)/2 
      feat.mid<- feat.max-feat.range 
      feat.col<-scale_colour_gradient2(
        low="red",
        mid="yellow",
        high="green",
        midpoint=feat.mid,
        breaks=c(feat.min,
                 feat.mid,
                 feat.max),
        labels=c("Below Normal",
                 "Normal",
                 "Above Normal"),
        limits=c(feat.min,feat.max)) 
      
      #feat.cols[[i]] <- feat.col$map(means[,i+4])
      phenotype.cols[last_max_sums_column+i,] <- feat.col$map(
        means[,i + 
                last_mean_sums_column])
      
    }
    
    #print(phenotype.cols)
    break
    
  }
  
  ## problem is somewhere here
  
 # maxes <- max(means[,
 #                     first_mean_sums_column:last_mean_sums_column])
  
  #mins <- min(means[,
  #                  first_mean_sums_column:last_mean_sums_column])
  
  
  maxes <- maxes[first_max_sums_column:last_max_sums_column]
  
  mins <- mins[first_max_sums_column:last_max_sums_column]
  
  
  
  while (T) {
    
    
    for (i in 1:length(colnames(means[,first_mean_sums_column:last_mean_sums_column]))) {
      
      feat.min<-mins[i]
      feat.max<-maxes[i]
      #feat.range<-(abs(feat.min)+feat.max)/2 
      feat.range<-(feat.max - feat.min)/2 
      feat.mid<- feat.max-feat.range 
      feat.col<-scale_colour_gradient2(
        low ="#d3d3d3",
        mid ="grey",
        high="black",
        midpoint=feat.mid,
        breaks=c(feat.min,
                 feat.mid,
                 feat.max),
        labels=c("Below Normal",
                 "Normal",
                 "Above Normal"),
        limits=c(feat.min,
                 feat.max)) 
      
      #feat.cols[[i]] <- feat.col$map(means[,i+4])
      phenotype.cols[i,] <- feat.col$map(means[,
                                               i+grouping_mean_column])
      
    }
    
    print(phenotype.cols)
    break
    
  }
  
  
  phenotype.cols$index <- row.names(phenotype.cols)
  
  long.phenotype.cols <- gather(phenotype.cols,
                                key = agebin, 
                                value = color,
                                -index )
  
  long.phenotype.cols$agebin <- factor(long.phenotype.cols$agebin,levels = means$Group.1)
  
  
  ggplot(long.phenotype.cols,
         aes(x = agebin ,
             y = index)) +
    geom_tile(data = long.phenotype.cols,
              width=0.95,
              height=0.95,
              aes(fill = color))+ 
    scale_fill_identity()+
    theme(panel.background = element_rect(fill = 'white'),
          axis.text.x = element_text(angle=45,
                                     vjust=0.5,
                                     size=11),
          axis.text.y = element_text(angle=0,
                                     vjust=0.5,
                                     size=11),
          axis.ticks.x = element_blank(),
          axis.ticks.y = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.title.x = element_text(size=24), 
          axis.title.y = element_text(size=24)
    )+
    scale_x_discrete(expand=c(0,0),
                     name=grouping_var,
                     labels=factor(means[,grouping_mean_column]),
                     position="bottom")+
    scale_y_discrete(expand=c(0,0),
                     name="Plasticity Features",
                     #labels= index2,
                     limits= rev(colnames(means)[2:ncol(means)]),
                     position="left")+
    coord_fixed(ratio=0.25)
  
}

