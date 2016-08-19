generate.axis.label = function(dataset,firstAxis,secondAxis){
  label.first = as.character(paste(colnames(PCA(dataset)$'PCAloadings')[firstAxis], ": ",
                                   round(PCA(dataset)$'eigenvaluePercentage'[firstAxis,2], dig = 2), 
                                   "%", 
                                   sep = ""))
  label.second = as.character(paste(colnames(PCA(dataset)$'PCAloadings')[secondAxis], ": ",
                                    round(PCA(dataset)$'eigenvaluePercentage'[secondAxis,2], dig = 2), 
                                    "%", 
                                    sep = ""))
  return(list(labelFirstAxis = label.first,
              labelSecondAxis = label.second))
  
}
