# Correspondence Analyses (CA)
CAfit = function(dataset,firstAxis,secondAxis){
  # Burt table estimate
  data.to.burt = as.matrix(dataset)
  table.burt = t(data.to.burt) %*% data.to.burt
  # Chi-square test on Burt table
  chi.square.burt = chisq.test(table.burt, simulate.p.value = TRUE)
  # Expected: expected counting under null hypothesis
  chi.square.burt$expected
  # Pearson Residuals: (observed - expected) / sqrt(expected)
  chi.square.burt$residuals
  
  #Correspondence analysis fit
  caFit = ca(dataset)
  # Mass and Inertia by dimension (only 2 first dimensions)
  # inertia estimate from 2 first dimensions
  inertia.dim = cbind(caFit$colmass*(caFit$colcoord[,firstAxis]^2),
                      caFit$colmass*(caFit$colcoord[,secondAxis]^2))
  mass.inertia.by.col = data.frame(caFit$colmass, inertia.dim)
  colnames(mass.inertia.by.col) = c("Mass", paste("Inertia_Dim", c(firstAxis,secondAxis), sep = ""))
  #Inertia
  principal.inertias.eigenvalues = data.frame(SingularValue = round(caFit$sv, dig = 6),
                                              EigenValue = round(caFit$sv^2, dig = 6), 
                                              Percentage_of_Inertia = paste(round(100*((caFit$sv^2)/sum(caFit$sv^2)), dig = 2), "%", sep = ""),
                                              Cum_Perc_Inertia = paste(cumsum(round(100*((caFit$sv^2)/sum(caFit$sv^2)), dig = 2)), "%", sep = ""))
  
  
  # printing main CA outputs
  return(list(
    # Burt table
    BurtTable = table.burt,
    # Chi-square test on Burt table
    ChisqBurtTable = chi.square.burt,
    # Expected: expected counting under null hypothesis
    ChisqBurtTableExp = chi.square.burt$expected,
    # Pearson Residuals: (observed - expected) / sqrt(expected)
    ChisqBurtTableResid = chi.square.burt$residuals,
    #CA fitting
    CAFitted = caFit,
    #Mass and Inertia
    MassInertia = mass.inertia.by.col,
    #Inertia eigenvalues
    InertiaEigen = principal.inertias.eigenvalues
  )
  )
  
}

###   End of CA ###
