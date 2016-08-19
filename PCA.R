###Start function here
###Start function here
PCA = function(dataset){
  #Principal component analysis
  pca_fit = prcomp(dataset, center = FALSE, scale. = FALSE)
  # eigenvalues root square from standardized data covariance matrix 
  pca_autovalor = pca_fit$sdev^2
  #eigenvalues and the percentage of variance retained by each eigenvalue and its cumulative
  autoval_porcent_explica = data.frame(pca_autovalor,
                                       100*pca_autovalor/sum(pca_autovalor),
                                       cumsum(pca_autovalor),
                                       100*cumsum(pca_autovalor/sum(pca_autovalor)))
  # putting column names
  colnames(autoval_porcent_explica) = c("Eigenvalues","% Total","Eigenvalues - Cumulative","% Total Cumulative")
  # values rounding - 4 decimal places
  autoval_porcent_explica = round(autoval_porcent_explica, dig = 4)
  # principal component scores
  pca_scores = pca_fit$x
  # eigenvectors or loadings from correlation or covariance matrix
  pca_load = pca_fit$rotation
  # correlation matrix among loadings and the dataset variables (features)
  cor_load_and_dataset = round(cor(dataset, pca_scores), dig = 2)
  
  return(list(
    PCAfit = pca_fit,
    eigenvaluePercentage = autoval_porcent_explica,
    PCAscores = pca_scores,
    PCAloadings = pca_load,
    CorLoadAndDataset = cor_load_and_dataset
  )
  )
  
}
