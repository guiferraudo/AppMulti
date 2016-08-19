#carregando o pacote
require(MASS)

#tornar MANOVA e LDA genericas

#MANOVA
lda_manova = manova(as.matrix(iris[,1:4]) ~ iris$"Species")
summary(lda_manova, test = "Wilks")  
E = lda_manova$res #residuals matrix

# MANOVA: H0: there is no difference among groups
#  vs
#  H1: there is difference among groups (at least one)
# R default is "Pillai"
summary(lda_manova, test = "Wilks")    

##
B = summary(lda_manova)$SS$"iris$Species"; B
# matriz B (SQPCHip ou SQPCClusters)
W = summary(lda_manova)$SS$"Res"; W
# matriz W (SQPCRes)

SQPCTotal = B + W

# Caption
# SQPCHip: Sum of squares and cross product of hypothesis
# SQPTrat: Sum of squares and cross product among groups
# SQPCRes: Sum of squares and cross product of Residual (within clusters)
# SQPCTotal: Sum of squares and cross product Total

# Comparing group 1 versus o group 2
lda_manova_1vs2 = manova(as.matrix(iris[,1:4]) ~
                         iris$Species, subset = iris$Species %in% c("versicolor","virginica"))

summary(lda_manova_1vs2, test = "Wilks")   

# ANOVA para todas as características individualmente.
summary.aov(lda_manova)   

#### End of Manova ###

###calculando a funcao linear discriminante na amostra treinamento
results_lda <- lda(Species ~ ., data = iris, prior = c(1,1,1)/3)
###imprimindo os resultados
results_lda

predictions_lda = predict(results_lda, iris)
classif_lda = as.character(predictions_lda$class)
lda_pred_final = data.frame(predictions_lda$x, classif_lda)
###Matriz de confusão - Tabela com os erros de predicoes por classes
predict.table = table(iris$"Species", predictions_lda$class)
# porcentagem correta para cada especie
diag(prop.table(predict.table, 1))
#apresentando a tabela final
predict.table.final = cbind(predict.table,100*diag(prop.table(predict.table, 1)))
colnames(predict.table.final)[4] = "% de acertos"
predict.table.final

ty.lda <- function(x, groups){
  
  
  x.lda <- lda(groups ~ ., as.data.frame(x))
  
  gr <- length(unique(groups))   ## groups might be factors or numeric
  v <- ncol(x) ## variables
  m <- x.lda$means ## group means
  
  w <- array(NA, dim = c(v, v, gr))
  
  for(i in 1:gr){
    tmp <- scale(subset(x, groups == unique(groups)[i]), scale = FALSE)
    w[,,i] <- t(tmp) %*% tmp
  }
  
  W <- w[,,1]
  for(i in 2:gr)
    W <- W + w[,,i]
  
  V <- W/(nrow(x) - gr)
  iV <- solve(V)
  
  class.funs <- matrix(NA, nrow = v + 1, ncol = gr)
  colnames(class.funs) <- levels(groups)
  rownames(class.funs) <- c("constant", colnames(x))
  
  for(i in 1:gr) {
    class.funs[1, i] <- -0.5 * t(m[i,]) %*% iV %*% (m[i,])
    class.funs[2:(v+1) ,i] <- iV %*% (m[i,])
  }
  
  x.lda$class.funs <- class.funs
  
  return(x.lda$class.funs)
}

ty.lda(x = iris[,1:4], groups = iris$"Species")

#results
summary(lda_manova, test = "Wilks")  
#classification matrix
predict.table.final
#discriminant function
ty.lda(x = iris[,1:4], groups = iris$"Species")

#bidimensional plot (root 1 and root 2)
plot(lda_pred_final[,1:2], main = "Iris data", type = "n")
points(lda_pred_final[lda_pred_final$"classif_lda" == "setosa",1],
       lda_pred_final[lda_pred_final$"classif_lda" == "setosa",2], pch = 19)
points(lda_pred_final[lda_pred_final$"classif_lda" == "versicolor",1],
       lda_pred_final[lda_pred_final$"classif_lda" == "versicolor",2], pch = 3)
points(lda_pred_final[lda_pred_final$"classif_lda" == "virginica",1],
       lda_pred_final[lda_pred_final$"classif_lda" == "virginica",2], pch = 24)
legend("top", legend = c("setosa","versicolor","virginica"), pch = c(19,3,24), horiz = TRUE)
abline(h=0,lty=2,lwd=3,col="darkblue")
abline(v=0,lty=2,lwd=3,col="darkblue")





