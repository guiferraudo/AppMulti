if (!require(vegan)) {install.packages("vegan")}
if (!require(FactoMineR)) {install.packages("FactoMineR")}
if (!require(cluster)) {install.packages("cluster")}
if (!require(DT)) {install.packages("DT")}
if (!require(RColorBrewer)) {install.packages("RColorBrewer")}
if (!require(psych)) {install.packages("psych")}
if (!require(GPArotation)) {install.packages("GPArotation")}
if (!require(ca)) {install.packages("ca")}
if (!require(MASS)) {install.packages("MASS")}
#if (!require(d3heatmap)) {install.packages("d3heatmap", dep = TRUE)}

require(vegan)
require(cluster)
require(DT)
require(RColorBrewer)
require(psych)
require(GPArotation)
require(ca)
require(MASS)
#require(d3heatmap)

#loading function to perform k-means and get results
source("kmeansClustering.R")
#loading function to perform PCA and get results
source("PCA.R")
#loading function which generate PCA label axis for plotting
source("generatingPCALabelAxis.R")
#loading function to perform EFA and get results
source("EFA.R")
#loading function to perform CA and get results
source("CA.R")

shinyServer(function(input, output, session) {
  
  # The selected file, if any
  userFile <- reactive({
    # If no file is selected, don't do anything
    validate(need(input$file, message = FALSE))
    input$file
  })
  
  # The user's data, parsed into a data frame
  dataframe <- reactive({
    read.csv(userFile()$datapath,
             header = input$header,
             sep =    input$sep,
             dec = input$decimal,
             quote = input$quote,
             row.names = input$rowNames, 
             encoding = input$encode)
  })
  
 scalingData = function(verif){
                                if (verif == "No")
                                  return(dataframe())
    
                                  return(round(scale(dataframe(),center = TRUE, scale = TRUE),dig=4))
  }
  
  #printing the dataset
  output$contents = renderDataTable({scalingData(input$scaleVar)})

  #Generate Compactly Display the Structure of an Arbitrary R Object
  output$struct = renderPrint({
                                df1 = scalingData(input$scaleVar)
                                str(df1)
                                }, width =12)
  #Generate a summary of the dataset
  output$summaryTable = renderPrint({
                                      df2 = scalingData(input$scaleVar)
                                      summary(df2)
                                      }, width =12)
  

  #Cluster analysis - Hierarchical clustering
  #Similarity measurements - Euclidean metric - 
  matDist <- reactive({
                        return(dist(scalingData(input$scaleVar), method = input$dist))
                        })

  #Generate a distance matrix
  output$distanceMatrix = renderDataTable({round(as.matrix(matDist()),dig=2)})
  
  #Generate cophenetic correlation
  output$cophen = renderPrint({
                                hc = hclust(matDist(), method = input$method)
                                return(round(cor(matDist(), cophenetic(hc)),dig=2))
  }, width = 12)
  
  #Generate a dendrogram image
  output$dendro <- renderPlot({
                                hc = hclust(matDist(), method = input$method)
                                par(mar=c(7,6,1,0.8))
                                plot(hc, xlab = "", sub = "",
                                        hang = -1, cex = 2, cex.lab = 2, cex.axis = 2, cex.main = 2, main = "")
                                
  }, height = 460, width = 980)
  
  #Generate download handler to export dendrogram
  output$down = downloadHandler(
                            #Specify file name
                            filename = function(){
                              #MyDendro.png
                              #MyDendro.pdf
                              paste("MyDendro", input$exportDendro, sep = ".")
                            },
                            content = function(file){
                              #open the device
                               # create the plot
                               # close the device
                                #png()
                                #pdf()
                            if (input$exportDendro == "png")
                                png(file)
                              else
                                pdf(file, width = input$widthDendro, height = input$heightDendro)
                              hc = hclust(matDist(), method = input$method)
                              par(mar=c(7,6,1,1))
                              plot(hc, 
                                   hang = -1, xlab = "", main = "", cex = 2, cex.lab = 2, cex.axis = 2, cex.main = 2)
                              dev.off()
                            }
)
  ##K-means
  dfKmeans <- reactive({
                        return(kmeansf(scalingData(input$scaleVar), input$nhclusters2))
  })
  
  #Generate original dataset by cluster 
  output$origDatakmeans = renderPrint({
                                       return(lapply(dfKmeans()$'originalDataByCluster',
                                                      function(y)round(y,dig=2)))
  }, width = 420)
  
  #Generate centroid by cluster 
  output$centroidkmeans = renderPrint({
                                       return(lapply(dfKmeans()$'centroids',
                                                      function(y)round(y,dig=2)))
  }, width = 420)
  
  #Generate euclidian distance from each cluster member to centroid cluster 
  output$distToCentroid = renderPrint({
                                       return(lapply(dfKmeans()$'distToCentroid',
                                                      function(y)round(y,dig=2)))
  }, width = 420)
  
  #Generate average by cluster 
  output$avgByCluster = renderPrint({
                                     return(round(dfKmeans()$'avgByCluster', dig = 2))
  }, width = 420)
  
  #Generate standard deviation by cluster 
  output$sdByCluster = renderPrint({
                                    return(round(dfKmeans()$'sdByCluster', dig = 2))
  }, width = 420)
  
  #Generate a profile plot of k-means
  output$profileK <- renderPlot({
                                  dfKmeansPlot = dfKmeans()$'dataToPlot'
                                  par(mar = c (9,9,7,6))
                                  interaction.plot(x.factor = dfKmeansPlot$'y', 
                                  trace.factor = dfKmeansPlot$'cluster', las = 2,
                                  response = dfKmeansPlot$'valores', fixed = TRUE, ann = FALSE,
                                  type = "l", xlab = "", ylab = "", xpd = FALSE,
                                  lty = rep(1,nlevels(dfKmeansPlot$'cluster')),
                                  lwd = rep(3,nlevels(dfKmeansPlot$'cluster')),
                                  col=brewer.pal(nlevels(dfKmeansPlot$'cluster'), input$kmeancolor),trace.label = "K-means clusters" 
                                  #ylim = c(floor(min(dfKmeansPlot$'valores')), ceiling(max(dfKmeansPlot$'valores')))
                                  )
                                  mtext(side = 2, text = "Values", line =4)
                                  abline(h=0, col="gray", lwd=4, lty=5)
    
  }, height = 460, width = 980)
  
  output$down2 = downloadHandler(
                                  #Specify file name
                                  filename = function(){
                                  #MyProfile_Kmeans.png
                                  #MyProfile_Kmeans.pdf
                                  paste("MyProfile_Kmeans", input$exportProfile, sep = ".")
                                                        },
                                  content = function(file){
                                  #open the device
                                  # create the plot
                                  # close the device
                                  #png()
                                  #pdf()
                                  if (input$exportProfile == "png")
                                  png(file)
                                     else
                                       pdf(file, width = input$widthProfile, height = input$heightProfile)
                                          dfKmeansPlot = dfKmeans()$'dataToPlot'
                                          par(mar = c (9,9,7,6), cex = 2, cex.lab = 2, cex.axis = 2, cex.main = 2)
                                          interaction.plot(x.factor = dfKmeansPlot$'y', 
                                                           trace.factor = dfKmeansPlot$cluster, las = 2,
                                                           response = dfKmeansPlot$valores, fixed = TRUE, ann = FALSE,
                                                           type = "l", xlab = "", ylab = "Values", xpd = FALSE,
                                                           lty = rep(1,nlevels(dfKmeansPlot$'cluster')),
                                                           lwd = rep(5,nlevels(dfKmeansPlot$'cluster')),
                                                           col=brewer.pal(nlevels(dfKmeansPlot$'cluster'), input$kmeancolor),trace.label = "K-means clusters" 
                                                           #ylim = c(floor(min(dfKmeansPlot$'valores')), ceiling(max(dfKmeansPlot$'valores')))
                                                           )
                                                            mtext(side = 2, text = "Values", line = 5, cex = 4.5)
                                                            abline(h=0, col="gray", lwd=4, lty=5)  
      
                                       dev.off()
    }
  )
  
  #PCA analysis
  #Generate percentage of variance explained by PC
  output$PCAexplainedVariance = renderDataTable({round(PCA(scalingData(input$scaleVar))$'eigenvaluePercentage',dig=2)})
  
  #PCA Generate screeplot and % explained variance
  output$PCAscreeplot <- renderPlot({
                                     #Screeplot (Variances X Principal Component)
                                     screeplot(PCA(scalingData(input$scaleVar))$'PCAfit', 
                                               type = "lines", pch =19, npcs = 20,
                                               main = "", ylim = c(0, 1.2 * max(PCA(scalingData(input$scaleVar))$'eigenvaluePercentage'[,1])))
                                               text(x = (1:nrow(PCA(scalingData(input$scaleVar))$'eigenvaluePercentage'))+0.02, 
                                                    y = PCA(scalingData(input$scaleVar))$'eigenvaluePercentage'[,1]+0.3, cex = 1.2, pos = 4,
                                               labels = as.character(paste(round(PCA(scalingData(input$scaleVar))$'eigenvaluePercentage'[,2], dig = 2), "%")))
                                               title("PCA Screeplot", cex = 4)
                                     #adding a horizontal line at variance = 1
                                     abline(h = 1, col = "red", lty = 2, lwd = 2)
                                      }, height = 460, width = 980)
  
  output$down3 = downloadHandler(
    #Specify file name
    filename = function(){
                          paste("MyScreePlot_PCA", input$exportPCAplots, sep = ".")
                          },
    content = function(file){
      #open the device
      # create the plot
      # close the device
      #png()
      #pdf()
      if (input$exportPCAplots == "png")
        png(file)
      else
        pdf(file, width = input$widthPCAplots, height = input$heightPCAplots)
              par(mar = c (6,8,6,6), cex.lab = 1.5, cex.axis = 1.5, cex.main = 1.5) 
                  screeplot(PCA(scalingData(input$scaleVar))$'PCAfit', 
                            type = "lines", pch =19, npcs = 20, las =2,
                            main = "", ylim = c(0, 1.2 * max(PCA(scalingData(input$scaleVar))$'eigenvaluePercentage'[,1])))
                            text(x = (1:nrow(PCA(scalingData(input$scaleVar))$'eigenvaluePercentage'))+0.02, 
                                 y = PCA(scalingData(input$scaleVar))$'eigenvaluePercentage'[,1]+0.3, cex = 1.2, pos = 4,
                                 labels = as.character(paste(round(PCA(scalingData(input$scaleVar))$'eigenvaluePercentage'[,2], dig = 2), "%")))
                            title("PCA Screeplot", cex = 4)
                  #adding a horizontal line at variance = 1
                  abline(h = 1, col = "red", lty = 2, lwd = 2)
      
      dev.off()
    }
  )
  
  #Generate PCA scores
  output$PCAscores = renderDataTable({round(PCA(scalingData(input$scaleVar))$'PCAscores',dig=2)})
  
  #Generate scores plot
  output$PCAscoresPlot <- renderPlot({
                                  # plotting the PCA scores
                                  plot(PCA(scalingData(input$scaleVar))$'PCAscores'[,input$FirstAxisOnPCA], 
                                       PCA(scalingData(input$scaleVar))$'PCAscores'[,input$SecondAxisOnPCA], 
                                       pch = 19, xlim = c(-6,6), ylim = c(-3,3), main = "PCA scores",
                                       xlab = generate.axis.label(scalingData(input$scaleVar), input$FirstAxisOnPCA,input$SecondAxisOnPCA)$'labelFirstAxis', 
                                       ylab = generate.axis.label(scalingData(input$scaleVar), input$FirstAxisOnPCA,input$SecondAxisOnPCA)$'labelSecondAxis')
                                  #adding label names 
                                  text(PCA(scalingData(input$scaleVar))$'PCAscores'[,input$FirstAxisOnPCA], 
                                       PCA(scalingData(input$scaleVar))$'PCAscores'[,input$SecondAxisOnPCA],
                                       cex = 1.5, pos = 4, 
                                       labels = rownames(PCA(scalingData(input$scaleVar))$'PCAscores'))
                                  #adding a horizontal line - blue
                                  abline(h = 0, col = "blue", lty = 2, lwd = 2)
                                  #adding a vertical line - blue
                                  abline(v = 0, col = "blue", lty = 2, lwd = 2)
                                   }, height = 460, width = 980)
  
  output$down4 = downloadHandler(
    #Specify file name
    filename = function(){
      paste("MyScoresPlot_PCA", input$exportPCAplots, sep = ".")
    },
    content = function(file){
      #open the device
      # create the plot
      # close the device
      #png()
      #pdf()
      if (input$exportPCAplots == "png")
        png(file)
      else
        pdf(file, width = input$widthPCAplots, height = input$heightPCAplots)
            par(mar = c (6,8,6,6), cex.lab = 1.5, cex.axis = 1.5, cex.main = 1.5) 
            # plotting the PCA scores
            plot(PCA(scalingData(input$scaleVar))$'PCAscores'[,input$FirstAxisOnPCA], 
                 PCA(scalingData(input$scaleVar))$'PCAscores'[,input$SecondAxisOnPCA], 
                 pch = 19, xlim = c(-6,6), ylim = c(-3,3), main = "PCA scores",
                 xlab = generate.axis.label(scalingData(input$scaleVar), input$FirstAxisOnPCA,input$SecondAxisOnPCA)$'labelFirstAxis', 
                 ylab = generate.axis.label(scalingData(input$scaleVar), input$FirstAxisOnPCA,input$SecondAxisOnPCA)$'labelSecondAxis')
            #adding label names 
            text(PCA(scalingData(input$scaleVar))$'PCAscores'[,input$FirstAxisOnPCA], 
                 PCA(scalingData(input$scaleVar))$'PCAscores'[,input$SecondAxisOnPCA],
                 cex = 1.5, pos = 4, 
                 labels = rownames(PCA(scalingData(input$scaleVar))$'PCAscores'))
            #adding a horizontal line - blue
            abline(h = 0, col = "blue", lty = 2, lwd = 2)
            #adding a vertical line - blue
            abline(v = 0, col = "blue", lty = 2, lwd = 2)
      
      dev.off()
    }
  )
  
  #Generate PCA loadings
  output$PCAloadings = renderDataTable({round(PCA(scalingData(input$scaleVar))$'PCAloadings',dig=2)})
  
  #Correlation among PCA loadings and dataset
  output$PCACorLoadAndDataset = renderDataTable({round(PCA(scalingData(input$scaleVar))$'CorLoadAndDataset',dig=2)})
  
  #Generate variable loading map 
  output$PCAvariableLoadMap <- renderPlot({
                                            plot(PCA(scalingData(input$scaleVar))$'CorLoadAndDataset'[,input$FirstAxisOnPCA],  
                                                 PCA(scalingData(input$scaleVar))$'CorLoadAndDataset'[,input$SecondAxisOnPCA],
                                                 xlab = generate.axis.label(scalingData(input$scaleVar),input$FirstAxisOnPCA,input$SecondAxisOnPCA)$'labelFirstAxis', 
                                                 ylab = generate.axis.label(scalingData(input$scaleVar),input$FirstAxisOnPCA,input$SecondAxisOnPCA)$'labelSecondAxis', 
                                                 type = "n", main = 'Variables loading map',
                                                 xlim = c(-1.1,1.1), ylim = c(-1.1,1.1))
                                            #adding label names 
                                            text(PCA(scalingData(input$scaleVar))$'CorLoadAndDataset'[,input$FirstAxisOnPCA], 
                                                 PCA(scalingData(input$scaleVar))$'CorLoadAndDataset'[,input$SecondAxisOnPCA],
                                                 cex = 1.5, pos = 3, labels = rownames(PCA(scalingData(input$scaleVar))$'CorLoadAndDataset'), col = "red")
                                            #adding a horizontal line - blue
                                            abline(h = 0, col = "blue", lty = 2, lwd = 2)
                                            #adding a vertical line - blue
                                            abline(v = 0, col = "blue", lty = 2, lwd = 2)
                                            #drawing the unitary circle
                                            x.circle <- seq(-1, 1, by = 0.001)
                                            y.circle <- sqrt(1 - x.circle^2)
                                            lines(x.circle, y = y.circle, lwd = 2)
                                            lines(x.circle, y = -y.circle, lwd = 2)
                                            #drawing arrows indicating the intensity and direction of variables
                                            arrows(x0 = 0, y0 = 0,
                                                   x1 = PCA(scalingData(input$scaleVar))$'CorLoadAndDataset'[,input$FirstAxisOnPCA], 
                                                   y1 = PCA(scalingData(input$scaleVar))$'CorLoadAndDataset'[,input$SecondAxisOnPCA],
                                                   angle = 15, code = 2, length = 0.2)
                                            #identifying the limits from -1 to 1 on X and Y axis
                                            abline(v = -1, col = "blue", lty = 2, lwd = 2)
                                            abline(v = 1, col = "blue", lty = 2, lwd = 2)
                                            abline(h = -1, col = "blue", lty = 2, lwd = 2)
                                            abline(h = 1, col = "blue", lty = 2, lwd = 2)
                                            }, height = 600, width = 600)
  
  output$down5 = downloadHandler(
    #Specify file name
    filename = function(){
      paste("MyVariableLoadingsMap_PCA", input$exportPCAplots, sep = ".")
    },
    content = function(file){
      #open the device
      # create the plot
      # close the device
      #png()
      #pdf()
      if (input$exportPCAplots == "png")
        png(file)
      else
        pdf(file, width = input$widthPCAplots, height = input$heightPCAplots)
      par(mar = c (6,8,6,6), cex.lab = 1.5, cex.axis = 1.5, cex.main = 1.5) 
      # plotting the PCA scores
      plot(PCA(scalingData(input$scaleVar))$'CorLoadAndDataset'[,input$FirstAxisOnPCA],  
           PCA(scalingData(input$scaleVar))$'CorLoadAndDataset'[,input$SecondAxisOnPCA],
           xlab = generate.axis.label(scalingData(input$scaleVar),input$FirstAxisOnPCA,input$SecondAxisOnPCA)$'labelFirstAxis', 
           ylab = generate.axis.label(scalingData(input$scaleVar),input$FirstAxisOnPCA,input$SecondAxisOnPCA)$'labelSecondAxis', 
           type = "n", main = 'Variables loading map',
           xlim = c(-1.1,1.1), ylim = c(-1.1,1.1))
      #adding label names 
      text(PCA(scalingData(input$scaleVar))$'CorLoadAndDataset'[,input$FirstAxisOnPCA], 
           PCA(scalingData(input$scaleVar))$'CorLoadAndDataset'[,input$SecondAxisOnPCA],
           cex = 1.5, pos = 3, labels = rownames(PCA(scalingData(input$scaleVar))$'CorLoadAndDataset'), col = "red")
      #adding a horizontal line - blue
      abline(h = 0, col = "blue", lty = 2, lwd = 2)
      #adding a vertical line - blue
      abline(v = 0, col = "blue", lty = 2, lwd = 2)
      #drawing the unitary circle
      x.circle <- seq(-1, 1, by = 0.001)
      y.circle <- sqrt(1 - x.circle^2)
      lines(x.circle, y = y.circle, lwd = 2)
      lines(x.circle, y = -y.circle, lwd = 2)
      #drawing arrows indicating the intensity and direction of variables
      arrows(x0 = 0, y0 = 0,
             x1 = PCA(scalingData(input$scaleVar))$'CorLoadAndDataset'[,input$FirstAxisOnPCA], 
             y1 = PCA(scalingData(input$scaleVar))$'CorLoadAndDataset'[,input$SecondAxisOnPCA],
             angle = 15, code = 2, length = 0.2)
      #identifying the limits from -1 to 1 on X and Y axis
      abline(v = -1, col = "blue", lty = 2, lwd = 2)
      abline(v = 1, col = "blue", lty = 2, lwd = 2)
      abline(h = -1, col = "blue", lty = 2, lwd = 2)
      abline(h = 1, col = "blue", lty = 2, lwd = 2)
      
      dev.off()
    }
  )
  
  #Generate biplot
  output$PCAbiplot <- renderPlot({
                                  # plotting the PCA biplot
                                  biplot(PCA(scalingData(input$scaleVar))$'PCAfit', 
                                         choices = c(input$FirstAxisOnPCA,input$SecondAxisOnPCA), main = "PCA biplot",
                                         scale = 1, pc.biplot = FALSE,
                                         xlab = generate.axis.label(scalingData(input$scaleVar), input$FirstAxisOnPCA,input$SecondAxisOnPCA)$'labelFirstAxis', 
                                         ylab = generate.axis.label(scalingData(input$scaleVar), input$FirstAxisOnPCA,input$SecondAxisOnPCA)$'labelSecondAxis')
                                  #adding a horizontal line - blue
                                  abline(h = 0, col = "blue", lty = 2, lwd = 2)
                                  #adding a vertical line - blue
                                  abline(v = 0, col = "blue", lty = 2, lwd = 2)
                                   }, height = 460, width = 980)
  
  output$down6 = downloadHandler(
    #Specify file name
    filename = function(){
      paste("Mybiplot_PCA", input$exportPCAplots, sep = ".")
    },
    content = function(file){
      #open the device
      # create the plot
      # close the device
      #png()
      #pdf()
      if (input$exportPCAplots == "png")
        png(file)
      else
        pdf(file, width = input$widthPCAplots, height = input$heightPCAplots)
      par(mar = c (6,8,6,6), cex.lab = 1.5, cex.axis = 1.5, cex.main = 1.5) 
      # plotting the PCA scores
      # plotting the PCA biplot
      biplot(PCA(scalingData(input$scaleVar))$'PCAfit', 
             choices = c(input$FirstAxisOnPCA,input$SecondAxisOnPCA), main = "PCA biplot",
             scale = 1, pc.biplot = FALSE,
             xlab = generate.axis.label(scalingData(input$scaleVar), input$FirstAxisOnPCA,input$SecondAxisOnPCA)$'labelFirstAxis', 
             ylab = generate.axis.label(scalingData(input$scaleVar), input$FirstAxisOnPCA,input$SecondAxisOnPCA)$'labelSecondAxis')
      #adding a horizontal line - blue
      abline(h = 0, col = "blue", lty = 2, lwd = 2)
      #adding a vertical line - blue
      abline(v = 0, col = "blue", lty = 2, lwd = 2)
      
      dev.off()
    }
  )
  #Exploratory Factor Analysis (EFA)
  
  #Generate EFA screeplot 
  output$EFAscreeplot <- renderPlot({
                                      #EFA Screeplot
                                      scree(scalingData(input$scaleVar),factor=FALSE,main = "EFA Screeplot")
                                      #adding a horizontal line at variance = 1
                                      abline(h = 1, col = "red", lty = 2, lwd = 2)
                                      }, height = 460, width = 980)
  
  output$down7 = downloadHandler(
                                  #Specify file name
                                  filename = function(){
                                                          paste("MyScreePlot_EFA", input$exportEFAplots, sep = ".")
                                                        },
                                  content = function(file){
                                  #open the device
                                  # create the plot 
                                  # close the device
                                  #png()
                                  #pdf()
                                  if (input$exportEFAplots == "png")
                                  png(file)
                                  else
                                  pdf(file, width = input$widthEFAplots, height = input$heightEFAplots)
                                    par(mar = c (6,8,6,6), cex.lab = 1.5, cex.axis = 1.5, cex.main = 1.5) 
                                    #EFA Screeplot
                                    scree(scalingData(input$scaleVar),factor=FALSE, main = "EFA Screeplot")
                                    #adding a horizontal line at variance = 1
                                    abline(h = 1, col = "red", lty = 2, lwd = 2)
                                  dev.off()
     }
  )
  
  #Generate EFA KMO output 
  output$KMO = renderPrint({
                            kmo = EFAfit(scalingData(input$scaleVar), 
                                         input$numberOfFactors, 
                                         input$rotateMethod)$'KMOresults'
                            return(kmo)
  }, width = 420)
  
  #Generate EFA p-value of Bartlett sphericity's test 
  output$pvalueCortestBartlett = renderPrint({
                                              pvalueBartlett = EFAfit(scalingData(input$scaleVar), 
                                                                      input$numberOfFactors, 
                                                                      input$rotateMethod)$'pvalorCortestBartlett'
                                              return(pvalueBartlett)
  }, width = 420)
  
  #Generate EFA p-value of Bartlett sphericity's test 
  output$factorsExtraction = renderPrint({
                                              factorsExtract = EFAfit(scalingData(input$scaleVar), 
                                                                      input$numberOfFactors, 
                                                                      input$rotateMethod)$'factorsExtraction'
                                              return(factorsExtract)
  }, width = 420)  
  
  #Generate EFA biplot 
  output$EFAbiplot <- renderPlot({
                                  biplot(EFAfit(scalingData(input$scaleVar), 
                                                input$numberOfFactors, 
                                                input$rotateMethod)$'factorsExtraction',
                                                choose = c(input$FirstAxisOnEFA, input$SecondAxisOnEFA))
                                      }, height = 460, width = 980)
  
  output$down8 = downloadHandler(
                                 #Specify file name
                                 filename = function(){
                                 paste("MyBiPlot_EFA", input$exportEFAplots, sep = ".")
                                                       },
                                 content = function(file){
                                 #open the device
                                 # create the plot 
                                 # close the device
                                 #png()
                                 #pdf()
                                 if (input$exportEFAplots == "png")
                                  png(file)
                                 else
                                 pdf(file, width = input$widthEFAplots, height = input$heightEFAplots)
                                  par(mar = c (6,8,6,6), cex.lab = 1.5, cex.axis = 1.5, cex.main = 1.5) 
                                   biplot(EFAfit(scalingData(input$scaleVar), 
                                                 input$numberOfFactors, 
                                                 input$rotateMethod)$'factorsExtraction',
                                                 choose = c(input$FirstAxisOnEFA, input$SecondAxisOnEFA))
      dev.off()
    }
  )
  
  #Generate EFA loadings variables diagram 
  output$EFAloadvardiagram <- renderPlot({
                                          fa.diagram(EFAfit(scalingData(input$scaleVar), 
                                                            input$numberOfFactors, 
                                                            input$rotateMethod)$'factorsExtraction', 
                                                     cut = input$cutFAdiagram, 
                                                     digits = 2, adj = 0.5,cex = 2, 
                                                     marg=c(2,2,2,2),g=FALSE,side=4,rsize = 0.5,e.size = 0.09)
  }, height = 460, width = 980)
  
  output$down9 = downloadHandler(
                                 #Specify file name
                                 filename = function(){
                                 paste("MyLoadVarDiagram_EFA", input$exportEFAplots, sep = ".")
                                                       },
                                 content = function(file){
                                 #open the device
                                 # create the plot 
                                 # close the device
                                 #png()
                                 #pdf()
                                 if (input$exportEFAplots == "png")
                                  png(file)
                                 else
                                 pdf(file, width = input$widthEFAplots, height = input$heightEFAplots)
                                  par(mar = c (6,8,6,6), cex.lab = 1.5, cex.axis = 1.5, cex.main = 1.5) 
                                  fa.diagram(EFAfit(scalingData(input$scaleVar), 
                                                                input$numberOfFactors, 
                                                                input$rotateMethod)$'factorsExtraction', 
                                             cut = input$cutFAdiagram, 
                                             digits = 2, adj = 0.5,cex = 2, 
                                             marg=c(2,2,2,2),g=FALSE,side=4,rsize = 0.5,e.size = 0.09)
                                 dev.off()
    }
  )
  
  #Correspondence analysis
  CAfunc <- reactive({
                      return(CAfit(scalingData(input$scaleVar), input$FirstAxisOnCA, input$SecondAxisOnCA))
  })
  
  #Generate Burt's table
  output$CABurtTable = renderDataTable({
                                        return(CAfunc()$BurtTable)
  })
  
  # Chi-square test output
  output$CAchisqTable = renderPrint({
                                     return(CAfunc()$ChisqBurtTable)
  }, width = 420)  
  
  # Expected counts under the null
  output$CAchisqTableExp = renderPrint({
                                        return(round(CAfunc()$ChisqBurtTableExp, dig = 2))
  }, width = 420)  
  
  # Pearson residuals
  output$CAchisqTableResid = renderPrint({
                                          return(round(CAfunc()$ChisqBurtTableResid, dig = 2))
  }, width = 420) 
  
  #Mass and Inertia
  output$CAMassInertia = renderDataTable({
                                        return(round(CAfunc()$MassInertia, dig = 5))
  })
  
  #Inertia eigenvalues
  output$CAInertiaEigen = renderDataTable({
                                           return(CAfunc()$InertiaEigen)
  })
  
  #Perceptual map - correspondences
  output$CAPercepMap <- renderPlot({
                                    par(cex = 2)
                                    plot(CAfunc()$CAFitted, dim = c(input$FirstAxisOnCA, input$SecondAxisOnCA),
                                         arrows = c("FALSE","FALSE"), what = c("none","all"), 
                                         col = "black", xlim = c(-2,2))
                                    title(main = "Correspondence Analysis")
  }, height = 600, width = 980)
  
  output$down10 = downloadHandler(
    #Specify file name
    filename = function(){
      paste("MyPerceptualMap_CA", input$exportCAplots, sep = ".")
    },
    content = function(file){
      #open the device
      # create the plot 
      # close the device
      #png()
      #pdf()
      if (input$exportCAplots == "png")
        png(file)
      else
        pdf(file, width = input$widthCAplots, height = input$heightCAplots)
         par(cex = 2)
         plot(CAfunc()$CAFitted, dim = c(input$FirstAxisOnCA, input$SecondAxisOnCA),
              arrows = c("FALSE","FALSE"), what = c("none","all"), 
              col = "black", xlim = c(-2,2))
         title(main = "Correspondence Analysis")
      dev.off()
    }
  )
  
  
  })

