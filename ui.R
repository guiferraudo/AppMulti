if (!require(vegan)) {install.packages("vegan")}
if (!require(FactoMineR)) {install.packages("FactoMineR")}
if (!require(cluster)) {install.packages("cluster")}
if (!require(DT)) {install.packages("DT")}
if (!require(RColorBrewer)) {install.packages("RColorBrewer")}
if (!require(psych)) {install.packages("psych")}
if (!require(GPArotation)) {install.packages("GPArotation")}
if (!require(ca)) {install.packages("ca")}
if (!require(MASS)) {install.packages("MASS")}
#if (!require(d3heatmap)) {install.packages("d3heatmap")}

require(vegan)
require(cluster)
require(DT)
require(RColorBrewer)
require(psych)
require(GPArotation)
require(ca)
require(MASS)
#require(d3heatmap)

shinyUI(
  navbarPage("", inverse = TRUE,
             tabPanel("Applied Multivariate analysis"),
             tabPanel("Data Importing",
                      mainPanel(tabsetPanel(type = "tabs",
                          #Page showing data importing table                     
                            tabPanel("Data table", 
                                                  sidebarLayout(
                                                                sidebarPanel(width = 4,
                                                                fileInput('file', 'Choose CSV File',
                                                                accept=c('text/csv', 
                                                                         'text/comma-separated-values,text/plain', 
                                                                         '.csv')),
                                                                tags$hr(),
                                                                checkboxInput('header', 'Header', TRUE),
                                                                radioButtons('sep', 'Separator',
                                                                             c(Comma=',',
                                                                             Semicolon=';',
                                                                             Tab='\t'),
                                                                             ','),
                                                                radioButtons('decimal', 'Decimal places',
                                                                             c(Comma = ',',
                                                                               Point = '.'),
                                                                             "."),
                                                                radioButtons('quote', 'Quote',
                                                                             c(None='',
                                                                             'Double Quote'='"',
                                                                             'Single Quote'="'"),
                                                                             '"'), 
                                                                radioButtons('encode', 'Encoding',
                                                                             c("latin1","UTF-8"),
                                                                             'latin1'),
                                                                radioButtons("scaleVar", label = "Would you like to standardize the variables?",
                                                                             choices = c("Yes","No"),
                                                                             selected = "No"),
                                                               numericInput("rowNames", "Column namber of the table which contains the row names:", 1)
                                                                                ), # end of sidebarPanel
                                                  mainPanel(dataTableOutput('contents')
                                                            ) # end of mainPanel
                                               ) # end of sidebarLayout
                                             ), # end of tabPanel
                                              tabPanel("Summary statistics", 
                                                        mainPanel(
                                                                  h4("R Object Structure Display"),
                                                                  verbatimTextOutput('struct'),
                                                                  h4("Summary stats"),
                                                                  verbatimTextOutput('summaryTable')
                                                                    ) #end of mainPanel
                                                        ) #end of tabPanel
                                            ) # end of tabsetPanel
                              ) # end of mainPanel
                      ), # end of tabPanel 
                        tabPanel("Cluster Analysis",
                                 mainPanel(tabsetPanel(type = "tabs",
                                            tabPanel("Hierarchical clustering", 
                                                     sidebarPanel(width = 3,
                                                                  #Distance measurements
                                                                  selectInput("dist", "Distance measurements:",
                                                                              choices = c("euclidean" = "euclidean",
                                                                                          "maximum" = "maximum",
                                                                                          "manhattan" = "manhattan",
                                                                                          "canberra" = "canberra",
                                                                                          "binary" = "binary",
                                                                                          "minkowski" = "minkowski"), selected = "euclidean"),
                                                                  selectInput("method", "Linkage methods:",
                                                                              choices = c("Ward" = "ward.D2",
                                                                                          "Single" = "single",
                                                                                          "Complete" = "complete",
                                                                                          "UPGMA" = "average",
                                                                                          "WPGMA" = "mcquitty",
                                                                                          "WPGMC" = "median",
                                                                                          "UPGMC" = "centroid"), selected = "ward.D2"),
                                                                  radioButtons("exportDendro", "Select the file type to dendrogram exporting:", 
                                                                               choices = list("png", "pdf")),
                                                                  numericInput("widthDendro", "Width (inches) for PDF file:", value = 24,min = 2, max = 100),
                                                                  numericInput("heightDendro", "Height (inches) for PDF file:", value = 16,min = 2, max = 100)
                                                                    ), #end of sidebarPanel
                                                     mainPanel(tabsetPanel(type = "tabs",
                                                                            tabPanel("Distance matrix observed",
                                                                                     dataTableOutput('distanceMatrix'),
                                                                                     h3("Cophenetic correlation of cluster analysis"),
                                                                                     verbatimTextOutput('cophen')
                                                                                    ), #end of tabPanel
                                                                           tabPanel("Dendrogram",
                                                                                    mainPanel(
                                                                                    plotOutput('dendro', width = "80%"),
                                                                                    h3("Download dendrogram"),
                                                                                    downloadButton("down", "Download")
                                                                                              ) #end od mainPanel
                                                                                    ) #end of tabPanel
                                                                          ) #end of tabsetPanel
                                                                ) #end of mainPanel
                                                   ), #end of tabPanel
                                            tabPanel("K-means", 
                                                     sidebarPanel(width = 3,
                                                                  #Distance measurements
                                                                  #Diverging palettes: RdBu, RdYlBu, RdYlGn, Spectral
                                                                  selectInput("kmeancolor", "Define color palette to k-means profile plot:",
                                                                              choices = c("RdBu" = "RdBu",
                                                                                          "RdYlBu" = "RdYlBu",
                                                                                          "RdYlGn" = "RdYlGn",
                                                                                          "Spectral" = "Spectral"
                                                                              ), selected = "Spectral"),
                                                                  numericInput("nhclusters2", "Number of clusters:", value = 3,min = 2, max = 100),
                                                                  radioButtons("exportProfile", "Select the file type to profile plot exporting:", 
                                                                               choices = list("png", "pdf")),
                                                                  numericInput("widthProfile", "Width (inches) for PDF file:", value = 24,min = 2, max = 100),
                                                                  numericInput("heightProfile", "Height (inches) for PDF file:", value = 16,min = 2, max = 100)
                                                     ), #end of sidebarPanel
                                                     mainPanel(tabsetPanel(type = "tabs",
                                                                           tabPanel("Summary output",
                                                                                        h3("Original data by cluster"),
                                                                                        verbatimTextOutput('origDatakmeans'),
                                                                                        h3("Centroid by cluster"),
                                                                                        verbatimTextOutput('centroidkmeans'),
                                                                                        h3("Euclidian distance from each cluster member to centroid cluster"),
                                                                                        verbatimTextOutput('distToCentroid'),
                                                                                        h3("Average by cluster"),
                                                                                        verbatimTextOutput('avgByCluster'),
                                                                                        h3("Standard deviation by cluster"),
                                                                                        verbatimTextOutput('sdByCluster')
                                                                                    ), #end of tabPanel
                                                                           tabPanel("Profile plot by cluster",
                                                                                    mainPanel(
                                                                                              plotOutput('profileK', width = "80%"),
                                                                                              h3("Download profile plot"),
                                                                                              downloadButton("down2", "Download")
                                                                                              ) #end od mainPanel
                                                                                      ) #end of tabPanel
                                                                            ) #end of tabsetPanel
                                                                ) #end of mainPanel
                                                    ) #end of tabPanel
                                       ) #end of tabsetPanel
                                  ) #end of mainPanel
             ), #end of tabPanel,
             tabPanel("Principal Component Analysis (PCA)",
                      sidebarPanel(width = 3,
                                             radioButtons("exportPCAplots", "Select the file type to PCA plots exporting:", 
                                                          choices = list("png", "pdf")),
                                             numericInput("widthPCAplots", "Width (inches) for PDF file:", value = 24,min = 2, max = 100),
                                             numericInput("heightPCAplots", "Height (inches) for PDF file:", value = 16,min = 2, max = 100),
                                             numericInput("FirstAxisOnPCA", "Choose the first axis on PCA:", value = 1, min = 1, max = 100),
                                             numericInput("SecondAxisOnPCA", "Choose the second axis on PCA:", value = 2, min = 1, max = 100)
                                   ), #end of sidebarPanel
                      mainPanel(tabsetPanel(type = "tabs",
                                  tabPanel("% of variance explained by PC",
                                           mainPanel(dataTableOutput('PCAexplainedVariance'),
                                                     plotOutput('PCAscreeplot', width = "80%", inline = TRUE),
                                                     h3("Download PCA screeplot"),
                                                     downloadButton("down3", "Download")
                                                     ) # end of mainPanel
                                           ), # end of tabPanel          
                                  tabPanel("PCA scores",
                                           mainPanel(dataTableOutput('PCAscores'),
                                                     plotOutput('PCAscoresPlot', width = "80%", inline = TRUE),
                                                     h3("Download PCA scores plot"),
                                                     downloadButton("down4", "Download")
                                                     ) # end of mainPanel
                                            ), # end of tabPanel     
                                  tabPanel("PCA loadings",
                                           mainPanel(dataTableOutput('PCAloadings')
                                                     ) # end of mainPanel
                                           ), # end of tabPanel 
                                  tabPanel("Correlation among PCA loadings and dataset",
                                           mainPanel(dataTableOutput('PCACorLoadAndDataset'),
                                                     plotOutput('PCAvariableLoadMap', width = "80%", inline = TRUE),
                                                     h3("Download PCA variable loadings map"),
                                                     downloadButton("down5", "Download")
                                                     ) # end of mainPanel
                                           ), # end of tabPanel 
                                  tabPanel("PCA biplot",
                                           mainPanel(plotOutput('PCAbiplot', width = "80%", inline = TRUE),
                                                     h3("Download PCA biplot"),
                                                     downloadButton("down6", "Download")
                                                     ) # end of mainPanel
                                           ) # end of tabPanel 
                                            ) # end of tabsetPanel
                                 ) # end of mainPanel
                       ),
             tabPanel("Exploratory Factor Analysis (EFA)",
                      sidebarPanel(width = 3,
                                   radioButtons("exportEFAplots", "Select the file type to EFA plots exporting:", 
                                                choices = list("png", "pdf")),
                                   numericInput("widthEFAplots", "Width (inches) for PDF file:", value = 24,min = 2, max = 100),
                                   numericInput("heightEFAplots", "Height (inches) for PDF file:", value = 16,min = 2, max = 100),
                                   numericInput("numberOfFactors", "Select the number of factors:", value = 2,min = 2, max = 100),
                                   numericInput("FirstAxisOnEFA", "Choose the first axis on EFA:", value = 1, min = 1, max = 100),
                                   numericInput("SecondAxisOnEFA", "Choose the second axis on EFA:", value = 2, min = 1, max = 100),
                                   selectInput("rotateMethod", "Rotation methods:",
                                               choices = c("none" = "none",
                                                           "varimax" = "varimax",
                                                           "quatimax" = "quatimax",
                                                           "promax" = "promax",
                                                           "oblimin" = "oblimin",
                                                           "simplimax" = "simplimax",
                                                           "cluster" = "cluster"), selected = "varimax"),
                                   numericInput("cutFAdiagram", "Select the loadings threshold for EFA diagram (0 to 1):", value = 0.6,min = 0, max = 1)
                                    ), #end of sidebarPanel
                      mainPanel(tabsetPanel(type = "tabs",
                                            tabPanel("EFA screeplot",
                                                     mainPanel(plotOutput('EFAscreeplot', width = "80%", inline = TRUE),
                                                               h3("Download EFA screeplot"),
                                                               downloadButton("down7", "Download")
                                                                ) # end of mainPanel
                                                      ), # end of tabPanel
                                            tabPanel("Summary output",
                                                     h3("KMO - MSA(Measure of Sampling Adequacy)"),
                                                     verbatimTextOutput('KMO'),
                                                     h3("P-value of Bartlett sphericity's test"),
                                                     verbatimTextOutput('pvalueCortestBartlett'),
                                                     h3("Factors extraction"),
                                                     verbatimTextOutput('factorsExtraction')
                                                      ), #end of tabPanel
                                            tabPanel("EFA biplot",
                                                     mainPanel(plotOutput('EFAbiplot', width = "80%", inline = TRUE),
                                                               h3("Download EFA biplot"),
                                                               downloadButton("down8", "Download")
                                                               ) # end of mainPanel
                                                      ), # end of tabPanel
                                            tabPanel("EFA loading variables diagram",
                                                     mainPanel(plotOutput('EFAloadvardiagram', width = "80%", inline = TRUE),
                                                               h3("Download EFA loading variables diagram"),
                                                               downloadButton("down9", "Download")
                                                                ) # end of mainPanel
                                                      ) # end of tabPanel
                                              ) # end of tabsetPanel
                                  ) # end of mainPanel
                        ), # end of tabPanel
             tabPanel("Correspondence Analysis (CA)",
                      sidebarPanel(width = 3,
                                   radioButtons("exportCAplots", "Select the file type to CA plots exporting:", 
                                                choices = list("png", "pdf")),
                                   numericInput("widthCAplots", "Width (inches) for PDF file:", value = 24,min = 2, max = 100),
                                   numericInput("heightCAplots", "Height (inches) for PDF file:", value = 16,min = 2, max = 100),
                                   numericInput("FirstAxisOnCA", "Choose the first axis on CA:", value = 1, min = 1, max = 100),
                                   numericInput("SecondAxisOnCA", "Choose the second axis on CA:", value = 2, min = 1, max = 100)
                                   ), #end of sidebarPanel
                      mainPanel(tabsetPanel(type = "tabs",
                                            tabPanel("CA Burt's table",
                                                     mainPanel(dataTableOutput("CABurtTable")
                                                               ) # end of mainPanel
                                            ), # end of tabPanel
                                            tabPanel("Chi-square test",
                                                     h3("Chi-square test output"),
                                                     verbatimTextOutput('CAchisqTable'),
                                                     h3("Expected counts under the null"),
                                                     verbatimTextOutput('CAchisqTableExp'),
                                                     h3("Pearson residuals"),
                                                     verbatimTextOutput('CAchisqTableResid')
                                            ), #end of tabPanel
                                            tabPanel("Mass and Inertia",
                                                     mainPanel(dataTableOutput('CAMassInertia')
                                                              ) # end of mainPanel
                                            ), # end of tabPanel
                                            tabPanel("Inertia eigenvalues",
                                                     mainPanel(dataTableOutput('CAInertiaEigen')
                                                               ) # end of mainPanel
                                            ), # end of tabPanel
                                            tabPanel("Perceptual map",
                                                     mainPanel(plotOutput('CAPercepMap', width = "80%", inline = TRUE),
                                                               h3("Download CA perceptual map"),
                                                               downloadButton("down10", "Download")
                                                     ) # end of mainPanel
                                            ) # end of tabPanel
                      ) # end of tabsetPanel
                      ) # end of mainPanel
             ), # end of tabPanel
             navbarMenu("Coming up",
                        tabPanel("Discriminant")
                        ) # end of navbarMenu
) #end of navbarPage
) #end of shinyUI
