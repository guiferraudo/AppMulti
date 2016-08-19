# Exploratory Factor Analyses
EFAfit = function(dataset, nfactorsUser, rotationMethod){
                                                       factorFit = principal(dataset,
                                                                             nfactors = nfactorsUser, 
                                                                             rotate = rotationMethod,
                                                                             scores = TRUE,
                                                                             normalize = FALSE)
# printing main EFA outputs
return(list(
            # KMO - MSA(Measure of Sampling Adequacy) 
            KMOresults = KMO(dataset),
            # p-value of Bartlett sphericity's test 
            pvalorCortestBartlett = round(cortest.bartlett(dataset, n = 1000)$p.value, dig = 4),
            #factors extraction
            factorsExtraction = factorFit
            )
       )
}

###   End of EFA ###
