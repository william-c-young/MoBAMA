bamaData <- prepare_bama_data()

fit <- MoBAMA(bamaData,
              dataType = "bama",
              nChains = 1,
              nIter = 200, ## set higher for a real analysis
              outFolder = NULL)
