# Check for missing data

checkStandDataForMissingValues <- function(treeData, crownShape="yokozawa",
    eta=5, crownColor="forestgreen", stemColor="brown"){

    noTrees <- nrow(treeData)

    if(!is.null(noTrees)){
        # Fill missing values as needed
        for(v in c("crownShape", "eta", "crownColor", "stemColor"))
          if (is.null(treeData[[v]]))
             treeData[[v]] <- rep(get(v), noTrees)

         treeData[["species"]] <- as.character(as.factor(
            paste(treeData$crownShape, treeData$eta, treeData$crownColor)))

        essentialVariables <- c("x","y","topHeight","heightCrownBase","crownWidth","dbh")
        for(v in essentialVariables)
          if (is.null(treeData[[v]]))
            stop(paste(v, "values needed to make stand"))
    }
    treeData
}

