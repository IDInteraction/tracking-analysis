
#------------------------------------------------------------------------------
# Copyright (c) 2015, 2016 The University of Manchester, UK.
#
# Licenced under LGPL version 2.1. See LICENCE for details.
#
# The IDInteraction Attention Classification was developed in the IDInteraction
# project, funded by the Engineering and Physical Sciences Research Council,
# UK through grant agreement number EP/M017133/1.
#
# Authors: Aitor Apaolaza
#          Robert Haines
#------------------------------------------------------------------------------

projectFolderPath <- "/home/docker/data/"
attentionSourceFolder <- paste(projectFolderPath, "attention/", sep="")
trackingSourceFolder <- paste(projectFolderPath, "tracking/", sep="")
outputPath <- paste(projectFolderPath, "output/", sep="")

#Participants to try:
#P05 Stable rotation, the box only moves when the participant switches attention
#P08 Similar to P05m the height changes sizes during the experiment.


##Function to install missing libraries. Any time a library() is added, its name should be added to this function.
#Any new R installation will need to run this function the first time.

installMissingPackages <- function(){

  list.of.packages <- c("rpart","randomForest","Rmisc","caret","grid","partykit","formula.tools")

  new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
  if(length(new.packages)) install.packages(new.packages)
  else print("All listed packages are already installed")
}




###Reset plotting tool
old.par <- par(mar = c(0, 0, 0, 0))
par(old.par)

loadCSVFilesForParticipant <-function(){
  annotatedCSVFilename <<- paste(attentionSourceFolder,participantCode,"-timings.csv",sep="")
  trackingCSVFilename <<- paste(trackingSourceFolder,participantCode,"_video.csv",sep="")
}

loadAndCreateFeatureDF <- function(){
  loadCSVFilesForParticipant()
  combineCSVsToDF()
  createFeatureDF()
  write.csv(file=paste(projectFolderPath,"/featureDF",participantCode,".csv",sep=""), x=featureDF)
}

participantLoop <-function(){
  participantCodeList <- c("P01","P03","P04","P05","P06","P08","P09","P10","P12")

  for(participantIndex in participantCodeList){
    print(paste("Processing participant:",participantIndex))
    participantCode <<- participantIndex
    loadAndCreateFeatureDF()
    compareFeatures()
    #treeClassifier_6Features()
  }

}


##Takes the annotatedCSV and the trackingCSV files and creates the "combinedDF" combining tracking information with the attention annotation.
combineCSVsToDF <- function(){
  print(paste("START of combineCSVsToDF()"))
  annotatedDF <- read.csv(annotatedCSVFilename)
  trackingDF <- read.csv(trackingCSVFilename)

  combinedDF <<- trackingDF

  ##############TIME SHIFT FIX
  #It was found that there was a mismatch between the tracking and the annotations. I add 5 seconds to all tracking results.
  #tracking code already includes the start of the tracking, so I need to shift all annotations by that timestamp
  annotatedDF$Timestamp..ms. = annotatedDF$Timestamp..ms. + trackingDF$Timestamp..ms.[1]

  ###############TIME SHIFT END


  #It will be assumed that each annotation relates to a change on attention
  #If so, all rows from the last timestamp till the next change will contain current row's data.
  lastTimestampms <- 0

  for (annotatedRowIndex in 1: nrow(annotatedDF)){
    #print(paste("Processing row",annotatedRowIndex, "of",nrow(annotatedDF)))
    #all rows with timestamp>=lastTimestamp AND timestamp<nextTimestamp
    nexTimeStampms <- annotatedDF$Timestamp..ms.[annotatedRowIndex+1]

    #Getting the list of indexes to be updated with the current row info from annotatedDF
    if (is.na(nexTimeStampms)){
      #There are not more nexTimestamps, set the rest to the current value
      indexList <- which(combinedDF$Timestamp..ms.>=lastTimestampms)
    }
    else{
      indexList <- which(combinedDF$Timestamp..ms.>=lastTimestampms & combinedDF$Timestamp..ms.<nexTimeStampms)
    }

    combinedDF[indexList,"To.iPad"] <<- annotatedDF$To.iPad[annotatedRowIndex]
    combinedDF[indexList,"To.TV"] <<- annotatedDF$To.TV[annotatedRowIndex]
    combinedDF[indexList,"To.Elsewhere"] <<- annotatedDF$To.Elsewhere[annotatedRowIndex]
    combinedDF[indexList,"Attention"] <<- annotatedDF$Attention.Location..ipad.tv.elsewhere.[annotatedRowIndex]


    lastTimestampms <- nexTimeStampms
  }
  print(paste("END of combineCSVsToDF()"))

}


##This function takes the combinedDF from combineCSVsToDF() and creates a feature based DF.
#New features, such as movement speed, and rotation change will be calculated here
#It will discard attention missing rows ('no attention'0's in all attention targets)
#
createFeatureDF <- function(){
  print(paste("START of createFeatureDF()"))

  featureDF <<- data.frame(participantCode = participantCode,
                           timestampms = combinedDF$Timestamp..ms,
                           timestampMMSS = paste(floor(combinedDF$Timestamp..ms/60/1000),":",floor((combinedDF$Timestamp..ms/1000)%%60),sep=""),
                           attentionName = as.factor(combinedDF$Attention),
                           attentionIpad = combinedDF$To.iPad,
                           attentionTV = combinedDF$To.TV,
                           attentionNowhere = combinedDF$To.Elsewhere,
                           boxRotation = combinedDF$Bounding.box.rotation..degrees.,
                           boxHeight = combinedDF$Bounding.box.height..px.,
                           boxWidth = combinedDF$Bounding.box.width..px.)
  featureDF[,"boxArea"] <<- featureDF$boxHeight * featureDF$boxWidth
  featureDF[,"boxYcoord"] <<- combinedDF$Bounding.box.centre.Y..px.
  #same as boxYcoord, but adjusted for the max and min
  featureDF[,"boxYcoordRel"] <<- normalise0to1(combinedDF$Bounding.box.centre.Y..px.)

  featureDF[,"widthHeightRatio"] <<- featureDF$boxHeight / featureDF$boxWidth

  print(paste("END of createFeatureDF()"))


  ###Additional temporal features will be calculated here



}

#given a numberSequence, normalises the numbers according to the min and max of the sequence
normalise0to1 <- function(numberSequence){
  return((numberSequence - min(numberSequence))/(max(numberSequence)-min(numberSequence)))
}

###This function plots the distribution of values for each feature for each attention target
#It will help determine if a feature is important or not.
compareFeatures <- function(){
  print(paste("START of compareFeatures()"))

  featureList = c("boxRotation", "boxHeight", "boxWidth", "boxArea", "boxYcoord", "widthHeightRatio")

  library(Rmisc)
  library(ggplot2)
  #par(mfrow=c(2,3))
  plots=list()


  for (featureIndex in featureList){

    attentioniPad <- featureDF[featureDF$attentionName=="ipad",][[featureIndex]]
    attentionTV <- featureDF[featureDF$attentionName=="tv",][[featureIndex]]

    #hist(attentioniPad, col=rgb(1,0,0,0.5),main=featureIndex, xlab="Variable")
    #hist(attentionTV, col=rgb(0,0,1,0.5), add=T)
    #box()
    plots[[featureIndex]] <- ggplot(featureDF, aes_string(x = featureIndex, fill = "attentionName")) +
                                geom_density(alpha = 0.5) +
                                ggtitle(paste(featureIndex,participantCode)) +
                                theme(text = element_text(size=40)) +
                                scale_x_continuous(labels = function(x) as.character(round(x,2)))
  }

  #multiplot(plotlist=plots,cols=2)

  filename = paste(outputPath,"features","_",participantCode,"_",gsub(":", ";", Sys.time()),".png",sep="")
  width=3
  height=1.5
  png(file = filename, width = width * 1000, height = height * 1000)
  multiplot(plotlist=plots,cols=2)
  dev.off()
  print(paste("END of compareFeatures()"))

}


###Classifier using the featureDF from createFeatyureDF() as input
linearClassifier <- function(){
  #http://www.statmethods.net/advstats/glm.html
  fitGlm=glm(attentionName ~ boxHeight + boxRotation + boxArea + boxWidth + boxYcoord + widthHeightRatio,
             data=featureDF,
             family=binomial)
  summary(fitGlm) # display results
  confint(fitGlm) # 95% CI for the coefficients
  exp(coef(fitGlm)) # exponentiated coefficients
  exp(confint(fitGlm)) # 95% CI for exponentiated coefficients
  plot(predict(fitGlm, type="response")) # predicted values
  plot(residuals(fitGlm, type="deviance")) # residuals
}


treeClassifier_6Features <- function(){
  print(paste("START of treeClassifier_6Features()"))

  #Output folder of the plots
  folderPath = paste(projectFolderPath,"/Resources/Classifiers_6Features/",sep="")
  width=1.5
  height=0.75


  library(rpart)

  ####Tree classifier using boxWidth, boxHeight and boxRotation as features to predict iPad attention
  #http://www.statmethods.net/advstats/cart.html
  print(paste("Tree classifier for attention based on boxWidth and boxHeight for Participant",participantCode))
  fit = rpart(attentionName ~ boxHeight + boxRotation + boxArea + boxWidth + boxYcoord + widthHeightRatio,
        data=featureDF,
        method="class")#"class" for a classification tree "anova" for a regression tree

  printcp(fit) # display the results

  ##PNG output start
  filename = paste(folderPath,"treeClassifierCrossValidation","_",participantCode,"_",gsub(":", ";", Sys.time()),".png",sep="")
  png(file = filename, width = width * 1000, height = height * 1000)

  #Plot contained inside the PNG output
  plotcp(fit) # visualize cross-validation results

  dev.off()
  ##PNG output end

  summary(fit) # detailed summary of splits

  ##PNG output start
  filename = paste(folderPath,"treeClassifierPlot","_",participantCode,"_",gsub(":", ";", Sys.time()),".png",sep="")
  png(file = filename, width = width * 1000, height = height * 1000)

  #Plot contained inside the PNG output
  plot(fit, uniform=TRUE,
       main=paste("Classification Tree for user attention for Participant",participantCode))
  text(fit, use.n=TRUE, all=TRUE, cex=.8)

  dev.off()
  ##PNG output end



  # prune the tree
  #From http://www.statmethods.net/advstats/cart.html
  #"Typically, you will want to select a tree size that minimizes the cross-validated error, the xerror column printed by printcp( )."
  pfit<- prune(fit, cp=fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"])

  # plot the pruned tree
  ##PNG output start
  filename = paste(folderPath,"treeClassifierPrunedPlot","_",participantCode,"_",gsub(":", ";", Sys.time()),".png",sep="")

  png(file = filename, width = width * 1000, height = height * 1000)

  #Plot contained inside the PNG output
  plot(pfit, uniform=TRUE,
       main=paste("Pruned Classification Tree for user attention for Participant",participantCode))
  text(pfit, use.n=TRUE, all=TRUE, cex=.8)

  dev.off()
  ##PNG output end



  ###REGRESSION TREE
  # Regression Tree Example
  library(rpart)

  # grow tree
  fit = rpart(attentionName ~ boxHeight + boxRotation + boxArea + boxWidth + boxYcoord + widthHeightRatio,
              data=featureDF,
              method="anova")#"class" for a classification tree "anova" for a regression tree

  printcp(fit) # display the results
  summary(fit) # detailed summary of splits

  # create additional plots
  ##PNG output start
  filename = paste(folderPath,"treeRegressionClassifierCrossValidation","_",participantCode,"_",gsub(":", ";", Sys.time()),".png",sep="")
  png(file = filename, width = width * 1000, height = height * 1000)

  #Plot contained inside the PNG output
  par(mfrow=c(1,2)) # two plots on one page
  rsq.rpart(fit) # visualize cross-validation results
  par(mfrow=c(1,1))

  dev.off()
  ##PNG output end

  ###PLOT TREE
  ##PNG output start
  filename = paste(folderPath,"treeRegressionClassifier","_",participantCode,"_",gsub(":", ";", Sys.time()),".png",sep="")
  png(file = filename, width = width * 1000, height = height * 1000)

  #Plot contained inside the PNG output
  plot(fit, uniform=TRUE,
       main=paste("Regression Tree for user attention for Participant",participantCode))
  text(fit, use.n=TRUE, all=TRUE, cex=.8)

  dev.off()
  ##PNG output end


}


##given an individual participant code, it creates the corresponding tree classifier
individualTreeClassifier <- function(participantCode = partCode){

    print(paste("START of individualTreeClassifier()"))

    library(rpart)

    ####Tree classifier using boxWidth, boxHeight and boxRotation as features to predict iPad attention
    print(paste("Tree classifier for attention based on boxWidth and boxHeight for Participant",participantCode))
    fit = rpart(attentionName ~ boxHeight + boxRotation + boxArea + boxWidth + boxYcoord + widthHeightRatio,#boxYcoordRel,
                data=featureDF,
                method="class")#"class" for a classification tree "anova" for a regression tree

    printcp(fit) # display the results

    #Plot contained inside the PNG output
    plotcp(fit) # visualize cross-validation results
    plot(fit, uniform=TRUE,
         main=paste("Classification Tree for user attention for Participant",participantCode))

    plot(fit, uniform=TRUE)
    text(fit, use.n=TRUE, all=TRUE, cex=.8)

    summary(fit)
}


#Apply tree classifier to other data to check accuracy
#https://stat.ethz.ch/R-manual/R-devel/library/rpart/html/predict.rpart.html
#http://stats.stackexchange.com/questions/64551/how-to-use-rparts-result-in-prediction
treePrediction <- function(fittedModel = fit,
                           testData = featureDF){
  #preds2 <- predict(fit, data = testData, type = c("prob"))
  #see https://stat.ethz.ch/R-manual/R-devel/library/rpart/html/predict.rpart.html for details about usage of "type"
  preds <- predict(fit, data = testData, type = c("class"))
  preds = data.frame(preds)
  preds$realValue = testData$attentionName
  preds$participantCode = testData$participantCode

}

randomForestAnalysis <- function(){

  ####RANDOM FOREST
  # Random Forest prediction of Kyphosis data
  library(randomForest)
  fit <- randomForest(attentionName ~ boxHeight + boxRotation + boxArea + boxWidth + boxYcoord + widthHeightRatio,
                      data=featureDF)
  print(fit) # view results
  importance(fit) # importance of each predictor
}

#Exports gplot to png
exportPlot <- function(plot, filename, width=2, height=1.5) {

  dataWorkspaceArray = unlist( strsplit(rDataWorkspacePath,"/"))
  contextName = dataWorkspaceArray [length(dataWorkspaceArray)]

  filename = paste(filename,"_",contextName,"_",gsub(":", ";", Sys.time()),".png",sep="")
  filenameTest <<- filename
  png(file = filename, width = width * 100, height = height * 100)
  print(gplot)
  paste(paste("Plot exported to ",filename))
  dev.off()
}



loadAndCombineParticipantFeatures <- function(){
  print(paste("Start of loadAndCombineParticipantFeatures()"))

  participantCodeList <- c("P01","P03","P04","P05","P06","P08","P09","P10","P12")
  participantCodeList <- c("P04","P05","P08")

  globalFeatureDF <<- NA

  ##Load all participant dataframes.
  for(participantIndex in participantCodeList){
    print(paste("Processing participant:",participantIndex))
    participantCode <<- participantIndex
    loadAndCreateFeatureDF()

    if (is.null(nrow(globalFeatureDF)))
      globalFeatureDF <<- featureDF
    else
      globalFeatureDF <<- rbind(globalFeatureDF,featureDF)
  }

  #Once the globalFeatureDF is created, I will create a classifier, and report different features based on measurements of true positives and false positives
  dataclass = globalFeatureDF
  #split 2/3 of the data into the training dataset and 1/3 of the data into the testing data set
  set.seed(6)
  ind = sample(2, nrow(dataclass), replace=TRUE, prob=c(0.67,0.33))
  trainsetclass = dataclass[ind == 1,]
  testsetclass = dataclass[ind == 2, ]
  print(nrow(trainsetclass))

  # dim(dataClass)
  # dim(trainset)
  # dim(testset)

  #build a classificatino model with recursive partitioning trees using the training set
  #method=anova (regression), method=class (classification)
  #fit = rpart(attention ~ updatetime + updaterange + silence + instudio + emphasis + explicit + implicit + spark + colourful + shotchanges + audiochanges, method="class", data=trainset)

  #files with exact times for audio and shot changes (no ranges)
  #treeclass = rpart(attention ~ updatetime + updaterange + silence + instudio + emphasis + explicit + implicit + spark + colourful + shotchanges + framediffs + audiochanges + shotchangerange + audiochangerange, method="class", data=trainsetclass)
  #files with exact times for audio and shot changes (no ranges)
  treeclass = rpart(attentionName ~ boxHeight + boxRotation + boxArea + boxWidth + widthHeightRatio + boxYcoordRel,#+ boxYcoord
                    method="class", data=trainsetclass)


  treeclass
  printcp(treeclass) # display the results
  plotcp(treeclass) # visualize cross-validation results
  summary(treeclass) # detailed summary of splits

  library(partykit)
  rparty.ctree <- as.party(treeclass)
  rparty.ctree
  plot(rparty.ctree)  # gives a nicer plot, with error bars


  #test the model, predict using the testset dataset
  predclass <- predict(treeclass, newdata = testsetclass, type = "class")
  predclassprob <- predict(treeclass, newdata = testsetclass, type = "prob")

  #pred
  # head(pred)

  #create a dataframe with the actual class values, predicted class values and estimated probabilities of interest (True)
  dfpredclass <- data.frame(testsetclass$attentionName, predclass, predclassprob)
  #
  #confusion matrix that can then be used to calculate accuracy and other statistics
  resclass <-table(dfpredclass$testsetclass.attentionName, predclass)
  print(resclass)

  ##get TN, FN, TP, FP
  tn = resclass[1,1]
  fn = resclass[1,2]
  fp = resclass[2,1]
  tp = resclass[2,2]

  #metrics:
  ## error-rate: the proportion of the incorrectly classified examples -> (fp+fn)/(tp+tn+fp+tn)
  error_rate <- (fp+fn)/(tp+tn+fp+fn)
  print(paste("error_rate",error_rate*100,sep=":"))

  ## accuracy: the proportion of the correctly classified examples -> (tp+tn)/(tp+tn+fp+tn)
  acc <-  (tp+tn)/(tp+tn+fp+fn)
  print(paste("accuracy",acc*100,sep=":"))

  ##As the number of instances of the response variable is not balanced (there are many more instances with false than true)
  ## the accuracy is not a good indicator... -> cost sensitive measures
  #Precision: positive predicted value (PPV) -> tp/(tp+fp),  a measure of exactness
  precision <-tp/(tp+fp)
  print(paste("precision",precision*100,sep=":"))
  #Recall/Sensitivity: true positive recognition rate -> tp/(tp+fn), a measure of completeness
  sensitivity <- tp/(tp+fn)
  print(paste("recall/sensitivity",sensitivity*100,sep=":"))
  #Specificity: true negative recognition rate -> tn/(tn+fp)
  specificity <- tn/(tn+fp)
  print(paste("specificity",specificity*100,sep=":"))
  #F measure: 2*tp/(2*tp+fp+fn), the balance between precision and recall
  f <- 2*tp/(2*tp+fp+fn)
  print(paste("F",f*100,sep=":"))
}
