###=========================================================================================####
### Create function to calculate the confusion matrix from testing data and model threshold ====

eval_matrix_function<- function(model= "Ensemble", bathy= T, testdata= pred_testdata, bathy_limit= 15, output_entire_table = T,
                                threshold= models_NoO_thr[which(models_NoO_thr$Model=="ensemble"), "NoO_Threshold"],
                                evaluation= "model_substrate"){
  testdata<- na.exclude(testdata)
  
  if(bathy== T){
    testdata<- testdata %>% filter(bathymetry <= bathy_limit)
  }
  
  if(evaluation == "model"){
    
    testdata$accuracy_ensemble<- NA
    testdata[which(testdata$pr_ab==1 & testdata[,paste(model)] < threshold),"accuracy_ensemble"]<- "FN"
    testdata[which(testdata$pr_ab==0 & testdata[,paste(model)] >= threshold),"accuracy_ensemble"]<- "FP"
    testdata[which(testdata$pr_ab==1 & testdata[,paste(model)] >= threshold),"accuracy_ensemble"]<- "TP"
    testdata[which(testdata$pr_ab==0 & testdata[,paste(model)] < threshold),"accuracy_ensemble"]<- "TN"
    
    N= length(c(testdata%>%filter(!is.na(accuracy_ensemble))%>%select(pr_ab))$pr_ab)
    
    counts= testdata%>%filter(!is.na(accuracy_ensemble))%>%
      group_by(accuracy_ensemble)%>%
      summarise(counts= round(length(pr_ab)/N * 100,2))
    
    colnames(counts)<- c("evaluation", "percentage")
    
    eval_matrix<- data.frame("Model"= rep(paste(model),length(counts$evaluation)), 
                             "NoO_Threshold"= rep(threshold, length(counts$evaluation)), 
                             "assessment"= rep("model",length(counts$evaluation)))
    eval_matrix<- cbind(eval_matrix, counts)
    
  }
  
  if(evaluation == "model_substrate"){
    testdata$accuracy_ensemble_subs<- NA
    testdata[which(testdata$pr_ab==1 & testdata[,paste(model)] < threshold),"accuracy_ensemble_subs"]<- "FN"
    testdata[which(testdata$pr_ab==0 & testdata[,paste(model)] >= threshold),"accuracy_ensemble_subs"]<- "FP"
    testdata[which(testdata$pr_ab==1 & testdata[,paste(model)] >= threshold),"accuracy_ensemble_subs"]<- "TP"
    testdata[which(testdata$pr_ab==0 & testdata[,paste(model)] < threshold),"accuracy_ensemble_subs"]<- "TN"
    summary(as.factor(testdata$accuracy_ensemble_subs))
    testdata[which(testdata$pr_ab==1 & testdata[,paste(model)] >= threshold & testdata$substrate_binary==0), "accuracy_ensemble_subs"]<- "FN"
    testdata[which(testdata$pr_ab==0 & testdata[,paste(model)] >= threshold & testdata$substrate_binary==0), "accuracy_ensemble_subs"]<- "TN"
    
    N= length(c(testdata%>%filter(!is.na(accuracy_ensemble_subs))%>%select(pr_ab))$pr_ab)
    
    counts= testdata%>%filter(!is.na(accuracy_ensemble_subs))%>%
      group_by(accuracy_ensemble_subs)%>%
      summarise(counts= round(length(pr_ab)/N * 100,2))
    
    colnames(counts)<- c("evaluation", "percentage")
    
    # if(nrow(counts)<= 3){
    # counts[4,1]<- c("XX")
    # counts[4,2]<- c(0)
    # }
    
    
    eval_matrix<- data.frame("Model"= rep(paste(model),length(counts$evaluation)), 
                             "NoO_Threshold"= rep(threshold, length(counts$evaluation)), 
                             "assessment"= rep("model_substrate",length(counts$evaluation)))
    eval_matrix<- cbind(eval_matrix, counts)
    
  }
  
  if(bathy == T){
    
    if(output_entire_table == T){
      eval_matrix$assessment <- paste(evaluation, "bathy", sep="_")
      return(list(eval_matrix, testdata))
      
    }else{
      eval_matrix$assessment <- paste(evaluation, "bathy", sep="_")
      return(eval_matrix)
    }
    
    
  }else{
    if(output_entire_table == T){
      return(list(eval_matrix, testdata))
      
    }else
      # eval_matrix$assessment <- paste(evaluation, sep="_")
      return(eval_matrix)
  }
  
  
  # }#else { return(paste("error: total sum different from 100%"))}
  
}

#### Second funtion to include evaluation with masked as unsuitable areas below 15 m ====
eval_matrix_function2<- function(model= "Ensemble", testdata= pred_test_sat, output_entire_table = T,# bathy_limit= 15, bathy= T,
                                 threshold= models_NoO_thr[which(models_NoO_thr$Model=="ensemble"), "NoO_Threshold"],
                                 evaluation= "model_substrate"){
  testdata<- na.exclude(testdata)
  
  # if(bathy== T){
  #   testdata<- testdata %>% filter(bathymetry <= bathy_limit)
  # }
  
  # if(evaluation == "model"){
  #   FN= length(which(testdata$pr_ab==1 & testdata[,paste(model)] < threshold))/length(!is.na(testdata[,"pr_ab"])) * 100
  #   FP= length(which(testdata$pr_ab==0 & testdata[,paste(model)] >= threshold))/length(!is.na(testdata[,"pr_ab"])) * 100
  #   TP= length(which(testdata$pr_ab==1 & testdata[,paste(model)] >= threshold))/length(!is.na(testdata[,"pr_ab"])) * 100
  #   TN= length(which(testdata$pr_ab==0 & testdata[,paste(model)] < threshold))/length(!is.na(testdata[,"pr_ab"])) * 100
  #   
  #   eval_matrix<- data.frame("Model"= rep(paste(model),4), "NoO_Threshold"= rep(threshold, 4), "assessment"= rep("model",4),
  #                            "evaluation"= c("FN", "FP", "TN", "TP"), percentage= round(c(FN,FP, TN,TP), 2))
  # }
  
  if(evaluation == "model_substrate"){
    testdata$accuracy_ensemble_subs<- NA
    testdata[which(testdata$pr_ab==1 & testdata[,paste(model)] < threshold),"accuracy_ensemble_subs"]<- "FN"
    testdata[which(testdata$pr_ab==0 & testdata[,paste(model)] >= threshold),"accuracy_ensemble_subs"]<- "FP"
    testdata[which(testdata$pr_ab==1 & testdata[,paste(model)] >= threshold),"accuracy_ensemble_subs"]<- "TP"
    testdata[which(testdata$pr_ab==0 & testdata[,paste(model)] < threshold),"accuracy_ensemble_subs"]<- "TN"
    summary(as.factor(testdata$accuracy_ensemble_subs))
    testdata[which(testdata$pr_ab==1 & testdata[,paste(model)] >= threshold & testdata$substrate_binary==0), "accuracy_ensemble_subs"]<- "FN"
    testdata[which(testdata$pr_ab==0 & testdata[,paste(model)] >= threshold & testdata$substrate_binary==0), "accuracy_ensemble_subs"]<- "TN"
    
    N= length(c(testdata%>%filter(!is.na(accuracy_ensemble_subs))%>%select(pr_ab))$pr_ab)
    
    counts= testdata%>%filter(!is.na(accuracy_ensemble_subs))%>%
      group_by(accuracy_ensemble_subs)%>%
      summarise(counts= round(length(pr_ab)/N * 100,2))
    
    colnames(counts)<- c("evaluation", "percentage")
    
    if(nrow(counts)<= 3){
      counts[4,1]<- c("XX")
      counts[4,2]<- c(0)
    }
    
    
    eval_matrix<- data.frame("Model"= rep(paste(model),4), "NoO_Threshold"= rep(threshold, 4), "assessment"= rep("model_substrate",4))
    eval_matrix<- cbind(eval_matrix, counts)
    
  }
  
  if(evaluation == "model_substrate_bathy"){
    testdata$accuracy_ensemble_subs_bathy<- NA
    testdata[which(testdata$pr_ab==1 & testdata[,paste(model)] < threshold),"accuracy_ensemble_subs_bathy"]<- "FN"
    testdata[which(testdata$pr_ab==0 & testdata[,paste(model)] >= threshold),"accuracy_ensemble_subs_bathy"]<- "FP"
    testdata[which(testdata$pr_ab==1 & testdata[,paste(model)] >= threshold),"accuracy_ensemble_subs_bathy"]<- "TP"
    testdata[which(testdata$pr_ab==0 & testdata[,paste(model)] < threshold),"accuracy_ensemble_subs_bathy"]<- "TN"
    summary(as.factor(testdata$accuracy_ensemble_subs_bathy))
    testdata[which(testdata$pr_ab==1 & testdata[,paste(model)] >= threshold & testdata$substrate_binary==0), "accuracy_ensemble_subs_bathy"]<- "FN"
    testdata[which(testdata$pr_ab==0 & testdata[,paste(model)] >= threshold & testdata$substrate_binary==0), "accuracy_ensemble_subs_bathy"]<- "TN"
    
    testdata[which(testdata$pr_ab==0 & testdata$bathymetry >15), "accuracy_ensemble_subs_bathy"]<- "TN"
    testdata[which(testdata$pr_ab==1 & testdata$bathymetry >15), "accuracy_ensemble_subs_bathy"]<- "FN"
    
    N= length(c(testdata%>%filter(!is.na(accuracy_ensemble_subs_bathy))%>%select(pr_ab))$pr_ab)
    
    counts= testdata%>%filter(!is.na(accuracy_ensemble_subs_bathy))%>%
      group_by(accuracy_ensemble_subs_bathy)%>%
      summarise(counts= round(length(pr_ab)/N * 100,2))
    
    colnames(counts)<- c("evaluation", "percentage")
    
    # if(nrow(counts)<= 3){
    #   counts[4,1]<- c("XX")
    #   counts[4,2]<- c(0)
    # }
    
    
    eval_matrix<- data.frame("Model"= rep(paste(model),length(counts$evaluation)), 
                             "NoO_Threshold"= rep(threshold, length(counts$evaluation)), 
                             "assessment"= rep("model_substrate_bathy2",length(counts$evaluation)))
    eval_matrix<- cbind(eval_matrix, counts)
    
  }
  
  if(output_entire_table == T){
    return(list(eval_matrix, testdata))
  }
  
  return(eval_matrix)
  # }
  
  
  # }#else { return(paste("error: total sum different from 100%"))}
  
}
