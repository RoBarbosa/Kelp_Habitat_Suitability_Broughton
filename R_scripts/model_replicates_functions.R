

### This function was edited to include testing data from a specific table;
# the function will merge training and testing data for each replicate
asemble_model_replicates<- function(replicates_number= 20, 
                                    training_data= train_data_subset, 
                                    testing_data= test_data_subset,
                                    variables= stack_vars){
  
  require(terra)
  
  options(stringsAsFactors = FALSE)
  
  # Creating the objects    
  x <- raster::stack()
  mod.evals <- list()
  l.model <- list()
  list_models<- list()
  list_models_gbm<- list()
  list_models_rf<- list()
  list_model_preds<- list()
  list_model_ens<- list()
  
  model_performance<- as.data.frame(matrix(ncol=27, nrow=0))
  colnames(model_performance)<- c("model_ID" , "model", "threshold", "thr_value", "n_presences", "n_absences", "TPR_mean", "TPR_sd",       
                                  "TNR_mean", "TNR_sd",  "SORENSEN_mean", "SORENSEN_sd", "JACCARD_mean",  "JACCARD_sd", "FPB_mean", "FPB_sd",       
                                  "OR_mean",  "OR_sd", "TSS_mean", "TSS_sd", "AUC_mean", "AUC_sd", "BOYCE_mean", "BOYCE_sd",     
                                  "IMAE_mean", "IMAE_sd", "replicate" )
  
  output<- as.data.frame(matrix(ncol=5, nrow=replicates_number))
  colnames(output)<- c("mod_replicate", "threshold.SS", "threshold.NoO", "threshold.ESS", "AUC")
  
  results_model<- as.data.frame(matrix(ncol = 5, nrow = 0))
  colnames(results_model)<- c("replicate",  "model", "variable", "variable_contrib", "permutation_importance")
  
  for(i in 1:replicates_number){ #
    start.time <- Sys.time()
    
    train_data_i<- training_data%>% filter(rdomreplicate == i)
    train_data_i$dataset<- "train"
    
    testing_data$dataset<- "test"
    colnames(train_data_i)
    colnames(testing_data)
    
    train_data_i<- rbind(train_data_i[,c(1,3:5,7:length(colnames(train_data_i)))], 
                         testing_data[,c(1:4,6:length(colnames(testing_data)))])
    
    # levels(as.factor(train_data_i$pr_ab))
    # train_data_i<- train_data_i%>%
    #   filter(pr_ab!=2)
    # 
    # # # Split into train and test datasets
    # train_data_i$X<-seq_len(nrow(train_data_i))
    # train_data_ib<- sample_frac(train_data_i, size= 0.70, replace = F)
    # train_data_i<- as.data.frame(train_data_i)
    # train_data_i$dataset<- NA
    # train_data_i[train_data_ib$X, "dataset"]<- "train"
    # train_data_i[is.na(train_data_i$dataset), "dataset"]<- "test"
    
    
    
    # Fit and validate a [generalized linear model](https://sjevelazco.github.io/flexsdm/reference/fit_glm.html)
    # spp_pa<- rbind(kelp2, backg2)
    
    glm_i <- fit_glm(
      data = train_data_i,
      response = "pr_ab",
      predictors = colnames(train_data_i)[6:ncol(train_data_i)-1],
      partition = "dataset",
      select_pred= F,
      poly = 2
    )
    
    
    list_models[[length(list_models) + 1]] <- glm_i
    list_models$names[length(list_models)]<- paste("glm", i , sep="_")
    
    
    mgbm_i <- fit_gbm(
      data = train_data_i,
      response = "pr_ab",
      predictors = colnames(train_data_i)[6:ncol(train_data_i)-1],
      partition = "dataset"
    )
    
    list_models_gbm[[length(list_models_gbm) + 1]] <- mgbm_i
    list_models_gbm$names[length(list_models_gbm)]<- paste("gbm", i , sep="_")
    
    rf_i <- fit_raf(
      data = train_data_i,
      response = "pr_ab",
      predictors = colnames(train_data_i)[6:ncol(train_data_i)-1],
      partition = "dataset"
    )
    
    list_models_rf[[length(list_models_rf) + 1]] <- rf_i
    list_models_rf$names[length(list_models_rf)]<- paste("rf", i , sep="_")
    
    # Create output tablle for i
    model_perf <- sdm_summarize(list(glm_i, mgbm_i, rf_i))
    model_perf<- as.data.frame(model_perf)
    model_perf$replicate<- i
    
    model_performance<- rbind(model_performance, model_perf)
    
    ## Fit ensemble model and predict based on threshold of mas_sens_spec
    ens_i <- fit_ensemble(
      models = list(glm_i, mgbm_i, rf_i),
      ens_method = "meanw",#Weighted average of models based on their performance. An evaluation metric and threshold type must be provided.
      thr = c("max_sens_spec", "equal_sens_spec", "lpt"),
      thr_model = "max_sens_spec",
      metric = "TSS"
    )
    
    list_model_ens[[length(list_model_ens) + 1]] <- ens_i
    list_model_ens$names[length(list_model_ens)]<- paste("mod_ensemble", i , sep="_")
    
    
    pr_i <- sdm_predict(
      models = ens_i,
      pred = variables,
      thr = "lpt",
      con_thr = TRUE,
      predict_area = NULL
    )
    
    list_model_preds[[length(list_model_preds) + 1]] <- pr_i
    list_model_preds$names[length(list_model_preds)]<- paste("pred_ensemble", i , sep="_")
    
    
    # as.data.frame(mgbm_i$performance)    
    # round(unique(mgbm_i$performance$AUC_mean),2)
    # round(unique(mgbm_i$performance$BOYCE_mean),2)
    
    Loop.r<- raster(pr_i$meanw$meanw)
    plot(Loop.r)
    names(Loop.r)<- paste("mod_rep", i, sep="_")
    x <- stack(x, Loop.r)
    end.time <- Sys.time()
    time.taken <- end.time - start.time
    print(paste("finished model",i, "time:", time.taken))
    
    
  }
  
  return(list(models_rf= list_models_rf, models_glm= list_models, models_gbm= list_models_gbm, 
              mod_performance=model_performance, preds=list_model_ens, preds_stack=x))
}


# replicates_number<- length(unique(train_data$rdomreplicate))

model_performance%>%
  ggplot(aes(x= as.factor(model), y= AUC_mean))+
  geom_boxplot()+
  theme_bw()


p_extra(
  training_data = train_data_i,
  x = "x",
  y = "y",
  pr_ab = "pr_ab",
  color_p = "black",
  extra_suit_data = pr_i$meanw$meanw,
  projection_data = filtered_vars001,
  geo_space = TRUE,
  prop_points = 0.05
)


p_pdp(model = rf_i$model, training_data = train_data_i, projection_data = filtered_vars001)
p_pdp(model = mgbm_i$model, training_data = train_data_i, projection_data = filtered_vars001)
p_pdp(model = glm_i$model, training_data = train_data_i, projection_data = filtered_vars001)






asemble_model_replicates0<- function(replicates_number= 20, presence_data= train_data, 
                                    test_data= test_data,
                            variables= stack_vars){
  train_data= presence_data
  stack_vars= variables 
  require(terra)
  
  options(stringsAsFactors = FALSE)
  
  # Creating the objects    
  x <- stack()
  mod.evals <- list()
  l.model <- list()
  list_models<- list()
  list_models_gbm<- list()
  list_models_rf<- list()
  list_model_preds<- list()
  list_model_ens<- list()
  
  model_performance<- as.data.frame(matrix(ncol=27, nrow=0))
  colnames(model_performance)<- c("model_ID" , "model", "threshold", "thr_value", "n_presences", "n_absences", "TPR_mean", "TPR_sd",       
                                  "TNR_mean", "TNR_sd",  "SORENSEN_mean", "SORENSEN_sd", "JACCARD_mean",  "JACCARD_sd", "FPB_mean", "FPB_sd",       
                                  "OR_mean",  "OR_sd", "TSS_mean", "TSS_sd", "AUC_mean", "AUC_sd", "BOYCE_mean", "BOYCE_sd",     
                                  "IMAE_mean", "IMAE_sd", "replicate" )
  
  output<- as.data.frame(matrix(ncol=5, nrow=replicates_number))
  colnames(output)<- c("mod_replicate", "threshold.SS", "threshold.NoO", "threshold.ESS", "AUC")
  
  results_model<- as.data.frame(matrix(ncol = 5, nrow = 0))
  colnames(results_model)<- c("replicate",  "model", "variable", "variable_contrib", "permutation_importance")
  
  for(i in 1:replicates_number){ #
    start.time <- Sys.time()
    
    # Creating the k-fold data
    # group.k1 <- kfold(loxA.4, 5)
    # 
    # loxTrain = loxData.L1[group.k1 != 1,]
    # loxTest.L1 = loxData.L1[group.k1 == 1,]
    
    
    train_data_i<- train_data%>% filter(rdomreplicate == i)
    
    # # Split into train and test datasets
    train_data_i$X<-seq_len(nrow(train_data_i))
    train_data_ib<- sample_frac(train_data_i, size= 0.70, replace = F)
    train_data_i<- as.data.frame(train_data_i)
    train_data_i$dataset<- NA
    train_data_i[train_data_ib$X, "dataset"]<- "train"
    train_data_i[is.na(train_data_i$dataset), "dataset"]<- "test"
    
   
    
    # Fit and validate a [generalized linear model](https://sjevelazco.github.io/flexsdm/reference/fit_glm.html)
    # spp_pa<- rbind(kelp2, backg2)
    
    glm_i <- fit_glm(
      data = train_data_i,
      response = "pr_ab",
      predictors = colnames(train_data_i)[9:ncol(train_data_i)-2],
      partition = "dataset",
      select_pred= F,
      poly = 2
    )
    
    
    list_models[[length(list_models) + 1]] <- glm_i
    list_models$names[length(list_models)]<- paste("glm", i , sep="_")
    
    
    mgbm_i <- fit_gbm(
      data = train_data_i,
      response = "pr_ab",
      predictors = colnames(train_data_i)[9:ncol(train_data_i)-2],
      partition = "dataset"
    )
    
    list_models_gbm[[length(list_models_gbm) + 1]] <- mgbm_i
    list_models_gbm$names[length(list_models_gbm)]<- paste("gbm", i , sep="_")

    rf_i <- fit_raf(
      data = train_data_i,
      response = "pr_ab",
      predictors = colnames(train_data_i)[9:ncol(train_data_i)-2],
      partition = "dataset"
    )
    
    list_models_rf[[length(list_models_rf) + 1]] <- rf_i
    list_models_rf$names[length(list_models_rf)]<- paste("rf", i , sep="_")
    
    # Create output tablle for i
    model_perf <- sdm_summarize(list(glm_i, mgbm_i, rf_i))
    model_perf<- as.data.frame(model_perf)
    model_perf$replicate<- i
    
    model_performance<- rbind(model_performance, model_perf)
    
    ## Fit ensemble model and predict based on threshold of mas_sens_spec
    ens_i <- fit_ensemble(
      models = list(glm_i, mgbm_i, rf_i),
      ens_method = "meanw",#Weighted average of models based on their performance. An evaluation metric and threshold type must be provided.
      thr = c("max_sens_spec", "equal_sens_spec", "lpt"),
      thr_model = "max_sens_spec",
      metric = "TSS"
    )
    
    list_model_ens[[length(list_model_ens) + 1]] <- ens_i
    list_model_ens$names[length(list_model_ens)]<- paste("mod_ensemble", i , sep="_")
    
    
    pr_i <- sdm_predict(
      models = ens_i,
      pred = filtered_vars001,
      thr = "max_sens_spec",
      con_thr = TRUE,
      predict_area = NULL
    )
    
    list_model_preds[[length(list_model_preds) + 1]] <- pr_i
    list_model_preds$names[length(list_model_preds)]<- paste("pred_ensemble", i , sep="_")
    
    
    # as.data.frame(mgbm_i$performance)    
    # round(unique(mgbm_i$performance$AUC_mean),2)
    # round(unique(mgbm_i$performance$BOYCE_mean),2)
    
    Loop.r<- raster(pr_i$meanw$meanw)
    plot(Loop.r)
    names(Loop.r)<- paste("mod_rep", i, sep="_")
    x <- stack(x, Loop.r)
    end.time <- Sys.time()
    time.taken <- end.time - start.time
    print(paste("finished model",i, "time:", time.taken))
    
    
  }
  
  return(list(models_rf= list_models_rf, models_glm= list_models, models_gbm= list_models_gbm, 
              mod_performance=model_performance, preds=list_model_ens, preds_stack=x))
}

  
# replicates_number<- length(unique(train_data$rdomreplicate))

model_performance%>%
  ggplot(aes(x= as.factor(model), y= AUC_mean))+
  geom_boxplot()+
  theme_bw()


p_extra(
  training_data = train_data_i,
  x = "x",
  y = "y",
  pr_ab = "pr_ab",
  color_p = "black",
  extra_suit_data = pr_i$meanw$meanw,
  projection_data = filtered_vars001,
  geo_space = TRUE,
  prop_points = 0.05
)


p_pdp(model = rf_i$model, training_data = train_data_i, projection_data = filtered_vars001)
p_pdp(model = mgbm_i$model, training_data = train_data_i, projection_data = filtered_vars001)
p_pdp(model = glm_i$model, training_data = train_data_i, projection_data = filtered_vars001)




#####----------------------------------------------------------------------#####
###--------  Model replicated with selected 25 variables ------------------#####
#####----------------------------------------------------------------------#####

model_replicates_Varsfunction<- function(replicates_number= 20, presence_data= train_data, 
                                           variables= stack_vars, n_rdompoints= 10000, selected_variables= 25, pred= F, bgd= "variable"){
  train_data= presence_data
  stack_vars= variables 
  
  
  options(stringsAsFactors = FALSE)
  
  # Creating the objects    
  x <- stack()
  mod.evals <- list()
  l.model <- list()
  list_models<- list()
  
  output<- as.data.frame(matrix(ncol=5, nrow=replicates_number))
  colnames(output)<- c("mod_replicate", "threshold.SS", "threshold.NoO", "threshold.ESS", "AUC")
  
  calib_area2<- stack_vars[[1]]
  crs(calib_area2)<- "epsg:3005"
  calib_area2<- raster(calib_area2)
  
  # Creating the background points
  background = as.data.frame(randomPoints(calib_area2, n_rdompoints))
  background_values<- sdm_extract(
    data = background,
    x = "x",
    y = "y",
    env_layer = stack_vars, # Raster with environmental variables
    variables = NULL, # Vector with the variable names of predictor variables Usage variables. = c("aet", "cwd", "tmin"). If no variable is specified, function will return data for all layers.
    filter_na = F
  )
  
  # pseudo_absence_values = extract(stack_variables, background)
  background_values <- as.data.frame(background_values)
  background_values<- na.exclude(background_values)
  
  # backg_data_i_sf <- (background_values) %>%
  #   st_as_sf(coords = c("x", "y"), crs = "EPSG:3005")
  # mapview::mapview(backg_data_i_sf, cex=2, add=T)# a visual check
  
  
  results_maxent<- as.data.frame(matrix(ncol = 5, nrow = 0))
  colnames(results_maxent)<- c("replicate",  "model", "variable", "variable_contrib", "permutation_importance")
  
  for(i in 1:replicates_number){ #
    
   if (bgd== "variable"){
     # Creating the background points
     background = as.data.frame(randomPoints(calib_area2, n_rdompoints))
     background_values<- sdm_extract(
       data = background,
       x = "x",
       y = "y",
       env_layer = stack_vars, # Raster with environmental variables
       variables = NULL, # Vector with the variable names of predictor variables Usage variables. = c("aet", "cwd", "tmin"). If no variable is specified, function will return data for all layers.
       filter_na = F
     )
     
     # pseudo_absence_values = extract(stack_variables, background)
     background_values <- as.data.frame(background_values)
     background_values<- na.exclude(background_values)
   }
    # Creating the k-fold data
    # group.k1 <- kfold(loxA.4, 5)
    # 
    # loxTrain = loxData.L1[group.k1 != 1,]
    # loxTest.L1 = loxData.L1[group.k1 == 1,]
    
    
    train_data_i<- train_data%>% filter(rdomreplicate == i)
    train_data_i<- train_data_i%>% filter(pr_ab == 1)
    
    # # Split into train and test datasets
    train_data_i$X<-seq_len(nrow(train_data_i))
    train_data_ib<- sample_frac(train_data_i, size= 0.70, replace = F)
    train_data_i<- as.data.frame(train_data_i)
    train_data_i$dataset<- NA
    train_data_i[train_data_ib$X, "dataset"]<- "train"
    train_data_i[is.na(train_data_i$dataset), "dataset"]<- "test"
    
    # train_data_i_sf <- (train_data_i) %>%
    #   st_as_sf(coords = c("x", "y"), crs = "EPSG:3005")
    # mapview::mapview(train_data_i_sf, cex=2, add=T, zcol= "dataset", col.regions= c("red", "blue"))# a visual check
    
    
    Test_data_i = train_data_i[which(train_data_i$dataset== "test"), ]
    train_data_i<- train_data_i[which(train_data_i$dataset== "train"), ]
    
    
    # Creating the Analysis Data
    train_y = c(rep(1,nrow(train_data_i)), rep(0,nrow(background_values))) # Main Training Data
    train_sdm_data = cbind(pa = train_y, rbind(train_data_i[,9:length(colnames(train_data_i))-2], background_values[,3:ncol(background_values)])) # Main SDM Data
    
    start.time <- Sys.time()
    Model_i = maxent(train_sdm_data[,-1], p = train_y)#, path = paste(SDM_path,"outputs_maxent", sep="/"), args = c("-J", "-P"),#, "replicates=5"
    # file.rename("MaxEnt results", paste0("MaxEnt results ", i))
    
    end.time <- Sys.time()
    time.taken <- end.time - start.time
    print(time.taken)
    
    list_models[[length(list_models) + 1]] <- Model_i
    list_models$names[length(list_models)]<- paste("maxent", i , sep="_")
    
    
    # Create output tablle for i
    results_maxent_i<- as.data.frame(matrix(ncol = 1, nrow = length(names(stack_vars))))
    colnames(results_maxent_i)<- c("replicate")
    results_maxent_i$replicate<- paste(i)
    
    vars_contrib_i<-  as.data.frame(Model_i@results) #Model_i@results[7:(length(names(stack_vars))+6)]#
    vars_contrib_i$variables<- row.names(vars_contrib_i)
    rownames(vars_contrib_i)<- NULL
    colnames(vars_contrib_i)[1]<- "variable_contrib"
    
    # if(selected_variables== 25){
    #   vars_permut_i<- vars_contrib_i[32:(31+length(names(stack_vars))),]#35:62
    # }
    # 
    # if(selected_variables== 20){
    #   vars_permut_i<- vars_contrib_i[27:(26+length(names(stack_vars))),]#35:62
    # }
    # 
    # if(selected_variables== 21){
    #   vars_permut_i<- vars_contrib_i[28:(27+length(names(stack_vars))),]#35:62
    # }
    # 
    # if(selected_variables== 12){
    #   vars_permut_i<- vars_contrib_i[19:(18+length(names(stack_vars))),]#35:62
    # }
    # 
    # if(selected_variables== 13){
    #   vars_permut_i<- vars_contrib_i[20:(19+length(names(stack_vars))),]#35:62
    # }
    # 
    # if(selected_variables== 14){
    #   vars_permut_i<- vars_contrib_i[21:(20+length(names(stack_vars))),]#35:62
    # }
    
    if(selected_variables== 16){
      vars_permut_i<- vars_contrib_i[23:(22+length(names(stack_vars))),]#35:62
    }
    
    vars_permut_i<- vars_permut_i %>% separate_wider_delim(variables, ".", names = c("variable", NA, NA))
    vars_permut_i<- as.data.frame(vars_permut_i[,c(1)])
    colnames(vars_permut_i)[1]<- "permutation_importance"
    
    vars_contrib_i<- vars_contrib_i[1:length(names(stack_vars))+6,]
    vars_contrib_i<- vars_contrib_i %>% separate_wider_delim(variables, ".", names = c("variable", NA))
    vars_contrib_i<- as.data.frame(vars_contrib_i[,c(2,1)])
    
    
    results_maxent_i$model<- "Maxent"
    results_maxent_i<- cbind(results_maxent_i, vars_contrib_i, vars_permut_i)
    results_maxent<- rbind(results_maxent, results_maxent_i)
    
    # Doing Model Evaluation
    mod.evT1 <- dismo::evaluate(Model_i, p = Test_data_i, a = background_values[, 3:ncol(background_values)])
    mod.evals <- list(mod.evals, mod.evT1)
    
    # Capturing the threshold values
    output$mod_replicate[i] <- i
    output$threshold.SS[i]  <- threshold(mod.evT1, "spec_sens")
    output$threshold.NoO[i] <- threshold(mod.evT1, "no_omission")
    output$threshold.ESS[i] <- threshold(mod.evT1, "equal_sens_spec")
    
    # Capturing the AUC values
    output$AUC[i] <- mod.evT1@auc
    
    
    if (pred== T){
      Loop.r <- predict(stack_vars, Model_i, progress='text', na.rm=TRUE)
      Loop.r<- raster(Loop.r)
      plot(Loop.r)
      names(Loop.r)<- paste("mod_rep", i, sep="_")
      x <- stack(x, Loop.r)
    }
    
    print(paste("finished model",i))
  }
  
  # mean_pred<- mean(x)
  # sd_pred<- sd(x)
  if(pred== T){
    return(list(variables_importance=results_maxent, models=list_models, mod_performance=output, mod.evaluation= mod.evals, preds=x))
  }
    return(list(variables_importance=results_maxent, models=list_models, mod_performance=output, mod.evaluation= mod.evals))#, preds=x, mean_preds= mean_pred, sd_preds= sd_pred
  
}



#####----------------------------------------------------------------------#####
###--------  Model replicated with selected 15 variables ------------------#####
#####----------------------------------------------------------------------#####

model_replicates_15Varsfunction<- function(replicates_number= 20, presence_data= train_data, 
                               variables= stack_vars, n_rdompoints= 10000, selected_variables15= T){
  train_data= presence_data
  stack_vars= variables 
  
  
  options(stringsAsFactors = FALSE)
  
  # Creating the objects    
  x <- stack()
  mod.evals <- list()
  l.model <- list()
  list_models<- list()
  
  output<- as.data.frame(matrix(ncol=5, nrow=replicates_number))
  colnames(output)<- c("mod_replicate", "threshold.SS", "threshold.NoO", "threshold.ESS", "AUC")
  
  calib_area2<- stack_vars[[1]]
  crs(calib_area2)<- "epsg:3005"
  calib_area2<- raster(calib_area2)
  
  # Creating the background points
  background = as.data.frame(randomPoints(calib_area2, n_rdompoints))
  background_values<- sdm_extract(
    data = background,
    x = "x",
    y = "y",
    env_layer = stack_vars, # Raster with environmental variables
    variables = NULL, # Vector with the variable names of predictor variables Usage variables. = c("aet", "cwd", "tmin"). If no variable is specified, function will return data for all layers.
    filter_na = F
  )
  
  
  # pseudo_absence_values = extract(stack_variables, background)
  background_values <- as.data.frame(background_values)
  background_values<- na.exclude(background_values)
  colnames(background_values)
  length(background_values$bathymetry)
  
  backg_data_i_sf <- (background_values) %>%
    st_as_sf(coords = c("x", "y"), crs = "EPSG:3005")
  # mapview::mapview(backg_data_i_sf, cex=2, add=T)# a visual check
  
  
  results_maxent<- as.data.frame(matrix(ncol = 5, nrow = 0))
  colnames(results_maxent)<- c("replicate",  "model", "variable", "variable_contrib", "permutation_importance")
  
  for(i in 1:replicates_number){ #
    
    # Creating the k-fold data
    # group.k1 <- kfold(loxA.4, 5)
    # 
    # loxTrain = loxData.L1[group.k1 != 1,]
    # loxTest.L1 = loxData.L1[group.k1 == 1,]
    
    
    train_data_i<- train_data%>% filter(rdomreplicate == i)
    train_data_i<- train_data_i%>% filter(pr_ab == 1)
    
    # # Split into train and test datasets
    train_data_i$X<-seq_len(nrow(train_data_i))
    train_data_ib<- sample_frac(train_data_i, size= 0.70, replace = F)
    train_data_i<- as.data.frame(train_data_i)
    train_data_i$dataset<- NA
    train_data_i[train_data_ib$X, "dataset"]<- "train"
    train_data_i[is.na(train_data_i$dataset), "dataset"]<- "test"
    
    # train_data_i_sf <- (train_data_i) %>%
    #   st_as_sf(coords = c("x", "y"), crs = "EPSG:3005")
    # mapview::mapview(train_data_i_sf, cex=2, add=T, zcol= "dataset", col.regions= c("red", "blue"))# a visual check
    
    
    loxTrain<- train_data_i[which(train_data_i$dataset== "train"), ]
    loxTest.L1 = train_data_i[which(train_data_i$dataset== "test"), ]
    
    
    # Creating the Analysis Data
    train_y = c(rep(1,nrow(train_data_i)), rep(0,nrow(background_values))) # Main Training Data
    train_sdm_data = cbind(pa = train_y, rbind(train_data_i[,9:length(colnames(train_data_i))-2], background_values[,3:ncol(background_values)])) # Main SDM Data
    
    start.time <- Sys.time()
    Model_i = maxent(train_sdm_data[,-1], p = train_y)#, path = paste(SDM_path,"outputs_maxent", sep="/"), args = c("-J", "-P"),#, "replicates=5"
    # file.rename("MaxEnt results", paste0("MaxEnt results ", i))
    
    end.time <- Sys.time()
    time.taken <- end.time - start.time
    time.taken
    
    list_models[[length(list_models) + 1]] <- Model_i
    list_models$names[length(list_models)]<- paste("maxent", i , sep="_")
    
    
    # Create output tablle for i
    results_maxent_i<- as.data.frame(matrix(ncol = 1, nrow = length(names(stack_vars))))
    colnames(results_maxent_i)<- c("replicate")
    results_maxent_i$replicate<- paste(i)
    # results_maxent_i$variable<- paste(names(stack_vars))
    
    vars_contrib_i<-  as.data.frame(Model_i@results) #Model_i@results[7:(length(names(stack_vars))+6)]#
    vars_contrib_i$variables<- row.names(vars_contrib_i)
    rownames(vars_contrib_i)<- NULL
    head(vars_contrib_i)
    colnames(vars_contrib_i)[1]<- "variable_contrib"
    
    if(selected_variables15== T){
      vars_permut_i<- vars_contrib_i[22:(21+length(names(stack_vars))),]#35:62
    }
    
    # if(selected_variables15== F){
    #   vars_permut_i<- vars_contrib_i[35:62,]
    # }
    
    vars_permut_i<- vars_permut_i %>% separate_wider_delim(variables, ".", names = c("variable", NA, NA))
    vars_permut_i<- as.data.frame(vars_permut_i[,c(1)])
    colnames(vars_permut_i)[1]<- "permutation_importance"
    
    
    vars_contrib_i<- vars_contrib_i[1:length(names(stack_vars))+6,]
    vars_contrib_i<- vars_contrib_i %>% separate_wider_delim(variables, ".", names = c("variable", NA))
    vars_contrib_i<- as.data.frame(vars_contrib_i[,c(2,1)])
    
    # vars_contrib_i<- Model_i@results[7:(length(names(stack_vars))+6)]
    # results_maxent_i$variable_contrib<- as.numeric(str_split_fixed(vars_contrib_i, ' ', 1))
    results_maxent_i$model<- "Maxent"
    results_maxent_i<- cbind(results_maxent_i, vars_contrib_i, vars_permut_i)
    results_maxent<- rbind(results_maxent, results_maxent_i)
    
    # Doing Model Evaluation
    mod.evT1 <- dismo::evaluate(Model_i, p = loxTest.L1, a = background_values[, 3:ncol(background_values)])
    mod.evals <- list(mod.evals, mod.evT1)
    
    # Capturing the threshold values
    output$mod_replicate[i] <- i
    output$threshold.SS[i]  <- threshold(mod.evT1, "spec_sens")
    output$threshold.NoO[i] <- threshold(mod.evT1, "no_omission")
    output$threshold.ESS[i] <- threshold(mod.evT1, "equal_sens_spec")
    
    # Capturing the AUC values
    output$AUC[i] <- mod.evT1@auc
    
    Loop.r <- predict(stack_vars, Model_i, progress='text', na.rm=TRUE)
    Loop.r<- raster(Loop.r)
    plot(Loop.r)
    names(Loop.r)<- paste("mod_rep", i, sep="_")
    x <- stack(x, Loop.r)
    print(paste("finished model",i))
  }
  
  # mean_pred<- mean(x)
  # sd_pred<- sd(x)
  return(list(variables_importance=results_maxent, models=list_models, mod_performance=output, mod.evaluation= mod.evals, preds=x))#, mean_preds= mean_pred, sd_preds= sd_pred
}

