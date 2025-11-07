
### Save scaling parameters from training dataset =========================================
# We need the original training data to scale with same parameters
library(dplyr)
train<- read.csv(paste(model_results_path,"train_selected_table_FINALMODELS_M7.csv", sep="/"))
train<- train[,-1]
names(train)

train_sel <- train %>% dplyr::select(all_of(c("kelp", vars_selected)))
train_sel$kelp<- as.factor(train_sel$kelp)

train_scaled_b <- scale(train_sel[, vars_selected])
train_means <- attr(train_scaled_b, "scaled:center")
train_sds   <- attr(train_scaled_b, "scaled:scale")


# Combine means and sds into one data frame
scale_params <- as.data.frame(t(c(
  setNames(train_means, paste0(names(train_means), "_mean")),
  setNames(train_sds,   paste0(names(train_sds), "_sd"))
)))
