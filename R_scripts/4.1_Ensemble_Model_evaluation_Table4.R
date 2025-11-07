###==================================================================
###     Results from Species Distribution models SDMs                ###########
###                                                                  ###########
### 4.1- Ensemble model evaluation of PERFORMANCE w/independent data ###########
### a- predict suitability at validation points for each model; 
### b- create table with outputs;
### 
### Author: Romina Barbosa                                           ###########
### Date: 13-March-2025                                              ###########
### Last edition: 13-March-2025
### Part of code from: https://rspatial.org/raster/sdm/5_sdm_models.html
###==================================================================





###==========================================================================###
### Extract Model predictions at testing data locations ========================
###==========================================================================###

## Testing the model with independent data =====================================
# satellite presences and absences from 2016-2017 to validate the model
# all_sat_validation_pts_FINAL_.csv

# val_pts <- read.csv("C:/Users/romi_/OneDrive - University of Victoria/Kelp_postdoc/SDMs/SDMs_validation/Sept2024/pred_eval_data_sat_exclude_confusingPoints.csv")
val_pts <- read.csv("/Volumes/Romina_BATI/BATI/SDMs/SDMs_validation/Sept2024/pred_eval_data_sat_exclude_confusingPoints.csv")
head(val_pts)

# Exclude the points at limiting positions between pixels at margings of bynary prediction
# these could add artifacts due to potential errors of small differences in values that when 
# creating the bynary maps based on thresholds are wrongly classified.
val_pts<- val_pts%>% filter(limit_cell!="Y")
length(val_pts$pr_ab) #total of 1947 ponts

val_pts_sf <- val_pts %>%
  st_as_sf(coords = c("lon", "lat"), crs = 3005)
mapview::mapview(val_pts_sf, cex=2,  zcol="pr_ab", col.regions= c("red", "blue"))# a visual check

pred_test_sat<- sdm_extract(data= val_pts, 
                            x = "lon",
                            y = "lat",
                            env_layer = rast(model_cl_stack),
                            variables = NULL,
                            filter_na = FALSE)


pred_test_sat<- sdm_extract(data= pred_test_sat, 
                            x = "lon",
                            y = "lat",
                            env_layer = rast(models_stack_masked),
                            variables = NULL,
                            filter_na = FALSE)

colnames(pred_test_sat)
pred_test_sat<- sdm_extract(data= pred_test_sat,
                            x = "lon",
                            y = "lat",
                            env_layer = rast(models_stack_maskedSub_Bathy),
                            variables = NULL,
                            filter_na = FALSE)


# write.csv(pred_test_sat, "C:/Users/romi_/OneDrive - University of Victoria/Kelp_postdoc/SDMs/SDMs_validation/March_2025/pred_testdata_satellite.csv")


## Testing the model with the 30% of data from 2023 ============================
# models_stack<- rast(models_stack)
pred_testdata<- sdm_extract(data= test_data, 
                            x = "x",
                            y = "y",
                            env_layer = rast(model_cl_stack),
                            variables = NULL,
                            filter_na = FALSE)


pred_testdata<- sdm_extract(data= pred_testdata, 
                            x = "lon",
                            y = "lat",
                            env_layer = rast(models_stack_masked),
                            variables = NULL,
                            filter_na = FALSE)

colnames(pred_testdata)
pred_testdata<- sdm_extract(data= pred_testdata,
                            x = "lon",
                            y = "lat",
                            env_layer = rast(models_stack_maskedSub_Bathy),
                            variables = NULL,
                            filter_na = FALSE)

# write.csv(pred_testdata, "C:/Users/romi_/OneDrive - University of Victoria/Kelp_postdoc/SDMs/SDMs_validation/March_2025/pred_testdata_30percent.csv")


# FP= length(which(pred_testdata$pr_ab==1 & pred_testdata$maxent_mean < thr_maxent$thrSS_mean))/length(pred_testdata$cluster6)




pred_testdata<- pred_testdata%>%
  group_by(cluster6, pr_ab)%>%
  mutate(n= length(pr_ab))

# pred_testdata <- data.frame(
#   gbm = predict(ensemble_models$models_gbm[[1]]$model, test_data[,subset], type='response'),
#   glm = as.vector(predict(ensemble_models$models_glm[[1]]$model, test_data[,subset], type='response')),
#   rf = predict(ensemble_models$models_rf[[1]]$model, test_data[,subset], type='response'),
#   maxent = predict(model_replicates_002$models[[1]], test_data[,subset], type="logistic")
# )
# pred_testdata<- cbind(test_data, pred_testdata)



###==========================================================================###
### Calculate AUC for Maxent ===================================================
p= c(pred_testdata[which(pred_testdata$pr_ab==1),"maxent_mean"])$maxent_mean
a= c(pred_testdata[which(pred_testdata$pr_ab==0),"maxent_mean"])$maxent_mean

comb <- pred_testdata$maxent_mean#c(p,a)
group <- pred_testdata$pr_ab #c(rep('presence', length(p$pr_ab)), rep('absence', length(a$pr_ab)))
boxplot(comb~group, col=c('blue', 'red'))

cor.test(comb, group)$estimate
##       cor
# 0.1112885 ## 0.1290877
mv <- wilcox.test(p,a)
auc_maxent <- as.numeric(mv$statistic) / (length(p) * length(a))
auc_maxent
# 0.565 ## 0.58

library(dismo) # Liu, C., M. White & G. Newell, 2011. Measuring and comparing the accuracy of species distribution models with presence-absence data. Ecography 34: 232-243.
e <- dismo::evaluate(p=p, a=a)
e
# class          : ModelEvaluation 
# n presences    : 40 
# n absences     : 40 
# AUC            : 0.565 
# cor            : 0.1112884768986 
# max TPR+TNR at : 0.5096546 

#  MArch 2025
# class          : ModelEvaluation 
# n presences    : 40 
# n absences     : 40 
# AUC            : 0.58 
# cor            : 0.1290877 
# max TPR+TNR at : 0.5506893 

par(mfrow=c(1, 2))
density(e)
boxplot(e, col=c('blue', 'red'), main="Maxent")



### Calculate AUC for RAF ===================================================
p= c(pred_testdata[which(pred_testdata$pr_ab==1),"RAF_mean"])$RAF_mean
a= c(pred_testdata[which(pred_testdata$pr_ab==0),"RAF_mean"])$RAF_mean

comb <- pred_testdata$RAF_mean#c(p,a)
group <- pred_testdata$pr_ab #c(rep('presence', length(p$pr_ab)), rep('absence', length(a$pr_ab)))
boxplot(comb~group, col=c('blue', 'red'))

cor.test(comb, group)$estimate
##       cor
## 0.9896772864238  ## 0.9896773
mv <- wilcox.test(p,a)
auc_raf <- as.numeric(mv$statistic) / (length(p) * length(a))
auc_raf
# 0.6402

AUC_raf # 0.6178125 for training data ## 0.6356


e2 <- dismo::evaluate(p=p, a=a)
e2
# class          : ModelEvaluation 
# n presences    : 40 
# n absences     : 40 
# AUC            : 1 
# cor            : 0.9896772864238 
# max TPR+TNR at : 0.71190001 

par(mfrow=c(1, 2))
density(e2)
boxplot(e2, col=c('blue', 'red'), main="RAF")



### Calculate AUC for Ensemble ===================================================
p= c(pred_testdata[which(pred_testdata$pr_ab==1),"Ensemble"])$Ensemble
a= c(pred_testdata[which(pred_testdata$pr_ab==0),"Ensemble"])$Ensemble

comb <- pred_testdata$Ensemble#c(p,a)
group <- pred_testdata$pr_ab #c(rep('presence', length(p$pr_ab)), rep('absence', length(a$pr_ab)))
boxplot(comb~group, col=c('blue', 'red'))

cor.test(comb, group)$estimate
##       cor
## 0.7806281
mv <- wilcox.test(p,a)
auc_Ensemble <- as.numeric(mv$statistic) / (length(p) * length(a))
auc_Ensemble
# 0.9481 ## 0.949375

e_3 <- dismo::evaluate(p=p, a=a)
e_3
# class          : ModelEvaluation 
# n presences    : 40 
# n absences     : 40 
# AUC            : 0.948125 
# cor            : 0.7806280882524 
# max TPR+TNR at : 0.47021345 

par(mfrow=c(3, 2))
density(e)
boxplot(e, col=c('blue', 'red'), main="Maxent")
density(e2)
boxplot(e2, col=c('blue', 'red'), main="RAF")
density(e_3)
boxplot(e_3, col=c('blue', 'red'), main="Ensemble")


i=2
eva_list<- list()
eva_df<- as.data.frame(matrix(ncol=3, nrow=0))
colnames(eva_df)<- c("model","cluster", "AUC")

e_data<- pred_testdata[which(pred_testdata$cluster6== 1),]

for (m in c(colnames(e_data)[38:40])) {
  eva_df_m<- as.data.frame(matrix(ncol=3, nrow=0))
  colnames(eva_df_m)<- c("model","cluster", "AUC")
  
  for (i in 2:6) {
    e_data<- pred_testdata[which(pred_testdata$cluster6 == i),]
    p= c(e_data[which(e_data$pr_ab==1),m])[[1]]
    a= c(e_data[which(e_data$pr_ab==0),m])[[1]]
    
    e_i <- evaluate(p=p, a=a)
    eva_list[[i-1]]<- e_i
    
    eva_df_m[i-1,"model"]<- m
    eva_df_m[i-1,"cluster"]<- i
    eva_df_m[i-1,"AUC"]<- e_i@auc
  }
  
  eva_df<- rbind(eva_df, eva_df_m)
}

eva_df$model<- as.factor(eva_df$model)
levels(eva_df$model)<- c("Ensemble", "Maxent", "RAF")
eva_df$cluster<- as.factor(eva_df$cluster)


eva_df$cluster<- as.factor(eva_df$cluster)

ggplot(eva_df, aes(x= cluster, y=AUC, fill=cluster))+
  geom_bar(stat = "identity", width=0.6)+
  facet_wrap(~model)+theme_bw()
# ggsave(paste(path_model_res,"AUC_x_model_x_cluster.pdf", sep="/"), width = 13, height = 6, units="cm", dpi = 300)
# write.csv(eva_df, paste(path_model_res,"/evaluation_cluster_df.csv", sep="/"))


