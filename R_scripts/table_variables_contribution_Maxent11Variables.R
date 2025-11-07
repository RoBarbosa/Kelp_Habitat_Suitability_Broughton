###==================================================================
###     Species Distribution models    SDMs                     ################
###                                                             ################
### Author: Romina Barbosa                                      ################
### Date: 23-Dec-2024                                           ################
### Last edition: 23-Dec-2024  
###==================================================================


load("C:/Users/romi_/OneDrive - University of Victoria/Kelp_postdoc/SDMs/outputs_SDMs/Sept2024/50repMaxent_18_11_4variables.RData")


variables_importance_002<- model_replicates_002$variables_importance
unique(variables_importance_002$variable)
str(variables_importance_002)

table_variables_importance_summary<- variables_importance_002%>% group_by(model, variable)%>%
  summarize(mean_var_importance= mean(permutation_importance), sd_var_importance= sd(variable_contrib),
            mean_var_contribution= mean(variable_contrib),
             sd_var_importance= sd(variable_contrib))

# model  variable    mean_var_importance sd_var_importance mean_var_contribution
# 1 Maxent TPI                        4.51              4.01                  6.55
# 2 Maxent eastherness                8.66              3.93                  9.76
# 3 Maxent janBT_ave                 16.3               3.02                 12.0 
# 4 Maxent julBS_ave                  8.18              2.75                  8.55
# 5 Maxent julBSpd_max                6.76              2.13                  4.38
# 6 Maxent julBT_ave                  6.63              1.54                  3.75
# 7 Maxent julBT_min                 11.4               2.59                  8.08
# 8 Maxent julSSpd_min                6.19              4.71                 14.9 
# 9 Maxent julST_ave                  8.91              1.39                  4.11
# 10 Maxent slope                      4.70              2.65                  5.21
# 11 Maxent tidal_cur                 17.8               6.87                 22.7 


a002<- variables_importance_002%>%
  ggplot(aes(y= variable, x= variable_contrib))+
  geom_boxplot(outlier.shape = NA, color="black")+
  geom_jitter(width = 0.15, size=0.5, alpha=0.5)+
  labs(y="Environmental Variable", x= "Importance (%)")+
  theme(legend.position = "NULL")+theme_bw()

b002<- variables_importance_002%>%
  ggplot(aes(y= variable, x= permutation_importance))+
  # geom_boxplot(outlier.shape = NA, color="blue")+
  # geom_jitter(width = 0.15, size=0.5, alpha=0.5)+
  geom_boxplot(aes(y= variable, x= permutation_importance), color="black",outlier.shape = NA, )+
  geom_jitter(width = 0.15, size=0.5, alpha=0.5)+
  labs(y="Environmental Variable", x= "Permutation Importance")+
  theme(legend.position = "NULL")+theme_bw()
  # geom_vline(xintercept=8, linetype="dashed",
  #            color = "red", size=1)+ theme_bw()

cowplot::plot_grid(a002,b002, labels = c("A)","B)"))
# ggsave("SDMs_13Variables_Importance_boxplots.png", units="cm", width=20, height= 14, dpi=300)
