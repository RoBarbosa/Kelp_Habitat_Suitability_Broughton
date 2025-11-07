

library(dplyr)
library(ggplot2)
data<- read.csv("/Volumes/Romina_BATI/BATI/Reports/Paper1_SDMs/Figures/Table_variables_importance_Figure_5.csv")
head(data)
unique(data$model)
data$variable<- as.factor(data$variable)
levels(data$variable)<- c("Eastherness"  ,                           "Slope (degrees)" ,                                  
                          "Average Summer Bottom Salinity (psu)",    "Maximum Summer Bottom Speed (m.s-1)",    
                           "Minimum Summer Bottom Temperature (°C)", "Average Summer Surface Temperature (°C)",
                          "Minimum Summer Surface Speed (m.s-1)",    "Tidal Current (m.s-1)" ,                 
                          "Topographic Position Index, TPI",         "Average Winter Bottom Temperature (°C)" ,
                          "Minimum Winter Surface Speed (m.s-1)" )
                          
levels(data$variable)

data_summary<- data%>%
  group_by(variable)%>%
  summarize(var_contrib_mean = mean(variable_contrib),
            var_contrib_SD = sd(variable_contrib),
            perm_import_mean = mean(permutation_importance),
            perm_import_SD = sd(permutation_importance)) 
  
# write.csv(data_summary, "/Volumes/Romina_BATI/BATI/Reports/Paper1_SDMs/Variables_importance_summary.csv")

data <- data %>%
  mutate(variable = reorder(variable, permutation_importance))

data$var_category<- data$variable
levels(data$var_category)<- c("Topographic",         "Topographic",                        
                              "Salinity",    "Water dynamics",   
                              "Topographic",    "Water dynamics" ,   
                              "Water dynamics" ,   "Temperature", 
                              "Temperature",  "Temperature",
                              "Water dynamics"  )

levels(data$var_category)
# [1] "Topographic"    "Salinity"       "Water dynamics" "Temperature"   


data%>%
  ggplot(aes(y=variable, x=permutation_importance, fill=var_category))+
  geom_boxplot(outlier.size = 0.3)+
  geom_jitter(size=0.3)+
  scale_fill_manual(values=c("magenta2", "turquoise2", "seagreen2", "orange2"))+
  labs(y="", x="Permutation Importance (%)", fill= "Variable Type")+
  xlim(c(0,50))+
  theme_bw()+
  theme(legend.position = c(0.82, 0.7),
        legend.text = element_text(size = 8),
        axis.text.x = element_text(size = 8, hjust = 1),
        axis.text.y = element_text(size = 8),
        axis.title = element_text(size = 9),
        legend.key.size = unit(0.8, "lines"))

# ggsave("/Volumes/Romina_BATI/BATI/Reports/Paper1_SDMs/Figures/Figure_5.pdf", width = 16, height = 7, dpi= 300, units="cm")
# ggsave("/Volumes/Romina_BATI/BATI/Reports/Paper1_SDMs/Figures/Figure_5.png", width = 16, height = 7, dpi= 300, units="cm")



          