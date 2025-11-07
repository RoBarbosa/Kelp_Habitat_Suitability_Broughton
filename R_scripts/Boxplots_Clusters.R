library(ggplot2)
library(dplyr)

### Cluster boxplots  #############################

path<- "/Volumes/Romina_BATI/BATI/SDMs"

############# Plots #############
df_cl<- read.csv(paste(path, "Clustering/6CLUSTERs_6Variables_07022024.csv", sep="/"))

# colnames(df_cl)[44]<- "clusters7"
str(df_cl)
df_cl$cluster<- as.factor(df_cl$cluster6)

colnames(df_cl)<- c("x", "Y", "wind_speed", "jul19_BTav", "jul19_STav","tidal_curr",
                    "jul19_BS_a", "jul19_SS_a", "cluster6", "cluster")

# colnames(df_cl)<- c("x", "Y","WIND SPEED (M.S-1)", "BOTTOM TEMPERATURE", "SURFACE TEMPERATURE",
# "TIDAL CURRENT (M.S-1)", "BOTTOM SALINITY (psu)", "SURFACE SALINITY (psu)", "cluster6", "cluster")
# write.csv(df_cl,"6CLUSTERs_6Variables_07022024.csv", row.names = FALSE)


#Tidal ampliture (m) from Foreman et al. 2000
p <- ggplot(df_cl, aes(x=cluster, y=tidal_curr, fill= cluster)) +
  # geom_violin()+
  geom_boxplot(width= 0.8, outlier.size = 0.5)+
  theme_bw()+
  scale_fill_manual(values=c("brown2", "purple", "seagreen2", "darkturquoise", "goldenrod2", "magenta2")) +#, "grey", "yellow3")) + #"#377eb8","#4daf4a","#e41a1c", "#ff7f00", "#984ea3", "grey"))+
  ylab("Tidal current (m.s-1)")+
  xlab("Cluster")+
  stat_summary(fun=mean, geom='point', shape=20, size = 3)+ 
  theme(legend.position = c(0.4, 0.8), legend.direction="horizontal")
# theme(legend.position = "none")
p

#Wind
q <- ggplot(df_cl, aes(x=cluster, y=wind_speed, fill= cluster)) + 
  # geom_violin()+
  geom_boxplot(width= 0.8, outlier.size = 0.5)+
  theme_bw()+
  scale_fill_manual(values=c("brown2", "purple", "seagreen2", "darkturquoise", "goldenrod2", "magenta2"))  +#, "grey", "yellow3")) +
  # scale_fill_manual(values=c("#377eb8","#4daf4a","#e41a1c", "#ff7f00", "#984ea3", "grey"))+ 
  ylab("Wind Power (W/m2)")+
  xlab("Cluster")+
  stat_summary(fun=mean, geom='point', shape=20, size = 3)+ theme(legend.position = "none")
q

# Average SST july 19
r <- ggplot(df_cl, aes(x=cluster, y=jul19_STav, fill= cluster)) + 
  # geom_violin( na.rm= TRUE)+ 
  geom_boxplot(width= 0.8, outlier.size = 0.5)+
  theme_bw()+
  # scale_fill_manual(values=c("#377eb8","#4daf4a","#e41a1c", "#ff7f00", "#984ea3", "grey"))+ 
  scale_fill_manual(values=c("brown2", "purple", "seagreen2", "darkturquoise", "goldenrod2", "magenta2")) +#, "grey", "yellow3")) +
  ylab("Summer Ave. Surface Temp. (°C)")+
  xlab("Cluster")+
  ylim(c(0, 18))+
  stat_summary(fun=mean, geom='point', shape=20, size = 3)+ theme(legend.position = "none")
r
# r+  jpeg("sstspring_ClusJune22.jpeg", units="in", width=5, height=3.5, res=300)
# dev.off()

#sst summer
n <- ggplot(df_cl, aes(x=cluster, y=jul19_BTav, fill= cluster)) + 
  # geom_violin()+
  geom_boxplot(width= 0.8, outlier.size = 0.5)+
  scale_fill_manual(values=c("brown2", "purple", "seagreen2", "darkturquoise", "goldenrod2", "magenta2")) +#, "grey", "yellow3")) +
  ylab("Summer Ave. Bottom temp. (°C)")+
  xlab("Cluster")+ theme_bw()+
  ylim(c(0, 16))+
  stat_summary(fun=mean, geom='point', shape=20, size = 3)+ theme(legend.position = "none")
n


# July 19 Surface salinity
ssa <- ggplot(df_cl, aes(x=cluster, y=jul19_SS_a, fill= cluster)) + 
  # geom_violin( na.rm= TRUE)+ 
  theme_bw()+
  geom_boxplot(width= 0.8, outlier.size = 0.5)+
  # scale_fill_manual(values=c("#377eb8","#4daf4a","#e41a1c", "#ff7f00", "#984ea3", "grey"))+ 
  scale_fill_manual(values=c("brown2", "purple", "seagreen2", "darkturquoise", "goldenrod2", "magenta2"))  +#, "grey", "yellow3")) +
  ylab("Summer Ave. Surface Salinity (psu)")+
  xlab("Cluster")+
  ylim(c(10, 35))+
  stat_summary(fun=mean, geom='point', shape=20, size = 3)+ theme(legend.position = "none")
ssa

# July 19 Bottom salinity
bsa <- ggplot(df_cl, aes(x=cluster, y=jul19_BS_a, fill= cluster)) + 
  # geom_violin()+
  geom_boxplot(width= 0.8, outlier.size = 0.5)+
  scale_fill_manual(values=c("brown2", "purple", "seagreen2", "darkturquoise", "goldenrod2", "magenta2"))  +#, "grey", "yellow3")) +
  ylab("Summer Ave. Bottom Salinity (psu)")+
  xlab("Cluster")+ theme_bw()+
  ylim(c(10, 35))+
  stat_summary(fun=mean, geom='point', shape=20, size = 3)+ theme(legend.position = "none")
# theme(legend.position = c(0.5, 0.25), legend.direction="horizontal")

bsa

# plot.new()
cowplot::plot_grid(r,#spring sst
                   n,#summer sst
                   p,#tidal amp
                   ssa, #Salinity
                   bsa,# Salinity
                   q, #wind
                   # nn, 
                   # nnp, #spring sst
                   # nnq, #summer sst
                   # labels = c("A", "B", "C", "D", "E", "F"),#,"G"),
                   ncol = 3, nrow = 2)

# Save plot
setwd(path)
dir()
# ggsave(paste(path, "Clustering/Clusters_6vars_boxplots_v2.pdf", sep="/"), units="cm", width=20, height= 15, dpi=300)
# ggsave("6Clusters_6vars_violins_07022024.jpeg", units="cm", width=25, height= 16, dpi=300)

