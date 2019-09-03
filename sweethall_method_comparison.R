library(sf)
library(dplyr)
library(tidyr)
library(ggplot2)

neg<-st_read("sweethall_neg_elev.shp")
neg<-neg %>% 
  arrange(ROUTEID, MEASURE_LO) %>% 
  mutate(type="neg")%>% 
  select(-OFFSET)
st_geometry(neg)<-NULL
neg<-distinct(neg)
View(neg)

pos<-st_read("sweethall_pos_elev.shp")
pos<-pos %>% 
  arrange(ROUTEID,MEASURE_LO)%>% 
  mutate(type="pos") %>% 
  select(-OFFSET, -POINTS)
st_geometry(pos)<-NULL
pos<-distinct(pos)
View(pos)

mid<-st_read("sweethall_mid_elev.shp")
mid<-mid %>% 
  arrange(ROUTEID,MEASURE_LO)%>% 
  mutate(type="mid")%>% 
  select(-OFFSET, -POINTS)
st_geometry(mid)<-NULL
mid<-distinct(mid)
View(mid)

transect_values<- 
  full_join(neg, mid) %>% 
  full_join(., pos) %>% 
  group_by(ROUTEID, MEASURE_LO) %>% 
  spread(key=type, value=RASTERVALU)
View(transect_values)

forest<-transect_values$neg - transect_values$mid
fores<-forest/25
mean(fores)
sd(fores)
summary(fores)

marsh<-transect_values$mid-transect_values$pos
mars<-marsh/25
mean(mars)
sd(mars)

sweethall<-data.frame("method"=c("slope_1m", "slope_2m", "transect"),
                      "marsh_mean"=c(0.04, 0.024,0.0057),
                      "marsh_sd"=c(0.0383, 0.0302, 0.0166),
                      "marshQ1"=c(0.0307, 0.016, 0.0012),
                      "marshQ3"=c(0.0503, 0.0265, 0.0089),
                      "upland_mean"=c(0.103, 0.086, 0.056),
                      "upland_sd"=c(0.099, 0.099, 0.077),
                      "uplandQ1"=c(0.062, 0.0335, 0.00062),
                      "uplandQ3"=c(0.167, 0.172, 0.118))
jitter<-data.frame("method"="transect",
                   "slope"=mars)
jitter2<-data.frame("method"="transect",
                    "slope"=fores)
View(sweethall)
a<-ggplot(sweethall, aes(x=method))
b<-a+
  geom_point(aes(y=marsh_mean), shape=17, color="darkblue", size=2)+
  geom_errorbar(aes(ymax=marsh_mean+marsh_sd, ymin=marsh_mean-marsh_sd))+
  geom_jitter(data=jitter, aes(x=method, y=slope), size=0.5, alpha=0.2)+
  geom_jitter(data=marsh_2m, aes(x=method, y=slope), size=0.5, alpha=0.2)+
  geom_linerange(aes(ymax=marshQ3, ymin=marshQ1), size=6, color="darkgreen", alpha=0.5)+
  ggtitle("Marsh Method Comparison")+theme_minimal()+ylab("Slope")
b

c<-ggplot(sweethall, aes(x=method))
d<-c+
  geom_point(aes(y=upland_mean), shape=17, color="darkblue", size=2)+
  geom_errorbar(aes(ymax=upland_mean+upland_sd, ymin=upland_mean-upland_sd))+
  geom_jitter()
  geom_linerange(aes(ymax=uplandQ3, ymin=uplandQ1), size=6, color="darkgreen", alpha=0.5)+
  ggtitle("Upland Method Comparison")+theme_minimal()+ylab("Slope")+
  scale_y_continuous(limits = c(-0.05,0.3))
d
library(cowplot)
install.packages("cowplot")
e<-plot_grid(b,d)
e

marsh_2m<-st_read("data/sweethall_2m_marsh.shp")
st_geometry(marsh_2m)<-NULL
marsh_2m<-marsh_2m %>% 
  select(-pointid) %>% 
  mutate(method="slope_2m", 
         slope=grid_code/100) %>% 
  select(-grid_code)

marsh_1m<-st_read("sweethall_1m_marsh.shp")
st_geometry(marsh_1m)<-NULL
marsh_1m<-marsh_1m %>% 
  select(-pointid) %>% 
  mutate(method="slope_1m", 
         slope=grid_code/100) %>% 
  select(-grid_code)

marsh_all<-jitter %>% 
  full_join(., marsh_1m) %>% 
  full_join(., marsh_2m)

f<-ggplot(marsh_all, aes(x=method, y=slope))
g<-f+
  geom_violin(aes(fill=method))+theme_classic()+
  ggtitle("Marsh Method Comparison")+scale_y_continuous(limits = c(-0.2, 0.9))+
  theme(legend.position = "none")
g

upland_2m<-st_read("sweethall_2m_forest.shp")
st_geometry(upland_2m)<-NULL
upland_2m<-upland_2m %>% 
  select(-pointid) %>% 
  mutate(method="slope_2m", 
         slope=grid_code/100) %>% 
  select(-grid_code)

upland_1m<-st_read("sweethall_1m_forest.shp")
st_geometry(upland_1m)<-NULL
upland_1m<-upland_1m %>% 
  select(-pointid) %>% 
  mutate(method="slope_1m", 
         slope=grid_code/100) %>% 
  select(-grid_code)

upland_all<-jitter2 %>% 
  full_join(., upland_1m) %>% 
  full_join(., upland_2m)

h<-ggplot(upland_all, aes(x=method, y=slope))
i<-h+
  geom_violin(aes(fill=method))+theme_classic()+
  ggtitle("Upland Method Comparison")+scale_y_continuous(limits = c(-0.2, 0.9))
i

j<-plot_grid(g,i)
j
