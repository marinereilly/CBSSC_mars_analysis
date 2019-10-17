library(dplyr)
library(broom)
library(ggplot2)
library(purrr)
library(tidyr)
library(sf)

#load data
filenames <- list.files("data", pattern="*.shp", full.names = FALSE)
file_names<-tools::file_path_sans_ext(filenames)

goodwin_1m_forest_points<-sf::st_read("data/goodwin_1m_forest_points.shp")
sf::st_geometry(goodwin_1m_forest_points)<-NULL
goodwin_1m_forest_points<-goodwin_1m_forest_points %>% 
  mutate(site_id="goodwin_1m_forest_points") %>% 
  separate(site_id, c("site", "method", "habitat", "delete"), "_") %>% 
  mutate(slope=grid_code/100) %>% 
  select(-delete, -pointid, -grid_code)

my_function<-function(x){
  st_read(paste0("data/",x,".shp"))
  }

g<-data.frame(file_names)
View(g)
g<-g %>% 
  group_by(file_names) %>% 
  mutate(data= map(g$file_names, my_function))

#
bigDF<-list.files('data', pattern='.+\\.shp', full.names = TRUE) %>% 
  map_dfr(function(f){
    site_id <- f %>% 
      basename() %>% 
      tools::file_path_sans_ext()
    sf::read_sf(f) %>% 
      sf::st_set_geometry(NULL) %>% 
      mutate(site_id = site_id) %>% 
      separate(site_id, c("site", "method", "habitat", "delete"), "_") %>% 
      select(-delete, -pointid)})
bigDFb<-bigDF %>% 
  mutate(slope=grid_code/100) %>% 
  select(-OBJECTID, -grid_code) 
#
bigDF_plot<-bigDFb %>% 
  ggplot(aes(x=site, y=slope, fill=method))+
  geom_boxplot()+facet_wrap(~habitat)+theme_classic()
bigDF_plot

bigDF_violin<-bigDFb %>% 
  ggplot(aes(x=site, y=slope, fill=method))+
  geom_violin()+facet_wrap(~habitat)+theme_classic()
bigDF_violin

#
#library(arsenal)
#
#my_controls<-tableby.control(numeric.stats = c("meansd", "medianq1q3", "iqr", "N"),
#                             stats.labels = list(
 #                              meansd = "Mean (SD)",
  #                             medianq1q3 = "Median (Q1, Q3)",
   #                            iqr = "IQR",
    #                           N= "N count"
     #                        ))

#table<-tableby(slope ~ habitat + interaction(site,method), 
 #                       data = bigDFb, 
  #                      control=my_controls)
#statisticsDF<-as.data.frame(table)
#View(statisticsDF)

#
stats_table<- bigDFb %>%
  group_by(site, method, habitat) %>% 
  summarise(mean=mean(slope),
            median=median(slope),
            sd=sd(slope),
            min=min(slope),
            max=max(slope),
            n=n(),
            IQR=IQR(slope),
            Q3 = quantile(slope, probs=3/4),
            Q1 = quantile(slope, probs=1/4)) %>% 
  arrange(site, habitat, method)

write.csv(stats_table,"slope_stats.csv")

library("htmltools")
library("webshot")    

export_formattable <- function(f, file, width = "100%", height = NULL, 
                               background = "white", delay = 0.2)
{
  w <- as.htmlwidget(f, width = width, height = height)
  path <- html_print(w, background = background, viewer = NULL)
  url <- paste0("file:///", gsub("\\\\", "/", normalizePath(path)))
  webshot(url,
          file = file,
          selector = ".formattable_widget",
          delay = delay)
}


library(formattable)
table1<-formattable(stats_table,
                    align= c("r", "c", "l", "c","c","c","c","c","c","c","c"),
                    list(`site` =formatter(
                      "span", style = ~ style(color= "grey59", font.weight = "bold"))))
table1
export_formattable(table1, "stats_table.pdf")


a<-stats_table %>% 
  ggplot(aes(x=site, color=method))
stats_plot<-a+
  geom_errorbar(aes(ymax=mean+sd, ymin=mean-sd))+
  geom_linerange(aes(ymax=Q3, ymin=Q1), size=6, alpha=0.5)+
  geom_point(aes(y=mean), shape=17, size=2)+
  geom_point(aes(y=mean), shape=2, color="black", size=2)+
  ggtitle("Method Comparison")+theme_classic()+ylab("Slope")+facet_grid(.~habitat)
stats_plot  
    
#
bigDF_nest<-bigDFb %>% 
  group_by(site, method, habitat) %>% 
  nest(slope, .key="data") 
bigDF_nest2<-stats_table%>%
  ungroup() %>% 
  select(site, method, habitat, mean, sd) %>% 
  full_join(., bigDF_nest) %>% 
  mutate(plot_name = paste0(site, " ", method, " ", habitat)) %>% 
  select(plot_name, mean, sd, data)
bigDF_nest3<-bigDF_nest2 %>% 
  mutate(plot = pmap(., ~ggplot(data=..4, aes(x=slope))+
                       geom_histogram(binwidth = 0.005, color="black",fill="olivedrab2")+
                       ggtitle(..1)+
                       geom_vline(xintercept = ..2)+
                       geom_vline(xintercept = ..2-..3, linetype="dashed")+
                       geom_vline(xintercept = ..2+..3, linetype="dashed")+
                       theme_minimal()
                       ))
bigDF_nest3$plot[[2]]

if(!dir.exists("./figures")){ #if a figures folder does not exist, create it.
  dir.create("./figures")
}
#use the map function with ggsave to save named figures. 
dir.create("./figures/slope_plots")
map2(paste0("./figures/slope_plots/", bigDF_nest3$plot_name, ".jpg"), bigDF_nest3$plot, ggsave)



bigDF_nest4<-bigDF_nest3 %>% 
  select(plot_name, plot) %>% 
  separate(plot_name, c("site", "method", "habitat"), " ")
bignest<-bigDF_nest4 %>% 
  group_by(site, habitat) %>% 
  spread(method, plot)
bignest2<-bignest %>% 
  mutate(plot = pmap(.,~ ggstatsplot::combine_plots(..3, 
                                                  ..4,
                                                  labels= c("1m", "2m"),
                                                  title.text = paste0(..1, " ", ..2),
                                                  ncol = 1)
    
  ))
