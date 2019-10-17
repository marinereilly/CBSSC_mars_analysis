library(googlesheets)
library(googledrive)
library(dplyr)

data<-
  drive_get(path="Mars Light Metrics - CERF 2019",
            team_drive=as_id("0AOTYwjhFwG_YUk9PVA")) %>% 
  select(id) %>% 
  combine() %>% 
  gs_key(lookup = FALSE,
         visibility = "private") %>% 
  gs_read_csv()

View(data)

data<-data[-1,]
data2<-data %>% 
  mutate(h_slr=as.numeric(h_slr),
         c_slr=as.numeric(c_slr),
         f_slr=as.numeric(f_slr),
         set_rate=as.numeric(set_rate),
         shore_dsas=as.numeric(shore_dsas),
         forest_dsas=as.numeric(forest_dsas)) %>% 
  mutate(net_lchange=forest_dsas-shore_dsas)

slope<-
  drive_get(path="slope_stats",
            team_drive=as_id("0AOTYwjhFwG_YUk9PVA")) %>% 
  select(id) %>% 
  combine() %>% 
  gs_key(lookup = FALSE,
         visibility = "private") %>% 
  gs_read_csv()

View(slope)

slope<-slope %>% 
  mutate(site= case_when(
    site=="goodwin"     ~ "Goodwin Island",
    site=="jugbay1"     ~ "Jug Bay 1",
    site=="jugbay2"     ~ "Jug Bay 2",
    site=="jugbay4"     ~ "Jug Bay 4",
    site=="monie3"      ~ "Monie Bay 3",
    site=="monie4"      ~ "Monie Bay 4",
    site=="nanticoke"   ~ "Nanticoke Creek",
    site=="parkers"     ~ "Parkers Creek",
    site=="phillips"    ~ "Phillips Creek",
    site=="serc"        ~ "SERC",
    site=="sweethall"   ~ "Sweethall Marsh"
  ))

data3<- slope%>%
  filter(method=="2m") %>% 
  filter(habitat=="forest") %>% 
  select(-method, -habitat) %>% 
  full_join(data2, .) %>% 
  select(-X1)
data4<- data3 %>% 
  mutate(h_tran_mean=h_slr/mean/1000,
         h_tran_q1=h_slr/Q1/1000,
         h_tran_q3=h_slr/Q3/1000,
         h_tran_med=h_slr/median/1000,
         c_tran_mean=c_slr/mean/1000,
         c_tran_q1=c_slr/Q1/1000,
         c_tran_q3=c_slr/Q3/1000,
         c_tran_med=c_slr/median/1000,
         f_tran_mean=f_slr/mean/1000,
         f_tran_q1=f_slr/Q1/1000,
         f_tran_q3=f_slr/Q3/1000,
         f_tran_med=f_slr/median/1000
         ) %>% 
  group_by(site) %>% 
  gather(key = scenario, value = t_pot, 25:36) %>% 
  separate(scenario, into = c("scenario", "delete", "m_type"), sep = "_") %>% 
  select(-IQR, -delete) %>% 
  arrange(site, scenario)
d4nest<-data4 %>% 
  mutate(site_scenario=paste0(site, "-",scenario)) %>% 
  group_by(site) %>% 
  nest()
d4plot<-d4nest %>% 
  mutate(plot=map2(site, data, ~ggplot(.y, aes(x=site_scenario))+
                     geom_point(aes(y=t_pot, shape=m_type))+
                     geom_point(aes(y=abs(forest_dsas)), size=2, shape=23, fill="navy")+
                     ggtitle(.x)
      ))
d4plot$plot[[2]]
cols<-c("Q1-Q3"="black", "DSAS"="navy", "Mean"="sandybrown")
data5<-data4 %>% 
  mutate(site_scenario=paste0(site, "-",scenario)) %>%
  select(-mean, -median, -Q1, -Q3) %>% 
  spread(m_type, t_pot) %>% 
  drop_na(forest_dsas)
data5$site_scenario<-factor(data5$site_scenario, levels = c("Goodwin Island-h",
                                                        "Goodwin Island-c",
                                                        "Goodwin Island-f",
                                                        "Jug Bay 1-h",
                                                        "Jug Bay 1-c",
                                                        "Jug Bay 1-f",
                                                        "Jug Bay 2-h",
                                                        "Jug Bay 2-c",
                                                        "Jug Bay 2-f",
                                                        "Jug Bay 4-h",
                                                        "Jug Bay 4-c",
                                                        "Jug Bay 4-f",
                                                        "Monie Bay 3-h",
                                                        "Monie Bay 3-c",
                                                        "Monie Bay 3-f",
                                                        "Monie Bay 4-h",
                                                        "Monie Bay 4-c",
                                                        "Monie Bay 4-f",
                                                        "Nanticoke Creek-h",
                                                        "Nanticoke Creek-c",
                                                        "Nanticoke Creek-f",
                                                        "Parkers Creek-h",
                                                        "Parkers Creek-c",
                                                        "Parkers Creek-f",
                                                        "Phillips Creek-h",
                                                        "Phillips Creek-c",
                                                        "Phillips Creek-f",
                                                        "SERC-h",
                                                        "SERC-c",
                                                        "SERC-f",
                                                        "Sweethall Marsh-h",
                                                        "Sweethall Marsh-c",
                                                        "Sweethall Marsh-f"))
trans_plot<-data5 %>% 
  ggplot(aes(x=site_scenario))+
    geom_linerange(aes(ymin=q3, ymax=q1, color="Q1-Q3"))+
    geom_point(aes(y=abs(forest_dsas), fill="DSAS"),  size=2, shape=22)+
    geom_point(aes(y=mean, fill="Mean"), size=1.5,shape=21)+
    theme_classic()+xlab("Site and Scenario")+
    ylab("Transgression Potential (m/yr)")+
    scale_color_manual(values=cols)+
    scale_fill_manual(values=cols)+
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
trans_plot
