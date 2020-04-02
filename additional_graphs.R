
countries <- c(
  "Italy",
  "Switzerland",
  "Germany",
  "France",
  "Austria",
  'Iran',
  "United Kingdom",
  "United States of America"
  # , "Japan"
  # , "Mainland China"
)



country_colors<-c(
  # "China" = "#EB5E8D",
  "United Kingdom" = "#556B2F"
  , "US" = "#000000"
  , "Italy" = "#FF0000"
  , "France" = "#0000FF"
  , "Germany" = "#FFA500"
  # , "Hong Kong" = "#1E8FCC"
  , "Iran" = "#9dbf57"
  # , "Japan" = "#208fce"
  # , "Singapore" = "#1E8FCC"
  # , "Korea, South" = "#208fce"
  , "Belgium" = "#c2b7af"
  , "Netherlands" = "#c2b7af"
  # , "Norway" = "#c2b7af"
  , "Spain" = "#8A2BE2"
  , "Sweden" = "#c2b7af")
  # , "Switzerland" = "#c2b7af"))

#############################################################################
titletext <- paste0("Top Countries with New Cases","\n","Created: ",Sys.time())

analysis <- corona %>% 
  group_by(Country, Date) %>%
  filter(Country %in% c("Italy","United Kingdom","US","Iran",
                       "Germany", 'France', "Spain",
                         "Germany","Belgium", "Netherlands","Sweden" )) %>%
  summarise(total_cases = sum(Confirmed, na.rm = T)) %>%
  mutate(new = total_cases - lag(total_cases))


library(glue)
title_top<-glue::glue("Top Countries as at {max(analysis$Date)}")

max(analysis$total_cases)

plot8<-ggplot(analysis) + 
  geom_line(aes(x = Date, y= new, col = Country),size = 1.2) +
  theme_classic() +
  scale_y_continuous(labels = scales::comma, expand = c(0,0)) +
  labs(title = title_top,
       subtitle = subtitletext, 
       caption = captiontext,
       y = "New cases per day") +
  scale_color_manual(values = country_colors) +
    theme(axis.title = element_text(colour = "#000000", size = 14,
                                    family = "Rockwell", hjust = 0.5,
                                    vjust = 0.7),
          axis.text = element_text(color = "#000000", size =10, 
                                   family = "Rockwell"  ),
          title = element_text(colour = "#000000", size = 14,
                               family = "Rockwell", hjust = 0.3,
                               vjust = 0.3)) +
  annotate("text", x=as.Date('2020-02-15'), y = 10000, label = "USA passes Italy,\n but Italy slows and Spain!")


# scale_colour_manual(values = c('#FF0000', '#FFA500','#000000',
#                                '#808000', '#00FFFF', '#E67451', '#7F525D',
#                                "#DAA520","#191970")) 
plot8
max(analysis$new, na.rm = T)

plot9<-ggplot(analysis) + 
  geom_line(aes(x = Date, y= total_cases, col = Country),size = 1.2) +
  theme_classic() +
  scale_y_continuous(limits = c(0,170000), expand = c(0, 0),label =comma,
                     breaks =seq(0,170000,10000)) +
  labs(title = title_top,
       subtitle = subtitletext, 
       caption = captiontext,
       y = "New cases per day") +
  scale_color_manual(values = country_colors) +
  theme(axis.title = element_text(colour = "#000000", size = 14,
                                  family = "Rockwell", hjust = 0.5,
                                  vjust = 0.7),
        axis.text = element_text(color = "#000000", size =10, 
                                 family = "Rockwell"  ),
        title = element_text(colour = "#000000", size = 14,
                             family = "Rockwell", hjust = 0.3,
                             vjust = 0.3)) +
  annotate("text", x=as.Date('2020-02-15'), 
          y = 40000, label = "USA passes Italy, \n, here comes Spain")

plot9



arranged3<-gridExtra::grid.arrange(plot8, plot9,
                                   nrow = 1,
                                   top = "Top Countries Leading the way!",
                                   bottom = textGrob(
                                     "this footnote is right-justified",
                                     gp = gpar(fontface = 3, fontsize = 9),
                                     hjust = 1,
                                     x = 1))

ggsave('plots_all3.png', arranged3, width = 12, height = 8,
       type = "cairo")
###################################################################################################


###############################################################################################

# top_30_c <- corona_master2 %>%
#   filter(Case_Type == "Confirmed") %>%
#   filter(Date == max(Date)) %>%
#   group_by(Country) %>%
#   summarize(Cases = sum(cases)) %>%
#   arrange(-Cases) %>%
#   head(n = 30L)


# Top Countries
x9<-corona_master2 %>%
  filter(Case_Type == "Confirmed") %>%
  filter(Date == max(Date)) %>%
  group_by(Country) %>%
  summarise(Confirmed = sum(cases, na.rm = T)) %>%
  mutate(new = Confirmed-lag(Confirmed)) %>%
  arrange(desc(Confirmed)) %>%
  filter(Country != "China", Country != "Cruise Ship",
         Confirmed>=10000)

max(x9$Confirmed, na.rm = T)

plot10<-ggplot(x9,aes(x=Country , y=Confirmed, fill = Country )) + 
  geom_col( stat="identity", position = "Dodge2")+ 
  theme_classic() +
  scale_y_continuous(limits = c(0,200000), expand = c(0, 0),label =comma,
                     breaks =seq(0,200000,10000))  +
  labs(title = "Corona-19: Cases outside China", subtitle = subtitletext, 
       x =  "Date", y="Total cases ('000)", 
       caption = paste("Data: https://github.com/CSSEGISandData/COVID-19.\n Viz: Allister Hodge\n Last Update:", format(Sys.Date(), "%b %d, %Y")))+
  theme(plot.title = element_text(face="bold",hjust = 0.135, size = 14),
        plot.subtitle = element_text(hjust = 0.055, size = 12),
        text=element_text(size=12, family = "Rockwell"),
        axis.text.x.bottom = element_text (angle = 90 ,
                                           size = 10, hjust = 0, 
                                           colour = "black"),
        panel.border = element_blank(),
        legend.title = element_blank(),
        legend.position = "right",
        strip.text = element_text(size = 10, face = "bold"))
plot10
ggsave('plots_all4.png', plot10, width = 12, height = 8,
       type = "cairo")

# ggsave(filename = paste0(i, "_", country, ".png"),
#        type = "cairo",
#        path = plots_folder,
#        width = 8,
#        height = 5,
#        units = "in")
# i=i+1
# }

########################################################################################


# covid_newest<-corona_master %>%
#   filter(Country == "United States of America") %>%
#   group_by(State) %>%
#   summarise(total = sum(Confirmed)) %>%
#   arrange(-total) %>%
#   head(20)
# 
# 
# unique(corona_master$Country)
# # Countries to filter
# 
# top<- corona_master %>%
#   filter(Country %in% countries)  %>%
#   select(Country, Ongoing, Date)
# 
# 
# # top<- corona_master %>%
# #   filter(Country == "United States of America" )  
# max(top$Ongoing,na.rm=T)
# ggplot(top, aes(x =  Date, y = Ongoing)) + 
#   geom_line(aes(col = Country), size =1.4) +
#   scale_x_date (breaks  = unique(x2$Date)) + theme_classic() +
#   scale_y_continuous(limits = c(0,50000), expand = c(0, 0),label =comma,
#                      breaks= seq(0,50000, 5000))  +
#   labs(title = titletext, subtitle = subtitletext, 
#        x =  "Date", y="Total Confirmed cases ('000)", 
#        caption = captiontext)+
#   theme(plot.title = element_text(face="bold",hjust = 0.135, size = 14),
#         plot.subtitle = element_text(hjust = 0.055, size = 12),
#         text=element_text(size=12, family = "Rockwell"),
#         axis.text.x.bottom = element_text ( angle = 90 ,size = 7, hjust  =  0 ),
#         panel.border = element_blank(),
#         legend.title = element_blank(),
#         legend.position = "top",
#         strip.text = element_text(size = 10, face = "bold"))
# 
# # state_abb<-c("WA",
#              "CA",
#              "MA",
#              "GA",
#              "TX",
#              "NJ",
#              "CO",
#              "IL",
#              "PA",
#              "VA",
#              "MD",
#              "IA",
#              "NC",
#              "AZ",
#              "IN",
#              "WI",
#              "OH",
#              "UT",
#              "VT",
#              "MN",
#              "FL",
#              "KY",
#              "LA",
#              "SC",
#              "TN",
#              "OR",
#              "KS",
#              "MO",
#              "NY",
#              "NH",
#              "D.C.",
#              "HI",
#              "OR",
#              "OK",
#              "RI",
#              "NE",
#              "NV",
#              "CT",
#              "SD",
#              "NM",
#              "MI",
#              "DE",
#              "OR ")
# states<-c("Alaska","Alabama","Arkansas","Arizona","California","Colorado",
# "Connecticut","District of Columbia","Delaware","Florida","Georgia",
# "Hawaii","Iowa","Idaho","Illinois","Indiana","Kansas","Kentucky",
# "Louisiana","Massachusetts","Maryland","Maine","Michigan","Minnesota",
# "Missouri","Mississippi","Montana","North Carolina","North Dakota",
# "Nebraska","New Hampshire","New Jersey","New Mexico","Nevada",
# "New York","Ohio","Oklahoma","Oregon","Pennsylvania","Puerto Rico",
# "Rhode Island","South Carolina","South Dakota","Tennessee","Texas",
# "Utah","Virginia","Vermont","Washington","Wisconsin",
# "West Virginia","Wyoming")
# 
# # subset the USA to add across states
# 
# # c("United States of America") use if you only want to the USA
# TotalCountry <- corona_master %>% 
#   filter(Country %in%  countries &
#            !str_detect(State, ",")) %>%  # remove all except just state (and ship) totals)
#   group_by(Date, Country) %>%
#   summarize(deaths = sum(Ongoing)) %>%
#   arrange(Country)
# 
# plot1<-ggplot(TotalCountry) +  geom_line(aes(x = Date, y= deaths, col = Country),size = 1.2) +
#   theme_bw() +
#   scale_y_continuous(limits = c(0,60000), expand = c(0, 0),label =comma,
#                      breaks =seq(0,60000,10000)) +
#   labs(title = "Top Countries", subtitle = "Data as at March", 
#        caption = "Source: World Health Organisation") +
#   facet_wrap(vars(Country), scales = "free_y")
# 
#   
# #USA Cases only  
# # us[1,3]<-0
# us<- corona_master %>%
#   filter(Country == "United States of America", 
#          State %in% states) %>%
#   arrange(State)
# 
# state<-datasets::state.abb
# state
# 
# us<-us %>%
#   filter(!State %in%state_abb)
# 
# us<-us %>%
#   filter(State != "OR ")
# 
# class(us$State)
# unique(us$State)
# unique(us$State)
# 
#   
# top_us<-us %>%
#   filter(State != "Diamond Princess" &
#          State != "Grand Princess") %>%
#   group_by(Date, State) %>%
#   summarise(Ongoing = abs(sum(New, na.rm = T))) %>%
#  arrange(State)
# 
# 
# top_us1<-us %>%
#   filter(State != "Diamond Princess" &
#            State != "Grand Princess") %>%
#   group_by( State) %>%
#   summarise(Ongoing = abs(sum(Confirmed, na.rm = T))) %>%
#   arrange(-Ongoing) %>%
#   head(10L)
# 
# 
# max(us$Ongoing)
# top_us2<-us %>%
#   filter(State != "Diamond Princess" &
#            State != "Grand Princess") %>%
#   group_by( Date) %>%
#   summarise(Ongoing = abs(sum(New, na.rm = T))) 
#   arrange(-Ongoing) 
#   head(10L)
# 
# 
# # Subset the UK by County
# uk<- corona_master %>% 
#   filter(Country == "United Kingdom") %>%
#   group_by(Country, Date) %>%
#   summarise(Ongoing = sum(New))
# 
# top<- top %>%
#   full_join(us, by = c("Country", "Date", "Ongoing")) %>%
#   full_join(uk,by = c("Country", "Date", "Ongoing"))
# 
# top[422,2]<-0
# ggplot(top, aes(x =  Date, y = Ongoing)) + geom_line(aes(col = Country), size =1.3) +
#   facet_wrap(vars(Country), scales = "free_y") +
#   theme(legend.position = "bottom") +
#   labs(title = " Active Cases by Country")
# 
# 
# 
# ####################################################################
# x8<-analysis <- corona %>% 
#   group_by(Country, Date) %>%
#   summarise(total_cases = sum(Confirmed)) %>%
#   mutate(new= total_cases - lag(total_cases)) %>%
#   top_n(6, total_cases) %>%
#   top_n(6,new) %>%
#   arrange(desc(total_cases)) %>%
#   arrange(desc(new)) %>%
#   filter(Country %in% countries)
# arrange(Country)
# 
# ggplot(x6) + 
#   geom_line(aes(x = Date, y= new, col = Country),size = 1.2) +
#   theme_classic() +
#   labs(title = "Top Countries", subtitle = subtitletext, caption = "Source: World Health Organisation") +
#   scale_colour_brewer(palette = "Set1")
# 
# #############################################################################
# 
# df1 <- corona %>% 
#   group_by(Country, Date) %>%
#   summarise(total_cases = sum(Confirmed)) %>%
#   mutate(new= total_cases - lag(total_cases)) %>%
#   filter(Country != "China") %>%
#   filter(Country != "Cruise Ship") %>%
#   arrange(desc(total_cases)) %>%
#   arrange(desc(new)) %>%
#   arrange(Country) %>%
#   filter(new >=200)
# unique(df1$Country)
# 
# plot7<-ggplot(df1) + 
#   geom_line(aes(x = Date, y= new, col = Country),size = 1.2) +
#   theme_classic() +
#   labs(title = "Top Countries", subtitle = subtitletext, caption = captiontext) +
#   scale_colour_manual(values = c("#FFA500","#0000A0","#FF0000",
#                                  "#FFFF00","#00FF00",'#A52A2A',
#                                  '#4E387E', '#151B54','#8D38C9',
#                                  '#F6358A',"#737CA1",'#52F3FF',
#                                  '#254117','#00FF00','#FBB117'))
# plot7
# ggsave('plot7.png', plot7, width = 10, height = 10)
# 
# 
# ggplot(df1) + geom_line(aes(x = Date, y= new, group = Country),size = 1.2) +
#   theme_bw() +
#   labs(title = "Top Countries: Outside China", subtitle = "Data as at March 17", 
#        caption = "Source: World Health Organisation", x = "Date",
#        y = "Total Confirmed Cases") +
#   scale_y_continuous(limits = c(0,20000), expand = c(0, 0),label =comma) +
#   facet_wrap(~Country, scales = "free_y")
