covidtracker_cols_to_keep = c(
  "date",
  "state",
  "positive",
  "negative",
  "pending",
  "hospitalized",
  "death",
  "dateChecked",
  "fips")

# dplyr::select(quote(covidtracker_cols_to_keep))
data_raw <- 
  read_csv("http://covidtracking.com/api/states/daily.csv") %>%
  dplyr::select(one_of(covidtracker_cols_to_keep)) %>%
  mutate(date = date %>% as.character() %>% lubridate::as_date()) %>% 
  filter(state %in% state.abb) %>%
  arrange(state)
# left_join(states_df)


us_ac<- data_raw %>%
  group_by(date) %>%
  summarise(positive = sum(positive, na.rm = T))

max(us_ac$positive, na.rm = T)

plot11<-ggplot(us_ac, aes(x = date, y = positive)) + geom_line( size = 1.2) +
  theme_classic() +
  scale_y_continuous(limits = c(0,200000), expand = c(0, 0),label =comma,
                     breaks =seq(0,200000,10000))  +
  labs(title = "USA, Cases Confirmed", subtitle = subtitletext, 
       x =  "Date", y="New Deaths Per day", 
       caption =captiontext)+
  theme(plot.title = element_text(face="bold",hjust = 0.135, size = 14),
        plot.subtitle = element_text(hjust = 0.055, size = 12),
        text=element_text(size=12, family = "Rockwell"),
        axis.text.x.bottom = element_text ( angle = 90 ,size = 10, hjust  =  0 ),
        panel.border = element_blank(),
        legend.title = element_blank(),
        legend.position = "top",
        strip.text = element_text(size = 10, face = "bold"))
plot11

ggsave('plots_all5.png', plot11, width = 12, height = 8,
       type = "cairo")
#####################################################################

states<-data_raw %>%
  group_by(state, date) %>%
  summarise(positive=sum(positive, na.rm = T)) %>%
  arrange(-positive) %>%
  # filter(state != "NY") %>%
  arrange(state) 

ggplot(states, aes(x = date, y = positive)) + geom_line( aes(col = state),size = 1.2)


top<-states %>%
  filter(date == max(date)) %>%
  select(-date) %>%
  arrange(-positive) %>%
  head(10L)
glimpse(top)

plot12<-ggplot(top,aes(x=state , y=top$positive, fill = state )) + 
  geom_col( stat="identity", position = "Dodge2")+ 
  theme_classic() +
  scale_y_continuous(limits = c(0,80000), expand = c(0, 0),label =comma,
                     breaks =seq(0,80000,10000))  +
  labs(title = "Corona-19: Top Cases USA by State", subtitle = subtitletext, 
       x =  "Date", y="Total cases ('000)", 
       caption = paste("Data: https://github.com/CSSEGISandData/COVID-19.\n Viz: Allister Hodge\n Last Update:", format(Sys.Date(), "%b %d, %Y")))+
  theme(plot.title = element_text(face="bold",hjust = 0.135, size = 14),
        plot.subtitle = element_text(hjust = 0.055, size = 12),
        text=element_text(size=12, family = "Rockwell"),
        axis.text.x.bottom = element_text (angle = 90 ,
                                           size = 10, hjust  = 0, 
                                           color = "black"),
        axis.text = element_text(color = "black", size = 12),
        axis.title.y = element_text(color = "black", size = 12),
        panel.border = element_blank(),
        legend.title = element_blank(),
        legend.position = "right",
        strip.text = element_text(size = 10, face = "bold"))

plot12
ggsave('plots_all6.png', plot12, width = 12, height = 8,
       type = "cairo")
###############################################################################################
# Group the death cases
death<-data_raw %>%
  group_by(date) %>%
  summarise(death = sum(death, na.rm = T))

states_d<-data_raw %>%
  group_by(state, date) %>%
  summarise(deaths =sum(death, na.rm = T))

top_d<-states_d %>%
  filter(date == max(date)) %>%
  select(-date) %>%
  arrange(-deaths) %>%
  head(10L)
glimpse(top_d)

##############################################################################

us_positive<-states_d<-data_raw %>%
  group_by(state, date) %>%
  summarise(positive =sum(positive, na.rm = T)) %>%
ungroup()


us_positive<- data_raw %>%
  group_by(date) %>%
  summarise(positive =sum(positive, na.rm = T)) %>%
  mutate(growth = (positive/lag(positive)-1)) %>%
  ungroup()


plot13<-ggplot(us_positive, aes(x= date, y = growth)) + geom_line(size = 1.2, color = '#4B0082') +
  scale_y_continuous(labels = scales::percent, expand = c(0,0),
                     breaks = seq(0,.7,.1)) +
  labs(title = "Growth Rate of Positive Cases in the USA", 
       subtitle = subtitletext, caption = captiontext,
       y = "Growth rate of positive cases (%)") +
  theme_ipsum_ps()

plot13

ggsave('plots_all14.png', plot13, width = 12, height = 8,
       type = "cairo")

###########################################################################################
new<-data_raw %>%
  group_by(state, date) %>%
  mutate(new = positive - lag(positive))

state_abb<-as.character(top$state)


top_s<-states %>%
  filter(state %in% state_abb)


ggplot(top_s, aes(date, positive, group = state)) + geom_line(aes(col = state), size = 1.2) +
  facet_wrap(vars(state), scales = "free", nrow  = 2) +
  theme_ipsum()


state_lookup = state.name %>% set_names(state.abb)
