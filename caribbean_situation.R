unique(corona$Country)

caribbean<-c("Saint Kitts and Nevis", "Saint Lucia", "Saint Vincent and the Grenadines",
"Jamaica", "Guyana", "Grenada" ,"Dominica",#"Dominican Republic",
"Cuba","Barbados","Bahamas","Antigua and Barbuda","Trinidad and Tobago") 


carib<- corona %>%
  filter(Country %in% caribbean)


p_carib<-ggplot(carib, aes(x = Date, y = Confirmed, group = Country)) +
  geom_line(aes(color = Country), size = 0.9) + 
  theme_bw()+
  labs(title = "Caribbean Situation, COVID19", subtitle = subtitletext,
       caption = captiontext) +
  theme(plot.title = element_text(face="bold",hjust = 0.135, size = 8),
        plot.subtitle = element_text(hjust = 0.055, size = 12),
        text=element_text(size=12, family = "Rockwell"),
        axis.text.x.bottom = element_text (angle = 90 ,size = 5, 
                                           hjust= 0,
                                           colour = "black"),
        axis.text = element_text(size = 6, colour = 'black'),
        panel.border = element_blank(),
        legend.title = element_blank(),
        legend.position = "top",
        strip.text = element_text(size = 8, face = "bold")) +
        facet_wrap(vars(Country), scales = "free") 
p_carib

ggsave("plot13.png", p_carib, width = 10, height = 12, type = "cairo")

