# Make sure that you have the "utils" and "httr" packages installed.

#these libraries need to be loaded
library(utils)
library(httr)

#download the dataset from the ECDC website to a local temporary file
GET("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv", 
    authenticate(":", ":", type="ntlm"), 
    write_disk(tf <- tempfile(fileext = ".csv")))
list()

#read the Dataset sheet into "R". The dataset will be called "data".
data <- read_csv(tf)
        

colnames((data))

co_days = 28
country = "Germany"


# format of loaded dataset from ECDC
# rename columns for simplicity
df1 <- data %>%
  janitor::clean_names() %>%
  select(one_of("date_rep","countries_and_territories",
              "cases","deaths","geo_id", "country_territory_code","pop_data_2018")) %>%
  rename(
    country_name = countries_and_territories,
    country_code = geo_id,
    date = date_rep
  ) %>%
  mutate(date = parse_date_time(date, orders = "dmy"))

# 2) Select a minimum cut-off date. Results before this date are not displayed
DateMin <- min(df1$date, na.rm = T)


# Get the latest date in the data
DateMax <- max(df1$date,na.rm=T)



covid19 <- df1 %>%
  group_by(date, country_name) %>%
  arrange(desc(date),
          desc(cases),
          desc(deaths))

covid19_all <- df1 %>%
  group_by(date) %>%
  summarize (sum_cases = sum(cases), sum_deaths = sum(deaths)) %>%
  arrange(desc(date), desc(sum_cases), desc(sum_deaths))

# How many days to display for individual countries
co_days <- 28

p1 <-
  ggplot(data = covid19_all, mapping = aes(x = date, y = sum_cases)) +
  geom_bar(stat = "identity", color = "red", fill = "red") +
  labs(title = "Global - Daily Cases", x = "Date", y = "Daily Case Count") +
  theme(plot.title = element_text(
    size = 14,
    face = "bold",
    margin = margin(10, 0, 10, 0)
  ))

p1

# p2 <-
#   ggplot(
#     data = filter(covid19, country_name == country)[1:co_days, ],
#     mapping = aes(x = date, y = cases)) +
#   geom_bar(stat = "identity", color = "red", fill = "#ff9999") +
#   labs(title = paste(country, "- Daily Cases"), x = "Date", y = "Daily Case Count") +
#   theme(plot.title = element_text(
#     size = 14,
#     face = "bold",
#     margin = margin(10, 0, 10, 0)))
# p2

# Print all plots onto a single page and arrange them in 2 rows
grid.arrange(p1, p2, nrow = 2)



# Create text for caption, giving data source and time figure was created
captiontext <- paste0("Created: ",Sys.time(), "\n",
                      "Source: European Centre for Disease Prevention and Control\n",
                      "URL: ",url)
subtitletext <- paste0("Number of cases in selected countries, as at ", DateMax)



# Make sure that you have the "readxl" and "httr" packages installed.

#these libraries are necessary

library(readxl)

library(httr)
library(tidyverse)

#create the URL where the dataset is stored with automatic updates every day

url <- paste("https://www.ecdc.europa.eu/sites/default/files/documents/COVID-19-geographic-disbtribution-worldwide-",format(Sys.time(), "%Y-%m-%d"), ".xlsx", sep = "")

#download the dataset from the website to a local temporary file

GET(url, authenticate(":", ":", type="ntlm"), write_disk(tf <- tempfile(fileext = ".xlsx")))

#read the Dataset sheet into "R"

df_ecdc <- read_excel(tf)


df_ecdc %>% 
  tail(1)

glimpse(df_ecdc)
class(df_ecdc$dateRep)

unique(df_ecdc$countriesAndTerritories)

# format of loaded dataset from ECDC
# rename columns for simplicity
df1 <- df_ecdc %>%
  janitor::clean_names(case = "snake") %>%
  rename(
    country_name = `countries_and_territories`,
    country_code = geo_id,
    date = date_rep) %>%
    mutate(country_name = case_when(
      country_name== "United_Kingdom" ~ "United Kingdom",
      country_name=="United_States_of_America" ~ "United States ofAmerica",
      # country_name=="Viet Nam" ~ "Vietnam",
      # country_name=="Republic of Korea" ~ "Korea, South",
      # country_name=="Moldova" ~ "Republic of Moldova",
      # country_name=="Iran (Islamic Republic of)" ~ "Iran",
      # country_name=="Taipei and environs"~"Taiwan*",
      # country_name=="Czech Republic" ~ "Czechia",
      # country_name=="Gibralter" ~ "United Kingdom",
      # country_name=="Channel Islands" ~ "United Kingdom",
      # country_name=="UK" ~ "United Kingdom",
      TRUE ~ country_name
    )) %>%
  mutate(date = lubridate::as_date(date,  tz = NULL))

unique(df1$country_name)

# dat_ECDC$country[dat_ECDC$country == "South_Korea"] <- "South Korea"
# dat_ECDC$country[dat_ECDC$country == "United_States_of_America"] <- "USA"
# dat_ECDC$country[dat_ECDC$country == "United_Kingdom"] <- "United Kingdom"

# 2) Select a minimum cut-off date. Results before this date are not displayed
DateMin <- min(df1$date, na.rm = T)


# Get the latest date in the data
DateMax <- max(df1$date,na.rm=T)


covid19 <- df1 %>%
  group_by(date, country_name) %>%
  arrange(desc(date),
          desc(cases),
          desc(deaths))

covid19_all <- df1 %>%
  group_by(country_name) %>%
  summarize (sum_cases = sum(cases), sum_deaths = sum(deaths)) %>%
  mutate(mortality = sum_deaths/ sum_cases*100) %>%
  arrange((-sum_cases),(-sum_deaths)) %>%
  filter(country_name != "China") %>%
  head(10L)

glimpse(covid19_all)

dat_ECDC <- df1 %>% 
  group_by(country_name) %>% 
  arrange(country_name, date) %>% 
  mutate(
    cum_cases = cumsum(cases),
    cum_deaths = cumsum(deaths),
    overall_cum_cases = max(cum_cases),
    cum_cases_l1 = lag(cum_cases),
    dailyGrowth = cum_cases / cum_cases_l1 - 1,
    day_in_dataset = 1:n())
    # country label only at the last data point of each timeline:
  #   country_label = if_else(day_in_dataset == max(day_in_dataset), as.character(country), NA_character_)
  # )

# How many days to display for individual countries
co_days <- 28


##################################################################################
df1$country_name<-(as.character(df1$country_name))
unique(df1$country_name)
EU_countries <- c(
  # "Austria",
  "Germany",
  "France",
  "Netherlands",
  # "Belgium",
  "Italy",
  "Portugal",
  "Spain",
  # "Ireland",
  # "Danmark",
  # "Sweden",
  # "Norway",
  # "Poland",
  # "Finland",
  "United Kingdom"
)


euro_df<- df1 %>%
  filter(country_name %in% EU_countries) %>%
  group_by(date, country_name) %>%
  arrange(country_name, date) %>%
  summarise(cases = sum(cases, na.rm =T), 
            cases2 = cumsum(cases)) %>%
  mutate(cases3 = cases/lag(cases)-1) %>%
  arrange(country_name)

library(ggrepel)
titletest<-glue::glue("Europe COVID-19 Data as {Sys.Date()}")
max(euro_df$cases, na.rm = T)

ggplot(euro_df, aes(x = date, y = cases,label = country_name )) + 
  geom_line(aes(col = country_name), size = 1.1) +
  theme_classic() +
  scale_y_continuous(limits = c(0,10000), expand = c(0, 0),label =comma,
                     breaks =seq(0,10000,1000))  +
  labs(title = titletest, subtitle = subtitletext, 
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
###################################################################################
ggplot(euro_df, aes(x = date, y = cases3,label = country_name )) + 
  geom_line(aes(col = country_name), size = 1.1) +
  theme_classic() +
  scale_y_continuous(expand = c(0, 0),label =percent,
                     breaks =seq(0,.80,.10))  +
  labs(title = titletest, subtitle = subtitletext, 
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
#####################################################################################

