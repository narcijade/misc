library(tidyverse)
library(cowplot)
library(plotly)
library(htmltab)
library(lubridate)

# import party affiliation statistics
url <- "https://news.gallup.com/poll/15370/party-affiliation.aspx"
in.df <- htmltab(url, 1)

colnames(in.df) <- c(  "Date",
                    "Republicans",
                    "Independents",
                    "Democrats"
                          )

df <- in.df %>% 
  separate(Date, c("date", "date2"), sep="-") %>%
  mutate(date = ymd(date)) %>%
  select(-date2) %>%
  gather(party, perc, -date) %>%
  mutate(perc = as.numeric(perc))
  

# plot
ggplot(df, aes(x=date, y=perc, color=party)) +
  geom_line() +
  stat_smooth(aes(y=perc, x=date), method = "auto", se = TRUE) +
  ylab("%") +
  xlab("Date") +
  ggtitle("Party affiliation")


ggsave("party_affiliation.png")
saveRDS(df, "party_affiliation.RDS")