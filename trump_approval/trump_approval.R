library(tidyverse)
library(cowplot)
library(plotly)
library(htmltab)

# import election statistics (from wikipedia)
url <- "https://en.wikipedia.org/wiki/United_States_presidential_election,_2016"
election.df <- htmltab(url, 38)

colnames(election.df) <- c(  "state",
                             "electoral_method",
                             "clinton_vote",
                             "clinton_perc",
                             "clinton_elector_count",
                             "trump_vote",
                             "trump_perc",
                             "trump_elector_count",
                             "johnson_vote",
                             "johnson_perc",
                             "johnson_elector_count",
                             "stein_vote",
                             "stein_perc",
                             "stein_elector_count",
                             "mcmullin_vote",
                             "mcmullin_perc",
                             "mcmullin_elector_count",
                             "others_vote",
                             "others_perc",
                             "others_elector_count",
                             "vote_margin",
                             "perc_margin",
                             "total_vote",
                             "state_abbr"
                          )

# import approval numbers (from morning consult)
approval_url <- "https://morningconsult.com/tracking-trump/"
approval.df <- htmltab(approval_url, 1)

colnames(approval.df) <- c("state_abbr",
                           "Jan17_approval",
                           "Jan17_disapproval",
                           "Jan17_margin_of_error",
                           "May18_approval",
                           "May18_disapproval",
                           "May18_margin_of_error"
                          )

df <- left_join(approval.df, election.df) 

# fix data conversion errors and such
filtered <- df %>%
  mutate("Voted for Trump" = ifelse(perc_margin > 0, "State Trump won", "State Clinton won")) %>%
  mutate("2016 Trump Vote" = as.numeric(gsub("%", "",trump_perc))/100) %>%
  mutate(Jan17_approval = as.numeric(gsub("%", "",Jan17_approval))/100) %>%
  mutate(Jan17_disapproval = as.numeric(gsub("%", "",Jan17_disapproval))/100) %>%
  mutate(May18_approval = as.numeric(gsub("%", "",May18_approval))/100) %>%
  mutate(May18_disapproval = as.numeric(gsub("%", "",May18_disapproval))/100) %>%
  mutate(Jan17_net = Jan17_approval - Jan17_disapproval) %>%
  mutate(May18_net = May18_approval - May18_disapproval) 

# plot
p1 <- ggplot(filtered, aes(x=`2016 Trump Vote`, color=`Voted for Trump`)) +
  geom_point(aes(y=Jan17_net, text = state), shape=1) +
  geom_point(aes(y=May18_net, text = state), shape=16) +
  geom_segment(aes(x = `2016 Trump Vote`, y = Jan17_net, xend = `2016 Trump Vote`, yend = May18_net), data = filtered) +
  scale_color_manual(values=c("blue3", "red3" )) +
  xlim(.25, .75) +
  geom_hline(yintercept=0, linetype="dashed") +
  ylab("Trump Net Approval")

# plotly for mouse over states.  Plotly doesn't render the arrows, just lines.
# maybe this can be fixed.
ggp <- ggplotly(p1, tooltip=c("text")) 
htmlwidgets::saveWidget(ggp, "trump_approval.html")
ggsave("trump_approval.png")
saveRDS(filtered, "trump_approval.RDS")
