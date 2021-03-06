# NBA Salaries
# Evan Kramer
# 5/15/2019

# Set up
# Set up
options(java.parameters = "-Xmx16G")
library(tidyverse)
library(lubridate)
library(haven)
library(rvest)

# Load payroll data from URL
payroll = read_html("https://www.basketball-reference.com/contracts/") %>% 
  html_nodes("table") %>% 
  html_table(fill = T) %>% 
  .[[1]] %>% 
  as.data.frame() 
names(payroll) = payroll[1, ]
payroll = payroll[2:nrow(payroll), ]
payroll = gather(payroll, "year", "payroll", 3:ncol(payroll)) %>% 
  transmute(team = Team, year, payroll = as.integer(str_replace_all(payroll, "[$,]", "")))

# Load standings from URL
read_html("http://www.espn.com/nba/standings") %>% 
  html_nodes("section") %>% 
  .[41] %>% 
  .[[1]] %>%
  .[[1]] %>%
  # .[[1]] %>%
  View()
  html_table(fill = T)
break()



# Salary data
read_csv("C:/Users/CA19130/Downloads/nba_salaries.csv") %>% 
  transmute(team = Team, salaries = as.integer(str_sub(`2018-19`, 2, -1))) %>% 
  # Winning percentages
  arrange(desc(salaries))

standings = read_csv("C:/Users/CA19130/Downloads/nba_records.csv", col_names = F) %>% 
  mutate(pos = NA)
for(i in 1:nrow(standings)) {
  standings$pos[i] = str_locate(standings$X1[i], "Place")[2]
  standings$team[i] = str_sub(standings$X1, standings$pos[i] + 1, -1)[i]
}
standings = transmute(standings, 
                      team = ifelse(team == "LA Clippers", "Los Angeles Clippers", team), 
                      plot = ifelse(str_detect(team, "Golden") | str_detect(team, "lazers"), team, "Other"),
                      win_pct = as.numeric(str_replace_all(X4, "Win Percentage", ""))) %>% 
  full_join(
    read_csv("C:/Users/CA19130/Downloads/nba_salaries.csv") %>% 
      transmute(team = Team, salaries = as.integer(str_sub(`2018-19`, 2, -1))),
    by = "team"
  ) 

# Plot
cor(standings$win_pct, standings$salaries)
ggplot(data = standings, aes(y = win_pct, x = salaries, color = plot)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = F) + 
  scale_x_continuous(name = "Total Salaries") + 
  scale_y_continuous(name = "Winning Percentage", limits = c(0, 1)) + 
  scale_color_discrete(name = "") +
  theme_bw() 

# Historical data? 
for(y in seq(year(now()) - 20, year(now()))) {
  read_html(str_c("https://www.basketball-reference.com/teams/LAL/", y, ".html")) %>% 
    html_nodes("div") %>% 
    .[14] %>% 
    View()
}