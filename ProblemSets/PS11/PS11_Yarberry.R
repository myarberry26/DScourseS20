# PS11 Yarberry 

library(rvest)
library(readr)
library(DT)

# Number of National Championship by School 
NCAA <- read_html("https://en.wikipedia.org/wiki/College_football_national_championships_in_NCAA_Division_I_FBS") 
NatChap <- NCAA %>% html_nodes("#mw-content-text > div > table:nth-child(66)") %>%
  html_table(fill=TRUE) 
FootballTable <- NatChap[[1]]
FootballTable[4] <- NULL
FootballTable[6, 2] = 11
FootballTable[33, 2] = 2
FootballTable[36, 2] = 2

#Finances for 2017 - 2018 
Finances2018 <- read_html("https://sports.usatoday.com/ncaa/finances/") 
NCAAFinances  <- Finances2018 %>% html_nodes("#content > div.full-width > div > section > div.datatable-wrapper.datatable-wrapper-fixed-column > table") %>%
  html_table(fill=TRUE) 
NCAAFinances2018 <- NCAAFinances[[1]]
NCAAFinances2018 = NCAAFinances2018[-231,]

# Head Coach Salaries 
Salary <- read_html("https://sports.usatoday.com/ncaa/salaries/") 
HeadCoachSalary  <- Salary %>% html_nodes("#content > div.full-width > div > section > div.datatable-wrapper.datatable-wrapper-fixed-column > table") %>%
  html_table(fill=TRUE) 
HeadCoachSalary <- HeadCoachSalary[[1]]
is.na(HeadCoachSalary) <- HeadCoachSalary == "--"
HeadCoachSalary = HeadCoachSalary[-131,]

