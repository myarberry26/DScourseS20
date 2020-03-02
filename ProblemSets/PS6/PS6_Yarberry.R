PS6_Yarberry.R 

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


#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

library('ggplot2')
# Plot 1 ~
ggplot(data = FootballTable, mapping = aes(x = factor(FootballTable$`Claimed  national  championships`))) +
  geom_bar(fill="light blue", alpha=.6, width=.7) +
  coord_flip() +
  ggtitle("Number of Universites with D1 Football National Championships") +
  xlab("# of Schools") +
  ylab("# of Championships") +
  theme_bw() +
  scale_x_discrete(limits=c("1", "2", "3", "4", "5", "6", "7", "8", "9", "11", "17", "27", "28"))

# Plot 2 ~
HeadCoachSalary$`School Pay` <- parse_number(HeadCoachSalary$`School Pay`)
HeadCoachSalary$`Total Pay` <- parse_number(HeadCoachSalary$`Total Pay`)
HeadCoachSalary$`Asst Pay Total` <- parse_number(HeadCoachSalary$`Asst Pay Total`)
AverageSalary <- aggregate(HeadCoachSalary[, c(5,6,9)], list(HeadCoachSalary$Conf), mean, na.rm=TRUE)

ggplot(HeadCoachSalary, aes(x=as.factor(HeadCoachSalary$School), y= HeadCoachSalary$`Total Pay`)) + 
  geom_point(aes(color = factor(HeadCoachSalary$Conf)), size = 4)+
  ggtitle("Total Coaching Salary based on D1 Athletic Programs") +
  ylab("Total Salary") +
  xlab("Universties with D1 Football Teams") +
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank())

# plot 3 ~
NCAAFinances2018$`Total Revenue` <- parse_number(NCAAFinances2018$`Total Revenue`)
NCAAFinances2018$`Total Expenses` <- parse_number(NCAAFinances2018$`Total Expenses`)
NCAAFinances2018$`Total Allocated` <- parse_number(NCAAFinances2018$`Total Allocated`)
AverageConf <- aggregate(NCAAFinances2018[, c(4:6)], list(NCAAFinances2018$Conf), mean)

ggplot() + 
  geom_boxplot(data = NCAAFinances2018, aes(y = NCAAFinances2018$`Total Revenue`, x = as.factor(NCAAFinances2018$Conf)), fill = "light blue") +
  geom_boxplot(data = NCAAFinances2018, aes(y = NCAAFinances2018$`Total Expenses`, x = as.factor(NCAAFinances2018$Conf)), fill = "orange") +
  coord_flip() +
  theme_bw() +
  ggtitle("Total Athletic Depatment Expenses & Revenue based on Conference") +
  ylab("Total Funds") +
  xlab("Conferences") +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE))
       