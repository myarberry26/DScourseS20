PS5 ~ Yarberry 
library(rvest)
NCAA <- read_html("https://en.wikipedia.org/wiki/College_football_national_championships_in_NCAA_Division_I_FBS") 
NatChap <- NCAA %>% html_nodes("#mw-content-text > div > table:nth-child(66)") %>%
    html_table(fill=TRUE) 
FootballTable <- NatChap[[1]]
FootballTable <- FootballTable >%> mutate(Ref=NULL)

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# API ~ Yahoo Finance 
library(XML)

symbol = "NKE"
url <- paste('https://finance.yahoo.com/quote/NKE/analysis?p=NKE',symbol,sep="")
webpage <- readLines(url)
html <- htmlTreeParse(webpage, useInternalNodes = TRUE, asText = TRUE)
NKEtable <- getNodeSet(html, "//table")

EarningEstimates <- readHTMLTable(NKEtable [[1]])
EevenueEstimates <- readHTMLTable(NKEtable [[2]])
EarningHistory <- readHTMLTable(NKEtable [[3]])
EpsTrend <- readHTMLTable(NKEtable [[4]])
EpsRevisions <- readHTMLTable(NKEtable [[5]])
GrowthEst <- readHTMLTable(NKEtable [[6]])