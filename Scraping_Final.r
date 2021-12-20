#RUN FIRST

rm(list=ls())

#Additional Data From Gallup - Year 2015 that Could Not Be scraped

Other <- read.csv("2015.csv", sep = ",", header = T)

#Scraping - 2017

library(xml2)

url1 <- read_html("https://news.gallup.com/poll/226643/2017-party-affiliation-state.aspx")

jurisdiction1 <- xml_text(xml_find_all(url1, "/html/body/div[2]/div/main/div/article/div[1]/div/figure/div/table/tbody/tr/th"))
Democrat_Lean1 <- xml_text(xml_find_all(url1, "//tbody[@class='row-group']/tr//td[1]"))
Republican_Lean1 <- xml_text(xml_find_all(url1, "//tbody[@class='row-group']/tr//td[2]"))
Democratic_Advantage1 <- xml_text(xml_find_all(url1, "//tbody[@class='row-group']/tr//td[3]"))
N1 <- xml_text(xml_find_all(url1, "//tbody[@class='row-group']/tr//td[4]"))
Classification1 <- xml_text(xml_find_all(url1, "//tbody[@class='row-group']/tr//td[5]"))

Party1 <- data.frame(jurisdiction1, Democrat_Lean1, Republican_Lean1, Democratic_Advantage1, N1, Classification1)
Party1$Year1 <- 2017

#Scraping - 2016

url2 <- read_html("https://news.gallup.com/poll/203117/gop-maintains-edge-state-party-affiliation-2016.aspx")

jurisdiction2 <- xml_text(xml_find_all(url2, "/html/body/div[2]/div/main/div/article/div[1]/div[1]/figure[4]/table/tbody/tr/th"))
Democrat_Lean2 <- xml_text(xml_find_all(url2, "//td[@data-th='Democrat/Lean Democratic']"))
Republican_Lean2 <- xml_text(xml_find_all(url2, "//td[@data-th='Republican/Lean Republican']"))
Democratic_Advantage2 <- xml_text(xml_find_all(url2, "//td[@data-th='Democratic advantage']"))
Democratic_Advantage2 <- Democratic_Advantage2[-(1:10)]
N2 <- xml_text(xml_find_all(url2, "//td[@data-th='N']"))
Classification2 <- xml_text(xml_find_all(url2, "//td[@data-th='State type']"))

#Data Frame of Scraping and Additional Data

Party2 <- data.frame(jurisdiction2, Democrat_Lean2, Republican_Lean2,Democratic_Advantage2, N2, Classification2)
Party2$Year2 <- 2016

names(Party2)[names(Party2)=="jurisdiction2"]<-"jurisdiction1"
names(Party2)[names(Party2)=="Democrat_Lean2"]<-"Democrat_Lean1"
names(Party2)[names(Party2)=="Republican_Lean2"]<-"Republican_Lean1"
names(Party2)[names(Party2)=="Democratic_Advantage2"]<-"Democratic_Advantage1"
names(Party2)[names(Party2)=="N2"]<-"N1"
names(Party2)[names(Party2)=="Classification2"]<-"Classification1"
names(Party2)[names(Party2)=="Year2"]<-"Year1"

#All states with 2 names, are now as one b/c they wouldn't merge otherwise

Party2$jurisdiction1[which(Party2$jurisdiction1 == "New Hampshire")] <- 'NewHampshire'
Party1$jurisdiction1[which(Party1$jurisdiction1 == "New Hampshire")] <- 'NewHampshire'

Party2$jurisdiction1[which(Party2$jurisdiction1 == "New Jersey")] <- 'NewJersey'
Party1$jurisdiction1[which(Party1$jurisdiction1 == "New Jersey")] <- 'NewJersey'

Party2$jurisdiction1[which(Party2$jurisdiction1 == "New York")] <- 'NewYork'
Party1$jurisdiction1[which(Party1$jurisdiction1 == "New York")] <- 'NewYork'

Party2$jurisdiction1[which(Party2$jurisdiction1 == "New Mexico")] <- 'NewMexico'
Party1$jurisdiction1[which(Party1$jurisdiction1 == "New Mexico")] <- 'NewMexico'

Party2$jurisdiction1[which(Party2$jurisdiction1 == "North Carolina")] <- 'NorthCarolina'
Party1$jurisdiction1[which(Party1$jurisdiction1 == "North Carolina")] <- 'NorthCarolina'

Party2$jurisdiction1[which(Party2$jurisdiction1 == "South Carolina")] <- 'SouthCarolina'
Party1$jurisdiction1[which(Party1$jurisdiction1 == "South Carolina")] <- 'SouthCarolina'

Party2$jurisdiction1[which(Party2$jurisdiction1 == "Rhode Island")] <- 'RhodeIsland'
Party1$jurisdiction1[which(Party1$jurisdiction1 == "Rhode Island")] <- 'RhodeIsland'

Party2$jurisdiction1[which(Party2$jurisdiction1 == "South Dakota")] <- 'SouthDakota'
Party1$jurisdiction1[which(Party1$jurisdiction1 == "South Dakota")] <- 'SouthDakota'

Party2$jurisdiction1[which(Party2$jurisdiction1 == "North Dakota")] <- 'NorthDakota'
Party1$jurisdiction1[which(Party1$jurisdiction1 == "North Dakota")] <- 'NorthDakota'

Party2$jurisdiction1[which(Party2$jurisdiction1 == "West Virginia")] <- 'WestVirginia'
Party1$jurisdiction1[which(Party1$jurisdiction1 == "West Virginia")] <- 'WestVirginia'

Party_All <- rbind(Party1, Party2, Other)



