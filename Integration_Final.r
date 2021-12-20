#RUN SECOND
#Data Set about incarceration rates in the united states

incarceration <- read.csv("incarc.csv", sep = ",", header = T)
incarceration$jurisdiction1 <- trimws(incarceration$jurisdiction1, "both")

incarceration$jurisdiction1[which(incarceration$jurisdiction1 == "New Hampshire")] <- 'NewHampshire'
incarceration$jurisdiction1[which(incarceration$jurisdiction1 == "New Jersey")] <- 'NewJersey'
incarceration$jurisdiction1[which(incarceration$jurisdiction1 == "New York")] <- 'NewYork'
incarceration$jurisdiction1[which(incarceration$jurisdiction1 == "New Mexico")] <- 'NewMexico'
incarceration$jurisdiction1[which(incarceration$jurisdiction1 == "North Carolina")] <- 'NorthCarolina'
incarceration$jurisdiction1[which(incarceration$jurisdiction1 == "South Carolina")] <- 'SouthCarolina'
incarceration$jurisdiction1[which(incarceration$jurisdiction1 == "Rhode Island")] <- 'RhodeIsland'
incarceration$jurisdiction1[which(incarceration$jurisdiction1 == "South Dakota")] <- 'SouthDakota'
incarceration$jurisdiction1[which(incarceration$jurisdiction1 == "North Dakota")] <- 'NorthDakota'
incarceration$jurisdiction1[which(incarceration$jurisdiction1 == "West Virginia")] <- 'WestVirginia'

#Merged Scraped data/Incarceration Data

all <- merge(incarceration, Party_All, by=c("jurisdiction1","Year1"), all.x = T)



