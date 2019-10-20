library(tidyr)
library(dplyr)
library(RMariaDB)
library(stringr)
library(stringi)
library(ggplot2)


keyword_raw <- read.csv("data_science_keywords.csv", header=TRUE, sep=",")

con <- dbConnect(MariaDB(),host="data607.mysql.database.azure.com", dbname="data607",username="data607@data607", client.flag = CLIENT_COMPRESS, password="!WORK,open,DOWN!")

dbWriteTable(con,"keyword_raw",keyword_raw)

keyword_tidy <- gather(keyword_raw, "Site", "Frequency",2:5)
keyword_tidy$Frequency<- as.numeric(gsub(",","",keyword_tidy$Frequency))
keyword_tidy

dbWriteTable(con, "keyword_tidy", keyword_tidy)

keyword_analysis <- group_by(keyword_tidy, Keyword)
keyword_analysis <- summarize(keyword_analysis, Sum_Frequency = sum(Frequency))
keyword_analysis <- mutate(keyword_analysis, Total=sum(Sum_Frequency))
keyword_analysis <- mutate(keyword_analysis, Pct_Frequency=round(Sum_Frequency/Total,3)*100)
analysis_df <-as.data.frame(keyword_analysis)

ggplot(analysis_df, aes(x=Keyword,y = Pct_Frequency)) +
  geom_bar(width = .75,stat = "identity", position="dodge") +
  ggtitle("Relative Frequency of Keywords In Data Science Job Listings", subtitle="Sites: Monster, SimplyHired, Indeed, LinkedIn") +
  labs(x="Keyword",y="Percentage") +
  theme(plot.title = element_text(hjust=0.5),plot.subtitle=element_text(hjust=0.5),axis.text.x = element_text(angle = 90,hjust=1,vjust=.5)) +
  scale_y_continuous(breaks = seq(0,25,by = 1))
