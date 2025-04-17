#Question 1 Part A:
ggplot(TS)+
 geom_histogram(aes(x=danceability))+
 labs(x="Danceability Score (range: 0 to 1)", 
      y="Count of Songs", 
      title="Distribution of Danceability Scores in Taylor Swift's Songs")+
 theme(axis.text = element_text(size = 25))+
 theme(axis.title = element_text(size = 25))+
 theme(title = element_text(size = 30))
TS%>%
 summarize(avgmean = mean(danceability))
TS%>%
 summarize(sdd = sd(danceability))

#Question 1 Part B:
TS = TS %>%
 mutate(album = factor(album, levels = c("Taylor Swift", "Speak Now", "1989", "reputation", 
                                         "Lover", "folklore", "evermore", "Fearless", "Red", "Midnights")))
ggplot(TS)+
 geom_boxplot(aes(x=valence, y=album))+
 labs(x="Valence Score (range: 0 to 1)",
      y="Album Name",
      title="Distribution of Valence Scores in Taylor Swift's Albums")+
 theme(axis.text = element_text(size = 25))+
 theme(axis.title = element_text(size = 25))+
 theme(title = element_text(size = 30))

#Question 1 Part C:
TS = TS %>%
 mutate(version=ifelse(album=="Red"|album=="Fearless",
                       yes="Taylor Version", no="Original Version"))
ggplot(TS)+
 geom_point(aes(x=loudness, y=energy))+
 facet_wrap(~version)+
 labs(x="Loudness (dBu)",
      y="Energy Score (range: 0 to 1)",
      title="Energy and Loudness in Taylor Swift's Original vs. Version Songs")+
 theme(axis.text = element_text(size = 25))+
 theme(axis.title = element_text(size = 25))+
 theme(title = element_text(size = 30))

#Question 1 Part D:
TS%>%
 summarize(max_duration=max(duration))
TS%>%
 summarize(mean=mean(duration))
TS%>%
 summarize(sd=sd(duration))
(613-237)/47.3
#Z-score = (613-237)/47.3 = 7.9
TS%>%
 summarize(mean=mean(duration))
TS%>%
 summarize(sd=sd(duration))
(404-237)/47.3
#Z-score = (404-237)/47.3 = 3.5



#Question 2:
xtabs(~death,data=avengers)%>%
 prop.table()%>%
 round(2)
xtabs(~death+return,data=avengers)%>%
 prop.table()%>%
 addmargins%>%
 round(2)
xtabs(~death+return,data=avengers)%>%
 prop.table(margin=1)%>%
 addmargins%>%
 round(2)
xtabs(~death+return,data=avengers)%>%
 prop.table(margin=2)%>%
 addmargins%>%
 round(2) 



#Question 3 Part A:
ad_graph = ads %>%
 group_by(year)%>%
 summarize(n=n(),
           prp=n/nrow(ads))%>%
 arrange(desc(n))%>%
 print(n=22)

ggplot(ad_graph)+
 geom_line(aes(x=year, y=n))+
 labs(title = "Number of Superbowl Ads per Year",
      x="Year", y="Number of Ads")+
 scale_x_continuous(breaks=seq(2000, 2020, by=2))+
 scale_y_continuous(breaks=seq(5, 15, by=1))+
 theme(axis.text = element_text(size = 25))+
 theme(axis.title = element_text(size = 25))+
 theme(title = element_text(size = 30))

#Question 3 Part B:
#1
xtabs(~animals, data=ads)%>%
 prop.table()
#2
ads %>%
 group_by(animals)%>%
 summarize(avg_median = median(views))
#3
ads %>%
 group_by(animals)%>%
 summarize(sd_dislikes = sd(dislikes))
#4
ads %>%
 group_by(animals)%>%
 summarize(q90 = quantile(likes, 0.90))

#Question 3 Part C:
ggplot(ads)+
 geom_bar(aes(y = brand, fill = animals))+
 labs(x="Number of Ads", 
      y="Brand Name", 
      title="Distribution of Danceability Scores in Taylor Swift's Songs",
      fill = "Animal Presence")+
 theme(axis.text = element_text(size = 25))+
 theme(axis.title = element_text(size = 25))+
 theme(title = element_text(size = 30))

#Question 3 Part D:
ggplot(ads)+
 geom_bar(aes(x=celebrity, fill=patriotic))+
 labs(x="Celebrity Appearence",
      y="Number of Ads",
      fill= "Patriotic",
      title = "Association Between Patriotic Ads and Ads with Celebrity Features")+
 theme(axis.text = element_text(size = 25))+
 theme(axis.title = element_text(size = 25))+
 theme(title = element_text(size = 30))

xtabs(~celebrity+patriotic, data = ads)%>%
 prop.table()
xtabs(~celebrity, data = ads)%>%
 prop.table()
xtabs(~patriotic, data = ads)%>%
 prop.table
0.29*0.16
#0.0464 is approximately equal to 0.044

#Question 3 Part E:
ggplot(ads)+
 geom_bar(aes(x=funny))+
 facet_wrap(~danger)+
 labs(x="Ads with and without Humor", y = "Number of Ads",
      title = "Association Between Humor and Danger in Super Bowl Ads")+
 theme(axis.text = element_text(size = 25))+
 theme(axis.title = element_text(size = 25))+
 theme(title = element_text(size = 30))
xtabs(~funny+danger, data =ads)%>%
 prop.table()
xtabs(~funny, data = ads)%>%
 prop.table()
xtabs(~danger, data = ads)%>%
 prop.table
0.69*0.31



##Question 4:
ggplot(all_seasons)+
 geom_point(aes(x=age, y=pts))+
 labs(x="Player Age", y="Average Points per Season", 
      title="Average Points per Season by Player Age")+
 theme(axis.text = element_text(size = 25))+
 theme(axis.title = element_text(size = 25))+
 theme(title = element_text(size = 30))