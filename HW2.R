#Question 1A
artist = tate %>%
 group_by(artist)%>%
 summarize(n=n())%>%
 arrange(desc(n))
head(artist, 10)

#Question 1B
female_artist = tate %>%
 group_by(year)%>%
 filter(gender == 'Female')%>%
 summarize(n=n())
ggplot(female_artist)+
 geom_line(aes(x=year, y=n))+
 labs(title = "Number of Works by Female Artists in the Tate Collection (per Year)",
      x="Year", y="Number of Works")+
 scale_x_continuous(breaks=seq(1545, 2020, by=20))+
 scale_y_continuous(breaks=seq(0, 110, by=10))+
 theme(axis.text = element_text(size = 25))+
 theme(axis.title = element_text(size = 25))+
 theme(title = element_text(size = 30))

#Question 1C
total_area = tate %>%
 mutate(area = height * width)
ggplot(total_area)+
 geom_histogram(aes(x=area), binwidth = 5)+
 labs(title = "Distribution of Artwork Area in the Tate Collection (Square Meters)",
      x="Area (Square Meters)", y="Number of Works")+
 scale_x_continuous(breaks=seq(0, 60, by=5))+
 scale_y_continuous(breaks=seq(0, 30000, by=2500))+
 theme(axis.text = element_text(size = 25))+
 theme(axis.title = element_text(size = 25))+
 theme(title = element_text(size = 30))
total_area%>%
 summarize(area_median = median(area))%>%
 round(2)
total_area%>%
 summarize(area_iqr = IQR(area))%>%
 round(2)

#Question 1D
medium_type = tate %>%
 group_by(medium)%>%
 summarize(count=n())%>%
 arrange(desc(count))
head(medium_type)

total_area = tate %>%
 mutate(area = height * width)
tate_area = tate %>%
 group_by(medium)%>%
 summarize(mean_area = mean(area))%>%
 arrange(desc(mean_area))
head(tate_area, 10)

total_area = tate %>%
 mutate(area = height * width)
tate_sd = tate %>%
 group_by(medium)%>%
 summarize(sd = sd(area))%>%
 arrange(desc(sd))
head(tate_sd, 10)

total_area = tate %>%
 mutate(area = height * width)
max_area = tate %>%
 group_by(medium)%>%
 summarize(max = max(area))%>%
 arrange(desc(max))
head(max_area, 10)

#Question 2 Slide 1
lm1 = lm(Income~Height, data = gss)
summary(lm1)

boot_income = do(10000)*lm(Income~Height, data = resample(gss))
confint(boot_income, level = 0.95) #This shows r-squared CI

ggplot(gss)+
 geom_point(aes(x=Height, y=Income))+
 labs(title = "Scatterplot of Income vs Height",
      x = "Height (in inches)",
      y = "Income in USD (values in scientific notation)")+
 scale_x_continuous(breaks=seq(60, 80, by=5))+
 theme(axis.text = element_text(size = 25))+
 theme(axis.title = element_text(size = 25))+
 theme(title = element_text(size = 30))


#Question 2 Slide 2
boot_income = do(10000)*lm(Income~Height, data = resample(gss))
confint(boot_income, level = 0.95) #This gives us CI for model slope as well as sigma

ggplot(gss)+
 geom_histogram(aes(x=Income))+
 labs(title = "Distribution of Income",
      x = "Income in USD (values in scientific notation)",
      y = "Count")+
 theme(axis.text = element_text(size = 25))+
 theme(axis.title = element_text(size = 25))+
 theme(title = element_text(size = 30))

gss%>%
 summarize(median = median(Income))
 

#Question 3A
georgia = georgia %>%
 mutate(undercounted = round(100*(ballots - votes)/ballots, 2),
        winner = ifelse(bush > gore, "Bush", "Gore"))

diffmean = do(10000)*diffmean(undercounted~equipment, data = resample(georgia))
head(diffmean)
confint(diffmean)

ggplot(georgia)+
 geom_histogram(aes(x=undercounted))+
 facet_wrap(~equipment, nrow=2)+
 labs(title = "Distribution of Undercounted Votes by Voting Equipment Type",
      x = "Percentage of Undercounted Votes",
      y = "Count of Counties")+
 theme(axis.text = element_text(size = 25))+
 theme(axis.title = element_text(size = 25))+
 theme(title = element_text(size = 30))

mean_values = georgia%>%
 group_by(equipment)%>%
 summarize(mean_undercounted = mean(undercounted))
mean_values

#Question 3B
diffprop_boot = do(10000)*diffprop(equipment~winner, data = resample(georgia))
head(diffprop_boot)
confint(diffprop_boot)

ggplot(georgia)+
 geom_bar(aes(x=winner, fill = equipment))+
 labs(title = "Distribution of Voting Equipment by Winning Candidate",
      x = "Winning Candidate",
      y = "Proportion of Counties",
      fill = "Voting Equipment Type")+
 theme(axis.text = element_text(size = 20))+
 theme(axis.title = element_text(size = 20))+
 theme(title = element_text(size = 25))

prop(~equipment, data = georgia)

bush_won = georgia %>%
 filter(winner == "Bush")
prop(~equipment, data = bush_won)

gore_won = georgia %>%
 filter(winner == "Gore")
prop(~equipment, data = gore_won)

#Question 4
fb_sim=do(100000)*nflip(99, prob=0.80)
head(fb_sim)
ggplot(fb_sim)+
 geom_histogram(aes(x=nflip), binwidth=1)+
 labs(title = "Simulated Distribution of Cognitive Scores for Elite Athletes",
      x = "Scores on the S2 Cognition Test",
      y = "Count",
      fill = "Voting Equipment Type")+
 theme(axis.text = element_text(size = 20))+
 theme(axis.title = element_text(size = 20))+
 theme(title = element_text(size = 25))+
 scale_x_continuous(breaks=seq(60, 90, by=5))

sum(fb_sim>88)
p_value = sum(fb_sim>88)/100000
p_value