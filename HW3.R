options(scipen=77)

#Question 1
lm1 = lm(approval~gas, data = approval)
coef(lm1)

ggplot(approval)+
 geom_point(aes(x=gas, y=approval))+
 labs(title = " Presidential Approval Ratings vs. Gas Prices (2002-2007)",
      x = "Average Monthly Gas Price (USD)",
      y = "Presidential Approval Rating (%)")+
 theme_minimal()+
 theme(axis.text = element_text(size = 25))+
 theme(axis.title = element_text(size = 25))+
 theme(title = element_text(size = 30))

confint(lm1, level = 0.95)

rsquared(lm1)
sigma(lm1)

#Question 2 Part A
ggplot(films)+
 geom_histogram(aes(x = votes, fill = test)) +
 facet_wrap(~ test, ncol = 1) +
 labs(title = "Distribution of IMDb Votes by Bechdel Test Result",
  x = "IMDb Votes",
  y = "Count",
  fill = "Test") +
 theme_minimal()+
 theme(axis.text = element_text(size = 25))+
 theme(axis.title = element_text(size = 25))+
 theme(title = element_text(size = 30))

films%>%
 group_by(test)%>%
 summarize(avg_mean = mean(votes))

#Question 2 Part B
ggplot(films)+
 geom_bar(aes(x=test))+
 facet_wrap(~R_rated)+
 labs(title = "Bechdel Test Results by MPA Rating",
      x = "Bechdel Test Result",
      y = "Count of Films")+
 theme_minimal()+
 theme(axis.text = element_text(size = 25))+
 theme(axis.title = element_text(size = 25))+
 theme(title = element_text(size = 30))


#Question 3
lm2 = lm(Skips ~ Solder + Size, data = ATT)
summary(lm2)
confint(lm2)

ggplot(ATT)+
 geom_boxplot(aes(x = Size, y = Skips))+
 labs(title = "Boxplot of Skips by Solder Gun Opening Size",
      x = "Soldier Gun Opening Size",
      y = "Number of Skips",
      caption = "Distribution of Skips Across Different Opening Sizes")+
 theme(axis.text = element_text(size = 25))+
 theme(axis.title = element_text(size = 25))+
 theme(title = element_text(size = 30))


ggplot(ATT, aes(x = Skips, fill = Solder)) +
 geom_histogram(aes(y = ..density..), binwidth = 1, color = "black", alpha = 0.5) +
 facet_wrap(~ Solder, ncol = 1) +
 labs(title = "Density Histograms of Skips by Solder Thickness",
      x = "Number of Skips",
      y = "Density",
      caption = "Distribution of Skips for Thick and Thin Solder") +
 theme_minimal()+
 theme(axis.text = element_text(size = 25))+
 theme(axis.title = element_text(size = 25))+
 theme(title = element_text(size = 30))
