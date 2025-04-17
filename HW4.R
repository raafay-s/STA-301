#Question 1 Slide 1
holiday %>%
 group_by(artist) %>%
 summarize(count = n()) %>%
 arrange(desc(count))

holiday %>%
 group_by(track) %>%
 summarize(count = n()) %>%
 arrange(desc(count))

holiday %>%
 select(track, energy)%>%
 arrange(desc(energy))

holiday %>%
 select(track, valence)%>%
 arrange(valence)

#Question 1 Slide 2
ggplot(holiday) +
 geom_boxplot(aes(x = key, y = danceability)) +
 labs(title = "Danceability Distribution Across Musical Keys",
  x = "Key",
  y = "Danceability") +
 theme_minimal() +
 theme(axis.text = element_text(size = 25))+
 theme(axis.title = element_text(size = 25))+
 theme(title = element_text(size = 30))


#Question 2 Slide 1
shelter %>%
 filter(animal_breed == "BEAGLE") %>%
 group_by(month) %>%
 summarize(count = n()) %>%
 arrange(count)

shelter %>%
 filter(animal_type == "WILDLIFE") %>%
 group_by(month) %>%
 summarize(count = n()) %>%
 arrange(desc(count))

shelter = shelter %>%
 mutate(dog = ifelse(animal_type == 'DOG', 'dog', 'not_dog'),
        cat = ifelse(animal_type == 'CAT', 'cat', 'not_cat'),
        stray = ifelse(intake_type == 'STRAY', 'stray', 'other_intake'),
        surrendered = ifelse(intake_type == 'OWNER SURRENDER', 'surrendered', 'other_intake'))

breeds = shelter %>%
 group_by(animal_breed) %>%
 summarize(animals = n(),
           adoption_rate = sum(outcome_type == 'ADOPTION')/animals)

filtered_breeds = breeds %>%
 filter(animals >= 25) %>%
 arrange(desc(adoption_rate))

filtered_breeds


#Question 2 Slide 2
breeds = shelter %>%
 group_by(animal_breed) %>%
 summarize(animals = n(),
           adoption_rate = sum(outcome_type == 'ADOPTION')/animals)
shelter %>%
 mutate(dog = ifelse(animal_type == "DOG", "dog", "not_dog")) %>%
 summarize(proportion_dogs = mean(dog == "dog"))

shelter %>%
 mutate(neither_cats_dogs = ifelse(!(animal_type %in% c("DOG", "CAT")), 1, 0)) %>%
 summarize(proportion_neither_cats_dogs = mean(neither_cats_dogs))

shelter %>%
 mutate(
  dog = ifelse(animal_type == "DOG", "dog", "not_dog"),
  stray = ifelse(intake_type == "STRAY", "stray", "other_intake")
 ) %>%
 filter(stray == "other_intake") %>%
 summarize(probability_not_dog = mean(dog == "not_dog"))

contingency_table <- table(
 shelter %>% mutate(cat = ifelse(animal_type == "CAT", "cat", "not_cat")) %>% pull(cat),
 shelter %>% mutate(surrendered = ifelse(intake_type == "OWNER SURRENDER", "surrendered", "other_intake")) %>% pull(surrendered)
)

chisq.test(contingency_table)


shelter %>%
 mutate(
  cat = ifelse(animal_type == "CAT", "cat", "not_cat"),
  surrendered = ifelse(intake_type == "OWNER SURRENDER", "surrendered", "other_intake")
 ) %>%
 ggplot(aes(x = surrendered, fill = cat)) +
 geom_bar(position = "fill") +
 labs(
  title = "Proportion of Cats and Non-Cats by Surrendered Status",
  x = "Surrendered Status",
  y = "Proportion",
  fill = "Cat Status"
 ) +
 theme_minimal() +
 theme(axis.text = element_text(size = 25))+
 theme(axis.title = element_text(size = 25))+
 theme(title = element_text(size = 30))

#Question 3 Slide 1
ercot_summary = ERCOT %>%
 mutate(weekday_label = ifelse(weekday == 1, "Weekday", "Weekend")) %>%
 group_by(weekday_label) %>%
 summarize(mean_power = mean(power),
  sd_power = sd(power),
  count = n())

ercot_summary

#Question 3 Slide 2
lmmodel_ercot = ggplot(ERCOT) +
 geom_point(aes(x=temperature, y=power), alpha = 0.2) +
 geom_smooth(aes(x=temperature, y=power), method='lm') +
 facet_wrap(~weekday, nrow=4) +
 labs(title = "Temperature's Effect on Power Consumption: Weekday vs. Weekend Trends", x="Percentage of Minority Residents (%)",
      y="FAIR Policies per 100 Housing Units")
lmmodel_ercot

lmmodel_ercot = lm(temperature ~ power + weekday, data=ERCOT)
summary(lmmodel_ercot)

#Question 4 Slide 1
ggplot(redlining) +
 geom_point(aes(x=minority, y=FAIR)) +
 geom_smooth(aes(x=minority, y=FAIR), method='lm') +
 labs(title = "Relationship Between Minority Population and FAIR Plan Policies",
      x = "Minority %", y = "FAIR plan policies (per 100 housing units)")

#Question 4 Slide 2
multi_reg_model = lm(FAIR ~ minority + fires + age + income, data = redlining)

redlining_std <- redlining %>%
 mutate(
  FAIR_z = scale(FAIR),
  minority_z = scale(minority),
  fires_z = scale(fires),
  age_z = scale(age),
  income_z = scale(income)
 )

std_model = lm(FAIR_z ~ minority_z + fires_z + age_z + income_z, data = redlining_std)

std_coefficients <- coef(std_model)
conf_intervals <- confint(std_model)

std_results <- data.frame(
 Variable = c("Intercept", "Minority", "Fires", "Age", "Income"),
 `Standardized Coefficient` = round(std_coefficients, 2),
 `Lower 95% CI` = round(conf_intervals[, 1], 2),
 `Upper 95% CI` = round(conf_intervals[, 2], 2)
)