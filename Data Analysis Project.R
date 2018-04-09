## Research quesion 1:Relationship between sleep time and general health
# There are two outliers for slep time that does not make any sense so remove.
remsleepout <- filter(brfss2013, sleptim1 < 100)

# Create df general health and mean of sleep time for each general health catagory.
hlthSleep <- remsleepout %>% group_by(genhlth) %>% summarise(mn_sleep = mean(sleptim1))

# display
hlthSleep

# make plots of mean sleep grouped by genhlth

ggplot(hlthSleep, aes(genhlth, mn_sleep)) +
  geom_point(aes(genhlth, mn_sleep)) +
  labs(title="mean hours of sleep for each general health self-rating",
       x="general health rating", y="mean hours of sleep")

## Research quesion 2: Relationship between exercising in last 30 days and income and employment status.

# Remove all data with NA's.
phyact_rm.na <- filter(brfss2013, !is.na(exerany2), !is.na(income2), !is.na(employ1))

# Find porportion that does any exercise grouped by income.
phyact_income <- phyact_rm.na %>% group_by(income2) %>% summarise(prop_exer = sum(exerany2 == "Yes") / n())

# Print.
phyact_income

# Plot - first replace spaces with line breaks in income2 names.
levels(phyact_income$income2) <- gsub(" ", "\n", levels(phyact_income$income2))
ggplot(phyact_income, aes(income2, prop_exer)) +
  geom_point(aes(income2, prop_exer)) +
  labs(title="proportion who exercised in last 30 days vs. income", x="income", y="proportion exercese")

# Find porportion that does any exercise grouped by employment status.
phyact_employ <- phyact_rm.na %>% group_by(employ1) %>% summarise(prop_exer = sum(exerany2 == "Yes") / n())

# Print.
phyact_employ

# Plot

levels(phyact_employ$employ1) <- gsub(" ", "\n", levels(phyact_employ$employ1))
ggplot(phyact_employ, aes(employ1, prop_exer)) +
  geom_point(aes(employ1, prop_exer)) +
  labs(title="proportion exercese in last 30 days vs employment status",
       x="employment status", y="proportion exercise")

Research quesion 3: Sugar intake vs weight.

# Remove NA's
sugar <- filter(brfss2013, !is.na(weight2), !is.na(height3), !is.na(ssbsugar), !is.na(ssbfrut2), weight2 != "")

# Select only columns I'll be working with.
sugar <- select(sugar, weight2, height3, ssbsugar, ssbfrut2)

# height3 given in ft and inches or in cm.  Convert to all inches, ht_inches.
# Make column of height as string
sugar <- mutate(sugar, ht_string = as.character(height3))
# Get first char of ht_string - it's the feet or if "9" height in cm. Convert to numeric.
sugar <- mutate(sugar, ft = as.numeric(substring(ht_string, 1, 1)))
# Get rest of char which is inches part of ft inches or cm if metric.  Convert to numeric.
sugar <- mutate(sugar, inches = as.numeric(substring(ht_string, 2, 4)))
# now find total ht in inches.  Either ft and inches or cm.
sugar <- mutate(sugar, ht_inches = ifelse(ft != 9, 12*ft + inches, round(0.393701*inches, digits = 0)))

# height given in pounds or sometimes with leading 9 in Kg.  So convert leading 9 form kg to lbs.
sugar <- mutate(sugar, wt_char = as.character(weight2))
sugar <- mutate(sugar, wt_lbs =  ifelse( nchar(wt_char) != 4, as.numeric(wt_char), round((as.numeric(wt_char) - 9000)*2.20462, digits = 0)))

# Remove a wt_lbs NA.
sugar <- filter(sugar, !is.na(wt_lbs))

# Find BMI - Body Mass Index.  Round to nearest whole.
sugar <- mutate(sugar, bmi = round(703*wt_lbs/(ht_inches*ht_inches), digits = 0))

# select only what I need
sugar <- select(sugar, bmi, ssbsugar, ssbfrut2)

#Now convert the amount of sugar drinks to same period - 1 year.
sugar <- mutate(sugar, ssbsugar_char = as.character(ssbsugar))
sugar <- mutate(sugar, sugar_period = substring(ssbsugar_char, 1, 1))
sugar <- mutate(sugar, sugar_amount = ifelse(sugar_period == "0", 0, as.numeric(substring(ssbsugar_char, 2, 3))))
sugar <- mutate(sugar, soda_year = ifelse(sugar_amount == 0, 0, ifelse(sugar_period == "1", 365*sugar_amount, ifelse(sugar_period == "2", 52*sugar_amount, 12*sugar_amount))))

#Now convert the amount of sugar-sweatened fruit drinks to same period - 1 year.
sugar <- mutate(sugar, ssbfrut2_char = as.character(ssbfrut2))
sugar <- mutate(sugar, frut2_period = substring(ssbfrut2_char, 1, 1))
sugar <- mutate(sugar, frut2_amount = ifelse(frut2_period == "0", 0, as.numeric(substring(ssbfrut2_char, 2, 3))))
sugar <- mutate(sugar, frut2_year = ifelse(frut2_amount == 0, 0, ifelse(frut2_period == "1", 365*frut2_amount, ifelse(frut2_period == "2", 52*frut2_amount, 12*frut2_amount))))

#Now keep only what I need
sugar <- select(sugar, bmi, soda_year, frut2_year)

ggplot(sugar, aes(soda_year, bmi)) +
  geom_point(aes(soda_year, bmi)) +
  labs(title="bmi vs number of sodas consumed per year",
       x="number of sodas consumed per year", y="bmi")

ggplot(sugar, aes(frut2_year, bmi)) +
  geom_point(aes(frut2_year, bmi)) +
  labs(title="bmi vs number of sweetened fruit juices consumed per year",
       x="number of sweetened fruit drinks consumed per year", y="bmi")


