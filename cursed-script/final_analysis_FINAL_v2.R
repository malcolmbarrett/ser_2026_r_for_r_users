####### smoking & birth weight analysis ########
####### *** this is the GOOD version, use this one!! *** ########

# install.packages("tidyverse")    # run this if it doesn't work

setwd("/Users/sam/Desktop/myproject")

births<-read.csv("births.csv")
births <- clean_names(births)

# restrict to adult moms only
births = births[births$mat_age >= age_min,]

# drop the ones flagged at the data meeting
births=births[!births$id %in% bad_ids,]

attach(births)

# quick look at the data
mean(birth_weight)
table(smoker)
head(births,n=20)


########## FIGURE FOR THE GRANT ###########
# (PI wanted this for the renewal, looks nice)

ggplot(plot_data, aes(x=smoker,y=birth_weight,fill=smoker)) + geom_boxplot() + theme_minimal() + labs(title="Birth weight by smoking status",x="Smoker",y="Birth weight (g)") + theme(legend.position="none")


# make the outcomes
births$low_bw <- ifelse(births$birth_weight < bw_cutoff, 1, 0)
births$preterm <- ifelse(births$ges_age<37,1,0)
births$smoke <- ifelse(births$smoker == "Yes",1,0)

complete <- T
if(complete==T){ births <- births[!is.na(births$birth_weight),] }


library(tidyverse)
library(broom)


######## TABLE 1 NUMBERS (for Table1.docx) ########
print(table(births$low_bw, births$smoker))
print(round(prop.table(table(births$low_bw,births$smoker),2),3))
print(mean(births$mat_age))
print(sd(births$mat_age))


######## THE MODEL ########
model1 <- glm(low_bw ~ smoke + mat_age + mat_bmi + as.factor(education) + parity, data=births, family=binomial)
summary(model1)

# odds ratios -- using my little helper function
make_or_table(model1)


# data for the figure above
plot_data = births


######## preterm model ########
model2 = glm(preterm~smoke+mat_age+mat_bmi+as.factor(education)+parity,data=births,family=binomial)
summary(model2)
make_or_table(model2)


# save cleaned data so i don't have to rerun all this
write.csv(births, "births_clean.csv")

# ... read it back in for the sensitivity analysis
clean = read.csv("births_clean.csv")

# TODO: sensitivity analysis, stratify by race/ethnicity

model3 <- glm(low_bw ~ smoke + mat_age + mat_bmi + as.factor(education) + parity + as.factor(race_eth), data=clean, family=binomial)
summary(model3)
make_or_table(model3)
