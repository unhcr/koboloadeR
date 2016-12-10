
###########################################################
### As part of the data exploration we want to explore possible correlations
## We can chisq or fisher test depending on the # of observation to see if the independance analysis is confirmed

names(data)



prop.table(table(data$Household_information.Family_Size))


## looking at correlation between categorical variables

table(data$What_are_you_current_locations, data$Are_you_intending_to_return_to)
chisq.test(data$What_are_you_current_locations, data$Are_you_intending_to_return_to, correct=FALSE)
fisher.test(data$What_are_you_current_locations, data$Are_you_intending_to_return_to)


