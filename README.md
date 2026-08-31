# Undergraduate Research
My undergraduate research is in sports analytics, specifically Formula One (F1). In Spring 2026 I answered the question *"does age impact the success of drivers in Formula One?"* I then presented my research at Murray State University's Spring Scholars Week Poster Competition. At this competition, I was awarded second place in undergraduate research. This fall semester, I am continuing my research into this question by broadening it to other sports to compare the peak ages of individual sports as well as what other factors might drive success in these sports.

## Fall 2026
Currently adding constructor position and number of teams a driver has raced for varbiales.

## Spring 2026
Hypothesis: The age of Formula One drivers impacts the total amount of points they get in a season. I suspect an initial rise in points at the beginning of a driver's career and then, after a certain age, I expect the total points to start decreasing.\
**Methods**
* Data was taken from the Formula One website
* Since multiple drivers stay on for multiple years in F1, the test is considered a repeated measures
* A Linear Mixed Model (LMM) was chosen because of the repeated measures
* The LMM was combined with the generalized additive model (GAM) because of the non-linear trend in the scatterplot of the data
* Several models were created and an AIC table was used to determine which model was the best fit

**Results/Conclusion**

Using the alpha-level 0.05, the predictor variable age is a significant predictor. However, the predictor Season and the interaction between age and Season were not significant predictors of driver success.
