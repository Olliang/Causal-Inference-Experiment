# Inferential Experiment Design

[Note: This is my class notes collected from [Mochen Yang](https://carlsonschool.umn.edu/faculty/mochen-yang)'s class. Copy right belongs to him.]


![process](https://github.com/Olliang/MSBA-Stats-Notes/blob/master/images/common%20thought%20process.png)


## 1. Correlation and Causation

A causal relationship between A and B means A and B are correlated. 

Correlation between A and B is not equal to a causal relationship between A and B.





## 2. Threats to Causal Inference

- **Selection**:  our sample is not representative

example - does product ratings on Amazon always reflect the true quality or customer satisfaction of a product? No. people who are very satisfied or very unsatisfied with the product would tend to rate their product. ---> Self-selection bias

- **Simultaneity**: X and Y cause each other

Rich get richer

The price and demand are affecting each other - the observed price and demand in your data is likely a result of complicated market process.

There is no way to isolate what portion of the observed association is due to X's effect on Y 

- **Omitted variables**: unobserved confounders for X

example - sales of sun glasses is strongly correlated with sales of ice creams. But they don't have causal relationships because they are caused by an omitted variable - weather. 

If X and Z are correlated, and Z affects Y, then *the effect of Z will be attributed to X* in your regression, inflating or deflating the estimates. 

- **Measurement error**: our measure of X is problematic

similar as omitted variable bias, you are omitting the measurement error component.

Cov(e,X) =0 is an example of random error

Cov(e,X) /= 0 is an example of systematic error

systematic measurement error can lead to bias





## 3. Endogeneity

Omitted variables, simultaneity, and measurement error issues are often referred to as sources of **endogeneity**. 

- some independent variable is correlated with the regression error term.

  (X is associated with relevant, yet unmodeled factors in the regression)



To establish **causality**: 

1. correlation between X and Y; 
2. Free from endogeneity
3. Variance in X precedes variance in Y



## 4. Experiment with Randomization

**4.1 Randomized controlled trials**

The researcher intervenes the manipulate X, at random, such that it is nearly guaranteed not to correlate with the error term.

- eliminate any possibility of an unobserved confound or simultaneity.

**Method:** Block randomization

- sort subjects into "groups" (such as gender), then apply simple randomization within each group.

**Randomization Check:** compare the treatment and control groups on observed features - there should not be any statistically significant difference between the two groups.



**4.2 How much data do we need?**

How large do we expect the treatment effect to be? 

- It takes more data to detect a smaller effect.

How sure do we wish to be?

- it takes more data to be more certain about our estimations.
- Need to set **significance level, and power level**



**Type1 error:** False Positive

- Claiming there is a treatment effect when there is not (reject the null when you shouldn't)
- Controlled by **significance level** (set a *low* significance level like 0.05 reduces the chance of type 1 error)

**Type2 error:** False Negative

- fail to find a treatment effect when there is an effect (fail to reject the null when you should) 

- controlled by **power level**(power: 1 - probability of Type2 error) (set a *high* power threshold like 0.8 reduces the chance of Type2 error)

- *Underpowered* experiment: refers to having a sample that is too small, then you can not reliably detect the effect.

  

Having more data generally reduces both types of errors, but needs to consider costs.



**4.3 Other threats to the experiment**

- interference

there are treatment spillovers between subjects in the two groups.

- are subjects aware of the experiment?

If so, the might change their behaviors

- social desirability bias

They may conceal behaviors they think are undesirable, or lie about behaviors

- demand effects 

doing what they think the experimenter would want them to do.




**Example:** The company is certainly profitable for the company to increase rental. Your manager wants to know how customers respond to price change.

- We can manually change price, so the demand would not affect price, which would avoid simultaneity issues.
- variables: whether or not receiving discount, discount amount varies randomly.
- Management wants to know: 
  - with at least 90% confidence, does a 50 cent discount increase rentals by at least 20%?  (treatment effect on average for all movies)
    - means we need to detect a difference of 0.1 (0.5*0.2)
    - alpha = 0.1
    - power = 0.8 (typical assumption)
  - are the effects of price discounts more effective for any particular kinds of movies? (treatment effect for different types of movies) 
    - movie popularity (likes)
    - how much rental of the movie is  (base price)

```r
# Descriptive plots: distributions of leases and prices
hist(movie$leases)
hist(movie$price)

#1. randomization check
#### Using Two Sample t-test
# "treatment" is discount on price, we can make a dummy variable of "receiving treatment or not" to facilitate randomization check
movie = movie %>% mutate(discount = base_price - price,
                         has_discount = ifelse(discount > 0, 1, 0))
# check randomization effort on base_price and likes
t.test(likes ~ has_discount, data = movie)
t.test(base_price ~ has_discount, data = movie)
# H0: true difference in means is equal to 0
# p-value is too large -> could not reject H0
# randomization check looks OK


#2. estimate treatment effect  (expectation management)

# Let's evaluate statistical power now.
# Regardless how much data we have, how big a sample we would need to detect the 20% change they hope to find?
power.t.test(n=NULL,type=c("two.sample"),power=0.8,sig.level=0.1,delta=0.1)
# sample size... 118 movies per group.
# delta: expected difference: 0.1 = 0.5 *0.2
### we are severely underpowered since we can only detect the difference management asked with at least 1273 subjects in each group.

# What sort of difference we can reliably detect with our current?
power.t.test(n=118,type=c("two.sample"),power=0.8,sig.level=0.1,delta=NULL)
## we can detect the effect of 60% increase using the current sample data we have.
# Do we have sufficient sample? What's the implication / advice for management?
# No, we have a highly insufficient sample to detect the desired effect. The general advice is to collect more data.

#3. explore treatment heterogeneity
m1 = lm(log(leases+1) ~ has_discount, data = movie)
summary(m1)
m2 = lm(lm(log(leases+1) ~ discount, data = movie)
summary(m2)
m3 = lm(leases ~ log(discount+1), data = movie)
summary(m3)
# discount does have a positive effect on leases, but marginally significant (again, due to small sample)

        
# Does treatment effect vary with base price?
m4 = lm(leases ~ has_discount + base_price + has_discount*base_price, data = movie)
summary(m4)
m5 = lm(log(leases +1) ~ log(discount+1) + base_price + log(discount+1)*base_price, data = movie)
summary(m5)
# all not significant, we do not have enough sample data to detect whether the treatment effect vary with base price.


# Does treatment effect vary with movie popularity?
m6 = lm(leases ~ has_discount + likes + has_discount*likes, data = movie)
summary(m6)
m7 = lm(log(leases+1) ~ log(discount+1) + likes + log(discount+1)*likes, data = movie)
summary(m7)

# What can we conclude?
# If a movie is really good, "I don't care what it costs!"
# The strength of the moderation, however, is pretty small from a practical point of view.
# How to read the coefficient of the interaction term is negative => when you have a popular movie, the positive effect of the discount on lease is reduced.
```

![technical notes](https://github.com/Olliang/MSBA-Stats-Notes/blob/master/images/experiment%20-%20technical.PNG)


## 5. Matching 

What we can do if we don't have an experiment?

- Find pairs of treated unit and control unit that are as similar as possible on observed variables, except for treatment assignment.
- conceptually same as "randomization check"

**5.1 Propensity score matching:**

For each treated observation, find control observations with closest propensity scores (up to a threshold, called caliper)

**propensity score:** the probability of getting treatment given all the factors we found.

**Assumption:** treatment assignment is fully determined by observed variables.

**problems:** 

1. omitted variable bias (no solution unless we run actual experiment)

2. lack of support - there may not be any good matches
3. propensity function - using logit or probit to estimate propensity scores is very arbitrary.

**Solutions:**

1. evaluate quality of matches - use different calipers, visually check matching quality
2. use more flexible matching function

**Sensitivity analysis:**

- explore the robustness of the result to alternative matching techniques.

- try different propensity functions

- try different parameters for PSM: different values of caliper, number of nearest neighbor, matching with replacement vs. without replacement.

Note: Matching can be paired with DID to ensure the quality of your treatment and control groups.

**Example:**

Does the introduction of Time-shift TV increase the amount of TV consumed?

Unit of analysis: the household

Treatment: availability of TSTV

period of analysis: weekly observations around the introduction of TSTV

Possible outcome: Total hours viewed of TV

**Process summary:** 

- Check whether two groups are randomized
- If they are not, then we should do matching to get a subgroup that are actually eligible in terms of randomization for treatment effect estimation
- Do matching and get the matched data
- Estimate treatment effect on the matched data

```r
library(dplyr)
library(ggplot2)
library(MatchIt)

# import data
data = read.csv("TSTV-Obs-Dataset.csv")

# Data Exploration
# When does the treatment begin?
# This is recorded by the "after" variable
min(data$week)
max(data$week)
min(data %>% filter(after==1) %>% select(week))

# How many and what proportion of customers were treated with TSTV?
# This is recorded by the "premium" variable
data %>% filter(premium == 1) %>% select(id) %>% unique() %>% nrow()
data %>% filter(premium == 0) %>% select(id) %>% unique() %>% nrow()

#How are the viewership variables distributed?
hist(data$view_time_total_hr)
hist(data$view_time_live_hr)
hist(data$view_time_tstv_hr)


# Let's just look at what is going on with average viewership behavior for treated vs. untreated, in the weeks around the treatment date.
# Create aggregated viewership data by averaging across households in the same group
week_ave = data %>% group_by(week, premium) %>%
  summarise(ave_view_total = mean(view_time_total_hr),
            ave_view_live = mean(view_time_live_hr),
            ave_view_tstv = mean(view_time_tstv_hr)) %>% ungroup()

# plot for total TV time
ggplot(week_ave, aes(x = week, y = ave_view_total, color = factor(premium))) + 
  geom_line() + 
  geom_vline(xintercept = 2227, linetype='dotted') + 
  ylim(0, 6) + xlim(2220,2233) + 
  theme_bw()

# plot for live TV time
ggplot(week_ave, aes(x = week, y = ave_view_live, color = factor(premium))) + 
  geom_line() + 
  geom_vline(xintercept = 2227, linetype='dotted') + 
  ylim(0, 6) + xlim(2220,2233) + 
  theme_bw()

# plot for TSTV time
ggplot(week_ave, aes(x = week, y = ave_view_tstv, color = factor(premium))) + 
  geom_line() + 
  geom_vline(xintercept = 2227, linetype='dotted') + 
  ylim(0, 6) + xlim(2220,2233) + 
  theme_bw()

### conclusion: the treatment and control groups are not randomized. Therefore, we can not use all the observations to estimate the treatment effect.
### We should select our the matching pairs to estimate the treatment effect.

# Propensity Score Matching

#For this demonstration, we will use data from the pre-period for matching, then estimate the effect of TSTV gifting in the post period.

# create a dataset of before vs. after for convenience
data_summary = data %>% group_by(id, after) %>%
  summarise_all(mean) %>% ungroup()

# Check covariance balancing with t.test
data_pre = data_summary %>% filter(after == 0)
t.test(view_time_total_hr ~ premium, data = data_pre)

# Let's see what propensity scores distribution look like 
PScore = glm(premium ~ view_time_total_hr, data = data_pre, family = "binomial")$fitted.values
data_pre$PScore = PScore
ggplot(data_pre, aes(x = PScore, color = factor(premium))) +
  geom_density()
## Conclusion: there are some descrepencies, we need to somehow balance them.

# Perform Matching
# Note: the matchit command may take a long time to run with large datasets
match_output <- matchit(premium ~ view_time_total_hr, data = data_pre, method = 'nearest', distance = "logit", caliper = 0.001, replace = FALSE, ratio = 1)
summary(match_output)
data_match = match.data(match_output)

# Evaluate covariance balance again, after matching
t.test(view_time_total_hr ~ premium, data = data_match)
ggplot(data_match, aes(x = PScore, color = factor(premium))) +
  geom_density()


#Now let's estimate the treatment effect with vs. without matching.
data_post = data_summary %>% filter(after == 1)

model_unmatch = lm(log(view_time_total_hr+1)~ premium, data = data_post)
summary(model_unmatch)

model_match = lm(log(view_time_total_hr+1)~ premium, data = data_post %>% filter(id %in% data_match$id))
summary(model_match)

# What difference do you see, with and without matching?


# Sensitivity checks:
# 1. change caliper to 0.005
match_output <- matchit(premium ~ view_time_total_hr, data = data_pre,
                        method = 'nearest', distance = "logit", caliper = 0.005, replace = FALSE, ratio = 1)
# 2. match with replacement
match_output <- matchit(premium ~ view_time_total_hr, data = data_pre,
                        method = 'nearest', distance = "logit", caliper = 0.005, replace = TRUE, ratio = 1)
# 3. match 1 treated unit with 2 control units
match_output <- matchit(premium ~ view_time_total_hr, data = data_pre,
                        method = 'nearest', distance = "logit", caliper = 0.005, replace = TRUE, ratio = 2)
```


![technical notes](https://github.com/Olliang/MSBA-Stats-Notes/blob/master/images/matching%20-%20technical.PNG)



## 6. Panel Data Models

What if we do not observe a confound?

- if we have multiple observations for each entity in our data, it turns out we can do a lot!

There are some confounds do not vary over time, and we can use fixed effect modeling to eliminate the time-invariant confounds (stable characteristics of the individual).

**Issues:** 

1. you might not reliably estimate the time-invariant causal effects in a fixed effect regression
2. Panel data regressions do not resolve time-varying endogeneity issues.
3. Only design for linear regression

**Fixed effect or Random effect?**

Depends on the assumption. If you do know that the time-invariant confound is uncorrelated with X, Random effect will give you a more precise result.

- Random effect assumes confound and X are uncorrelated
- Fixed effect do not make this assumption

Method: Hausman Test

- reject, use fixed effect
- fail to reject, use random effect



Example:

data: Facebook posts collected from 41 companies' Facebook business pages.

dependent variable: the number of likes received by each post.

We are canceling out the potential confound that is unique for different **companies** each post belongs to.

```r
library(dplyr)
library(plm)

# read data
data = read.csv("FB data.csv") %>%
  filter(WC > 3 & likes_count < 100) %>%
  select(likes_count, WC, Posemo, Negemo, picture, type, company, postId)

# naive regression
model1 = lm(likes_count ~ WC + Posemo + Negemo + picture + type, data = data)
summary(model1)

# with company fixed-effect regression
FE_model = plm(likes_count ~ WC + Posemo + Negemo + picture + type, data = data, index = "company", effect="individual", model="within")
summary(FE_model)
# with company random-effect regression
RE_model = plm(likes_count ~ WC + Posemo + Negemo + picture + type, data = data, index = "company", effect="individual", model="random")
summary(RE_model)

### Type video effect on the number of likes are no longer significant as it is in the naive regression. The confound of being different companies affects the effect bringing from whether the post is a video or not on the number of likes.

# Hausman test - what can we conclude from this?
# H0: correlation between the confound and treatment variable is 0
# rejected, I have a strong reason to use the fixed effect regression
phtest(FE_model, RE_model)
```


![technical notes](https://github.com/Olliang/MSBA-Stats-Notes/blob/master/images/Fixed%20effect%20-%20technical.PNG)




## 7. Difference-indifference

How do you handle time-variant confound?

1. take the after-before difference in each group, which account for any individual-level confounds that are time-invariant (conceptually the same as a individual-level fixed effect)
2. calculate the difference in those difference, which accounts for inter-temporal (time-variant) confounds that are common to both individuals.

**Assumption:**

1. **parallel trend:** assume that before the treatment takes place, the treated group and the controlled group have parallel trend on the outcome variable. 
   - because the control group needs to be a good counterfactual for the treated group.
2. **no interference:** the treated subjects are not influencing control subjects.

```r

library(plm)
library(dplyr)
library(ggplot2)

#### Load the data ####
data = read.csv("TSTV-Obs-Dataset.csv")
# As descriptive visualization, let's look at average weekly viewership for both premium and regular viewers
week_ave = data %>% group_by(week, premium) %>%
  summarise(ave_view = mean(view_time_total_hr)) %>% ungroup()
ggplot(week_ave, aes(x = week, y = ave_view, color = factor(premium))) + 
  geom_line() + 
  geom_vline(xintercept = 2227, linetype='dotted') + 
  ylim(0, 6) + xlim(2220,2233) + 
  theme_bw()


#### Difference in Differences Regression ####
# Interpret the treatment effect
did_basic = lm(log(view_time_total_hr+1) ~ premium + after + premium*after, data=data)
summary(did_basic)

## Conclusion: People who got TSTV watched 7% more tv than people who did not get TSTV


# To show that DID is conceptually the same as adding individual-level and week-level dummy variables (fixed-effect)
# Let's try replacing the treatment dummy with subject fixed effects.
# What happened to the estimate of premium?
did_fe = plm(log(view_time_total_hr+1) ~ premium + after + premium*after, data = data, index=c("id"), effect="individual", model="within")
summary(did_fe)

## The DID is conceptually equivalent to fixed effect regression which 

# Further add week fixed effects
did_sfe_tfe = plm(log(view_time_total_hr+1) ~ premium + after + premium*after, data = data, index=c("id", "week"), effect="twoway", model="within")
summary(did_sfe_tfe)

```

How do we evaluate parallel trends?

- run a "dynamic" DID Model:
- **intuition**: we are estimating many DiD regressions all at once. The baseline week is our "pre" period now, and we estimate diff-in-diff relative to every other period, comparing treatment group with control.
- This also allows you to visualize how treatment effect changes overtime!

```r
# Let's try dynamic DiD instead.
did_dyn_sfe_tfe <- lm(log(view_time_total_hr+1) ~ premium + factor(week) + premium*factor(week), data = data)
summary(did_dyn_sfe_tfe)

#### From the significant interaction coefficients starting from week2221, we are confident that there is already some significant systematic difference between two groups before the treatment takes place.
### But how big is this issues? We can plot it out to see.


# Let's retrieve the coefficients and standard errors, and create confidence intervals
model = summary(did_dyn_sfe_tfe)
coefs_ses = as.data.frame(model$coefficients[16:28,c("Estimate", "Std. Error")])
colnames(coefs_ses) = c("beta", "se")
coefs_ses = coefs_ses %>%
  mutate(ub90 = beta + 1.96*se,
         lb90 = beta - 1.96*se,
         week = 1:nrow(coefs_ses))

# Let's connect the estimates with a line and include a ribbon for the CIs. 
ggplot(coefs_ses, aes(x = week, y = beta)) + 
  geom_line() + 
  geom_hline(yintercept=0,linetype="dashed") + 
  geom_vline(xintercept=6,linetype="dashed") + 
  geom_ribbon(aes(ymin = lb90, ymax = ub90), alpha = 0.3) + 
  theme_bw()

### Conclusion: even if the parallel trend assumption for DiD used in this case is violated, since the changes of the treatment effect before the treat ment is not 0, but they are not much deviated from 0. We can still use DiD to estimate the treatment effect with caution.

# Time for our placebo test... 
### To test the robustness of DID regression, we want to use this placebo test to demostrate if there is any special difference happened in the week when the treatment was taken place.

# Let's shift the treatment date back in time (e.g., to week 2228), artificially, and estimate the "treatment" effect
data_placebo = data %>%
  mutate(after_placebo = ifelse(week > 2228, 1, 0))
## Basically run the same did regression on a fake treatment taking place time. Ideally you should have an insignificant or different coeffecient in interaction term for the fake week regression, so that you can prove that 2227 is indeed special.
did_basic_placebo = lm(view_time_total_hr ~ premium + after_placebo + premium*after_placebo, data = data_placebo)
summary(did_basic_placebo)
```

![technical notes](https://github.com/Olliang/MSBA-Stats-Notes/blob/master/images/DID-technical.PNG)
