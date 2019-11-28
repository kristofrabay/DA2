library(tidyverse)
library(estimatr)

### LOADING THE DATA, FILTERING FOR ANALYSIS
 
hotels_features <- read_csv("../../assignment_2/hotels-europe_features.csv")
hotels_prices <- read_csv("../../assignment_2/hotels-europe_price.csv")
hotels <- left_join(hotels_prices, hotels_features, by = "hotel_id")

hotels %>% group_by(city) %>% summarize(count = n()) %>% arrange(desc(count)) %>% View()

bp <- hotels %>% filter(city == "Budapest" & 
                              nnights == 1 & 
                              month == 11 &
                              holiday == 0 &
                              weekend == 0)

#Encoding(bp$neighbourhood) <- "UTF-8"

View(bp)

### CREATING BINARY VARIABLE: HIGH_RATING

bp$high_rating <- ifelse(bp$rating >= 4, 1, 0)

write_csv(bp, "logit_probit/bp_hotels.csv")

### HIGH_RATING ~ PRICE + DISTANCE + STARS

bp_nona <- bp %>% filter(!is.na(high_rating))

View(anti_join(bp,bp_nona, by = "hotel_id")) #8 didnt match, they had NA rating values

bp <- bp_nona

summary(lm_robust(high_rating ~ price, data = bp)) #r2 3.9%
summary(lm_robust(high_rating ~ distance, data = bp)) #r2 1.2%
summary(lm_robust(high_rating ~ stars, data = bp)) #r2 11%
summary(lm_robust(high_rating ~ rating, data = bp)) #highest cor: 58%

model <- lm(high_rating ~ price + distance + stars, data = bp) #r2 14%

bp$high_rating_pred <- predict(model)

### VISUALIZATIONS WITH PROBABILITY MODELS

probit <- glm(high_rating ~ price + distance + stars, 
              data = bp, 
              family = binomial(link = "probit"))

logit <- glm(high_rating ~ price + distance + stars, 
              data = bp, 
              family = binomial(link = "logit"))

#bp$probit <- predict.glm(probit, type="response")

(prob_models <- bp %>% ggplot(aes(high_rating_pred, high_rating)) +
  geom_vline(xintercept = c(0,1), size = 0.8, color = "lightblue") +
  geom_hline(yintercept = c(0,1), size = 0.8, color = "lightblue") +
  geom_point(size = 1, color = "darkblue") +
  stat_smooth(method="glm", method.args=list(family=binomial(link = "probit")), se=F, color = "darkgreen") +
  stat_smooth(method="glm", method.args=list(family=binomial(link = "logit")), se=F, color = "green") +
  labs(title ="Probability models: probit (dark green) and logit (green)",
       x = "Predictions: high_rating ~ distance + price + stars",
       y = "High rating binary") +
  theme_bw())

ggsave("logit_probit/prob_models.png", device = "png", plot = prob_models)


(binary_cut <- bp %>% ggplot(aes(rating, high_rating)) +
  geom_hline(yintercept = c(0,1), size = 0.8, color = "lightblue") +
  geom_point(size = 1, color = "darkblue") +
  stat_smooth(method="glm", method.args=list(family=binomial(link = "probit")), se=F, color = "darkgreen") +
  stat_smooth(method="glm", method.args=list(family=binomial(link = "logit")), se=F, color = "green") +
  labs(title ="Checking model: binary cutoff at 4 ratings",
       x = "Predictions: high_rating ~ rating",
       y = "High rating binary") +
  theme_bw())

ggsave("logit_probit/binary_cutoff.png", device = "png", plot = binary_cut)

(price_prob <- bp %>% ggplot(aes(price, high_rating)) +
  geom_hline(yintercept = c(0,1), size = 0.8, color = "lightblue") +
  geom_point(size = 1, color = "darkblue") +
  stat_smooth(method="glm", method.args=list(family=binomial(link = "probit")), se=F, color = "darkgreen") +
  stat_smooth(method="glm", method.args=list(family=binomial(link = "logit")), se=F, color = "green") +
  labs(title ="High rating probabilty in terms of high prices",
       x = "Predictions: high_rating ~ price",
       y = "High rating binary") +
  theme_bw())

ggsave("logit_probit/price_prob.png", device = "png", plot = price_prob)


### COEFF, MARGINAL DIFF, PRED PROB - DISCUSS

# TODO




