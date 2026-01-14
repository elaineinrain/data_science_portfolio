# Data Cleaning
billboard <- read.csv(file.choose(), header=TRUE) 
library(dplyr)
billboard_clean <- billboard %>%
  distinct(year, ranking, .keep_all = TRUE)
billboard_unique <- billboard_clean %>%
  distinct(song, .keep_all = TRUE)
spotify <- read.csv(file.choose(), header=TRUE)
library(stringr)
spotify_unique <- spotify %>%
  group_by(track_name) %>%
  slice_head(n = 1) %>%
  ungroup() %>%
  dplyr::select(track_name, artist_names, danceability, energy, 
                loudness, speechiness, instrumentalness, acousticness,
                liveness, valence, tempo)
merged <- billboard_unique %>%
  left_join(
    spotify_unique,
    by = c("song" = "track_name")
  ) 
features <- c("danceability", "energy", "loudness",
              "speechiness","instrumentalness", "liveness", "valence", "tempo", "acousticness")

for (feat in features) {
  
  xname <- paste0(feat, ".x")
  yname <- paste0(feat, ".y")
  
  merged[[feat]] <- ifelse(
    is.na(merged[[xname]]) & 
      str_detect(merged$artist_names, fixed(merged$band_singer)),
    merged[[yname]],
    merged[[xname]]
  )
}

merged <- merged %>%
  dplyr::select(-ends_with(".x"), -ends_with(".y"), -artist_names)
merged <- merged %>%
  filter(!is.na(danceability))

# Defining Repetition
install.packages("tidytext")
install.packages("stringr")
library(tidytext)
library(stringr)
my_stopwords <- stop_words %>%
  filter(!word %in% c(
    "oh", "ooh", "oooh", 
    "mm", "mmm",
    "yeah", "yea", "yah",
    "la", "na", "nanana",
    "uh", "uhh", "uhhh",
    "hey", "ha", "hah", "haha"
  ))

total_words_billboard <- merged %>%
  unnest_tokens(word_all, lyrics) %>%
  group_by(song) %>%
  summarise(total_words = n(), .groups = "drop")

repeated_words_billboard <- merged %>%
  unnest_tokens(word, lyrics) %>%
  anti_join(my_stopwords, by = "word") %>%       
  group_by(song, word) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(song) %>%
  summarise(repeated_words = sum(n[n > 1]), .groups = "drop")

billboard_rep <- total_words_billboard %>%
  left_join(repeated_words_billboard, by = "song") %>%
  mutate(
    repeated_words = ifelse(is.na(repeated_words), 0, repeated_words),
    repetition_ratio = repeated_words / total_words
  )

billboard_final <- merged %>%
  left_join(billboard_rep, by = "song")
attach(billboard_final)

# Exploratory Visualizations
library(ggplot2)

hist(billboard_final$repetition_ratio, main="Histogram of Repetition Ratio", 
     xlab="Repetition Ratio") # Histogram of danceability

ggplot(billboard_final, aes(x = repetition_ratio, y = danceability)) +  # Scatterplot between Danceability and Repetition Ratio
  geom_point(alpha = 0.4) +
  theme_minimal() +
  labs(
    title = "Repetition Ratio vs Danceability",
    x = "Repetition Ratio",
    y = "Danceability"
  )

ggplot(billboard_final, aes(x = repetition_ratio, y = danceability)) +  # Scatterplot between Danceability and Repetition Ratio
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  theme_minimal() +
  labs(
    title = "Repetition Ratio vs Danceability",
    x = "Repetition Ratio",
    y = "Danceability"
  )

library(party)
dp.tree <- ctree(danceability~repetition_ratio, data=billboard_final) # Regression Tree
plot(dp.tree)

# Model Fitting and Transformation
fit <- lm(danceability~repetition_ratio, data=billboard_final) # Linear regression between danceability and repetition ratio
summary(fit) 
plot(fit$residuals~fit$fitted.values, main="Residuals of Single-Var Model") # Residual Plot
abline(h = 0, col = "red")

par(mfrow = c(1, 1))
library(MASS) 
boxcox(fit)
fit.boxcox <- lm((danceability)^1.5~repetition_ratio, data=billboard_final) # Box-Cox Transformation
plot(fit.boxcox$residuals~fit.boxcox$fitted.values, main="Residuals of Box-Cox Model")
abline(h = 0, col = "red")
summary(fit.boxcox)
fit.log <- lm(log(danceability)~repetition_ratio, data=billboard_final) # Log Transformation
summary(fit.log)
plot(fit.log$residuals~fit.log$fitted.values, main="Residuals of Logged Model")
abline(h = 0, col = "red")
billboard_final$logit_dance <- log(billboard_final$danceability / (1 - billboard_final$danceability)) # Logit Transformation
fit.logit <- lm(logit_dance ~ repetition_ratio, data = billboard_final)
summary(fit.logit)
plot(fit.logit$residuals~fit.logit$fitted.values, main="Residuals of Logit Model")
abline(h = 0, col = "red")
fit.quad <- lm(danceability ~ repetition_ratio + I(repetition_ratio^2), data = billboard_final) # Adding a Quadratic Term
summary(fit.quad)
plot(fit.quad$residuals~fit.quad$fitted.values, main="Residuals of Quadratic Model")
abline(h = 0, col = "red")

# Cross Validation
# BIC
BIC(fit)
BIC(fit.boxcox)
BIC(fit.log)
BIC(fit.logit)
BIC(fit.quad)

# Fitted Line of the Quadratic Model
install.packages("ggeffects")
library(ggeffects)
eff <- ggpredict(model_full, terms = "repetition_ratio [all]")

ggplot(billboard_final, aes(x = repetition_ratio, y = danceability)) +
  geom_point(alpha = 0.4) +
  # Quadratic LM Fit
  geom_smooth(
    aes(color = "Quadratic Model"),
    method = "lm",
    formula = y ~ x + I(x^2),
    se = FALSE
  ) +
  # Full Model Partial Effect
  geom_line(
    data = eff,
    aes(x = x, y = predicted, color = "Full Model"),
    size = 1.2
  ) +
  scale_color_manual(
    name = "Models",
    values = c(
      "Quadratic Model" = "blue",
      "Full Model" = "orange"
    )
  ) +
  theme_minimal() +
  theme(
    legend.position = c(0.95, 0.1),          # bottom-right corner
    legend.justification = c("right", "bottom"),
    legend.background = element_rect(fill = "white", color = "gray80")
  ) +
  labs(
    title = "Repetition Ratio vs Danceability",
    x = "Repetition Ratio",
    y = "Danceability"
  )

# Model Comparison
model_vars <- c("danceability", "energy", "loudness", "speechiness",
                "instrumentalness", "liveness", "valence", "tempo", "acousticness", "repetition_ratio")

model_base <- lm(
  danceability ~ energy  + loudness + speechiness + valence + tempo + acousticness + instrumentalness + liveness,
  data = billboard_final
) # Initial base model
summary(model_base)

model_full <- lm(
  danceability ~ energy  + loudness + speechiness + valence + tempo + acousticness + instrumentalness + liveness + repetition_ratio + I(repetition_ratio^2),
  data = billboard_final, x=TRUE
) # Full model (with repetition_ratio and quadratic term)
summary(model_full) 

anova(model_base, model_full) # Extra Sums of Squares F-Test

BIC(model_base) # Cross Validation
BIC(model_full)


xlim <- range(c(billboard_final$pred_base, billboard_final$pred_full), na.rm = TRUE)
ylim <- range(billboard_final$danceability, na.rm = TRUE)

billboard_final$pred_base <- predict(model_base, newdata = billboard_final) 
billboard_final$pred_full <- predict(model_full, newdata = billboard_final)

p1 <- ggplot(billboard_final, aes(x = pred_base, y = danceability)) +
  geom_point(alpha = 0.5, color = "steelblue") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  labs(
    x = "Predicted Danceability (Base Model)",
    y = "Actual Danceability",
    title = "Predicted vs Actual Danceability (Without Repetition Ratio)"
  ) +
  coord_cartesian(xlim = xlim, ylim = ylim) +
  theme_minimal()

p2 <- ggplot(billboard_final, aes(x = pred_full, y = danceability)) +
  geom_point(alpha = 0.5, color = "steelblue") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  labs(
    x = "Predicted Danceability (Full Model)",
    y = "Actual Danceability",
    title = "Predicted vs Actual Danceability (With Repetition Ratio)"
  ) +
  coord_cartesian(xlim = xlim, ylim = ylim) +
  theme_minimal()


install.packages("patchwork")
library(patchwork)
p1+p2

# Residual Analysis
billboard_final$residuals_base <- residuals(model_base)
p3 <- ggplot(billboard_final, aes(x = pred_base, y = residuals_base)) +
  geom_point(alpha = 0.5, color = "darkgreen") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(
    x = "Predicted Danceability (Base Model)",
    y = "Residuals",
    title = "Residuals of Base Model"
  ) +
  theme_minimal()

billboard_final$residuals_full <- residuals(model_full)
p4 <- ggplot(billboard_final, aes(x = pred_full, y = residuals_full)) +
  geom_point(alpha = 0.5, color = "darkgreen") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(
    x = "Predicted Danceability (Full Model)",
    y = "Residuals",
    title = "Residuals of Full Model"
  ) +
  theme_minimal()

p3+p4

library(caret)

# Variance Importance Plot
vi <- varImp(model_full)
vi_df <- data.frame(
  Variable = rownames(vi),
  Importance = vi$Overall,
  row.names = NULL
)

vi_df <- vi_df[order(vi_df$Importance, decreasing = TRUE), ]

ggplot(vi_df, aes(x = reorder(Variable, Importance), y = Importance)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Variable Importance (Full Model)",
    x = "Predictor",
    y = "Importance"
  ) +
  theme_minimal(base_size = 13)


