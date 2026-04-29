# Load libraries 
library(tidyverse)
library(dplyr)
library(olsrr)
library(interactions)
library(ggplot2)
library(scales)

# load the final data with total occupation counts and industry category derived from "prob_auto_merged_national_code" and conversations with ChatGPT to generate additional columns not in original dataset
prob_auto_final = read.csv(file.choose(), header=T) 
#file name is 'final_prob_auto_w_weights_industry.csv'

# quick data check 
head(prob_auto_final)

#Create column "industry_num" from industry
prob_auto_final <- prob_auto_final %>%
  mutate(
    industry_num = case_when(
      industry == "Healthcare" ~ 1,
      industry == "Education" ~ 2,
      industry == "Technology" ~ 3,
      industry == "Business & Finance" ~ 4,
      industry == "Legal" ~ 5,
      industry == "Construction & Trades" ~ 6,
      industry == "Manufacturing" ~ 7,
      industry == "Transportation" ~ 8,
      industry == "Sales" ~ 9,
      industry == "Service" ~ 10,
      industry == "Other" ~ 11
    )
  )

#Create column "edlevelnum" from education_level 
prob_auto_final <- prob_auto_final %>%
  mutate(
    edlevelnum = case_when(
      education_level == "Bachelor's or Master's degree" ~ 1,
      education_level == "Bachelor's degree" ~ 2,
      education_level == "High school diploma or equivalent" ~ 3,
      education_level == "Associate's degree" ~ 4,
      education_level == "Bachelor's or Associate's degree" ~ 5,
      education_level == "Doctoral or professional degree" ~ 6,
      education_level == "Master's degree" ~ 7,
      education_level == "Master's or Doctoral degree" ~ 8,
      education_level == "High school diploma or Associate's" ~ 9,
      education_level == "No formal educational credential" ~ 10,
      education_level == "Postsecondary nondegree award" ~ 11,
      education_level == "Associate's or Bachelor's degree" ~ 12,
      education_level == "Bachelor's or Associate's" ~ 13,
      education_level == "Some college" ~ 14,
      education_level == "Certificate" ~ 15
    )
  )

#Boxplots for each variable
boxplot(prob_auto_final$p_automation,
        main = "Distribution of Automation Probability",
        ylab = "Probability of Automation")

boxplot(prob_auto_final$routineness,
        main = "Distribution of Routineness",
        ylab = "Routineness")

boxplot(prob_auto_final$complexity,
        main = "Distribution of Task Complexity",
        ylab = "Complexity")

boxplot(prob_auto_final$no_ed_requirement,
        main = "No Educational Requirement",
        ylab = "Indicator (0/1)")

boxplot(prob_auto_final$hs_diploma_or_equiv,
        main = "High School Diploma or Equivalent",
        ylab = "Indicator (0/1)")

boxplot(prob_auto_final$post_secondary_no_degree,
        main = "Post-Secondary Education, No Degree",
        ylab = "Indicator (0/1)")

boxplot(prob_auto_final$associates,
        main = "Associate’s Degree Requirement",
        ylab = "Indicator (0/1)")

boxplot(prob_auto_final$bachelors,
        main = "Bachelor’s Degree Requirement",
        ylab = "Indicator (0/1)")

boxplot(prob_auto_final$masters,
        main = "Master’s Degree Requirement",
        ylab = "Indicator (0/1)")

boxplot(prob_auto_final$doctoral,
        main = "Doctoral or Professional Degree Requirement",
        ylab = "Indicator (0/1)")

boxplot(prob_auto_final$is_medical,
        main = "Medical Occupations",
        ylab = "Indicator (0/1)")

boxplot(prob_auto_final$is_programming,
        main = "Programming-Related Occupations",
        ylab = "Indicator (0/1)")

boxplot(prob_auto_final$is_blue_collar,
        main = "Blue-Collar Occupations",
        ylab = "Indicator (0/1)")

boxplot(prob_auto_final$is_service,
        main = "Service-Sector Occupations",
        ylab = "Indicator (0/1)")

boxplot(prob_auto_final$industry_num,
        main = "Industry Category (Numeric Encoding)",
        ylab = "Industry Code")

boxplot(prob_auto_final$edlevelnum,
        main = "Education Level Category (Numeric Encoding)",
        ylab = "Education Level Code")


# Bubble Scatter Plot routineness vs. automation 
ggplot(data = prob_auto_final) + 
  
  geom_point(aes(x = routineness, y = p_automation, size = TOT_EMP), alpha = I(0.5))
# This scatter plot is a good visualization, but there are some "oddball" occupations that fall outside of the clusters with weakly positive trend between routineness and probability of automation.


# ------------------------------------------------------------
# Identify "oddball" occupations (off-diagonal quadrants)
# ------------------------------------------------------------
oddballs <- prob_auto_final %>%
  filter(
    (routineness < 0.5 & p_automation > 0.5) |
      (routineness > 0.5 & p_automation < 0.5)
  ) %>%
  mutate(
    quadrant = case_when(
      routineness < 0.5 & p_automation > 0.5 ~ "Low Routineness / High Automation",
      routineness > 0.5 & p_automation < 0.5 ~ "High Routineness / Low Automation"
    )
  )

# ------------------------------------------------------------
# Select top 8 occupations per quadrant by employment
# ------------------------------------------------------------
quadrant_labels <- oddballs %>%
  arrange(desc(TOT_EMP)) %>%
  group_by(quadrant) %>%
  slice_head(n = 8) %>%
  summarise(
    occupations = paste(occupation, collapse = "\n"),
    .groups = "drop"
  )

# Fixed annotation positions
top_left <- quadrant_labels %>%
  filter(quadrant == "Low Routineness / High Automation") %>%
  mutate(x = 0.04, y = 0.96)

bottom_right <- quadrant_labels %>%
  filter(quadrant == "High Routineness / Low Automation") %>%
  mutate(x = 0.96, y = 0.04)

# ------------------------------------------------------------
# Bubble scatterplot
# ------------------------------------------------------------
ggplot() +
  
  # All occupations
  geom_point(
    data = prob_auto_final,
    aes(routineness, p_automation, size = TOT_EMP),
    alpha = 0.25,
    color = "grey50"
  ) +
  
  # Highlight oddballs
  geom_point(
    data = oddballs,
    aes(routineness, p_automation, size = TOT_EMP),
    alpha = 0.7,
    color = "#2C3E50"
  ) +
  
  # Overall trend line
  geom_smooth(
    data = prob_auto_final,
    aes(routineness, p_automation),
    method = "lm",
    se = FALSE,
    color = "grey40",
    linetype = "dotdash",
    linewidth = 0.8
  ) +
  
  # Quadrant reference lines
  geom_vline(xintercept = 0.5, linetype = "dashed", color = "grey60") +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "grey60") +
  
  # Occupation text blocks
  geom_label(
    data = top_left,
    aes(x = x, y = y, label = occupations),
    hjust = 0,
    vjust = 1,
    size = 3,
    fill = alpha("white", 0.75),
    label.size = 0,
    lineheight = 1.1
  ) +
  geom_label(
    data = bottom_right,
    aes(x = x, y = y, label = occupations),
    hjust = 1,
    vjust = 0,
    size = 3,
    fill = alpha("white", 0.75),
    label.size = 0,
    lineheight = 1.1
  ) +
  
  # Scales
  scale_x_continuous(limits = c(0, 1)) +
  scale_y_continuous(limits = c(0, 1)) +
  scale_size_continuous(
    name = "Total Employment",
    range = c(2, 12),
    breaks = c(5e5, 1e6, 2e6, 4e6),
    labels = label_comma()
  ) +
  
  # Labels and theme
  labs(
    title = "Routineness and Automation Risk Across Occupations",
    subtitle = "Most occupations follow a positive relationship between routineness and automation risk; labeled jobs deviate from that pattern",
    x = "Task Routineness",
    y = "Probability of Automation"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "grey92"),
    plot.margin = margin(10, 30, 10, 30)
  )



# Now looking at bubble scatter plot for complexity vs. p_automation
ggplot(data = prob_auto_final) + 
  
  geom_point(aes(x = complexity, y = p_automation, size = TOT_EMP), alpha = I(0.5))
# Again, this is a basic vis. Want to follow same methodology as routineness.




# ------------------------------------------------------------
# Identify "oddball" occupations (off-diagonal quadrants)
# Negative relationship: top-right and bottom-left
# ------------------------------------------------------------
oddballs <- prob_auto_final %>%
  filter(
    (complexity > 0.5 & p_automation > 0.5) |
      (complexity < 0.5 & p_automation < 0.5)
  ) %>%
  mutate(
    quadrant = case_when(
      complexity > 0.5 & p_automation > 0.5 ~
        "High Complexity / High Automation",
      complexity < 0.5 & p_automation < 0.5 ~
        "Low Complexity / Low Automation"
    )
  )

# ------------------------------------------------------------
# Select top 8 occupations per quadrant by employment
# ------------------------------------------------------------
quadrant_labels <- oddballs %>%
  arrange(desc(TOT_EMP)) %>%
  group_by(quadrant) %>%
  slice_head(n = 8) %>%
  summarise(
    occupations = paste(occupation, collapse = "\n"),
    .groups = "drop"
  )

# Fixed annotation positions
top_right <- quadrant_labels %>%
  filter(quadrant == "High Complexity / High Automation") %>%
  mutate(x = 0.96, y = 0.96)

bottom_left <- quadrant_labels %>%
  filter(quadrant == "Low Complexity / Low Automation") %>%
  mutate(x = 0.04, y = 0.04)

# ------------------------------------------------------------
# Final bubble scatterplot with highlighted oddballs
# ------------------------------------------------------------
ggplot() +
  
  # All occupations
  geom_point(
    data = prob_auto_final,
    aes(x = complexity, y = p_automation, size = TOT_EMP),
    alpha = 0.25,
    color = "grey50"
  ) +
  
  # Highlight oddballs
  geom_point(
    data = oddballs,
    aes(x = complexity, y = p_automation, size = TOT_EMP),
    alpha = 0.7,
    color = "#2C3E50"
  ) +
  
  # Overall trend line (negative relationship)
  geom_smooth(
    data = prob_auto_final,
    aes(x = complexity, y = p_automation),
    method = "lm",
    se = FALSE,
    color = "grey40",
    linetype = "dotdash",
    linewidth = 0.8
  ) +
  
  # Quadrant reference lines
  geom_vline(xintercept = 0.5, linetype = "dashed", color = "grey60") +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "grey60") +
  
  # Occupation text blocks
  geom_label(
    data = top_right,
    aes(x = x, y = y, label = occupations),
    hjust = 1,
    vjust = 1,
    size = 3,
    fill = alpha("white", 0.75),
    label.size = 0,
    lineheight = 1.1
  ) +
  geom_label(
    data = bottom_left,
    aes(x = x, y = y, label = occupations),
    hjust = 0,
    vjust = 0,
    size = 3,
    fill = alpha("white", 0.75),
    label.size = 0,
    lineheight = 1.1
  ) +
  
  # Scales
  scale_x_continuous(limits = c(0, 1)) +
  scale_y_continuous(limits = c(0, 1)) +
  scale_size_continuous(
    name = "Total Employment",
    range = c(2, 12),
    breaks = c(5e5, 1e6, 2e6, 4e6),
    labels = label_comma()
  ) +
  
  # Labels and theme
  labs(
    title = "Task Complexity and Automation Risk Across Occupations",
    subtitle = "Most occupations show a negative relationship between complexity and automation risk; labeled jobs deviate from that pattern",
    x = "Task Complexity",
    y = "Probability of Automation"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "grey92"),
    plot.margin = margin(10, 30, 10, 30)
  )



#Boxplot of industry in p_automation
ggplot(data = prob_auto_final) +
  
  geom_boxplot(aes(x = industry, y = p_automation))
#This is a good start, but I need the vis to be cleaner

prob_auto_final %>%
  mutate(
    industry = reorder(industry, p_automation, FUN = median)
  ) %>%
  ggplot(aes(x = industry, y = p_automation)) +
  
  geom_boxplot(
    outlier.alpha = 0.4,
    color = "grey30",
    fill = "white"
  ) +
  
  coord_flip() +
  
  labs(
    title = "Probability of Automation by Industry",
    subtitle = "Industries ordered by median automation risk",
    x = "Industry",
    y = "Probability of Automation"
  ) +
  
  scale_y_continuous(limits = c(0, 1)) +
  
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    plot.title = element_text(face = "bold"),
    axis.title.y = element_text(margin = margin(r = 10)),
    axis.title.x = element_text(margin = margin(t = 10))
  )




#Boxplot of education_level in p_automation
ggplot(data = prob_auto_final) +
  
  geom_boxplot(aes(x = education_level, y = p_automation))

# This boxplot is on the right track, but still busy
prob_auto_final %>%
  mutate(
    education_level = reorder(education_level, p_automation, FUN = median)
  ) %>%
  ggplot(aes(x = education_level, y = p_automation)) +
  geom_boxplot(outlier.alpha = 0.4) +
  coord_flip() +
  labs(
    title = "Probability of Automation by Education Level",
    subtitle = "Education levels ordered by median automation risk",
    x = "Education Level",
    y = "Probability of Automation"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank()
  )








# Begin linear regression modeling. Utilizing backward regression. alpha = .05
model1 <- lm(p_automation ~ routineness + complexity + no_ed_requirement + hs_diploma_or_equiv + post_secondary_no_degree + associates + bachelors + masters + doctoral + is_medical + is_programming + is_blue_collar + is_service + industry_num + edlevelnum, 
             
             data = prob_auto_final, 
             
             weights = TOT_EMP)

summary(model1)
#R2 = 69.38%; Adj R2 = 67.58%
#This is a good basis, but I want to explore an interaction term with routineness*complexity







# Model introducing interaction term.
model1rc <- lm(p_automation ~ routineness * complexity + no_ed_requirement + hs_diploma_or_equiv + post_secondary_no_degree + associates + bachelors + masters + doctoral + is_medical + is_programming + is_blue_collar + is_service + industry_num + edlevelnum, 
             
             data = prob_auto_final, 
             
             weights = TOT_EMP)


interact_plot(
  model1rc,
  pred  = routineness,
  modx  = complexity,
  
  # Compare simpler vs more complex jobs
  modx.values = "plus-minus",
  modx.labels = c(
    "Simpler Jobs",
    "More Complex Jobs"
  ),
  
  # Show data points (de-emphasized)
  plot.points = TRUE,
  point.alpha = 0.25,
  point.size  = 1.5,
  point.color = "gray60",
  
  # Confidence bands
  interval       = TRUE,
  interval.alpha = 0.15,
  line.size      = 1.3,
  
  # Line colors
  colors = c("#D55E00", "#0072B2"),
  
  # Lay-friendly labels
  x.label = "How Routine the Job Tasks Are",
  y.label = "Estimated Automation Risk",
  
  legend.main = "Job Complexity",
  theme = theme_minimal(base_size = 13)
)
#This plot shows how the probability of automation (or risk of automation) increases as jobs become more routine. Each bubble represents an occupation, and the two lines compare less complex jobs to more complex jobs. While automation risk is rises with routineness, the increase is steeper for more complex jobs, indicating that routineness does play a role in determining probability of automation for complex work.


summary(model1rc)
#R2 = 69.85%; Adj R2 = 67.95%
#This has improved from model1. I'll continue backward regression here to see if the model will improve.

#remove masters with p-value of .7288

model2rc <- lm(p_automation ~ routineness * complexity + no_ed_requirement + hs_diploma_or_equiv + post_secondary_no_degree + associates + bachelors + doctoral + is_medical + is_programming + is_blue_collar + is_service + industry_num + edlevelnum, 
              
              data = prob_auto_final, 
              
              weights = TOT_EMP)

summary(model2rc)
#R2 = 69.83%; Adj R2 = 68.06%
#While R2 decreased slightly, adjusted R2 increased, so I'll continue removing variables

#remove industry_num with p-value of .7381

model3rc <- lm(p_automation ~ routineness * complexity + no_ed_requirement + hs_diploma_or_equiv + post_secondary_no_degree + associates + bachelors + doctoral + is_medical + is_programming + is_blue_collar + is_service + edlevelnum, 
               
               data = prob_auto_final, 
               
               weights = TOT_EMP)

summary(model3rc)
#R2 = 69.82%; Adj R2 = 68.17%
#While R2 decreased slightly, adjusted R2 increased, so I'll continue removing variables

#remove bachelors with p-value of .6397

model4rc <- lm(p_automation ~ routineness * complexity + no_ed_requirement + hs_diploma_or_equiv + post_secondary_no_degree + associates + doctoral + is_medical + is_programming + is_blue_collar + is_service + edlevelnum, 
               
               data = prob_auto_final, 
               
               weights = TOT_EMP)

summary(model4rc)
#R2 = 69.79%; Adj R2 = 68.26%
#While R2 decreased slightly, adjusted R2 increased, so I'll continue removing variables

#remove post_secondary_no_degree with p-value of .462681

model5rc <- lm(p_automation ~ routineness * complexity + no_ed_requirement + hs_diploma_or_equiv + associates + doctoral + is_medical + is_programming + is_blue_collar + is_service + edlevelnum, 
               
               data = prob_auto_final, 
               
               weights = TOT_EMP)

summary(model5rc)
#R2 = 69.73%; Adj R2 = 68.32%
#While R2 decreased slightly, adjusted R2 increased, so I'll continue removing variables

#remove associates with p-value of.405894

model6rc <- lm(p_automation ~ routineness * complexity + no_ed_requirement + hs_diploma_or_equiv + doctoral + is_medical + is_programming + is_blue_collar + is_service + edlevelnum, 
            
            data = prob_auto_final, 
            
            weights = TOT_EMP)

summary(model6rc)
#R2 = 69.65%; Adj R2 = 68.36%
#While R2 decreased slightly, adjusted R2 increased, so I'll continue removing variables

#remove is_blue_collar with p-value of .285794

model7rc <- lm(p_automation ~ routineness * complexity + no_ed_requirement + hs_diploma_or_equiv + doctoral + is_medical + is_programming + is_service + edlevelnum, 
               
               data = prob_auto_final, 
               
               weights = TOT_EMP)

summary(model7rc)
#R2 = 69.51%; Adj R2 = 68.34%
#R2 decreases slightly AND adjusted R2 decreased. model6rc is the best fit.

#Final model - model6rc - has adjusted R2 of 68.36%, improving 67.58% in original model - model1. 
summary(model6rc)
#Given alpha = .05, routineness, complexity, is_medical, and routineness*complexity interaction are all statistically significant predictors of p_automation (probability of automation)


# Final formula: p_automation_hat = 0.578257 + (0.222604 * routineness) + (-0.729915 * complexity) + (0.067229 * no_ed_requirement) + (0.150432 * hs_diploma_or_equiv) + (-0.101934 * doctoral) + (-0.086656 * is_medical) + (+0.070972 * is_programming) + (+0.045363 * is_blue_collar) + (-0.073215 * is_service) + (+0.011118 * edlevelnum) + (+0.379401 * (routineness * complexity))

plot_data <- fortify(model6rc)

# Create the residual plot
ggplot(plot_data, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Residuals vs Fitted", x = "Fitted Values", y = "Residuals")
#This residual plot indicates that the model captures the main directional relationships well, but the form does struggle to fully accomodate the non-linearity of automation probabilities - especially in the mid prediction range.




# Create absolute error plot
plot_data$abs_error <- abs(plot_data$.resid)

ggplot(plot_data, aes(x = .fitted, y = abs_error)) +
  geom_point(alpha = 0.5, size = 1.5) +
  geom_smooth(method = "loess", se = FALSE, color = "steelblue", linewidth = 1) +
  labs(
    title = "Model Validation: Prediction Accuracy Across Automation Risk Levels",
    x = "Estimated Automation Risk",
    y = "Magnitude of Prediction Error"
  ) +
  theme_minimal(base_size = 13)
#This plot shows that the model is not unusually sensitive to any particular range of automation risk as prediction errors remain fairly modest across all risk probabilities, though there is a slight increase in the mid-range. However, the model does become more reliable for the higher-risk occupations, which would be more beneficial for planning/guidance purposes.



summary(model6rc)

#Plots of significant binary (0/1) variables:

  #hs_diploma_or_equiv
ggplot(data = prob_auto_final) + geom_boxplot(aes(x = as.factor(hs_diploma_or_equiv), y = p_automation)) +
  labs(x = "Only High School Diploma or Equivalent Needed", y = "Probability of Automation")
    #this shows the increased p_automation when only HS diploma or equivalent is needed
  #is_medical
ggplot(data = prob_auto_final) + geom_boxplot(aes(x = as.factor(is_medical), y = p_automation)) +
  labs(x = "Occupation is in Medical Field", y = "Probability of Automation")
    #this shows the decreased p_automation when occupation is in the medical field


#plots of significant numerical variables: 
  #routineness
ggplot(prob_auto_final, aes(x = routineness, y = p_automation)) +
  geom_point(alpha = 0.25, size = 1.2, color = "black") +
  geom_smooth(method = "lm", se = FALSE, color = "#0072B2", linewidth = 1) +
  labs(
    x = "Task Routineness",
    y = "Probability of Automation",
    title = "Routineness and Automation Risk Across Occupations"
  ) +
  scale_y_continuous(limits = c(0, 1)) +
  theme_minimal(base_size = 12)
    #this shows the increase in probability of automation as task routineness increases
  #complexity
ggplot(prob_auto_final, aes(x = complexity, y = p_automation)) +
  geom_point(alpha = 0.25, size = 1.2, color = "black") +
  geom_smooth(method = "lm", se = FALSE, color = "#0072B2", linewidth = 1) +
  labs(
    x = "Task Complexity",
    y = "Probability of Automation",
    title = "Task Complexity and Automation Risk"
  ) +
  scale_y_continuous(limits = c(0, 1)) +
  theme_minimal(base_size = 12)
    #this shows the decrease in probability of automation as task complexity increases


#Plots of other binary (0/1) variables still in model:
  #no_ed_requirement
ggplot(data = prob_auto_final) + geom_boxplot(aes(x = as.factor(no_ed_requirement), y = p_automation)) +
  labs(x = "No Education Requirement", y = "Probability of Automation")
    #this shows the increased p_automation when there is no education requirement
  #doctoral               
ggplot(data = prob_auto_final) + geom_boxplot(aes(x = as.factor(doctoral), y = p_automation)) +
  labs(x = "Doctoral Degree Required", y = "Probability of Automation")
    #this shows the decreased p_automation when occupation requires a doctoral degree.
  #is_programming                         
ggplot(data = prob_auto_final) + geom_boxplot(aes(x = as.factor(is_programming), y = p_automation)) +
  labs(x = "No Education Requirement", y = "Probability of Automation")
    #this shows the increased p_automation when the occupation involved computer programming
  #is_blue_collar                                   
ggplot(data = prob_auto_final) + geom_boxplot(aes(x = as.factor(is_blue_collar), y = p_automation)) +
  labs(x = "No Education Requirement", y = "Probability of Automation")
    #this shows the increased p_automation when the occupation is blue collar
  #is_service                                      
ggplot(data = prob_auto_final) + geom_boxplot(aes(x = as.factor(is_service), y = p_automation)) +
  labs(x = "No Education Requirement", y = "Probability of Automation")
    #this shows the decreased p_automation when the occupation is in service-oriented






#Now, I want to introduce a LASSO regression



####### FROM CHATGPT - WANT TO MEET WITH DR. FOLLETT TO BREAKDOWN AND UNDERSTAND WHAT IS HAPPENING.

############################################################
# SAFE LASSO IMPLEMENTATION (FIXES glmnet ERROR)
############################################################

library(glmnet)

# ----------------------------------------------------------
# 1. Construct model matrix
# ----------------------------------------------------------

X <- model.matrix(
  ~ routineness * complexity +
    no_ed_requirement +
    hs_diploma_or_equiv +
    post_secondary_no_degree +
    associates +
    bachelors +
    masters +
    doctoral +
    is_medical +
    is_programming +
    is_blue_collar +
    is_service +
    industry_num +
    edlevelnum
  - 1,
  data = prob_auto_final
)

y <- prob_auto_final$p_automation
w <- prob_auto_final$TOT_EMP

# ----------------------------------------------------------
# 2. Drop rows with NA / infinite values
# ----------------------------------------------------------

good_rows <- complete.cases(X, y, w) & is.finite(w) & w > 0

X <- X[good_rows, ]
y <- y[good_rows]
w <- w[good_rows]

# ----------------------------------------------------------
# 3. Remove zero-variance predictors
# ----------------------------------------------------------

nzv <- apply(X, 2, function(col) sd(col) > 0)

X <- X[, nzv]

# ----------------------------------------------------------
# 4. Cross-validated LASSO (NO STANDARDIZATION)
# ----------------------------------------------------------

set.seed(123)

cv_lasso <- cv.glmnet(
  x = X,
  y = y,
  alpha = 1,              # LASSO
  weights = w,
  standardize = FALSE     # KEY FIX
)

plot(cv_lasso)

# ----------------------------------------------------------
# 5. Extract nonzero coefficients at lambda.1se
# ----------------------------------------------------------

lasso_coefs <- coef(cv_lasso, s = "lambda.1se")

lasso_df <- data.frame(
  variable = rownames(lasso_coefs),
  coefficient = as.numeric(lasso_coefs)
)

selected_vars <- lasso_df$variable[
  lasso_df$coefficient != 0 &
    lasso_df$variable != "(Intercept)" &
    lasso_df$variable != "" &
    !is.na(lasso_df$variable)
]

print(selected_vars)

# ----------------------------------------------------------
# 6. Build post-LASSO formula
# ----------------------------------------------------------

final_formula <- as.formula(
  paste("p_automation ~", paste(selected_vars, collapse = " + "))
)

print(final_formula)

# ----------------------------------------------------------
# 7. Post-LASSO weighted OLS
# ----------------------------------------------------------

post_lasso_model <- lm(
  final_formula,
  data = prob_auto_final[good_rows, ],
  weights = w
)

summary(post_lasso_model)


