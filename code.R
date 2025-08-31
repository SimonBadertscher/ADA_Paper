# Load necessary packages

library(tidyverse)
library(haven)
library(glmnet)
library(dplyr)
library(data.table) 
library(broom)
library(pdftools)
library(sjlabelled)
library(labelled)
library(caret)
library(leaps)
library(stargazer)
library(scatterplot3d)
library(np)
library(plm)
library(ggpubr)
library(RColorBrewer)
library(lmtest)
library(sandwich)
extrafont::loadfonts(device="all")



# Set working directory.

wd <- "YOUR_PATH"
setwd(wd)


# You should have a folder "data" with all SHP files (household and individual).



### Data preparation

# Setting up the years and variables to then combine the annual personal and 
# annual household data.

year <- c("00","01","02", "03", "04", "05", "06", "07", "08", "09", 10:23)

# These are the names that the variables will have after the combination.
setnames_variables_p <- c("personal_id", "interview_type", "househ_id","spouse_id","wstat", "sex", "age", "nationality", "civstat",
                          "isced", "educyrs", "annual_income", "conWH", "occupation_isco",
                          "industry", "part_time", "supervisory_role", "health_status", "first_lang",
                          "firm_size","public_private", "n_children","dep_freq")

setnames_variables_h <- c("househ_id", "househ_type", "new_born","canton","hh_g_income")


list_of_p_dfs <- lapply(year, function(y) {
  file_path <- paste0("data/shp", y, "_p_user.dta")
  df_p <- read_dta(file_path)
  
  # Define old variable names for the current year to dynamically extract them.
  select_variables_p <- c("idpers",  paste0("status",y), paste0("idhous", y), paste0("idspou",y), 
                          paste0("wstat",y), paste0("sex", y), paste0("age", y),
                          paste0("reg_1_", y), paste0("civsta", y), 
                          paste0("isced", y), paste0("edyear", y),
                          paste0("i", y, "wyg"), paste0("p", y, "w74"), 
                          paste0("is1maj", y),paste0("noga2m", y),
                          paste0("p", y, "w39"), paste0("p", y, "w87"),
                          paste0("p", y, "c01"), paste0("p", y, "e16"),
                          paste0("p",y,"w85"),paste0("p",y,"w32"),
                          paste0("ownkid",y),paste0("p", y, "c17"))
  
  # Select the specified variables
  df_p <- df_p[, select_variables_p]
  
  # Rename the variables
  setnames(df_p, old = select_variables_p, new = setnames_variables_p)
  
  # Filter for in-person interviews, clean, and add the year column
  if (y != "99"){
    df_p <- df_p %>%
      filter(interview_type == 0) %>%
      select(-interview_type) %>%
      mutate(year = as.numeric(paste0("20", y)))}
  else{
    df_p <- df_p %>%
      filter(interview_type == 0) %>%
      select(-interview_type) %>%
      mutate(year = 1999)
  }
  
  return(df_p)
})

# Combine all the yearly data frames from the list
data_p <- bind_rows(list_of_p_dfs)

# Save the combined personal dataset
saveRDS(data_p, "data/data_p.rds")


# The same procedure for the household data
list_of_h_dfs <- lapply(year, function(y) {
  file_path <- paste0("data/shp", y, "_h_user.dta")
  df_h <- read_dta(file_path)
  
  select_variables_h <- c(paste0("idhous", y), paste0("hldcen", y), paste0("nbb_", y),
                          paste0("canton",y),paste0("i",y,"htyg"))
  
  df_h <- df_h[, select_variables_h]
  setnames(df_h, old = select_variables_h, new = setnames_variables_h)
  
  if (y != "99"){df_h$year <- as.numeric(paste0("20", y))}
  else{df_h$year <- 1999}
  
  return(df_h)
})

# Combine the list of yearly household data frames
data_h <- bind_rows(list_of_h_dfs)
saveRDS(data_h, "data/data_h.rds")


# Merge the two data frames by household id and year
dat <- merge(data_p, data_h, by = c("househ_id", "year"))

# Save the final raw dataset 
saveRDS(dat, "data/raw_data.rds")

raw_dat <- readRDS("data/raw_data.rds")



# Keep the relevant variables and calculate age squared.
# Also, remove negative values in the
# relevant variables (which are generally NA in the SHP).

dat <- raw_dat %>%
  filter(annual_income > 0 & conWH > 0 & hh_g_income > 0 & age > 0) %>%
  mutate(age2 = age^2,
         hh_g_income_without = hh_g_income- annual_income) %>% 
  filter(!if_any(c(age, educyrs, civstat, n_children,canton,
                   nationality, sex, annual_income), ~ .x < 0)) %>%
  select(c(age, age2, educyrs, civstat, n_children, conWH,hh_g_income,canton,
           nationality, sex, annual_income, hh_g_income, personal_id, year))


# Recode some relevant variables like nationality and civil status.

dat <- dat %>%
  mutate(nationality = case_when(
    nationality == 10 ~ 0,
    nationality %in% c(11:17, 31) ~ 1,
    nationality %in% c(20, 30, 40, 50, 60) ~ 2
  )) %>%
  set_value_labels(nationality = c("Switzerland" = 0, "Europe or North America" = 1, "Rest of the world" = 2)) %>%
  set_variable_labels(nationality = "First nationality") 



dat <- dat %>%
  mutate(civstat = case_when(
    civstat %in% c(1, 3, 4, 5, 7) ~ 0,
    civstat %in% c(2, 6) ~ 1
  )) %>%
  set_value_labels(civstat = c("Single" = 0, "Married or registered partnership" = 1)) %>%
  set_variable_labels(civstat = "Martial status")


dat <- dat %>%
  filter( sex != 3)%>%
  mutate(sex = case_when(
    sex == 1 ~ 0,
    sex == 2 ~ 1
  )) %>%
  set_value_labels(sex = c("Male" = 0, "Female" = 1)) %>%
  set_variable_labels(sex = "Sex")


# Define K as the number of max years over which the pre-birth annual income and 
# household income. If there are less, less are taken.

K <- 3

# Generate the event variable and pre-birth variables.
dat <- dat %>%
  arrange(personal_id, year) %>%
  group_by(personal_id) %>%
  # Identify first birth year only  (seen with 0 AND >0 kids)
  mutate(
    first_birth_year = if (any(n_children == 0, na.rm = TRUE) && any(n_children > 0, na.rm = TRUE)) {
      min(year[n_children > 0], na.rm = TRUE)
    } else {
      NA
    },
    # children count at first birth (handles twins/higher multiples)
    children_at_first = ifelse(
      !is.na(first_birth_year),
      n_children[match(first_birth_year, year)],
      NA
    ),
    # second birth year = next parity increase AFTER first-birth year (does not
    # remove twins)
    second_birth_year = {
      after_first <- !is.na(first_birth_year) & (year > first_birth_year)
      cand <- year[ after_first & n_children >= (children_at_first + 1) ]
      ifelse(length(cand) > 0, min(cand), NA)
    },
    # create pre-birth window
    in_pre_window = !is.na(first_birth_year) &
      year >= (first_birth_year - K) & year <= (first_birth_year - 1),
    # number of usable observations in the window
    n_pre_inc  = ifelse(!is.na(first_birth_year),
                        sum(in_pre_window & !is.na(annual_income)), NA),
    n_pre_oth  = ifelse(!is.na(first_birth_year),
                        sum(in_pre_window & !is.na(hh_g_income - annual_income)), NA),
    # averages over whatever is available (1 to K), if none, set NA
    annual_income_pre_birth = ifelse(n_pre_inc > 0,
                                     mean(annual_income[in_pre_window], na.rm = TRUE),
                                     NA),
    other_income_pre_birth  = ifelse(n_pre_oth > 0,
                                     mean((hh_g_income - annual_income)
                                          [in_pre_window], na.rm = TRUE),
                                     NA)
  ) %>%
  ungroup() %>%
  # Event time and binning such that only first childbirths are included using the
  # variables defined before like first and second birth year 
  mutate(
    event_time = ifelse(!is.na(first_birth_year), year - first_birth_year, NA),
    keep_obs   = !is.na(event_time) & (is.na(second_birth_year) | year < second_birth_year),
    event_time_binned = case_when(
      keep_obs & event_time <= -4 ~ -4,
      keep_obs & event_time >=  6 ~  6,
      keep_obs                   ~ event_time,
      TRUE                       ~ NA
    )
  ) %>%
  filter(!is.na(event_time_binned), !is.na(annual_income_pre_birth))


# Transform to factor and more importantly set the reference year to the year
# before the event/birth
dat$event_time_binned_factor <- relevel(factor(dat$event_time_binned), ref = "-1")

# Run regressions for men and women seperately


# Run the pooled OLS model using plm
ols_hours_plm_female <- plm(
  conWH ~ event_time_binned_factor +
    annual_income_pre_birth + other_income_pre_birth +
    age + age2 + educyrs +
    factor(civstat) + factor(nationality) +
    factor(canton) + factor(year),
  data  = dat %>% filter(sex == 1),
  model = "pooling",
  index = c("personal_id","year")
)

ols_hours_plm_male <- plm(
  conWH ~ event_time_binned_factor +
    annual_income_pre_birth + other_income_pre_birth +
    age + age2 + educyrs +
    factor(civstat) + factor(nationality) +
    factor(canton) + factor(year),
  data  = dat %>% filter(sex == 0),
  model = "pooling",
  index = c("personal_id","year")
)


# Run the FE models with invidiual FE and canton*year FEs
fe_hours_plm_female  <- plm(
  conWH ~ event_time_binned_factor +
    factor(civstat) +                       
    factor(canton):factor(year),            
  data   = dat %>% filter(sex == 1),
  model  = "within",
  effect = "individual",                         
  index = c("personal_id","year")
)

fe_hours_plm_male  <- plm(
  conWH ~ event_time_binned_factor +
    factor(civstat) +                       
    factor(canton):factor(year),            
  data   = dat %>% filter(sex == 0),
  model  = "within",
  effect = "individual",                          
  index = c("personal_id","year")
)



# Calculate clustered standard errors for all models.

robust_se_ols_female <- coeftest(ols_hours_plm_female, 
                                 vcov = vcovHC(ols_hours_plm_female, type = "HC3", cluster = "group"))

robust_se_ols_male <- coeftest(ols_hours_plm_male, 
                               vcov = vcovHC(ols_hours_plm_male, type = "HC3", cluster = "group"))


robust_se_fe_female <- coeftest(fe_hours_plm_female, 
                                vcov = vcovHC(fe_hours_plm_female, type = "HC3", cluster = "group"))

robust_se_fe_male <- coeftest(fe_hours_plm_male, 
                              vcov = vcovHC(fe_hours_plm_male, type = "HC3", cluster = "group"))



# Save the results using stargazer (only the last one is in the paper.)

dir.create("tables", recursive = TRUE, showWarnings = FALSE)



stargazer(ols_hours_plm_female, ols_hours_plm_male,
          keep = "^event_time_binned_factor",
          title = "",
          se = list(robust_se_ols_female[,2],
                    robust_se_ols_male[,2]),
          header = FALSE,
          dep.var.labels.include = FALSE,
          model.names = FALSE,
          covariate.labels = c("Year $\\le$ -4","Year -3","Year -2","Year 0","Year 1","Year 2","Year 3","Year 4","Year 5","Year $\\geq$ 6"),
          notes.label = "",
          dep.var.caption = "",
          out="tables/pooled_ols_results.tex",
          digits = 3,
          column.labels = c("Female","Male"),
          model.numbers =FALSE,
          omit.stat = c("f","ser"),          
          float = FALSE,
          type = "latex")


stargazer(fe_hours_plm_female, fe_hours_plm_male,
          keep = "^event_time_binned_factor",
          title = "",
          se = list(robust_se_fe_female[,2],
                    robust_se_fe_male[,2]),
          header = FALSE,
          dep.var.labels.include = FALSE,
          model.names = FALSE,
          covariate.labels = c("Year $\\le$ -4","Year -3","Year -2","Year 0","Year 1","Year 2","Year 3","Year 4","Year 5","Year $\\geq$ 6"),
          notes.label = "",
          dep.var.caption = "",
          out="tables/fe_results.tex",
          digits = 3,
          column.labels = c("Female","Male"),
          model.numbers =FALSE,
          omit.stat = c("f","ser"),          
          float = FALSE,
          type = "latex")



stargazer(ols_hours_plm_female, ols_hours_plm_male, fe_hours_plm_female, fe_hours_plm_male,
          keep = "^event_time_binned_factor",
          title = "",
          se = list(robust_se_ols_female[,2], robust_se_ols_male[,2],
                    robust_se_fe_female[,2], robust_se_fe_male[,2]),
          header = FALSE,
          dep.var.labels.include = FALSE,
          model.names = FALSE,
          covariate.labels = c("Year $\\le$ -4","Year -3","Year -2","Year 0","Year 1","Year 2","Year 3","Year 4","Year 5","Year $\\geq$ 6"),
          notes.label = "",
          dep.var.caption = "",
          out="tables/reg_results.tex",
          digits = 3,
          column.labels = c(" OLS Female","OLS Male","FE Female","FE Male"),
          model.numbers =FALSE,
          omit.stat = c("f","ser"),          
          float = FALSE,
          type = "latex")



# Put the data from the regressions into a suitable format for the plots 
# seperately for men and women.

dir.create("plots",  recursive = TRUE, showWarnings = FALSE)


plot_data_fe_female <- tidy(robust_se_fe_female, conf.int = TRUE) %>%
  filter(grepl("event_time_binned_factor", term)) %>%
  mutate(event_time = as.numeric(gsub("event_time_binned_factor", "", term)))
plot_data_fe_female <- rbind(
  plot_data_fe_female,c("event_time_binned_factor-1",
                        0,0,0,0,0,0,-1))
plot_data_fe_female$model <- "Fixed Effects"
plot_data_ols_female <- tidy(robust_se_ols_female, conf.int = TRUE) %>%
  filter(grepl("event_time_binned_factor", term)) %>%
  mutate(event_time = as.numeric(gsub("event_time_binned_factor", "", term)))
plot_data_ols_female <- rbind(
  plot_data_ols_female,c("event_time_binned_factor-1",
                         0,0,0,0,0,0,-1))
plot_data_ols_female$model <- "Pooled OLS"
plot_data_female <- rbind(plot_data_ols_female,plot_data_fe_female)
plot_data_female[,2:8] <- mutate_all(plot_data_female[,2:8], function(x) as.numeric(as.character(x)))


plot_data_fe_male <- tidy(robust_se_fe_male, conf.int = TRUE) %>%
  filter(grepl("event_time_binned_factor", term)) %>%
  mutate(event_time = as.numeric(gsub("event_time_binned_factor", "", term)))
plot_data_fe_male <- rbind(
  plot_data_fe_male,c("event_time_binned_factor-1",
                      0,0,0,0,0,0,-1))
plot_data_fe_male$model <- "Fixed Effects"
plot_data_ols_male <- tidy(robust_se_ols_male, conf.int = TRUE) %>%
  filter(grepl("event_time_binned_factor", term)) %>%
  mutate(event_time = as.numeric(gsub("event_time_binned_factor", "", term)))
plot_data_ols_male <- rbind(
  plot_data_ols_male,c("event_time_binned_factor-1",
                       0,0,0,0,0,0,-1))
plot_data_ols_male$model <- "Pooled OLS"
plot_data_male <- rbind(plot_data_ols_male,plot_data_fe_male)
plot_data_male[,2:8] <- mutate_all(plot_data_male[,2:8], function(x) as.numeric(as.character(x)))


# Plot the regression results for women
plot_female <- ggplot(plot_data_female, aes(x = event_time, y = estimate, color = model)) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high),
                  position = position_dodge(width = 0.4), size = 0.8) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  labs(
    x = "Years relative to first birth",
    y = "Change in weekly hours",
    color = "Type"
  ) +
  scale_x_continuous(breaks=seq(-3, 6, 1))+
  scale_color_brewer(palette = "Set1", name = "Type", direction = 1) +
  theme_minimal() +
  theme(text = element_text(size = 18, family = "CMU Serif"),
          axis.title = element_text(size = 18),
          axis.text = element_text(size = 16),
          axis.title.x = element_text(margin = margin(t = 10)),
          axis.title.y = element_text(margin = margin(r = 10)),
          legend.position = "bottom")


ggsave("plots/plot_female.png", plot_female, width = 8, height = 5, dpi = 800)                             

# Plot the regression results for men
plot_male <- ggplot(plot_data_male, aes(x = event_time, y = estimate, color = model)) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high),
                  position = position_dodge(width = 0.4), size = 0.8) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  labs(
    x = "Years relative to first birth",
    y = "Change in weekly hours",
    color = "Type"
  ) +
  scale_x_continuous(breaks=seq(-3, 6, 1))+
  scale_color_brewer(palette = "Dark2", name = "Type", direction = 1) +
  theme_minimal() +
  theme(text = element_text(size = 18, family = "CMU Serif"),
        axis.title = element_text(size = 18),
        axis.text = element_text(size = 16),
        axis.title.x = element_text(margin = margin(t = 10)),
        axis.title.y = element_text(margin = margin(r = 10)),
        legend.position = "bottom")
  

ggsave("plots/plot_male.png", plot_male, width = 8, height = 5, dpi = 800)                             





### Kernel density estimation plot


setwd("plots")


# Define the data for the KDE
plot_dat_female <- dat %>%
  filter(sex == 1, event_time_binned %in% c(-2:3)) %>%
  select(conWH, event_time_binned) %>%
  mutate(conWH = as.vector(conWH),
         event_time_binned = event_time_binned) %>%
  na.omit()



# Bandwidth selection
bw <- npudensbw(~event_time_binned + conWH,
                data = plot_dat_female,
                tol = .1,
                ftol = .1)

# Generate grids
event_time_binned.seq <- sort(unique(plot_dat_female$event_time_binned))
conWH.seq <- seq(min(plot_dat_female$conWH),
                 max(plot_dat_female$conWH),length=50)
eval <- expand.grid(event_time_binned=event_time_binned.seq, 
                    conWH=conWH.seq)

# Fit KDE with the bandwidth
fhat <- fitted(npudens(bws=bw,newdata=eval))

# Apply fitted KDE to the values of the event time
f <- matrix(fhat, length(unique(plot_dat_female$event_time_binned)), 50)


# Plot the results comb plot and save as PDF first and then as png.
# The reason for that are issues with the font.
cairo_pdf("kde_comb_plot_female.pdf", 
          width = 8, 
          height = 6, 
          family = "CMU Serif",pointsize = 16)

scatterplot3d(
  eval$event_time_binned, 
  eval$conWH,             
  fhat/max(fhat),                       
  ylab = "Contractual working hours", 
  xlab = "Years relative to first birth", 
  zlab = "Joint relative density",
  angle = 65,
  box = FALSE,
  type = "h", 
  grid = TRUE,
  color = "blue"
)


dev.off() 

pdf_convert(
  pdf = "kde_comb_plot_female.pdf", 
  format = "png",
  dpi = 800)




# The same procedure for men.
plot_dat_male <- dat %>%
  filter(sex == 0, event_time_binned %in% c(-2:3)) %>%
  select(conWH, event_time_binned) %>%
  mutate(conWH = as.vector(conWH),
         event_time_binned = event_time_binned) %>%
  na.omit()



# Bandwidth selection
bw <- npudensbw(~event_time_binned + conWH,
                data = plot_dat_male,
                tol = .1,
                ftol = .1)

# Generate grids
event_time_binned.seq <- sort(unique(plot_dat_male$event_time_binned))
conWH.seq <- seq(min(plot_dat_male$conWH),max(plot_dat_male$conWH), length=50)
eval <- expand.grid(event_time_binned=event_time_binned.seq, conWH=conWH.seq)

# Fit KDE with the bandwidth
fhat <- fitted(npudens(bws=bw,newdata=eval))

# Apply fitted KDE to the values of the event time
f <- matrix(fhat, length(unique(plot_dat_male$event_time_binned)), 50)


# Plot the results comb plot and save as PDF first and then as png.
# The reason for that are issues with the font.

cairo_pdf("kde_comb_plot_male.pdf", 
          width = 8, 
          height = 6, 
          family = "CMU Serif",pointsize = 16)

scatterplot3d(
  eval$event_time_binned, 
  eval$conWH,             
  fhat/max(fhat),                       
  ylab = "Contractual working hours", 
  xlab = "Years relative to first birth", 
  zlab = "Joint relative density",
  angle = 65,
  box = FALSE,
  type = "h", 
  grid = TRUE,
  color = "blue"
)


dev.off() 

pdf_convert(
  pdf = "kde_comb_plot_male.pdf", 
  format = "png",
  dpi = 800)


# This script by Jeffrey Racine from his website: https://jeffreyracine.github.io/research/ 
# to generate Figure 2.4 on page 20 in
# Racine, J. (2014). Nonparametric Econometrics: A Primer. Now Publishers.

# 
# 
# library(np)
# options(np.messages=FALSE)
# library(scatterplot3d)
# data(wage1)
# attach(wage1)
# 
# bw <- npudensbw(~lwage+ordered(numdep), tol=.1,ftol=.1, data=wage1)
# 
# numdep.seq <- sort(unique(numdep))
# lwage.seq <- seq(min(lwage),max(lwage),length=50)
# wage1.eval <- expand.grid(numdep=ordered(numdep.seq), lwage=lwage.seq)
# 
# fhat <- fitted(npudens(bws=bw,newdata=wage1.eval))
# 
# f <- matrix(fhat,length(unique(numdep)),50)
# 
# postscript(file="persp.ps", horizontal=FALSE, pagecentre=FALSE, height=8.5, width=8.5, pointsize=16, paper="special")
# persp(numdep.seq,
#       lwage.seq,
#       f,
#       ticktype="detailed", 
#       ylab="Log wage",
#       xlab="Number of Dependents",
#       zlab="Joint Density",
#       theta=110,
#       phi=40)
# dev.off()
# 
# 
# postscript(file="scatter.ps", horizontal=FALSE, pagecentre=FALSE, height=8.5, width=8.5,pointsize=16, paper="special")
# ## Hack since scatterplot converts ordered 0-6 to numeric 1-7
# scatterplot3d(as.numeric(wage1.eval[,1])-1 ,wage1.eval[,2], fhat,
#               ylab="Log wage",
#               xlab="Number of Dependents",
#               zlab="Joint Density",
#               angle=15,box=FALSE,type="h",grid=TRUE,color="blue")
# dev.off()
# 
# summary(bw)
# 
# library(xtable)
# 
# xtable(table(numdep))
