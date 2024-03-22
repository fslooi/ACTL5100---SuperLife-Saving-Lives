# Set system locale to "English_United States.1252"
Sys.setlocale(category = "LC_ALL", locale = "English_United States.1252")

# Declare vector of R packages to be installed
libs <- c(
  "tidyverse",
  "labelled",
  "magrittr",
  "survival",
  "survminer",
  "readr",
  "patchwork",
  "discSurv"
)

# Check if each package in libs is installed
installed_packages <- libs %in% rownames(installed.packages())
# If any package is not installed, install it
if (any(installed_packages == FALSE)) {
  install.packages(libs[!installed_packages])
}
# Load required libraries
invisible(lapply(libs, require, character.only = TRUE))

###########################################################################

# Policy.type, Sex, Smoker.Status, Urban vs Rural are binary variables
# T20, M, NS, Urban are over half and marked as 0, alternative as 1

# length(unique(eda$Policy.number)) == length((eda$Policy.number))
# This variable is useless

# Lapse.Indicator has a strange Y value, it's actually 1

# The data is pre-processed by python
# The left truncated and right censored time is obtained
data_full <- read_csv("data_by_py.csv")

# Survival object: "Issue.age", "Exit.age", "Cause.of.Exit" ("Death.indicator")
# binary covariate: "Policy.type", "Sex", "Smoker.Status", "Urban.vs.Rural"
# other covariate: "Face.amount", "Underwriting.Class", "Region", "Distribution.Channel"



data_surv <- data_full %>% filter(Surv.Time>0.01)

km_surv_plt <- function(data_plt,covar){

  formula_surv <- "Surv(Issue.age,Exit.age,Death.indicator)~" %>% paste(covar) %>% as.formula()
  km_fit_Sex <- do.call(survfit, args = list(formula = formula_surv, data = data_plt))
  
  p1 = km_fit_Sex %>%
    ggsurvplot(
      data = data_plt,
      fun = "pct",
      # linetype = "strata", # Change line type by groups
      # pval = TRUE,
      conf.int = TRUE,
      risk.table = TRUE,
      fontsize = 3, # used in risk table
      surv.median.line = "hv", # median horizontal and vertical ref lines
      ggtheme = theme_light(),
      #palette = c("goldenrod", "sienna", "tomato"),
      title = "Kaplan-Meier Survival Function Estimate",
      legend.title = "",
      #legend.labs = levels(data_plt[covar]),
      xlim = c(26,87)
    )
  
  p2 = km_fit_Sex %>%
    ggsurvplot(
      data = data_plt,
      fun = "cumhaz",
      # linetype = "strata", # Change line type by groups
      # pval = TRUE,
      conf.int = TRUE,
      risk.table = TRUE,
      fontsize = 3, # used in risk table
      surv.median.line = "hv", # median horizontal and vertical ref lines
      ggtheme = theme_light(),
      #palette = c("goldenrod", "sienna", "tomato"),
      title = "Kaplan-Meier Survival Function Estimate",
      legend.title = "",
      legend.labs = levels(data_plt[covar]),
      xlim = c(26,87)
    )
  
  arrange_ggsurvplots(list(p1,p2))
}

km_surv_plt(data_surv,"Sex")
km_surv_plt(data_surv,"Policy.type")
km_surv_plt(data_surv,"Smoker.Status")
km_surv_plt(data_surv,"Urban.vs.Rural")
km_surv_plt(data_surv,"Face.amount")
km_surv_plt(data_surv,"Underwriting.Class")
km_surv_plt(data_surv,"Region")
km_surv_plt(data_surv,"Distribution.Channel")

cph_fit_0 = coxph(formula = Surv(Issue.age,Exit.age,Death.indicator)~
                    Sex+Policy.type+Smoker.Status+Urban.vs.Rural+
           Face.amount+Underwriting.Class+as.factor(Region)+Distribution.Channel,
           data = data_surv)

cph_fit_1 = coxph(formula = Surv(Issue.age,Exit.age,Death.indicator)~
                    Sex+Policy.type+Smoker.Status+
                    Underwriting.Class+Distribution.Channel,
                  data = data_surv)

# The effect of Distribution.Channel being Online is significant
# However, we find that the max age of this group is 79, while others being 87
# It's because it started at 2009 while others started at 2001
# We can also see that the mortality before 79 are almost the same

data_surv$Underwriting.Class = data_surv$Underwriting.Class %>% as.factor %>% relevel("very low risk")

cph_fit_2 = coxph(formula = Surv(Issue.age,Exit.age,Death.indicator)~
                    Sex+Policy.type+Smoker.Status+
                    as.factor(Underwriting.Class),
                  data = data_surv)

# discrete time survival analysis may be more appropriate

data_base = data_surv[1,]
data_base$Policy.type=0
data_base$Sex=0
data_base$Smoker.Status=0
data_base$Urban.vs.Rural=0
data_base$Underwriting.Class=factor("very low risk")
levels(data_base$Underwriting.Class)=levels(data_surv$Underwriting.Class)

baselinefit <- survfit(cph_fit_2, newdata = data_base)

###########################################################################

# assume enter at start of the year and exit at end of the year
data_full_adj <- data_full %>% mutate(Surv.Time.Adj = Surv.Time + 1)

data_full_adj$Underwriting.Class = data_full_adj$Underwriting.Class %>% as.factor %>% relevel("very low risk")

data_full_adj$Distribution.Channel = data_full_adj$Distribution.Channel %>% as.factor %>% relevel("Online")

data_full_adj <- data_full_adj %>% mutate(FromOnline = Distribution.Channel=="Online")

cph_fit_adj = coxph(formula = Surv(Issue.age,Exit.age+1,Death.indicator)~
                      Sex+Policy.type+Smoker.Status+
                      as.factor(Underwriting.Class)+
                      FromOnline,
                    data = data_full_adj)

# cph_fit_adj %>% tbl_regression(estimate_fun = function(x) style_number(x, digits = 5))

data_base = data_full_adj[1,]
data_base$Policy.type=0
data_base$Sex=0
data_base$Smoker.Status=0
data_base$Urban.vs.Rural=0
data_base$FromOnline=FALSE
data_base$Underwriting.Class=factor("very low risk")
levels(data_base$Underwriting.Class)=levels(data_full_adj$Underwriting.Class)

baselinefit <- survfit(cph_fit_adj, newdata = data_base)