# INPUTS
setwd("C:/Users/zz012/OneDrive/桌面/新建文件夹")
mort_data <- read.csv("BASE_MORT_TBL.csv")
exp_data <- read.csv("EXPENSE_TBL.csv")
int_data <- read.csv("INT_TBL.csv")
lapse_data <- read.csv("LAPSE_TBL.csv")
pol_data <- read.csv("POL_TBL.csv")

max_age <- 120
int_start_year <- 1962

output_file <- "R_output.csv"

######################################################################
# Do not amend the code below
px <- mort_data$SURV_PROB
rpx <- mort_data$NEWSURV_PROB
agentcomm_rate <- exp_data$VALUE[1]
gen_expense <- exp_data$VALUE[2]
lapse_expense <- exp_data$VALUE[3]
MORT_DECREASING_EXPENSE1 <- exp_data$VALUE[4]
noneagentcomm_rate<- exp_data$VALUE[5]
MORT_DECREASING_EXPENSE2 <- exp_data$VALUE[6]

dist_factor <- (1 + int_data$RATE) ^ (-1)

lapse_rate <- lapse_data$LAPSE
no_lapse_rate <- lapse_data$NO_LAPSE1
no_lapse_rate2 <- lapse_data$NO_LAPSE2

year <- pol_data$ISSUE_YEAR
age <- pol_data$ISSUE_AGE
face_amt <- pol_data$FACE_AMT
pol_type <- pol_data$POL_TYPE
dist <- pol_data$DIST_CHAN
smoke <- pol_data$SMOKER
mort_load <- pol_data$MORT_LOADING
rmort_load <- pol_data$MORT_LOADING_incentive

BENEFIT0 <- rep(NA, length(mort_load))
EXPENSE0 <- rep(NA, length(mort_load))
PREMIUM0 <- rep(NA, length(mort_load))
UNITPREMIUM0<- rep(NA, length(mort_load))
BENEFIT1 <- rep(NA, length(mort_load))
EXPENSE1 <- rep(NA, length(mort_load))
PREMIUM1 <- rep(NA, length(mort_load))
PROFIT1 <- rep(NA, length(mort_load))
for (i in 1:length(mort_load)){
  pol_px <- px ^ mort_load[i]
  pol_rpx <- rpx ^ rmort_load[i]
  pol_age <- age[i]
  pol_year <- year[i]
  pol_dist <- dist[i]
  pol_smoke<- smoke[i]
  pol_tpx <- c(1)
  pol_vt <- c(1)
  pol_rtpx<- c(1)
  for (j in 1:(max_age - pol_age)){
    pol_tpx = c(pol_tpx, prod(pol_px[(pol_age + 1):(pol_age + j)]))
    pol_rtpx = c(pol_rtpx, prod(pol_rpx[(pol_age + 1):(pol_age + j)]))
    pol_vt = c(pol_vt, pol_vt[j] * dist_factor[pol_year - int_start_year + j])
  }
  pol_vt = c(pol_vt, pol_vt[length(pol_vt)] * dist_factor[pol_year - int_start_year + max_age - pol_age + 1])
  pol_qxt <-1 - pol_px[(pol_age + 1):(max_age + 1)]
  pol_rqxt <-1 - pol_rpx[(pol_age + 1):(max_age + 1)]
  
  # Calculations
  if (pol_type[i] == 1){
    BENEFIT0[i] = face_amt[i] * sum(pol_tpx * pol_qxt * pol_vt[2:length(pol_vt)])
    EXPENSE0[i] = agentcomm_rate * (1 - pol_dist)* face_amt[i] + gen_expense + pol_dist * noneagentcomm_rate
    PREMIUM0[i] = BENEFIT0[i] + EXPENSE0[i]
    BENEFIT1[i] = face_amt[i] * sum(pol_rtpx * pol_rqxt * pol_vt[2:length(pol_vt)])# profit change with program(remaining the premium without program)
    EXPENSE1[i] =agentcomm_rate*(1-pol_dist)* face_amt[i] + gen_expense + pol_dist * noneagentcomm_rate + pol_smoke * MORT_DECREASING_EXPENSE2 +MORT_DECREASING_EXPENSE1*sum(pol_rtpx * pol_vt[1:(length(pol_vt) - 1)])
    PREMIUM1[i] = BENEFIT1[i] + EXPENSE1[i]
  }else{
    BENEFIT0[i] = face_amt[i] * sum(pol_tpx[1:20] * pol_qxt[1:20] * pol_vt[1:20] * no_lapse_rate)
    EXPENSE0[i] =  agentcomm_rate * (1 - pol_dist) * face_amt[i] + gen_expense + lapse_expense * sum(pol_rtpx[1:20] * pol_vt[2:21] * no_lapse_rate2 *lapse_rate)+pol_dist * noneagentcomm_rate
    UNITPREMIUM0[i] = sum(pol_tpx[1:20] * pol_vt[1:20] *  no_lapse_rate)
    PREMIUM0[i] =(BENEFIT0[i]+EXPENSE0[i])/UNITPREMIUM0[i]
    BENEFIT1[i] = face_amt[i] * sum(pol_rtpx[1:20] * pol_rqxt[1:20] * pol_vt[1:20] * no_lapse_rate)# profit change with interest upper scenario(remaining the premium without program)
    EXPENSE1[i] = agentcomm_rate * (1 - pol_dist) * face_amt[i] + gen_expense + lapse_expense * sum(pol_rtpx[1:20] * pol_vt[2:21] * no_lapse_rate2 *lapse_rate) + pol_dist * noneagentcomm_rate + pol_smoke * MORT_DECREASING_EXPENSE2 +MORT_DECREASING_EXPENSE1*sum(pol_rtpx * pol_vt[1:(length(pol_vt) - 1)])
    UNITPREMIUM0[i] = sum(pol_tpx[1:20] * pol_vt[1:20] *  no_lapse_rate)
    PREMIUM1[i] =(BENEFIT1[i]+EXPENSE1[i])/UNITPREMIUM0[i]
  }
}
# Export csv file 
pricing_data <- data.frame(BENEFIT0, EXPENSE0, PREMIUM0, BENEFIT1, EXPENSE1, PREMIUM1)
write.csv(pricing_data, file = output_file, row.names = FALSE)