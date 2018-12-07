sdtVarData <- readRDS("sdtVarData.RDS")
time_01 <- readRDS("time_01.RDS")
bw_object <- readRDS("bw_object.RDS")
tvvar_obj <- readRDS("tvvar_obj.RDS")
sdtVarData_values <- readRDS("sdtVarData_values.RDS")

pred_obj <- predict(object = tvvar_obj,
                    data = sdtVarData_values,
                    tvMethod = "weighted")
library(tidyverse)

sdtVarData <- readr::read_rds("sdtVarData.RDS")

sdtVarData <- sdtVarData %>% 
  tidyr::spread(Field, Value) %>%
  dplyr::select(autonomy, competence, relatedness, Date) %>% 
  dplyr::arrange(Date) %>% 
  na.omit(.)

df <- sdtVarData$Date
time_01 <- df - df[1]
time_01 <- as.numeric(time_01)
time_01 <- time_01 / max(time_01) # normalise time, not necessary for mgm, though

sdtVarData_values <- sdtVarData %>% 
  dplyr::select(-Date) %>% 
  dplyr::mutate_all(funs(as.numeric(.)))

bwSeq <- seq(0.01, 1, length = 10) # ten equally spaced values in [0:01; 1]

# Find optimal bandwidth
bw_object <- mgm::bwSelect(data = sdtVarData_values,
                           type = rep("g", 3),
                           level = rep(1, 3),
                           bwSeq = bwSeq,
                           bwFolds = 2,
                           bwFoldsize = 20,
                           modeltype = "mvar",
                           lags = 1,
                           scale = TRUE,
                           timepoints = time_01)

bandwidth <- bwSeq[which.min(bw_object$meanError)]

data.frame(bwSeq, bw_object$meanError) %>% plot()

# --------- Fit time-varying VAR -----------------------------------

set.seed(1)
tvvar_obj <- mgm::tvmvar(data = sdtVarData_values,
                         type = rep("g", 3),
                         level = rep(1, 3),
                         lambdaSel = "CV",
                         timepoints = time_01,
                         estpoints = seq(from = 0, to = 1, length = 50), 
                         bandwidth = bandwidth,
                         lags = 1,
                         saveData = TRUE,
                         scale = TRUE)

tvvar_obj
sdtVarData_values_datamatrix <- data.matrix(sdtVarData_values)

pred_obj <- predict(object = tvvar_obj,
                    data = sdtVarData_values_datamatrix,
                    tvMethod = "weighted")

residuals <- pred_obj$true - pred_obj$predicted
