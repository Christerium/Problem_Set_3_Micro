library(quantreg)
library(ggplot2)
library(hrbrthemes)
library(dplyr)
library(tidyr)
library(viridis)

data <- read.csv("ps3.csv", header = T, sep = " ")

data$black = factor(data$black)

ggplot(data, aes(STS7)) +
  geom_histogram(aes(y = ..density.., fill = black), 
                 alpha = 0.6, 
                 bins = 50) +
  labs(title = "Histogram of the STS7 test scores") +
  scale_fill_discrete(name = "black", 
                      labels = c("White Children", "Black Children"))

attach(data)

rqfit <- rq(data)

summary(rqfit)

regress <- function(y = -4, reg_data = data) {
  reg_data$STS7 <- ifelse(reg_data$STS7 <= y, 1, 0)
  summary(reg_data$STS7)
  l_reg <- glm(reg_data, family = "binomial")
  return(l_reg)
}

bl_children <- subset(data, black == 1)
bl_children <- subset(bl_children, select = -c(black))
wh_children <- subset(data, black == 0)
wh_children <- subset(wh_children, select = -c(black))

y <- c(-4:3)
bl_list <- lapply(y, regress, reg_data = bl_children)
wh_list <- lapply(y, regress, reg_data = wh_children)

NEWDATA_bl <- sapply(bl_children, mean)
NEWDATA_bl <- data.frame(t(data.frame(NEWDATA_bl)))

NEWDATA_wh <- sapply(wh_children, mean)
NEWDATA_wh <- data.frame(t(data.frame(NEWDATA_wh)))

bl_pred_list <- sapply(bl_list, predict, 
                       newdata = NEWDATA_bl, type = "response")
wh_pred_list <- sapply(wh_list, predict, 
                       newdata = NEWDATA_wh, type = "response")

bl_pred_df <- data.frame(x = y, y = bl_pred_list)
wh_pred_df <- data.frame(x = y, y = wh_pred_list)

size = 1.2
ggplot(data, aes(STS7)) + 
  ggtitle("CDF's of the STS7 test results and its estimates split by ethnicity") +
  stat_ecdf(aes(colour='All', linetype = "All"), geom = "step", size=size) +
  stat_ecdf(data=bl_children, aes(y = STS7, color = "Black", linetype = "Black")
            , geom = "step", size=size) +
  stat_ecdf(data=wh_children, aes(y = STS7, colour='White', linetype = "White")
            , geom = "step", size=size) +
  geom_line(data=bl_pred_df, aes(y = y, x = x, 
                                 colour='Black_est', 
                                 linetype = "Black_est"), 
            size=size) +
  geom_line(data=wh_pred_df, aes(y = y, x = x, 
                                 colour='White_est', 
                                 linetype = "White_est"), 
            size=size) +
  geom_hline(yintercept = 0, size=size/1.5, 
             colour = "gray60", 
             linetype = "dashed") +
  geom_hline(yintercept = 1, size=size/1.5, 
             colour = "gray60", 
             linetype = "dashed") +
  labs(colour = "Ethnicity", shape = "Estimate", 
       y = expression(hat(F)["Y"]*"(y)")) + 
  scale_color_manual(name = "Ethnicity", 
                     values = c("All" = "black",
                                "Black" = "firebrick1",
                                "White" = "blue4",
                                "Black_est" = "firebrick",
                                "White_est" = "steelblue3")) +
  scale_linetype_manual(name = "Ethnicity", 
                        values = c("All" = "solid",
                                   "Black" = "solid",
                                   "White" = "solid", 
                                   "Black_est" = "dotdash",
                                   "White_est" = "dotdash"))

diff <- data.frame(x = y, y = bl_pred_df$y - wh_pred_df$y)

ggplot(diff, aes(x)) + 
  geom_line(aes(y = y), size = size) + 
  labs(title = "Difference in distribution",
       x = "STS7 Score",
       y = expression("F"["Y"]^"Black"*"(y)" -
                        "F"["Y"]^"White"*"(y)"))

# Bonus
tau_seq <- seq(0.1, 0.9, 0.1)
tau_bl <- quantile(bl_children$STS7, 
                   probs = tau_seq, names = F)
tau_bl

tau_wh <- quantile(wh_children$STS7, 
                   probs = tau_seq, names = F)
tau_wh

bl_list_tau <- lapply(tau_bl, 
                      regress, 
                      reg_data = bl_children)
wh_list_tau <- lapply(tau_wh, 
                      regress, 
                      reg_data = wh_children)

bl_coeffs <- data.frame(t(sapply(bl_list_tau, coefficients)))
wh_coeffs <- data.frame(t(sapply(wh_list_tau, coefficients)))

setDT(bl_coeffs, keep.rownames = TRUE)[]
setDT(wh_coeffs, keep.rownames = TRUE)[]

bl_coeffs$rn <- as.numeric(bl_coeffs$rn) / 10
wh_coeffs$rn <- as.numeric(wh_coeffs$rn) / 10

bl_age <- bl_coeffs[, c(1,3,4)]
bl_age <- melt(bl_age,  
               id.vars = 'rn', 
               variable.name = 'series')
wh_age <- wh_coeffs[, c(1,3,4)]
wh_age <- melt(wh_age,  
               id.vars = 'rn', 
               variable.name = 'series')

bl_inc <- bl_coeffs[, c(1, c(5:10))]
bl_inc <- melt(bl_inc,  
               id.vars = 'rn', 
               variable.name = 'series')
wh_inc <- wh_coeffs[, c(1, c(5:10))]
wh_inc <- melt(wh_inc,  
               id.vars = 'rn', 
               variable.name = 'series')

bl_m_educ <- bl_coeffs[, c(1, seq(11, 17, 2))]
bl_m_educ <- melt(bl_m_educ,  
                  id.vars = 'rn', 
                  variable.name = 'series')
bl_d_educ <- bl_coeffs[, c(1, seq(12, 18, 2))]
bl_d_educ <- melt(bl_d_educ,  
                  id.vars = 'rn', 
                  variable.name = 'series')

wh_m_educ <- wh_coeffs[, c(1, seq(11, 17, 2))]
wh_m_educ <- melt(wh_m_educ,  
                  id.vars = 'rn', 
                  variable.name = 'series')
wh_d_educ <- wh_coeffs[, c(1, seq(12, 18, 2))]
wh_d_educ <- melt(wh_d_educ,  
                  id.vars = 'rn', 
                  variable.name = 'series')

bl_weight <- bl_coeffs[, c(1, 19)]
bl_weight <- melt(bl_weight,  
                  id.vars = 'rn', 
                  variable.name = 'series')
wh_weight <- wh_coeffs[, c(1, 19)]
wh_weight <- melt(wh_weight,
                  id.vars = 'rn', 
                  variable.name = 'series')

bl_sib <- bl_coeffs[, c(1, c(20:26))]
bl_sib <- melt(bl_sib,  
               id.vars = 'rn', 
               variable.name = 'series')
wh_sib <- wh_coeffs[, c(1, c(20:26))]
wh_sib <- melt(wh_sib,  
               id.vars = 'rn', 
               variable.name = 'series')


require(gridExtra)

pdf("age.pdf", height = 4, width = 6)
ggplot(bl_age) + 
  labs(title = "Quantile effects of Age", 
       y = expression(hat(beta)[tau]), 
       x = expression(tau)) + 
  geom_line(size = 1.3, aes(x = rn, 
                            y = value, 
                            col = series,  
                            linetype = "Black")) + 
  geom_line(data = wh_age, size = 1.3, 
            aes(x = rn, 
                y = value, 
                col = series,  
                linetype = "White")) +
  scale_linetype_manual(name = "Ethnicity", 
                        values = c("Black" = "solid", 
                                   "White" = "dotdash"))
dev.off()

pdf("bl_inc.pdf", height = 4, width = 6)
ggplot(bl_inc, 
       aes(x = rn, 
           y = value, 
           col = series)) + 
  labs(title = "Quantile effects of Income (Black)", 
       y = expression(hat(beta)[tau]), 
       x = expression(tau)) + 
  geom_line(size = 1.3) 
dev.off()

pdf("wh_inc.pdf", height = 4, width = 6)
ggplot(wh_inc, 
       aes(x = rn, 
           y = value, 
           col = series)) + 
  labs(title = "Quantile effects of Income (White)", 
       y = expression(hat(beta)[tau]), 
       x = expression(tau)) + 
  geom_line(size = 1.3) 
dev.off()

pdf("m_educ.pdf", height = 4, width = 6)
ggplot(bl_m_educ) + 
  labs(title = "Quantile effects of Education (Mother)", 
       y = expression(hat(beta)[tau]), 
       x = expression(tau)) + 
  geom_line(size = 1.3, 
            aes(x = rn, 
                y = value, 
                col = series,  
                linetype = "Black")) + 
  geom_line(data = wh_m_educ, size = 1.3, 
            aes(x = rn, y = value, col = series,  
                linetype = "White")) +
  scale_linetype_manual(name = "Ethnicity", 
                        values = c("Black" = "solid", 
                                   "White" = "dotdash"))
dev.off()

pdf("d_educ.pdf", height = 4, width = 6)
ggplot(bl_d_educ) + 
  labs(title = "Quantile effects of Education (Father)", 
       y = expression(hat(beta)[tau]), 
       x = expression(tau)) + 
  geom_line(size = 1.3, 
            aes(x = rn, 
                y = value, 
                col = series,  
                linetype = "Black")) + 
  geom_line(data = wh_d_educ, size = 1.3, 
            aes(x = rn, y = value, col = series,  
                linetype = "White")) +
  scale_linetype_manual(name = "Ethnicity", 
                        values = c("Black" = "solid", 
                                   "White" = "dotdash"))
dev.off()

pdf("weight.pdf", height = 4, width = 6)
ggplot(bl_weight) + 
  labs(title = "Quantile effects of Height", 
       y = expression(hat(beta)[tau]), 
       x = expression(tau)) + 
  geom_line(size = 1.3, 
            aes(x = rn, 
                y = value, 
                col = series,  
                linetype = "Black")) + 
  geom_line(data = wh_weight, size = 1.3, 
            aes(x = rn, y = value, col = series,  
                linetype = "White")) +
  scale_linetype_manual(name = "Ethnicity", 
                        values = c("Black" = "solid", 
                                   "White" = "dotdash"))
dev.off()

pdf("bl_sib.pdf", height = 4, width = 6)
ggplot(bl_sib, 
       aes(x = rn, 
           y = value, 
           col = series)) + 
  labs(title = "Quantile effects of Siblings (Black)", 
       y = expression(hat(beta)[tau]), 
       x = expression(tau)) + 
  geom_line(size = 1.3) 
dev.off()

pdf("wh_sib.pdf", height = 4, width = 6)
ggplot(wh_sib, 
       aes(x = rn, 
           y = value, 
           col = series)) + 
  labs(title = "Quantile effects of Siblings (White)", 
       y = expression(hat(beta)[tau]), x = expression(tau)) + 
  geom_line(size = 1.3) 
dev.off()
