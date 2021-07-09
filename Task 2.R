library(quantreg)
library(ggplot2)
library(hrbrthemes)
library(dplyr)
library(tidyr)
library(viridis)

data <- read.csv("ps3.csv", header = T, sep = " ")

data$black = factor(data$black)

attach(data)

bl_children <- subset(data, black == 1)
bl_children <- subset(bl_children, select = -c(black))
wh_children <- subset(data, black == 0)
wh_children <- subset(wh_children, select = -c(black))

counter <- function(data = bl_children, 
                    column = bl_children$STS7, value = -4) {
  rows <- nrow(data[column <= value, ])
  return(rows)
}

no_bl <- sapply(c(-4:3), 
                counter, 
                data = bl_children, 
                column = bl_children$STS7)
no_wh <- sapply(c(-4:3), 
                counter, 
                data = wh_children, 
                column = wh_children$STS7)

F_bl <- no_bl/length(bl_children$STS7)
F_wh <- no_wh/length(wh_children$STS7)

regress <- function(y = -4, reg_data = data) {
  reg_data$STS7 <- ifelse(reg_data$STS7 <= y, 1, 0)
  summary(reg_data$STS7)
  l_reg <- glm(reg_data, family = "binomial")
  return(l_reg)
}

y <- c(-4:3)
wh_list <- lapply(y, regress, reg_data = wh_children)

wh_pred_list <- sapply(wh_list, 
                       predict, 
                       newdata = bl_children, 
                       type = "response")

bw_mean <- colMeans(wh_pred_list)

FWB <- data.frame(x = y, y = bw_mean)

total_difference <- F_wh - F_bl

explained_part <- F_wh - bw_mean

unexplained_part <- bw_mean - F_bl

total_difference - (explained_part + unexplained_part)

plot_data <- data.frame(x = y, 
                        diff = total_difference, 
                        explain = explained_part, 
                        unexplained = unexplained_part)

size = 1.3
ggplot(plot_data, aes(x = y)) + 
  labs(title = "Decomposition differences in the propabilities 
       between black and white",
       x = "STS7 score",
       y = "Probability difference") +
  geom_line(aes(y = diff, 
                color = "Total", 
                linetype = "Total"), 
            size = size) + 
  geom_line(aes(y = explain, 
                color = "Explain", 
                linetype = "Explain"), 
            size = size) + 
  geom_line(aes(y = unexplained, 
                color = "Unexplain", 
                linetype = "Unexplain"), 
            size = size) +
  scale_color_manual(name = "Part", 
                     values = c("Total" = "black", 
                                "Explain" = "red", 
                                "Unexplain" = "blue")) + 
  scale_linetype_manual(name = "Part", 
                        values = c("Total" = "solid", 
                                   "Explain" = "dashed", 
                                   "Unexplain" = "dotdash"))



