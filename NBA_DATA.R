library(tidyverse)
library(magrittr)
library(benford.analysis)

try <- read.csv("2012-18_playerBoxScore.csv")

bfd.cp <- benford(try$playAST,number.of.digits = 1)

plot(bfd.cp)

try_awayAST <- filter(try,try$teamLoc=="Away")
try_homeAST <- filter(try,try$teamLoc=="Home")

try$playAST <- as.numeric(try$playAST)

hist(x=try_awayAST$playAST)
hist(x=try_homeAST$playAST)

chisq.test(try_awayAST$playAST)
