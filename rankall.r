rankall <- function (Outcome = " ", num = "best") {
	file <- read.csv("outcome-of-care-measures.csv")
	states <- levels(file[,7])
	file8 <- vector(mode = "character", length = 54) 
	file9 <- vector(mode = "character", length = 54) 
	outcomes <- list("heart attack", "heart failure", "pneumonia")
	if (Outcome %in% outcomes) {
		if (Outcome == "heart attack") for (i in 1:54){{ file2 <- subset(file, file[,11] != "Not Available", select = 	 c("Hospital.Name","State", "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"))
			file3 <- subset(file2, State == states[i])
			Names <- as.character(file3[,1]); State2 <- as.character(file3[,2]);Rate <- as.numeric(as.character(file3[,3]))
			file4 <- data.frame(Names, Rate, State2)
			n <- length(file4$Rate)
			file5 <- with(file4, file4[order(Rate, Names, State2),])
			file5$Rank <- c(1:n)
			file6 <- data.frame(Hospital.Names = file5$Names, Disease.Rate = file5$Rate, State3 = file5$State2,  Rank1 = file5$Rank)
			if (num == "best") {
				num1 = 1
				file7 <- file6[num1,]
				file8[i] <- as.character(file7$Hospital.Names)
				file9[i] <- as.character(file7$State3)
				}
			if (num == "worst") {
				num1 = n
				file7 <- file6[num1,]
				file8[i] <- as.character(file7$Hospital.Names)
				file9[i] <- as.character(file7$State3)
				}
			if (num >= 1 && num <= n) {
				file7 <- file6[num,]
				file8[i] <- as.character(file7$Hospital.Names)
				file9[i] <- as.character(file7$State3)
			}
			if (num != "best" && num != "worst" && num > n) {
				file8[i] <- "NA"
				file9[i] <- as.character(states[i])
			}
			}
			}
			if (Outcome == "heart failure") for (i in 1:54){{ file2 <- subset(file, file[,17] != "Not Available", select = 	 c("Hospital.Name","State", "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"))
			file3 <- subset(file2, State == states[i])
			Names <- as.character(file3[,1]); State2 <- as.character(file3[,2]);Rate <- as.numeric(as.character(file3[,3]))
			file4 <- data.frame(Names, Rate, State2)
			n <- length(file4$Rate)
			file5 <- with(file4, file4[order(Rate, Names, State2),])
			file5$Rank <- c(1:n)
			file6 <- data.frame(Hospital.Names = file5$Names, Disease.Rate = file5$Rate, State3 = file5$State2,  Rank1 = file5$Rank)
			if (num == "best") {
				num1 = 1
				file7 <- file6[num1,]
				file8[i] <- as.character(file7$Hospital.Names)
				file9[i] <- as.character(file7$State3)
				}
			if (num == "worst") {
				num1 = n
				file7 <- file6[num1,]
				file8[i] <- as.character(file7$Hospital.Names)
				file9[i] <- as.character(file7$State3)
				}
			if (num >= 1 && num <= n) {
				file7 <- file6[num,]
				file8[i] <- as.character(file7$Hospital.Names)
				file9[i] <- as.character(file7$State3)
			}
			if (num != "best" && num != "worst" && num > n) {
				file8[i] <- "NA"
				file9[i] <- as.character(states[i])
			}
			}
			}
			if (Outcome == "pneumonia") for (i in 1:54){{ file2 <- subset(file, file[,23] != "Not Available", select = c("Hospital.Name","State", "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"))
			file3 <- subset(file2, State == states[i])
			Names <- as.character(file3[,1]); State2 <- as.character(file3[,2]);Rate <- as.numeric(as.character(file3[,3]))
			file4 <- data.frame(Names, Rate, State2)
			n <- length(file4$Rate)
			file5 <- with(file4, file4[order(Rate, Names, State2),])
			file5$Rank <- c(1:n)
			file6 <- data.frame(Hospital.Names = file5$Names, Disease.Rate = file5$Rate, State3 = file5$State2,  Rank1 = file5$Rank)
			if (num == "best") {
				num1 = 1
				file7 <- file6[num1,]
				file8[i] <- as.character(file7$Hospital.Names)
				file9[i] <- as.character(file7$State3)
				}
			if (num == "worst") {
				num1 = n
				file7 <- file6[num1,]
				file8[i] <- as.character(file7$Hospital.Names)
				file9[i] <- as.character(file7$State3)
				}
			if (num >= 1 && num <= n) {
				file7 <- file6[num,]
				file8[i] <- as.character(file7$Hospital.Names)
				file9[i] <- as.character(file7$State3)
			}
			if (num != "best" && num != "worst" && num > n) {
				file8[i] <- "NA"
				file9[i] <- as.character(states[i])
			}
			}
			}
			}
			else { if (!(Outcome %in% outcomes)) {stop("invalid outcome")}
				if (!(State2 %in% states)) {stop("invalid state")}
				}
			file10 <- data.frame(hospital = file8, state = file9)
			print(file10)
			}
