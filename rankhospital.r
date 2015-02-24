rankhospital <- function (State2 = " ", Outcome = " ", num = " ") {
	file <- read.csv("outcome-of-care-measures.csv")
	states <- levels(file[,7])
	outcomes <- list("heart attack", "heart failure", "pneumonia")
	if (State2 %in% states && Outcome %in% outcomes) {
		if (Outcome == "heart attack") { file2 <- subset(file, file[,11] != "Not Available", select = c("Hospital.Name","State", "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"))
			file3 <- subset(file2, State == State2)
			Names <- as.character(file3[,1]); Rate <- as.numeric(as.character(file3[,3]))
			file4 <- data.frame(Names, Rate)
			n <- length(file4$Rate)
			file5 <- with(file4, file4[order(Rate, Names),])
			file5$Rank <- c(1:n)
			file6 <- data.frame(Hospital.Names = file5$Names, Disease.Rate = file5$Rate, Rank1 = file5$Rank)
			if (num == "best") {
				num1 = 1
				file7 <- file6[num1,]
				file8 <- vector(mode = "character", length = 1)
				file8 <- as.character(file7$Hospital.Names)
				#print(file8)
				}
			if (num == "worst") {
				num1 = n
				file7 <- file6[num1,]
				file8 <- vector(mode = "character", length = 1)
				file8 <- as.character(file7$Hospital.Names)
				#print(file8)
				}
			if (num >= 1 && num <= n) {
				file7 <- file6[num,]
				file8 <- vector(mode = "character", length = 1)
				file8 <- as.character(file7$Hospital.Names)
				#print(file8)
			}
			if (num != "best" && num != "worst" && num > n) {
				file8 <- vector(mode = "character", length = 1)
				file8 <- "NA"
				#print(file8)
			}
			}
			
		if (Outcome == "heart failure") { file2 <- subset(file, file[,17] != "Not Available", select = 	 c("Hospital.Name","State", "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"))
			file3 <- subset(file2, State == State2)
			Names <- as.character(file3[,1]); Rate <- as.numeric(as.character(file3[,3]))
			file4 <- data.frame(Names, Rate)
			n <- length(file4$Rate)
			file5 <- with(file4, file4[order(Rate, Names),])
			file5$Rank <- c(1:n)
			file6 <- data.frame(Hospital.Names = file5$Names, Disease.Rate = file5$Rate, Rank1 = file5$Rank)
			if (num == "best") {
				num1 = 1
				file7 <- file6[num1,]
				file8 <- vector(mode = "character", length = 1)
				file8 <- as.character(file7$Hospital.Names)
				#print(file8)
				}
			if (num == "worst") {
				num1 = n
				file7 <- file6[num1,]
				file8 <- vector(mode = "character", length = 1)
				file8 <- as.character(file7$Hospital.Names)
				#print(file8)
				}
			if (num >= 1 && num <= n) {
				file7 <- file6[num,]
				file8 <- vector(mode = "character", length = 1)
				file8 <- as.character(file7$Hospital.Names)
				#print(file8)
			}
			if (num != "best" && num != "worst" && num > n) {
				file8 <- vector(mode = "character", length = 1)
				file8 <- "NA"
				#print(file8)
			}
			}
			
		if (Outcome == "pneumonia") { file2 <- subset(file, file[,23] != "Not Available", select = 	 c("Hospital.Name","State", "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"))
			file3 <- subset(file2, State == State2)
			Names <- as.character(file3[,1]); Rate <- as.numeric(as.character(file3[,3]))
			file4 <- data.frame(Names, Rate)
			n <- length(file4$Rate)
			file5 <- with(file4, file4[order(Rate, Names),])
			file5$Rank <- c(1:n)
			file6 <- data.frame(Hospital.Names = file5$Names, Disease.Rate = file5$Rate, Rank1 = file5$Rank)
			if (num == "best") {
				num1 = 1
				file7 <- file6[num1,]
				file8 <- vector(mode = "character", length = 1)
				file8 <- as.character(file7$Hospital.Names)
				#print(file8)
				}
			if (num == "worst") {
				num1 = n
				file7 <- file6[num1,]
				file8 <- vector(mode = "character", length = 1)
				file8 <- as.character(file7$Hospital.Names)
				#print(file8)
				}
			if (num >= 1 && num <= n) {
				file7 <- file6[num,]
				file8 <- vector(mode = "character", length = 1)
				file8 <- as.character(file7$Hospital.Names)
				#print(file8)
			}
			if (num != "best" && num != "worst" && num > n) {
				file8 <- vector(mode = "character", length = 1)
				file8 <- "NA"
				#print(file8)
			}
			}
			
			}
	else { if (!(Outcome %in% outcomes)) {stop("invalid outcome")}
				if (!(State2 %in% states)) {stop("invalid state")}
				}
print(file8)
	}