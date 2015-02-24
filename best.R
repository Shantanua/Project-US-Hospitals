best <- function (State2 = " ", Outcome = " ") {
	file <- read.csv("outcome-of-care-measures.csv")
	states <- levels(file[,7])
	outcomes <- list("heart attack", "heart failure", "pneumonia")
	if (State2 %in% states && Outcome %in% outcomes) {
		if (Outcome == "heart attack") { file2 <- subset(file, file[,11] != "Not Available", select = 	 c("Hospital.Name","State", "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"))
			file3 <- subset(file2, State == State2)
			Names <- as.character(file3[,1]); State <- as.character(file3[,2]); Rate <- as.numeric(as.character(file3[,3]))
			file4 <- data.frame(Names, State, Rate)
			rmin <- min((file4[,3]))
			file5 <- subset(file4, file4[,3] == rmin)		
			file6 <- as.character(file5$Names) 
			file7 <- sort(file6)
			file8 <- vector(mode = "character")
			file8 <- as.character(file7[[1]])
			#print(as.character(file8))
			}
			
			if (Outcome == "heart failure") { file2 <- subset(file, file[,17] != "Not Available", select = c("Hospital.Name","State", "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"))
			file3 <- subset(file2, State == State2)
			Names <- as.character(file3[,1]); State <- as.character(file3[,2]); Rate <- as.numeric(as.character(file3[,3]))
			file4 <- data.frame(Names, State, Rate)
			rmin <- min((file4[,3]))
			file5 <- subset(file4, file4[,3] == rmin)		
			file6 <- as.character(file5$Names) 
			file7 <- sort(file6)
			file8 <- vector(mode = "character")
			file8 <- file7[[1]]
			#print(as.character(file8))
			}
			
			if (Outcome == "pneumonia") { file2 <- subset(file, file[,23] != "Not Available", select = c("Hospital.Name","State", "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"))
			file3 <- subset(file2, State == State2)
			Names <- as.character(file3[,1]); State <- as.character(file3[,2]); Rate <- as.numeric(as.character(file3[,3]))
			file4 <- data.frame(Names, State, Rate)
			rmin <- min((file4[,3]))
			file5 <- subset(file4, file4[,3] == rmin)		
			file6 <- as.character(file5$Names) 
			file7 <- sort(file6)
			file8 <- vector(mode = "character")
			file8 <- file7[[1]]
			#print(file8)
			}
			print(file8)
				}
				
	else { if (!(Outcome %in% outcomes)) {stop("invalid outcome")}
				if (!(State2 %in% states)) {stop("invalid state")}
				}

	}