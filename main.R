## what is the relationship between mean covered charges (Average.Covered.Charges) 
## and mean total payments (Average.Total.Payments) in New York? 

library(dplyr) 
setwd("~/Desktop/ReproducibleResearch_PlottingPractice") 
data <- read.csv("payments.csv", header = TRUE, stringsAsFactors = FALSE) 

## str(data)
dataset <- tbl_df(data) 

ny <- filter(dataset, Provider.City == "NEW YORK") 
pdf("plot1.pdf") 

boxplot(ny$Average.Covered.Charges, ny$Average.Total.Payments, 
        data = ny, ylim = c(2500, 65000),
        names = c("Covered charges", "Total payments"), 
        ylab="USD", main="Covered charges and Total payments in New York") 
dev.off()

## Make a plot (possibly multi-panel) that answers the question:
## how does the relationship between mean covered charges (Average.Covered.Charges) 
## and mean total payments (Average.Total.Payments) vary by medical condition (DRG.Definition) 
## and the state in which care was received (Provider.State)? 

pdf("plot2.pdf") 
par(mfrow=c(2,3)) 

by_condition_state <- group_by(dataset, DRG.Definition, Provider.State) 
summary <- summarise(by_condition_state, Mean.Charges = mean(Average.Covered.Charges), Mean.Payments = mean(Average.Total.Payments)) 

conditions <- unique(dataset$DRG.Definition) 
states <- unique(dataset$Provider.State) 
plot_colors <- c("darkblue","red") 

for (disease in conditions) {
    state_charges <- c() 
    state_payments <- c() 
        for (state in states) { 
            mean_data <- filter(summary, DRG.Definition == disease & Provider.State == state) 
            state_charges <- append(state_charges, mean_data$Mean.Charges) 
            state_payments <- append(state_payments, mean_data$Mean.Payments) }
            m <- matrix(c(state_charges, state_payments), ncol = 6, nrow = 2, byrow = TRUE) 
            rownames(m) <- c("Mean covered charges" , "Mean total payments") 
            colnames(m) <- states 
            names(m) <- paste(states, sep = " ")
            
            barplot(m, main = disease, col = plot_colors, beside = F, las = 2, ylab = "USD") 
            legend(x = "topright", inset = 0, cex = 0.7, 
                   legend = c("Mean covered charges" , "Mean total payments"), 
                   col = plot_colors, lwd = 0.8, horiz = F, pch = 15) 
        } 

title(main = "Mean covered charges and mean total payments by medical condition and the state" , outer = T, x = "top", cex=1) 
dev.off()