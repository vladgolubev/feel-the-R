# Load libraries
library(class)

# Set working directory to source file location
this.dir <- dirname(parent.frame(2)$ofile)
setwd(this.dir)

# Read CSV file
students <- read.csv(file="pzs-students14.csv", head=TRUE, sep=",")

# Draw a plot
plot(x = students$certificate,
     y = students$eit,
     main = "Відношення між балом атестату та балом ЗНО",
     xlab = "Бал атестату",
     ylab = "Бал ЗНО")

# Draw a simplified line displaying the correlation
lines(stats::lowess(x = students$certificate[order(students$certificate)],
                    y = students$eit[order(students$eit)]))

# Find students whose certificate scores are below the average regarding their EIT scores
eit_certificate_ratio <- (students$eit / students$certificate)
students.high_eit_ratio <- (students[eit_certificate_ratio > mean(eit_certificate_ratio), ])
# Sort them in descending order by ratio
students.high_eit_ratio <- students.high_eit_ratio[with(students.high_eit_ratio, order(-(students.high_eit_ratio$eit / students.high_eit_ratio$certificate))), ]
