library(ggplot2)
data = read.table("/Users/vishalchopra/Documents/School/Mustang Capital/DatesYields.txt", sep="|")
p <- qplot(data[1], data[2])