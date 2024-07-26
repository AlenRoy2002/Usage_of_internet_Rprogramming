cat("\n                *****R Microproject*****\n")
cat("\n               ==============================\n")
cat("                 usage of internet \n")
cat("               ==============================\n\n")

cat("Summary of the dataset:\n\n")

k=read.csv(file.choose())
cat("1Number of people are using internet in world wide")
x<-k$Year
y<-k$Individuals.using.the.Internet....of.population.
relation=lm(y~x)
a=data.frame(x=20)
result=predict(relation,a)
cat("Conclusion:")
cat("Number of people using internet in world wide",result,"\n")
print(result)

#2Plotting a Graph of the usage of internet by  individuals in the countries
plot(k$Year,k$Individuals.using.the.Internet....of.population.,
     xlab="Year's",
     ylab="Usage",
     col="red",
     main="Usage of internet",
     type="l"
)

cat("\n\t\t\t\tQuestion Number 3\n\n")
print("  *) Find The Average usage of internet")
mpr=k$Individuals.using.the.Internet....of.population.
c=mean(mpr)
print(c)

cat("\t\t\t\tQuestion Number 4\n\n")
print("  *)Find The Cor-Relation Between year And usage of internet")
print("Ans)The Relationship Obtained Is")
co1=c(k$Year)
co2=c(k$Individuals.using.the.Internet....of.population.)
result=cor(co1,co2,method="pearson")
print(result)
if(result>0){
  print("Positive Cor-relationBetween year And usage of internet")
  print("it Shows That When year moves forward usage of internet will increasing")
}else if(result==0){
  print("There is No Cor-relation Between year and usage of internet")
  print("No Cor-relation Occurs")
}else{
  print("Negative Cor-relation Between year and internet")
  print("it Shows That When year moves forward usage of internet will decresing ")
}

cat("\t\t\t\tQuestion Number 5\n\n")
print("  *From The Above Graph List print the percentage  of internet using by year")
print("Ans The Above Are The Detail's Of percentage of usage of internet by year")
x=k$Year
y=k$Individuals.using.the.Internet....of.population.
plot(x,y,col="red")

cat("\t\t\t\tQuestion Number 6\n\n")
print("  *Check Whether The internet usage Is increase or not by upcoming years")
int2=mean(k$Individuals.using.the.Internet....of.population.)

print(paste("mean of internet usage  Is:",int2))
test=t.test(k$Individuals.using.the.Internet....of.population.,alternative = "less",conf.level = 0.99)
print(test)
if(test$p.value<0.05){
  print("AnsSince the p-value is < 0.05 we reject Null hypothesis,thus we Conclude That on upcoming years internet usage will decrease");
}else{
  print("Since the p-value is > 0.05 we accept Null hypothesis,Thus We Conclude That  upcoming years internet usage will increase")
}

