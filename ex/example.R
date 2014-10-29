library(datasets);library(car);data(mtcars)
mtcars$am<-factor(mtcars$am)

# using the function created in the course: data analysis and statistical inference
names(mtcars)
ol=c("cyl","disp","hp","drat","wt","qsec","vs","am","gear","carb")
model_sel("mpg", option_list=ol, must_list=c("am"), data=mtcars)