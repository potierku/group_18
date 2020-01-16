#lab 1 

library(gapminder)
library(ggplot2)
gapminder
p <- ggplot(data = gapminder,
            mapping = aes(x = gdpPercap, y = lifeExp))
p + geom_point()

a <- ggplot(data = mpg,
            mapping = aes(x = displ, y = hwy,color=class))
a + geom_point()
#1
#the plot does show an intuitive relationship in that a vehicle with a larger engine gets less mpg. This may not be causal as we do not know if other characteristics of the vehicle are similar. 

b <- ggplot(data = mpg,
            mapping = aes(x = class, y = drv))
b + geom_point()
#the plot isn't very easy to read and it is not clear how many points could be on top of one another.

#1b
#The class of the vehicle and engine displacement seem to be related. 
p + geom_point()
p + geom_smooth()
?geom_smooth
p + geom_point() + geom_smooth() + geom_smooth(method = ...) + geom_smooth(method = ...)
p + geom_point() + geom_smooth() + geom_smooth(method = ...) + geom_smooth(method = ..., color = "red")
p + geom_point() + geom_smooth(method = "lm") + scale_x_log10()

#What does scale_x_log10 do? explain in 2-3 sentences
