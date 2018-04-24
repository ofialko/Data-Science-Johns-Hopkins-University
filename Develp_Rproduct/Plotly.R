library(plotly)
data("mtcars")
mtcars$cyl <- as.factor(mtcars$cyl)
plot_ly(mtcars,x=~wt,y=~mpg,color = ~cyl,size=~hp,mode='markers')

temp <- rnorm(100,mean = 30,sd=5)
pressue <- rnorm(100)
dtime <- 1:100

plot_ly(x=temp,y=pressue,z=dtime,type = 'scatter3d',mode='markers',color=temp)

data("airmiles")
plot_ly(x=time(airmiles),y=airmiles,mode='lines')

library(tidyr)
library(dplyr)
data("EuStockMarkets")
stocks <- as.data.frame(EuStockMarkets) %>% gather(index,price) %>%
    mutate(time=rep(time(EuStockMarkets),4))

plot_ly(stocks,x=~time,y=~price,color=~index)

plot_ly(x=precip,type='histogram')
plot_ly(iris,y=~Petal.Length,color=~Species,type='box')

terrain1 <- matrix(rnorm(100*100),nrow = 100,ncol = 100)
plot_ly(z=terrain1,type='heatmap')
terrain1 <- matrix(sort(rnorm(100*100)),nrow = 100,ncol = 100)
plot_ly(z=terrain1,type='surface')

# 

state_pop <- data.frame(State=state.abb, Pop = as.vector(state.x77[,1]))
state_pop$hover <- with(state_pop,paste(State,'<br>','Population:',Pop))
borders <- list(color=toRGB('red'))
map_options <- list(scope='usa',
                    projection = list(type='albers usa'),
                    showlakes = TRUE,
                    lakecolor = toRGB('white'))

plot_ly(state_pop,z=~Pop,text=~hover,locations=~State,type='choropleth',
        locationmode = 'USA-states',color=~Pop,colors='Blues',marker=list(line=borders)) %>%
    layout(title='US Population in 1975',geo=map_options)



d <- diamonds[sample(nrow(diamonds),1000),]
p <- ggplot(data=d,aes(x=carat,y=price)) + 
    geom_point(aes(text=paste('Clarity:',clarity)),size=4) + 
    geom_smooth(aes(colour=cut,fill=cut))+facet_wrap(~cut)
gg <- ggplotly(p)
gg

plotly_POST(gg)

