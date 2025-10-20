myfunction <- function(a, b, c){

neighbours <- c
neighbours_weight <- nb2listw(neighbours, style = "W")

moran_global <- moran.test(b, neighbours_weight)

moran_local <- localmoran(x = b, neighbours_weight)
moran_map <- a
moran_map@data <- cbind(a@data, moran_local)
moran_continuous <- tm_shape(moran_map) + tm_fill(col = "Ii", style = "quantile", title = "local moran statistic")

quadrant <- vector(mode="numeric",length=nrow(moran_local))
m.variable <- b - mean(b)
m.local <- moran_local[,1] - mean(moran_local[,1])
signif <- 0.1
quadrant[m.variable <0 & m.local>0] <- 1      
quadrant[m.variable <0 & m.local<0] <- 2
quadrant[m.variable >0 & m.local<0] <- 3
quadrant[m.variable >0 & m.local>0] <- 4 
quadrant[moran_local[,5]>signif] <- 0  
brks <- c(0,1,2,3,4)
colors <- c("white","blue",rgb(0,0,1,alpha=0.4),rgb(1,0,0,alpha=0.4),"red")
moran_lisa <- plot(a,border="lightgray",col=colors[findInterval(quadrant,brks,all.inside=FALSE)])
box()
legend("bottomleft",legend=c("insignificant","low-low","low-high","high-low","high-high"),
       fill=colors,bty="n")


moran_final <- grid.newpage()
pushViewport(viewport(layout=grid.layout(1,3)))

print(moran_global, vp=viewport(layout.pos.col = 1, layout.pos.row =1))
print(moran_continuous, vp=viewport(layout.pos.col = 2, layout.pos.row =1))
print(moran_lisa, vp=viewport(layout.pos.col = 3, layout.pos.row =1))

return(moran_final)
}
