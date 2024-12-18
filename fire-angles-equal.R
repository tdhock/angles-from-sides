library(data.table)
p <- function(point, x, y)data.table(point,x,y)
initial <- data.table(algorithm="initial", rbind(
  p("A",0,0),
  p("B",0,-44.5),
  p("C",27,-54),
  p("D",50,-40.5),
  p("E",55,-12.25),
  p("F",45.125,0)))
initial.long <- melt(initial, measure.vars=c("x","y"))
(initial.vec <- with(initial.long, structure(value, names=paste0(variable,point))))
fun <- function(x){
  h <- function(x1,x2,y1,y2)sqrt((x1-x2)^2+(y1-y2)^2)
  target.degrees <- 130
  cost <- function(A,B,C){
    radians <- acos((A^2+B^2-C^2)/(2*A*B))
    degrees <- 360*radians/(2*pi)
    (target.degrees-degrees)^2
  }
  with(as.list(x),{
    dEF <- h(xE,xF,yE,0)
    dDF <- h(xD,xF,yD,0)
    dCE <- h(xE,xC,yE,yC)
    dCD <- h(xD,xC,yD,yC)
    dBD <- h(xD,0,yD,yB)
    cost(dEF,28.5,dDF)+
      cost(28.5,dCD,dCE)+
      cost(28.5,dCD,dBD)
  })
}
result <- optimx::opm(initial.vec,fun)
result.dt <- data.table(algorithm=rownames(result),result)
result.wide <- melt(
  result.dt,
  measure.vars=measure(value.name, point, pattern="([xy])([A-F])")
)[order(algorithm,point)]
angle.dt <- result.wide[, {
  more <- .SD[c(1:.N,1:2)]
  i <- seq(2, nrow(more)-1)
  before <- more[i-1,.(x,y)]
  here <- more[i,.(x,y)]
  after <- more[i+1,.(x,y)]
  h <- function(a,b)sqrt(rowSums((a-b)^2))
  A=h(before,here)
  B=h(after,here)
  C=h(before,after)
  radians <- acos((A^2+B^2-C^2)/(2*A*B))
  data.table(more[i], degrees=360*radians/(2*pi))
}, by=algorithm]
dist.dt <- result.wide[, {
  more <- .SD[c(1:.N,1)]
  i <- seq(2, nrow(more))
  before <- more[i-1,.(x,y)]
  here <- more[i,.(x,y)]
  h <- function(a,b)sqrt(rowSums((a-b)^2))
  data.table((before+here)/2, dist=h(before,here))
}, by=algorithm]
library(ggplot2)
ggplot()+
  geom_polygon(aes(
    x,y),
    fill=NA,
    color="black",
    data=angle.dt)+
  geom_label(aes(
    x,y,label=paste0(point,"\n",as.integer(degrees),"°")),
    data=angle.dt)+
  geom_label(aes(
    x,y,label=sprintf('%.2f"',dist)),
    data=dist.dt)+
  facet_grid(. ~ algorithm, labeller=label_both)+
  coord_equal()

only <- function(DT)DT[algorithm=="BFGS"]
angle.show <- only(angle.dt)[, round.degrees := round(degrees,1)][]
dist.show <- only(dist.dt)
gg <- ggplot()+
  ggtitle(paste0(
    "Hocking fabrication, Dec 2024, Sum of angles shown: ",
    sum(angle.show$round.degrees),
    "°"))+
  geom_label(aes(
    x,y,
    hjust=ifelse(x==0, 1, 0),
    vjust=ifelse(y < -10, 1, 0),
    label=sprintf(
      '%s\n%s°\nx= %.2f"\ny= %.2f"',point,as.character(round.degrees),x,y)),
    data=angle.show)+
  geom_polygon(aes(
    x,y),
    fill=NA,
    color="black",
    data=angle.show)+
  geom_point(aes(
    x,y),
    data=angle.show)+
  geom_label(aes(
    x,y,label=sprintf('%.2f"',dist)),
    data=dist.show)+
  coord_equal()+
  scale_x_continuous(
    'x coordinate (inches = pouces = ")',
    limits=c(-10,70), breaks=seq(-100,100,10))+
  scale_y_continuous(
    'y coordinate (inches = pouces = ")',
    limits=c(-70,10), breaks=seq(-100,100,10))
png("fire-angles-equal.png", width=7, height=7, units="in", res=200)
print(gg)
dev.off()
