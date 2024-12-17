init.par <- c(xb=25,yb=-50,xc=50,yc=-40,xd=60,yd=-20)
fun <- function(x){
  term <- function(xpos,xneg,ypos,yneg,target){
    (sqrt((xpos-xneg)^2+(ypos-yneg)^2)-target)^2
  }
  with(as.list(x),{
    term(xb,0,-44.5,yb,25.75)+
      term(xc,xb,yc,yb,25.5)+
      term(xd,xc,yd,yc,26)+
      term(xd,43,0,yd,13.25)
  })
}
result <- optimx::optimx(init.par,fun)
library(data.table)
result.dt <- data.table(
  result,
  xa=0, ya=-44.5, xe=43, ye=0, xf=0, yf=0,
  algorithm=rownames(result))
result.long <- melt(
  result.dt,
  measure.vars=measure(coord, point, pattern="([xy])([a-f])"),
  value.name="pos")
result.wide <- dcast(
  result.long,
  algorithm + point ~ coord,
  value.var="pos")

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
off <- 0.5
gg <- ggplot()+
  ggtitle(paste0(
    "Hocking fabrication, Dec 2024, Sum of angles shown: ",
    sum(angle.show$round.degrees),
    "°"))+
  geom_label(aes(
    x-off,y-off,label=sprintf('%s\n%.1f°\nx= %.2f"\ny= %.2f"',point,degrees,x,y)),
    hjust=1,
    vjust=1,
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
    limits=c(-10,NA))+
  scale_y_continuous(
    'y coordinate (inches = pouces = ")',
    limits=c(-60,NA))
png("fire-angles.png", width=7, height=7, units="in", res=200)
print(gg)
dev.off()

## manual
p <- function(point, x, y)data.table(point,x,y)
result.wide <- data.table(algorithm="Manual", rbind(
  p("f",0,0),
  p("a",0,-44.5),
  p("b",25.4,-48.73),
  p("c",48.11,-37.13),
  p("d",57.35,-18.565),
  p("e",48.11,0)))
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
only <- function(DT)DT[algorithm=="Manual"]
angle.show <- only(angle.dt)[, round.degrees := round(degrees,1)][]
dist.show <- only(dist.dt)
gg <- ggplot()+
  ggtitle(paste0(
    "Hocking fabrication, Dec 2024, Sum of angles shown: ",
    sum(angle.show$round.degrees),
    "°"))+
  geom_label(aes(
    x,y,label=sprintf(
      '%s\n%s°\nx= %.2f"\ny= %.2f"',
      point,as.character(round.degrees),x,y),
    hjust=ifelse(x==0, 1, 0),
    vjust=ifelse(y < -10, 1, 0)),
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
    limits=c(-10,70))+
  scale_y_continuous(
    'y coordinate (inches = pouces = ")',
    limits=c(-60,10))
png("fire-angles-manual.png", width=7, height=7, units="in", res=200)
print(gg)
dev.off()
