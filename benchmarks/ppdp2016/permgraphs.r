library(ggplot2)
library(ggthemes)
library(plyr)
library(reshape2)
library(tikzDevice)
library(scales)

gm = function(x, na.rm=TRUE, zero.propagate = FALSE){
  if(any(x < 0, na.rm = TRUE)){
    return(NaN)
  }
  if(zero.propagate){
    if(any(x == 0, na.rm = TRUE)){
      return(0)
    }
    exp(mean(log(x), na.rm = na.rm))
  } else {
    exp(sum(log(x[x > 0]), na.rm=na.rm) / length(x))
  }
}

## http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

## |     n\m | system + query   |      16 |      32 |        64 |
## |---------+------------------+---------+---------+-----------|
## |   10000 | postgres plain   |   0.574 |   1.302 |     3.725 |
## |         | llinks   plain   |   0.783 |   1.559 |     3.915 |
## |         | llinks   lineage |   1.972 |  12.278 |   121.013 |
## |         | perm     plain   |   0.628 |   1.612 |     4.242 |
## |         | perm     lineage |   1.055 |   1.922 |     5.698 |
## |---------+------------------+---------+---------+-----------|
## |  100000 | postgres plain   |   2.699 |   6.063 |    21.572 |
## |         | llinks   plain   |   4.854 |  12.446 |    22.862 |
## |         | llinks   lineage |   6.428 |  29.960 |   191.346 |
## |         | perm     plain   |   5.412 |  13.746 |    23.660 |
## |         | perm     lineage |   5.709 |  13.840 |    24.323 |
## |---------+------------------+---------+---------+-----------|
## | 1000000 | postgres plain   |  55.014 |  84.679 |   339.230 |
## |         | llinks   plain   |  22.960 |  84.331 |   434.266 |
## |         | llinks   lineage | 101.151 | 475.809 | 33744.603 |
## |         | perm     plain   |  20.136 | 176.859 |   506.047 |
## |         | perm     lineage | 203.336 | 379.538 |   817.538 |

## |---------+----------------+---------+---------+---------|
## |         |                |      16 |      32 |      64 |
## |---------+----------------+---------+---------+---------|
## |   10000 | links where    |   0.745 |   1.651 |   4.352 |
## |  100000 | links where    |   6.605 |  14.019 |  24.155 |
## | 1000000 | links where    |  47.546 | 153.224 | 378.863 |
## |---------+----------------+---------+---------+---------|
## |   10000 | links plain    |   0.716 |   1.599 |   4.357 |
## |  100000 | links plain    |   6.871 |  13.057 |  21.942 |
## | 1000000 | links plain    |  37.430 | 104.578 | 345.413 |
## |---------+----------------+---------+---------+---------|
## |   10000 | perm where     |   1.241 |   2.666 |   6.699 |
## |  100000 | perm where     |   8.691 |  19.827 |  36.011 |
## | 1000000 | perm where     | 189.833 | 321.083 | 970.044 |
## |---------+----------------+---------+---------+---------|
## |   10000 | perm plain     |   0.980 |   2.062 |   5.265 |
## |  100000 | perm plain     |   5.159 |  14.351 |  23.863 |
## | 1000000 | perm plain     |  44.809 | 112.557 | 412.503 |
## |---------+----------------+---------+---------+---------|
## |   10000 | postgres plain |   0.621 |   1.511 |   4.264 |
## |  100000 | postgres plain |   6.157 |  12.447 |  22.869 |
## | 1000000 | postgres plain | 124.859 | 190.438 | 366.352 |
## |---------+----------------+---------+---------+---------|

## Postgres filtered numbers
## 0.512, 1.351, 3.760,
## 6.348, 11.849, 21.828,
## 120.101, 185.063, 356.294

t = c(0.745 ,   1.651,   4.352,
      6.605 ,  14.019 ,  24.155,
      47.546 , 153.224 , 378.863,
      0.716 ,   1.599 ,   4.357,
      6.871 ,  13.057 ,  21.942,
      37.430 , 104.578 , 345.413,
      1.241 ,   2.666 ,   6.699,
      8.691 ,  19.827 ,  36.011,
      189.833 , 321.083 , 970.044,
      0.980 ,   2.062 ,   5.265,
      5.159 ,  14.351 ,  23.863,
      44.809 , 112.557 , 412.503,
      0.512, 1.351, 3.760,     
      6.348, 11.849, 21.828,   
      120.101, 185.063, 356.294)

n = c(10000, 10000, 10000,
      100000, 100000, 100000,
      1000000, 1000000, 1000000,
      10000, 10000, 10000,      
      100000, 100000, 100000,   
      1000000, 1000000, 1000000,
      10000, 10000, 10000,      
      100000, 100000, 100000,   
      1000000, 1000000, 1000000,
      10000, 10000, 10000,      
      100000, 100000, 100000,   
      1000000, 1000000, 1000000,
      10000, 10000, 10000,      
      100000, 100000, 100000,   
      1000000, 1000000, 1000000)

m = c(16, 32, 64,
      16, 32, 64,
      16, 32, 64,
      16, 32, 64,
      16, 32, 64,
      16, 32, 64,
      16, 32, 64,
      16, 32, 64,
      16, 32, 64,
      16, 32, 64,
      16, 32, 64,
      16, 32, 64,
      16, 32, 64,
      16, 32, 64,
      16, 32, 64)
      
q = c("wlinks where", "wlinks where", "wlinks where",
      "wlinks where", "wlinks where", "wlinks where",
      "wlinks where", "wlinks where", "wlinks where",
      "wlinks plain", "wlinks plain", "wlinks plain",
      "wlinks plain", "wlinks plain", "wlinks plain",
      "wlinks plain", "wlinks plain", "wlinks plain",
      "perm where", "perm where", "perm where",
      "perm where", "perm where", "perm where",
      "perm where", "perm where", "perm where",
      "perm plain", "perm plain", "perm plain",
      "perm plain", "perm plain", "perm plain",
      "perm plain", "perm plain", "perm plain",
      "postgres plain", "postgres plain", "postgres plain",
      "postgres plain", "postgres plain", "postgres plain",
      "postgres plain", "postgres plain", "postgres plain")
      

## t = c(0.574, 0.783, 1.972, 0.628, 1.055,
##       1.302, 1.559, 12.278, 1.612, 1.922,
##       3.725, 3.915, 121.013, 4.242, 5.698,
##       2.699, 4.854, 6.428, 5.412, 5.709,
##       6.063, 12.446, 29.960, 13.746, 13.840,
##       21.572, 22.862, 191.346, 23.660, 24.323,
##       55.014, 22.960, 101.151, 20.136, 203.336,
##       84.679, 84.331, 475.809, 176.859, 379.538,
##       339.230, 434.266, NA, #33744.603,
##       506.047, 817.538)
      
df = data.frame(n, m, q, t)
## df = df[with(df, order(q)), ]
## mydf$task <- factor(mydf$task, levels = c("up", "down", "left", "right", "front", "back"))
## x$name <- factor(x$name, levels = x$name[order(x$val)])
df$q = factor(df$q, levels=c("wlinks where", "wlinks plain", "perm where", "perm plain", "postgres plain"))

str(df)

## tikz("~/Documents/language-integrated-provenance/scp2016/where_group_m.tex", width=4.5, height=3.5)
## ggplot(data=df, aes(factor(n), t,
##                     group=q,
##                     fill=q)) +
##     scale_y_log10("Time (s)") +
##     scale_x_discrete("rows")+
## facet_wrap(~m,nrow=3)+
## geom_bar(position="dodge",stat="identity")+
##     theme_tufte() +
##     theme(plot.margin=margin(t=-5, r=0, b=5, l=0), panel.spacing=unit(5, "mm")) +
##     ## theme(plot.background= element_rect()) +
##     theme(legend.position="none") +
##     theme(legend.margin=margin(t=-8, r=0, b=0, l=0)) +
##     theme(legend.text=element_text(size=7))+
##     ## theme(legend.background = element_rect()) +
##     theme(axis.title = element_text(size=7)) +
##     theme(axis.text.x = element_text(size=7)) +
##     theme(axis.text.y = element_text(size=5)) +
##     scale_fill_manual(values=cbbPalette)
## dev.off()

# levels=c("wlinks where", "wlinks plain", "perm where", "perm plain", "postgres plain")

tikz("~/Documents/language-integrated-provenance/scp2016/where_group_n.tex", width=4.5, height=3.5)
ggplot(data=df, aes(factor(m), t,
                    group=q,
                    fill=q)) +
    scale_y_log10("Time (s)") +
    scale_x_discrete("width")+
facet_wrap(~n,nrow=3)+
geom_bar(position="dodge",stat="identity")+
guides(fill=guide_legend(title=""))+
    theme_tufte() +
    theme(plot.margin=margin(t=-5, r=0, b=5, l=0), panel.spacing=unit(5, "mm")) +
    ## theme(plot.background= element_rect()) +
    theme(legend.position="bottom") +
    theme(legend.margin=margin(t=-8, r=0, b=0, l=0)) +
    theme(legend.text=element_text(size=7))+
## theme(legend.background = element_rect()) +
    theme(axis.title = element_text(size=7)) +
    theme(axis.text.x = element_text(size=7)) +
    theme(axis.text.y = element_text(size=5)) +
    scale_fill_manual(values=cbbPalette)
dev.off()
