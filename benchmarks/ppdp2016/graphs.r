library(ggplot2)
library(ggthemes)
library(plyr)
library(reshape2)
library(tikzDevice)

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

df <- read.csv2("lindata.csv")
str(df)
           
tikz("~/Documents/language-integrated-provenance/scp2016/lingraph.tex", width=4.5, height=3.5)
ggplot(df, aes(factor(N), medianms, group=prov, color=prov)) +
    geom_line() +
    geom_point(size = 1, aes(shape=prov)) +
    scale_y_log10("Time (ms), median over 5 runs.") +
    scale_x_discrete("Number of departments.") +
    facet_wrap(~query, scales="free") +
    theme_tufte() +
    theme(plot.margin=margin(t=-5, r=0, b=5, l=0), panel.spacing=unit(5, "mm")) +
    ## theme(plot.background= element_rect()) +
    theme(legend.position="bottom") +
    theme(legend.margin=margin(t=-8, r=0, b=0, l=0)) +
    ## theme(legend.background = element_rect()) +
    theme(axis.title = element_text(size=7)) +
    theme(axis.text.x = element_text(angle=90, vjust=0.5, size=5)) +
    theme(axis.text.y = element_text(size=5)) +
    scale_color_manual(values=cbbPalette)
dev.off()

querymean = function(q) {
    ql <- df[df$prov == "lineage" & df$query == q,]
    qn <- df[df$prov == "nolineage" & df$query == q,]
    gm(ql$medianms / qn$medianms)
}

options(digits=3)

querymean("AQ6")
querymean("Q3")
querymean("Q4")
querymean("Q5")
querymean("Q6N")
querymean("Q7")
querymean("QC4")
querymean("QF3")
gm(df[df$prov == "lineage" & df$query == "QF4" & df$N != 4,]$medianms / df[df$prov == "nolineage" & df$query == "QF4" & df$N != 4,]$medianms) #QF4 has a 0

foo = function (q, n) {
    l <- df[df$prov == "lineage" & df$query == q & df$N == n,]
    n <- df[df$prov == "nolineage" & df$query == q & df$N == n,]
    print("lin")
    print(l$medianms)
    print("nolin")
    print(n$medianms)
}

foo("AQ6",1024)
foo("Q3",1024)
foo("Q4",1024)
foo("Q5",1024)
foo("Q6N",1024)
foo("Q7",128)
foo("QC4",16)
foo("QF3",512)
foo("QF4",1024)
