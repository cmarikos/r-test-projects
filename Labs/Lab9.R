install.packages("ggplot2")
install.packages("corrplot")
library("ggplot2")
library("corrplot")
library("dplyr")

x <- rnorm(n = 50, mean = 30, sd = 2)
y <- x
test <- data.frame(x = x, y = y) #data frame format for ggplot

plot(x, y)
ggplot(test, aes(x=x, y = y))+ geom_point()

cor(x,y) #for the values
cor(test$x, test$y) #for the data frame version

y1 <- rnorm(n = 50, mean = 48, sd = 2)
y2 <- x-3
y3 <- jitter(x)
y4 <- x * -1

cor(x,y1)
cor(x,y2)
cor(x,y3)
cor(x,y4)

all <- data.frame(x, y, y1, y2, y3, y4)

M <- cor(all)

corrplot(M, method = "circle")
corrplot(M, method = "color")
corrplot(M, method = "number")

?cor()

#gold correlation


#df$A <- gsub(">", "", df$A)
gold$Ta.ppm <- gsub(">", "", gold$Ta.ppm)
gold$U.ppm <- gsub(">", "", gold$U.ppm)
gold$Yb.ppm <- gsub(">", "", gold$Yb.ppm)

gold <- na.omit(gold)

x <- gold$Au.oz.ton

# Common Elements
Al <- gold$Al.
Ca <- gold$Ca.
Fe <- gold$Fe.
K <- gold$K.
Mg <- gold$Mg.
Na <- gold$Na.
P <- gold$P.
Ti <- gold$Ti.

# Additional Elements (with .ppm)
Mn.ppm <- gold$Mn.ppm
Ag.ppm <- gold$Ag.ppm
As.ppm <- gold$As.ppm
Ba.ppm <- gold$Ba.ppm
Be.ppm <- gold$Be.ppm
Bi.ppm <- gold$Bi.ppm
Cd.ppm <- gold$Cd.ppm
Ce.ppm <- gold$Ce.ppm
Co.ppm <- gold$Co.ppm
Cr.ppm <- gold$Cr.ppm
Cu.ppm <- gold$Cu.ppm
Eu.ppm <- gold$Eu.ppm
Ga.ppm <- gold$Ga.ppm
Ho.ppm <- gold$Ho.ppm
La.ppm <- gold$La.ppm
Li.ppm <- gold$Li.ppm
Mo.ppm <- gold$Mo.ppm
Nb.ppm <- gold$Nb.ppm
Nd.ppm <- gold$Nd.ppm
Ni.ppm <- gold$Ni.ppm
Pb.ppm <- gold$Pb.ppm
Sc.ppm <- gold$Sc.ppm
Sn.ppm <- gold$Sn.ppm
Sr.ppm <- gold$Sr.ppm
Ta.ppm <- gold$Ta.ppm
Th.ppm <- gold$Th.ppm
U.ppm <- gold$U.ppm
V.ppm <- gold$V.ppm
Y.ppm <- gold$Y.ppm
Yb.ppm <- gold$Yb.ppm

Elements <- names(gold)[grepl("Al\\.|Ca\\.|Fe\\.|K\\.|Mg\\.|Na\\.|P\\.|Ti\\.|Mn\\.ppm|Ag\\.ppm|As\\.ppm|Ba\\.ppm|Be\\.ppm|Bi\\.ppm|Cd\\.ppm|Ce\\.ppm|Co\\.ppm|Cr\\.ppm|Cu\\.ppm|Eu\\.ppm|Ga\\.ppm|Ho\\.ppm|La\\.ppm|Li\\.ppm|Mo\\.ppm|Nb\\.ppm|Nd\\.ppm|Ni\\.ppm|Pb\\.ppm|Sc\\.ppm|Sn\\.ppm|Sr\\.ppm|Ta\\.ppm|Th\\.ppm|U\\.ppm|V\\.ppm|Y\\.ppm|Yb\\.ppm", names(gold))]

for (Element in Elements){
  correlation <- cor(x, y = gold[[Element]], use = "complete.obs")
  print(paste("Correlation between x and", Element, "is:", correlation))
}  




cor(filtered_gold$Au.oz.ton, filtered_gold$Al.)
