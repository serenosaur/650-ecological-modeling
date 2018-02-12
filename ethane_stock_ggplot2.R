#START with the out file from the StellaR and the ethanemodel.txt exercise
#END with the same plot as what Stella gives us

#load up the needed packages
library(tidyr)
library(ggplot2)

#change out to a dataframe
out.df2 <- as.data.frame(out)

#use gather to make out into a ggplot2-friendly dataset
out.dftest <- gather(out.df2, "hemi", "stock", -time)

#plot the data in a scatterplot
ggplot(out.dftest, aes(x=time, y=stock, color=hemi)) +geom_point()

#plot the data as a line, and adjust the x-axis scaling
ggplot(out.dftest, aes(x=time, y=stock, color=hemi)) +
  geom_line() +
  scale_x_continuous(limits=c(0,10))
