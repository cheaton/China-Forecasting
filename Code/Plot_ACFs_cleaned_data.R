### Plots the ACFs of the cleaned data for the variables whose names appear in the vector variable.names, which is created by Read_the_data.R.

for (i in 1:length(variable.names))
{
#  X11()
  pdf(paste("../Output/Plots/Plot_of_ACFs_Cleaned_",variable.names[i],".pdf",sep=""))
  cmd = parse(text=paste("Acf(",variable.names[i],".cleaned$x.clean,lag.max=60,main='')",sep=""))
  eval(cmd)
  dev.off()
}

for (i in 1:length(variable.names))
{
#  X11()
  pdf(paste("../Output/Plots/Plot_of_ACFs_Differenced_Cleaned_",variable.names[i],".pdf",sep=""))
  cmd = parse(text=paste("Acf(diff(",variable.names[i],".cleaned$x.clean),lag.max=60,main='')",sep=""))
  eval(cmd)
  dev.off()
}
