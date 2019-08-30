### Plots the raw data for the variables whose names appear in the vector variable.names, which is created by Read_the_data.R.

for (i in 1:length(variable.names))
{
#  X11()
  pdf(paste("../Output/Plots/Plot_of_Raw_",variable.names[i],".pdf",sep=""))
  cmd = parse(text=paste("plot(",variable.names[i],",xlab='Year',ylab='",variable.names[i],"')",sep=""))
  eval(cmd)
  dev.off()
 }
  

