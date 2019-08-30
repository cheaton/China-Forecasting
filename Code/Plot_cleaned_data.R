### Plots the cleaned data for the variables whose names appear in the vector variable.names, which is created by Read_the_data.R.

for (i in 1:length(variable.names))
{
#  X11()
  if (variable.names[i] %in% c("PPI","IP"))
  {
    title <- paste(variable.names[i],": year-on-year growth rate",sep="")
  }else
  {
    title <- paste(variable.names[i],": month-on-month growth rate",sep="")
  }
  pdf(paste("../Output/Plots/Plot_of_Cleaned_",variable.names[i],".pdf",sep=""))
  cmd = parse(text=paste("plot(",variable.names[i],".cleaned$x.clean,xlab='Year',ylab='%',main=title)",sep=""))
  eval(cmd)
  dev.off()
}
  
for (i in 1:length(variable.names))
{
#  X11()
  if (variable.names[i] %in% c("PPI","IP"))
  {
    title <- paste(variable.names[i],": first difference of year-on-year growth rate",sep="")
  }else
  {
    title <- paste(variable.names[i],": first difference of month-on-month growth rate",sep="")
  }
  pdf(paste("../Output/Plots/Plot_of_Differenced_Cleaned_",variable.names[i],".pdf",sep=""))
  cmd = parse(text=paste("plot(diff(",variable.names[i],".cleaned$x.clean),xlab='Year',main=title)",sep=""))
  eval(cmd)
  dev.off()
}
