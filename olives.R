setwd("~github/oil")
year <- c(2021,2021)
dates <- c("10-12","10-26")
date <- as.Date(dates,format="%m-%d") #data in formato mese-giorno
olives_sp <- c(100,0) #olive di San Placido (kg)
olives_f <- c(70,115) #olive di Foddiri
production <- c(27.8,19.2) #olio in kg
volume <- round(1/.916,digit=3) #volume olio in base al peso specifico (lt per kg)
cost <- c(.21,.21) #Euro cent per chilo di olive
iva <- c(.04,.04)
oil <- data.frame(year,date,olives_sp,olives_f,production,volume,cost,iva)
oil <- transform(oil,
                 oil_return=round(production/(olives_sp+olives_f),digit=3),
                 prod_litre=round(production*volume,digit=1),
                 tot_cost=round((olives_sp+olives_f)*cost*(1+iva))) #add oil in litres and total cost per date

oil_table <- data.frame(format(as.Date(oil$date,format="%y-%m-%d"),"%d/%m/%Y"),paste(oil$olives_sp+oil$olives_f," Kg"),paste(oil$production," Kg"),paste(oil$oil_return*100," %"),paste(oil$prod_litre," Lt"),paste("EUR ",oil$tot_cost))
colnames(oil_table) <- c("Data","Raccolto","Olio prodotto (Kg)","Resa","Olio prodotto (Lt)","Costo")

par(mfrow=c(1,3),mai=c(1,.4,1,.4))
oil_plot <- barplot(cbind(oil$olives_sp,oil$olives_f)~oil$date,beside=TRUE,ylim=c(0,200),col=c("#00A600","#EAB64E"),xaxt="n",xlab="",ylab="Kg",main="Olive raccolte (Kg)")
axis(1,at=c(2,5),labels=oil$date)
text(x=t(oil_plot),y=cbind(oil$olives_sp,oil$olives_f),labels=cbind(paste(c(oil$olives_sp,oil$olives_f)," Kg")),pos=3,cex=.8) #note that I transpose the plot coordinates (t(oil_plot)), for the bar labels to be correct (they would be mixed up, otherwise)
legend(1,180,c("San Placido","Foddiri"),fill=c("#00A600","#EAB64E"),cex=.85)

oil_plot2 <- barplot(cbind(sum(oil$olives_sp),sum(oil$olives_f)),beside=TRUE,space=c(1,.4),ylim=c(0,200),col=c("#00A600","#EAB64E"),main="Olive raccolte per campagna (Kg)")
axis(1,at=oil_plot2,labels=c("San Placido","Foddiri"))
text(x=oil_plot2,y=cbind(sum(oil$olives_sp),sum(oil$olives_f)),labels=paste(cbind(sum(oil$olives_sp),sum(oil$olives_f))," Kg"),pos=3,cex=.9)
#legend(.4,185,c("San Placido","Foddiri"),fill=c("#00A600","#EAB64E"),cex=.85)

product_matrix <- cbind(oil$olives_sp+oil$olives_f,oil$production) #I put olives harvested by date and oil produced (in chilos) in a matrix
product_plot <- barplot(product_matrix~oil$date,beside=TRUE,ylim=c(0,200),col=c("#ABC32F","#207567"),xaxt="n",xlab="",main="Olio prodotto per raccolto (Kg)") #I plot that matrix on date
axis(1,at=c(2,5),labels=oil$date)
text(x=product_plot,y=t(product_matrix),labels=paste(t(product_matrix)," Kg"),pos=3)
legend(x=3.5,y=190,c("Olive","Olio"),fill=c("#ABC32F","#207567"),cex=.85)
