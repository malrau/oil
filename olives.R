setwd("~github/oil")
year <- c(2021,2021)
date <- as.Date(c("10-12","10-26"),format=("%m-%d")) #data in formato mese-giorno
olives_sp <- c(100,0) #olive di San Placido (kg)
olives_f <- c(70,115) #olive di Foddiri
production <- c(27.8,19.2) #olio in kg
volume <- round(1/.916,digit=3) #volume olio in base al peso specifico (lt per kg)
cost <- c(.21,.21) #Euro cent per chilo di olive
iva <- c(.04,.04)
oil <- data.frame(year,date,olives_sp,olives_f,production,volume,cost,iva)
oil <- transform(oil,
                 return=round(production/(olives_sp+olives_f),digit=3),
                 prod_litre=round(production*volume,digit=1),
                 tot_cost=round((olives_sp+olives_f)*cost*(1+iva))) #add oil in litres and total cost per date
barplot(cbind(oil$olives_sp,oil$olives_f)~oil$date,beside=TRUE,ylim=c(0,150),col=c("green","orange"),xaxt="n",xlab="",ylab="Kg",main="Olive raccolte (Kg)")
axis(1,at=c(2,5),labels=oil$date)
legend(1,140,c("San Placido","Foddiri"),fill=c("green","orange"))

        