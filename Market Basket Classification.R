#install.packages("arules")
#install.packages("arulesViz")
#install.packages("caTools")
#install.packages("prabclus")
#install.packages("DEoptimR")
#Ainstall.packages("trimcluster")
#install.packages("tcltk")

library(arules)
library(arulesViz)
library(caret)
library(readr)
library(varImp)
library(dplyr)
library(tcltk)
library(readxl)

Dataset = read.transactions("ElectronidexTransactions2017.csv", format = "basket", sep = ",", rm.duplicates = TRUE)
products = read_csv("Termekek.csv")

#Exploring the data

inspect (Dataset) # You can view the transactions. Is there a way to see a certain # of transactions?
length (Dataset) # Number of transactions.
size (Dataset) # Number of items per transaction
LIST(Dataset) # Lists the transactions by conversion (LIST must be capitalized)
itemLabels(Dataset)# To see the item labels

itemFrequencyPlot(Dataset, topN = 20) 
image(Dataset) 
Dataset %>% sample() %>% image()
image(sample(Dataset, 200))

#Rules

RulesName = apriori(Dataset, parameter = list(support = 0.0055, conf = 0.6))
inspectDT(RulesName)
summary(RulesName)

inspect(sort(RulesName, by = "confidence", decreasing = TRUE))

#Investigating the rules
red_rules = is.redundant(RulesName)
red_rules
summary(red_rules)
RulesName = RulesName[!red_rules] #Remove duplicated rules
inspectDT(RulesName)

plot(RulesName, method="graph")


Rules_for_imac = apriori(Dataset, parameter = list(support = 0.01, conf = 0.5), appearance = list(default = "rhs", lhs = "iMac"))
inspect(Rules_for_imac)
inspect(sort(Rules_for_imac, by = "confidence", decreasing = TRUE))

Rules_for_HPlaptop = apriori(Dataset, parameter = list(support = 0.07, conf = 0.3), appearance = list(default = "rhs", lhs = "HP Laptop"))
inspect(Rules_for_HPlaptop)

#Visualizing the rules
?plot
plot(RulesName[34], method="graph", control=list(type="items")) 
                 
                 
dim(Dataset)
dimnames(Dataset)

itms <- itemFrequency(Dataset, type = "absolute")
head(sort(itms, decreasing = TRUE), n = 20)

plot(RulesName, measure = c("support", "confidence"), shading = "lift", jitter=0)

?plot

#The products 
products = read_excel("Termekek.xlsx")
summary(products)

products %>% group_by(Producttype) %>% summarise(avg= mean(Volume),
                                                 Total = sum(Volume)) %>% 
                                                 arrange(avg)


library(treemap)

tmp = products %>% group_by(Producttype) %>% summarise(n=n())
treemap(tmp,index=c("Producttype"),vSize="n",title="",palette="Set3",border.col="#FFFFFF")
tmp <- tmp %>% left_join(departments,by="Volume")

#Counting the types of products 
itemLabels(Dataset) 

laptops <- "LG Touchscreen Laptop|Acer Aspire|HP Laptop|ASUS Chromebook|Apple Macbook Pro|Apple Macbook Air|Dell Laptop|Eluktronics Pro Gaming Laptop|Alienware Laptop|HP Notebook Touchscreen Laptop PC"
laptop_out = as.character(unlist(lapply(LIST(Dataset), function(x) sum(grepl(laptops,x)))))
laptop_out = as.factor(laptop_out)


PC = "Lenovo Desktop Computer|iMac|HP Desktop|ASUS Desktop|Dell Desktop|Dell 2 Desktop|Intel Destkop|Acer Destkop|CYBERPOWER Gamer Destkop"
PC_out = as.character(unlist(lapply(LIST(Dataset), function(x) sum(grepl(PC,x)))))
PC_out = as.factor(PC_out)
 

Laptops_and_PC = "LG Touchscreen Laptop|Acer Aspire|HP Laptop|ASUS Chromebook|Apple Macbook Pro|Apple Macbook Air|Dell Laptop|Eluktronics Pro Gaming Laptop|Alienware Laptop|HP Notebook Touchscreen Laptop PC|Lenovo Desktop Computer|iMac|HP Desktop|ASUS Desktop|Dell Desktop|Dell 2 Desktop|Intel Destkop|Acer Destkop|CYBERPOWER Gamer Destkop"
Laptops_and_PC_out = as.character(unlist(lapply(LIST(Dataset), function(x) sum(grepl(Laptops_and_PC,x)))))
Laptops_and_PC_out = as.factor(Laptops_and_PC_out)


Accessories <- "Microsoft Office Home and Student 2016|Computer Game|Belkin Mouse Pad|Large Mouse Pad"                                             
Accessories_out <- as.character(unlist(lapply(LIST(Dataset), function(x) sum(grepl(Accessories,x)))))
Accessories_out = as.factor(Accessories_out)
summary(Accessories_out)

Display <- "Acer Monitor|LG Monitor|ASUS Monitor|ASUS 2 Monitor|Dell Monitor|Samsung Monitor|Sceptre Monitor|ViewSonic Monitor|AOC Monitor|HP Monitor"                                             
Display_out <- as.character(unlist(lapply(LIST(Dataset), function(x) sum(grepl(Display,x)))))
Display_out = as.factor(Display_out)


PcDisplyMonitor <- "Acer Monitor|LG Monitor|ASUS Monitor|ASUS 2 Monitor|Dell Monitor|Samsung Monitor|Sceptre Monitor|ViewSonic Monitor|AOC Monitor|HP Monitor|LG Touchscreen Laptop|Acer Aspire|HP Laptop|ASUS Chromebook|Apple Macbook Pro|Apple Macbook Air|Dell Laptop|Eluktronics Pro Gaming Laptop|Alienware Laptop|HP Notebook Touchscreen Laptop PC|Lenovo Desktop Computer|iMac|HP Desktop|ASUS Desktop|Dell Desktop|Dell 2 Desktop|Intel Destkop|Acer Destkop|CYBERPOWER Gamer Destkop"                                            
PcDisplyMonitor_out <- as.character(unlist(lapply(LIST(Dataset), function(x) sum(grepl(PcDisplyMonitor,x)))))
PcDisplyMonitor_out = as.factor(PcDisplyMonitor_out)


printer <- "Epson Printer|HP Wireless Printer|Canon Office Printer|Brother Printer|DYMO Laber Manker"
printer_out = as.character(unlist(lapply(LIST(Dataset), function(x) sum(grepl(printer,x)))))
printer_out = as.factor(printer_out)


ink <- "Epson Black Ink|HP Black & Tri-color Ink|Canon Ink|Brother Printer Toner|DYMO Labeling Tape"
ink_out = as.character(unlist(lapply(LIST(Dataset), function(x) sum(grepl(ink,x)))))
ink_out = as.factor(ink_out)


printer_ink <- "Epson Black Ink|HP Black & Tri-color Ink|Canon Ink|Brother Printer Toner|DYMO Labeling Tape|Epson Printer|HP Wireless Printer|Canon Office Printer|Brother Printer|DYMO Laber Manker"
printer_ink_out = as.character(unlist(lapply(LIST(Dataset), function(x) sum(grepl(printer_ink,x)))))
printer_ink_out = as.factor(printer_ink_out)

try <- "DYMO Laber Manker|DYMO Labeling Tape"
try_out = as.character(unlist(lapply(LIST(Dataset), function(x) sum(grepl(try,x)))))
try_out = as.factor(try_out)
summary(try_out)

summary(laptop_out)
summary(PC_out)
summary(Laptops_and_PC_out)
summary(Display_out)
summary(PcDisplyMonitor_out)
summary(printer_out)
summary(ink_out)
summary(printer_ink_out)
summary(Accessories_out)

AccesoriesALL <- "Microsoft Office Home and Student 2016|Computer Game|Belkin Mouse Pad|Large Mouse Pad|Apple Earpods|Monster Beats By Dr Dre|Otium Wireless Sports Bluetooth Headphone|Panasonic In-Ear Headphone|APIE Bluetooth Headphone|Philips Flexible Earhook Headphone|HDMI Cable 6ft|Ethernet Cable|Etekcity Power Extension Cord Cable|Audio Cable|VGA Monitor Cable|iPhone Charger Cable|HDMI Adapter|USB Cable|Samsung Charging Cable|Zombie Gaming Headset|Logitech ClearChat Headset|Panasonic On-Ear Stereo Headphones|PC Gaming Headset|Kensington Headphones|Logitech Stereo Headset|Koss Home Headphones|Microsoft Headset|Ailihen Stereo Headphones|XIBERIA Gaming Headset|3-Button Mouse|Logitech Wireless Mouse|Microsoft Basic Optical|Logitech 3-button Mouse|Redragon Gaming Mouse|HP Wireless Mouse|Generic Black 3-Button|Wireless Portable Mouse|Gaming Mouse Professional|Slim Wireless Mouse|Halter Acrylic Monitor Stand|Height-Adjustable Standing Desk|Multi Media Stan|Halter Mesh Metal Monitor Stand|Full Motion Monitor Mount|1TB Portable External Hard Drive|2TB Portable External Hard Drive|3TB Portable External Hard Drive|5TB Desktop Hard Drive|Slim 2TB Portable External Hard Drive|HP USB Keyboard|Logitech Wireless Keyboard|Rii LED Keyboard|Logitech Keyboard|Backlit LED Gaming Keyboard|Dell Wired Keyboard|Apple Wired Keyboard|Apple Wireless Keyboard|Apple Magic Keyboard|Logitech MK550 Wireless Wave Keyboard and Mouse Combo|Logitech Desktop MK120 Mouse and keyboard Combo|Logitech MK270 Wireless Keyboard and Mouse Combo|Dell KM117 Wireless Keyboard & Mouse|EagleTec Wireless Combo Keyboard and Mouse|Microsoft Wireless Comfort Keyboard and Mouse|Microsoft Wireless Desktop Keyboard and Mouse|Rii LED Gaming Keyboard & Mouse Combo|Logitech MK360 Wireless Keyboard and Mouse Combo|Cambridge Bluetooth Speaker|JBL Splashproof Portable Bluetooth Speaker|DOSS Touch Wireless Bluetooth|Logitech Multimedia Speakers|Rokono Mini Speaker|Cyber Acoustics|Bose Companion Speakers|Mackie CR Speakers|Sonos"
AccesoriesALL_out = as.character(unlist(lapply(LIST(Dataset), function(x) sum(grepl(AccesoriesALL,x)))))
AccesoriesALL_out = as.factor(AccesoriesALL_out)
summary(AccesoriesALL_out)

