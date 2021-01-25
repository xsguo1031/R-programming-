## ----------------scatter plot of annualized volatility and excess returns--------------------
scatter <- as.data.frame(read.csv("~/Desktop/Paper2/Paper2 code&data for geometric return/2001wma G=10.csv"))
head(scatter)
scatter <- scatter %>% filter(Group=="TSMOM"|Group=="TSMDR")
scatter$Strategy <- scatter$Strategy %>%
  str_replace_all("lambda", paste("\u03BB"))

scatter %>% ggplot(aes(y = Excess.return, x = Volatility, fill = Group, label=Strategy)) + 
  geom_point() +
  ylab("Annualized excess return")+
  geom_label_repel(aes(label = Strategy),
                   box.padding   = 0.35, 
                   size = 4.5,
                   fontface = "bold",
                   point.padding = 0.35,
                   segment.color = 'grey50') +
  theme_classic()+
  theme(legend.position = c(0.15, 0.85), 
        legend.text = element_text(size=15, face="bold"), 
        legend.title = element_text(size=15, face="bold"),
        axis.title=element_text(size=15,face="bold"),
        axis.text=element_text(size=12),
        legend.key.size = unit(2,"line"))+
  #theme(legend.position="none")+
  xlab("Annualized volatility") +
  scale_fill_manual(values = brewer.pal(5, "Pastel1")) + 
  guides(fill = guide_legend(override.aes = list(label = c("\u03b1,C*", "C"), size=4))) 


## ----------------scatter plot of annualized volatility and excess returns--------------------
scatter <- as.data.frame(read.csv("2001wma G=6.csv"))
head(scatter)
scatter$Strategy <- scatter$Strategy %>%
  str_replace_all("lambda", paste("\u03BB"))

scatter %>% ggplot(aes(y = Excess.return, x = Volatility, fill = Group, label=Strategy)) + 
  geom_point() +
  ylab("Annualized excess return")+
  geom_label_repel(aes(label = Strategy),
                   box.padding   = 0.35, 
                   size = 4.5,
                   fontface = "bold",
                   point.padding = 0.35,
                   segment.color = 'grey50') +
  theme_classic()+
  theme(legend.position = c(0.15, 0.85), 
        legend.text = element_text(size=15, face="bold"), 
        legend.title = element_text(size=15, face="bold"),
        axis.title=element_text(size=15,face="bold"),
        axis.text=element_text(size=12),
        legend.key.size = unit(2,"line"))+
  #theme(legend.position="none")+
  xlab("Annualized volatility") +
  scale_fill_manual(values = brewer.pal(5, "Pastel1")) + 
  guides(fill = guide_legend(override.aes = list(label = c("\u03b1,\u03BB", "\u03BB", "Type", "\u03b1,C*", "C"), size=4))) 



# Compare scenario mean and observation 
compare <- as.data.frame(cbind(r_m[(T+G+1):length(r_m)], apply(scenario, 2, mean)))
colnames(compare) <- c("realization", "Mean of sceanrios")
compare$Date <- trbm$DATE[(T+G+1):length(r_m)]
C <- compare %>% 
  gather(key = "Comparison", value = "Return rate", cols = `realization`:`Mean of sceanrios`)
C %>% ggplot(aes(x = Date, y = `Return rate`, group = Comparison, colour = Comparison)) + geom_line() + ylab("Index return rate")
