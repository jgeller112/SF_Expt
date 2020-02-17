#take a vector of words, places them in center screen, and saves as png file. 


for (i in 1:length(text1)) { 
  png(paste(text1[i], "generate.png", sep=""))
  plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
  text(x = 0.5, y = 0.5, paste(text1[i]),cex=3, col = "black", family="Arial")
  dev.off()
}


#generate condition by removing all vowels

gsub('(A|E|I|O|U)','_',toupper(san2$targ1))