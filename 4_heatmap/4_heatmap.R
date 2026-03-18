


#install.packages("pheatmap")


library(pheatmap)          
inputFile="input.txt"      
groupFile="group.txt"  
outFile="heatmap.pdf"      
setwd("D:\\biowolf\\bioR\\17.heatmap")      
rt=read.table(inputFile,header=T,sep="\t",row.names=1,check.names=F)    
ann=read.table(groupFile,header=T,sep="\t",row.names=1,check.names=F)   


ß
pdf(file=outFile,width=6,height=5.5)
pheatmap(rt,
         annotation=ann,
         cluster_cols = T,
         color = colorRampPalette(c("blue", "white", "red"))(50),
         show_colnames = T,
         scale="row",  
         #border_color ="NA",
         fontsize = 8,
         fontsize_row=6,
         fontsize_col=6)
dev.off()

