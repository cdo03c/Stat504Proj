
#Load in R packages
library(pdftools)

#Note set your working directory to the file location until we write the pull 
#from Github

###Load the NHES 2012 code book for extracting the data

#Use PDF_Tools to create a character vector from NHES 2012 codebook
dict = pdf_text("./NHES_2012_pfi_codebook.pdf")

#Loop through the character vector and extract all the variable names into a
#new character vector called header and extract the fixed width values into a 
#vector called widths
header = vector()
widths = vector()
for(i in 1:length(dict)){
  m <- gregexpr('([A-Z]{5,}\\d+|[A-Z]_[A-Z]{5,}|[A-Z]{4,})', dict[i])
  vars = unlist(regmatches(dict[i], m))
  if(!identical(vars, character(0))){
    vars = vars[!vars %in% c('NHES','IMPUTATION', 'FINAL', 'REPLICATE', 'WEIGHT')]
    header = c(header,unique(vars))
  }
  w <- gregexpr('(\\d+-\\d+)', dict[i])
  ws = unlist(regmatches(dict[i], w))
  ws = strsplit(ws, '-')
  for(y in 1:length(ws)){
    width = as.integer(unlist(ws[y]))[2] - as.integer(unlist(ws[y]))[1]+1
    if(width < 14){
      widths = c(widths,width)
    }
  }
}
#print(header)
#print(widths)

#Load data table for NHES 2012 study
df = read.fwf("./pfi_pu_pert_ascii.dat", widths = widths)
head(df)
length(header)

#Subset to the first 91 columns of data
df2 = df[1:91]
colnames(df2) = header[1:91]
head(df2,1)
