
#Load in R packages
library(pdftools)

#Note set your working directory to the file location until we write the pull 
#from Github

###Load the NHES 2012 code book for extracting the data

#Download the .pdf of the NHES 2012 code book if it does not exist in the
#working directory and use PDF_Tools to create a character vector from NHES 2012
#codebook
if(!file.exists("./NHES_2012_pfi_codebook.pdf")){
  download.file(url = "https://raw.githubusercontent.com/cdo03c/Stat504Proj/master/NHES_2012_pfi_codebook.pdf",
                destfile = "./NHES_2012_pfi_codebook.pdf")
}
dict = pdf_text("./NHES_2012_pfi_codebook.pdf")

#Loop through the character vector and extract all the variable names into a
#new character vector called header and extract the fixed width values into a 
#vector called widths
header = vector()
for(i in 1:length(dict)){
  m <- gregexpr('([A-Z]{3,}\\d+|[A-Z]{1,}\\d+[A-Z]{2,}|[A-Z]_[A-Z]{5,}|[A-Z]{4,})', dict[i])
  vars = unlist(regmatches(dict[i], m))
  if(!identical(vars, character(0))){
    vars = vars[!vars %in% c('NHES','IMPUTATION', 'FINAL', 'REPLICATE',
                             'ADHD', 'TANF', 'CHIP', 'TAYLOR','SERIES',
                             'STRATUM','WEIGHT')]
    header = c(header,unique(vars))
  }
}

#Trims the header and width to just the variables form the NHES survey and
#removes all the weight and imputation variables.
header = header[1:356]
widths = c(12,8,1,rep(2,21),3,rep(2,181),4,rep(2,149))

#Load data table for NHES 2012 study
if(!file.exists("./pfi_pu_pert_ascii.dat")){
  download.file(url = "https://raw.githubusercontent.com/cdo03c/Stat504Proj/master/pfi_pu_pert_ascii.dat",
                destfile = "./pfi_pu_pert_ascii.dat")
}
df = read.fwf("./pfi_pu_pert_ascii.dat", widths = widths)
#head(df)
#length(header)

#Subset to the first 356 columns of data to match the header character vector
df2 = df[1:356]
colnames(df2) = header
#head(df2,2)

#Subset data by PATH variable where E stands for elementary student and subset
#the SEGRADES variables which will be the independent variable for this study
#by the four levels that indicate the general letter grade of the student.
df2 = df2[df2$PATH == 'E',]
df2 = df2[df2$SEGRADES %in% c(1,2,3,4),]

#Removes columns in the data that represents variables that are only applicable
#to non-elementary students such as the binary variable SEREPT7 which
#captures if a student repeated 7th grade and does not apply to our target population.
df2 = df2[-c(34:39, 119:124, 206:207, 355:356)]

#Write out the truncated data set as a .csv for faster loading of the data in
#analysis script
write.csv(df2, 'processed_NHES_data.csv')
