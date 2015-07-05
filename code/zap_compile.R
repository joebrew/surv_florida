library(knitr)
# generate PDF
Sweave2knitr("zap.Rnw", "zap.Rnw")
knit2pdf("zap.Rnw")

# copy pdf to 'file'
file.copy("zap.pdf", file)

# # delete generated files
# file.remove("report.pdf", "report.tex",
#             "report.aux", "report.log")
# 
# # delete folder with plots
# unlink("figure", recursive = TRUE)