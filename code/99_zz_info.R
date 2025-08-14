# simply capture system info


source(file.path(rprojroot::find_root(rprojroot::has_file("config.R")),"config.R"),echo=FALSE)


sink(file.path(codedir,"99_zz_info.txt"),type = "output")
devtools::session_info()
version
sink()
