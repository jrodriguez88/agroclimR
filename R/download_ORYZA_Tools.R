download_ORYZA_Tools <-
function(path){
  
  # set dir to download
  wd <- getwd()
  setwd(path)
  folder <- "."
  
  ip <- function() {
    if (.Platform$OS.type == "windows") {
      ipmessage <- system("ipconfig", intern = TRUE)
    } else {
      ipmessage <- system("ifconfig", intern = TRUE)
    }
    validIP <- "((25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)[.]){3}(25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)"
    any(grep(validIP, ipmessage))
  }  
  
  if (all(c("ORYZA3.exe", "DRATE(v2).exe", "PARAM(v2).exe", "standard.crp", "AutoCalibration3.exe") %in% list.files())){
    
    print("All files in destination folder")
    
    
  } else if(ip()==T){
    
    # Download DRATES and PARAM app  
    download.file(url='https://sites.google.com/a/irri.org/oryza2000/downloads/new-release/download-oryza-version3/AllTools.zip',
                  destfile='AllTools.zip', method='auto')
    ls_tools<- unzip('AllTools.zip', list = T)
    unzip('AllTools.zip', exdir = folder, files = ls_tools$Name[c(1,2,4)])
    
    # Download ORYZA.exe
    download.file(url='https://sites.google.com/a/irri.org/oryza2000/downloads/new-release/download-oryza-version3/ORYZA3.zip',
                  destfile='ORYZA3.zip', method='auto')
    unzip('ORYZA3.zip', exdir = folder, files="ORYZA3.exe")
    
    #Download standard.crp
    download.file("https://sites.google.com/a/irri.org/oryza2000/downloads/new-release/download-oryza-version3/standard.crp",
                  destfile = paste(folder, "standard.crp", sep = "/"), method='auto')
    
    file.remove('AllTools.zip')
    file.remove('ORYZA3.zip')
  } else {
    mens <- cat(
      "#####################################################
####       WARNING! NO INTERNET CONECTION        ####
####      It is need copy ORYZA model Tools:     ####
####  ORYZA3.exe & drate(v2).exe & PARAM(v2).exe ####
####        AND CROP FILE standard.crp           ####
#####################################################")
    
    print(mens)
  }
  
  
  setwd(wd)  
  
}
