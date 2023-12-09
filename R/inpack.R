inpack <-
function(pack){
    new_pack <- pack[!(pack %in% installed.packages()[, "Package"])]
    if (length(new_pack)) 
        install.packages(new_pack, dependencies = TRUE)
    sapply(pack, require, character.only = TRUE)
}
