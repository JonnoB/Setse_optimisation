#' Extracts Attack stats from saved
#'
#' Extracts the attack statistics for each network in a folder and combines it into a single file that is also saved.
#' This function only works on a folder of folders...It needs to be changed
#'
#' @param RootFolder :The folder where the attack files are saved
#' @param NewFolderPath : The folder where the summary files will be saved to
#' @param Generation The name of the variable that stores the net generation data. character string
#' @param Edgename A character string. The default is "name".
#' @param PowerFlow A character String. The default is "PowerFlow".
#' @param Link.Limit A character string. The default is "Link.Limit".
#' @export

ExtractAttackStats<- function(RootFolder, 
                              NewfolderPath, 
                              Generation = "BalencedPower", 
                              EdgeName = "name", 
                              PowerFlow = "PowerFlow", 
                              Link.Limit = "Link.Limit" ){

  list.files(RootFolder , full.names = TRUE) %>%
    walk(~{

      rootfolder <- .x
      targetfolder <- basename(.x)
      savename <-paste0(targetfolder, ".rds") %>% file.path(NewfolderPath,.)
      print(savename)

      #Create directory if needed
      if(!file.exists(NewfolderPath)){
        dir.create(NewfolderPath)
      }

      if(!file.exists(savename)){

        print(paste("Extracting summary data for", targetfolder))

        summarydata <-list.files(rootfolder) %>%
          map_df(~{
            print(.x)
            read_rds(file.path(rootfolder, .x)) %>%
              ExtractNetworkStats(Generation = Generation, EdgeName = EdgeName, PowerFlow = PowerFlow, Link.Limit = Link.Limit)%>%
              mutate( simulationID = gsub("\\.rds", "", .x ) %>% gsub("Simulation_ID_", "", .) %>% as.integer)
          }
          ) %>%
          mutate(alpha = targetfolder)


        saveRDS(summarydata, savename)

        print(paste("File", savename, "saved"))

      } else {print(paste("saved file for" , targetfolder, "exists"))}

    }
    )

}
