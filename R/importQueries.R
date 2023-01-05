#' @title importQueries: Imports queries from RAM front end
#'
#' @description This function imports the queries from a named ODBC datasource from
#' NETN forest backend databases and prepares the data for plotting in wetlandViz.
#'
#' @param type Select whether to use the default DSN to import data or a different database
#' \describe{
#' \item{"DSN"}{Default. DSN database. If not specified, will use "NETNFVM" .}
#' \item{"file"}{A different database than default DSN}
#' }
#' @param path Quoted path of database backend file, including the name of the backend.
#' @return Assigns database tables to global environment
#'
#' @examples
#' # Import database in specific folder:
#' importQueries(type='file', path='./Data/NETN_RAM_Frontend.mdb')
#'
#' # Import ODBC named database
#' importQueries(type='DSN', odbc="RAM_FE")
#'
#' @export

importQueries<- function(type=c('DSN','file'), odbc='RAM_FE', path=NA){
  type<-match.arg(type)

  if(!requireNamespace("odbc", quietly = TRUE)){
    stop("Package 'odbc' needed for this function to work. Please install it.", call. = FALSE)
  }
  if(!requireNamespace("DBI", quietly = TRUE)){
    stop("Package 'DBI' needed for this function to work. Please install it.", call. = FALSE)
  }
  pb = txtProgressBar(min = 0, max = 23, style = 3)
  db<- if (type=='DSN'){
    db<- DBI::dbConnect(drv=odbc::odbc(),dsn=odbc)
  }
  else if (type=='file'){
    db<- DBI::dbConnect(drv=odbc::odbc(),
                        .connection_string=paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=",path))
  }
  assign("spplist", DBI::dbReadTable(db, "qry_Species_List_All"), envir = .GlobalEnv)
  setTxtProgressBar(pb, 1)
  assign("vmmi", DBI::dbReadTable(db, "tbl_VMMI"), envir = .GlobalEnv)
  setTxtProgressBar(pb, 2)
  assign('plants',DBI::dbReadTable(db,"tlu_Plant"), envir = .GlobalEnv)
  setTxtProgressBar(pb, 3)
  assign('loc',DBI::dbReadTable(db, "tbl_Location"), envir = .GlobalEnv)
  setTxtProgressBar(pb, 4)
  DBI::dbDisconnect(db)
  close(pb)
  noquote('database import complete')
}
