#' @title plot_hydro_site_year: Plots a hydrograph by each year within a site
#'
#' @importFrom dplyr filter select
#' @import ggplot2
#'
#' @description This function creates hydrographs of each year within a site. This function
#' requires a data frame with the following fields: doy_h, year, and water level field.
#' The doy_h field is the day of year with a decimal point for hour (eg 173.09 is June 22 at 9am).
#' Year is a 4 digit number which is a group variable and used for facet wrap.
#'
#' @param df the name of the data frame
#' @param yvar Quoted name of the water level field
#' @param site Quoted name of the site for the title
#' @param years years to plot
#'
#' @examples
#' \dontrun{
#' welld<-read.csv('./Analysis/FINAL_DATA/well_prec_data_2013-2018.csv')
#' plot_hydro_site_year(df=welld,yvar='BIGH_WL',site='Big Heath')
#' plot_hydro_site_year(df=welld,yvar='DUCK_WL',site='Duck Pond')
#'}
#'
#' @return Returns a panel of hydrographs during the growing season for each year
#' in the data frame.
#'
#' @export

plot_hydro_site_year <- function(df, yvar, site, years = 2013:as.numeric(format(Sys.Date(), "%Y"))){

  minWL <- min(df[,yvar], na.rm = TRUE)

  df <- df |> filter(Year %in% years) |>
              filter(doy > 134 & doy < 275) |>
              select(doy_h, yvar, Year, lag.precip) |> droplevels()

  colnames(df) <- c('doy_h', 'WL', 'Year', 'lag.precip')

  p <- ggplot(df, aes(x = doy_h, y = WL, group = Year))+
          geom_line(col = 'black')+
          geom_line(aes(x = doy_h, y = lag.precip*5 + minWL, group = Year), col ='blue')+
          facet_wrap(~Year, nrow = length(unique(df$Year)))+
          geom_hline(yintercept = 0, col = 'brown')+
          theme_bw()+
          theme(plot.title = element_text(hjust = 0.5),
                panel.grid.minor = element_blank(),
                panel.grid.major = element_blank(),
                axis.text.y.right = element_text(color = 'blue'),
                axis.title.y.right = element_text(color = 'blue'),
                strip.text = element_text(size = 11))+
          labs(title = site, y = 'Water Level (cm)\n', x = 'Date')+
          scale_x_continuous(breaks = c(121, 152, 182, 213, 244, 274),
                             labels = c('May-01', 'Jun-01',
                                        'Jul-01', 'Aug-01',
                                        'Sep-01', 'Oct-01'))+
          scale_y_continuous(sec.axis = sec_axis(~.,
                                                 breaks = c(minWL, minWL+10),
                                                 name = 'Hourly Precip. (cm)\n',
                                                 labels = c('0', '2')))
  return(p)
}
