#------------------
# Generate briefs for all parks
# Code iterates through params$park and renames html output with park code
#------------------
library(purrr)
library(knitr)
library(rmarkdown)

sites = c("DUCK", "WMTN", "BIGH", "GILM", "LIHU", "NEMI", "GRME", "HEBR", "HODG", "FRAZ")
nwca_codes <- c("NWCA16-R301", "NWCA16-R302", "NWCA16-R303", "NWCA16-R304", "NWCA16-R305",
                "NWCA16-R306", "NWCA16-R307", "NWCA16-R308", "NWCA16-R309", "NWCA16-R310")

indir <- paste0("./reports/")
outdir <- paste0("./reports/output/")
rmd_site <- "NWCA_site_level_reports.Rmd"
rmd_vegplot <- "NWCA_veg_plot_reports.Rmd"
rmd_tree <- "NWCA_veg_tree_reports.Rmd"
rmd_spp <- "NWCA16_ACAD_species_lists_site.Rmd"

report_files <- c(rmd_site, rmd_vegplot, rmd_tree, rmd_spp)
report_names <- c("Site_Level", "Veg_Plot", "Veg_Tree", "Veg_Spp")

render_reports <- function(site_name, code, rmd, report){
  render(input = paste0(indir, rmd),
         params = list(site = site_name),
         output_file = paste0(code, "_", site_name, "_", report, ".html"),
         output_dir = outdir)

}

map2(sites, nwca_codes, ~render_reports(.x, .y, report_files[1], report_names[1]))
map2(sites, nwca_codes, ~render_reports(.x, .y, report_files[2], report_names[2]))
map2(sites, nwca_codes, ~render_reports(.x, .y, report_files[3], report_names[3]))
map2(sites, nwca_codes, ~render_reports(.x, .y, report_files[4], report_names[4]))

map2(sites, nwca_codes, ~render_reports(.x, .y, report_files[3], report_names[3]))
