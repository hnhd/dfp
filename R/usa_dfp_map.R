#' Load U.S. Census FIPS Information from U.S Census website and returns a dataframe. 
#' @export

load_census_fips <- function(){
  census_fips <- xml2::read_html("https://www.census.gov/geo/reference/ansi_statetables.html") %>% 
    rvest::html_nodes(xpath = '//*[@id="middle-column"]/div/div[1]/div[2]/table') %>%
    rvest::html_table(header = TRUE)
  census_fips <- census_fips[[1]]
  names(census_fips) <- c("name", "fips", "abbr")
  census_fips$name_lower <- tolower(census_fips$name)
  census_fips$conus <- ifelse(census_fips$abbr %in% c("HI","AK"), FALSE, TRUE)
  assign("census_fips", census_fips, envir = .GlobalEnv)
}

#' Adjusts coordinates of Alaska and Hawaii.
#' @param usa 
#' @param alaskaFix 
#' @param hawaiiFix

# write a function to adjust the position of alaska and hawaii
fixup <- function(usa, alaskaFix, hawaiiFix){
  
  # adjust alaska
  alaska=usa[usa$NAME=="Alaska",]
  alaska = prelim_fix(alaska,alaskaFix)
  proj4string(alaska) <- proj4string(usa)
  
  # adjust hawaii
  hawaii = usa[usa$NAME=="Hawaii",]
  hawaii = prelim_fix(hawaii,hawaiiFix)
  proj4string(hawaii) <- proj4string(usa)
  
  # bind hawaii and alaska back into the USA
  usa = usa[! usa$NAME %in% c("Alaska","Hawaii"),]
  usa = rbind(usa,alaska,hawaii)
  
  # return the merged and adjusted USA shapefile
  return(usa)
}

#' Subfunction that adjusts specific coordinates.
#' @param object 
#' @param params 

prelim_fix <- function(object, params){
  r=params[1];scale=params[2];shift=params[3:4]
  object = elide(object,rotate=r)
  size = max(apply(bbox(object),1,diff))/scale
  object = elide(object,scale=size)
  object = elide(object,shift=shift)
  object
}

#' Plot Data for Progress specified USA map.
#' @param states_data State data frame that contains column entitle "abbr".
#' @param color_value Column name in the data frame for color fill value. Must be input as a character.
#' @param alpha_value Column name in the data frame for alpha transparency value. Must be input as a character. Defaults to NULL.
#' @param title Map title name. Defaults to NULL.
#' @param subtitle Map subtitle name. Defaults to NULL.
#' @param font Font of choice with a limited selection to most base fonts. Defaults to Montserrat.
#' @param color_scheme Map fill color scheme. Defaults to "orange_to_green". Also supports "brown_to_purple", "teal_to_pink", "plum_to_gold" and "red_to_blue".
#' @param limits Range of the color scale. Defaults to c(0.2, 0.8) for most polling data.
#' @param save_plot Saves plot if TRUE. Defaults to FALSE, which only displays the plot. 
#' @param output_folder Output folder destination. Defaults to NULL, which outputs in the same folder.
#' @examples
#' usa_dfp_map(
#'     states_data = mc_trump_approval, 
#'     color_value = "trump_approval_april", alpha_value = NULL, 
#'     itle = "Trump Approval Rating", subtitle = NULL, 
#'     font = "Montserrat", color_scheme = "orange_to_green", limits = c(0.2, 0.8), 
#'     save_plot = TRUE, output_folder = NULL
#' )
#' @export

usa_dfp_map <- function(
    states_data, color_value, alpha_value=NULL,
    title=NULL, subtitle=NULL, 
    font="Montserrat", color_scheme="orange_to_green", limits=c(0.2,0.8), 
    save_plot=TRUE, output_folder=NULL
  ){

  # load in the census fips dataframe
  load_census_fips()
  
  # load in the usa state shapefile (that has been cleaned) 
  # and transform it 
  states <- readOGR(dsn = system.file("cb_2017_us_state_20m_clean", package = "dfp"), layer="cb_2017_us_state_20m_clean", verbose = FALSE) %>%
    subset(NAME %in% census_fips$name) %>%
    spTransform(CRS("+init=epsg:2163")) %>%
    # add clean up paramaters for alaska and hawaii, respectively
    fixup(c(-35,1.8,-2600000,-2500000),c(-35,1,5300000,-1600000))
  
  # create the nationwide shapefile merging the statewide polygons
  nationwide <- unionSpatialPolygons(states, rep(1,length(states)))
  
  # create an id lookup to attach the state abbreviation to state shapefile
  id_lookup <- data.frame("id"=rownames(states@data), "STUSPS"=states@data$STUSPS)
  
  # fortify the state shapefile and attach the abbreviation
  states_shp <- fortify(states)
  states_shp$abbr <- id_lookup[match(states_shp$id, id_lookup$id),]$STUSPS
  
  # fortify the nationwide shapefile
  nationwide_shp <- fortify(nationwide)
  
  # join it into the census fip basetable
  states_data <- suppressMessages(join(census_fips, states_data))
  
  # choose color scheme
  if(color_scheme=="orange_to_green"){
    color_scheme_values=c("#ff8000","#FFFFFF","#006600")
  }else if(color_scheme=="brown_to_purple") {
    color_scheme_values=c("#802b00","#FFFFFF","#4d0099")
  }else if(color_scheme=="teal_to_pink"){
    color_scheme_values=c("#00b3b3","#FFFFFF","#b300b3")
  }else if(color_scheme=="plum_to_gold"){
    color_scheme_values=c("#993366","#FFFFFF","#b38f00")
  }else if(color_scheme=="red_to_blue"){
    color_scheme_values=c("#d65454","#FFFFFF","#3989cb")
  }else if(color_scheme=="grayscale"){
    color_scheme_values=c("#FFFFFF","#b3b3b3","#4d4d4d")
  }
  
  # if there is an overflow, choose overflow colors & provide informative warnings
  top_range <- max(states_data[, color_value])/100
  bottom_range <- min(states_data[, color_value])/100
  no_max_overflow <- max(limits) > top_range
  no_min_overflow <- min(limits) < bottom_range
  
  if(no_max_overflow & no_min_overflow) {
    message(paste0("Range: ", top_range, " to ", bottom_range))
    message("All data points are contained within the mapping limits.")
    overflow_color <- "#FFFFFF"
  } else if (no_max_overflow & !no_min_overflow) {
    message(paste0("Range: ", top_range, " to ", bottom_range))
    message("There is an overflow at the lower mapping limit. Setting mapping overflow color to min color.")
    overflow_color <- color_scheme_values[1]
  } else if (!no_max_overflow & no_min_overflow) {
    message(paste0("Range: ", top_range, " to ", bottom_range))
    message("There is an overflow at the lower mapping limit. Setting mapping overflow color to max color.")
    overflow_color <- color_scheme_values[3]
  } else if (!no_max_overflow & !no_max_overflow) {
    message(paste0("Range: ", top_range, " to ", bottom_range))
    stop("There is an overflow at the both mapping limits. Please correct and remap.")
  }
  
  # join the data to the shapefile
  states <- suppressMessages(join(states_shp, states_data))
  
  # write the plot
  plot <- ggplot()
  
  # fill with color, create conditional on alpha value existing
  if(!is.null(alpha_value)) {
    # if using an alpha value add alpha scale & remove alpha legend
    plot <- plot +
      geom_polygon(data=states, aes(x=long, y=lat, group=group, fill = get(color_value)/100,  alpha = get(alpha_value)), color = "black", size = 0.2) +
      scale_alpha_continuous(guide = "none")
  } else {
    plot <- plot +
      geom_polygon(data=states, aes(x=long, y=lat, group=group, fill = get(color_value)/100), color = "black", size = 0.2)
  }
  
  # add national border & fill gradient & title
  plot <- plot + geom_polygon(data=nationwide, aes(x=long, y=lat, group=group), fill = NA, color = "black", size = 0.8) +
    scale_fill_gradientn(
      name = "", label=percent, colors=color_scheme_values, values = c(0, 0.45, 0.55, 1),
      guide = guide_colorbar(direction = "horizontal", barheight = 1.5, barwidth = 45, ticks = FALSE, guide = guide_legend()),
      limits = limits, na.value = overflow_color) +
    labs(title = title, subtitle = toupper(subtitle))

  # add plot theme
  plot <- plot +
    theme(
      panel.background = element_blank(),
      plot.title = element_text(hjust = 0.5, size = 40),
      plot.subtitle = element_text(hjust = 0.5, size = 16, face = "bold"),
      text = element_text(family = font),
      legend.text = element_text(size = 16),
      legend.position="bottom",
      axis.title.y=element_blank(),
      axis.text.y=element_blank(),
      axis.ticks.y=element_blank(),
      axis.title.x=element_blank(),
      axis.text.x=element_blank(),
      axis.ticks.x=element_blank()
    )
  
  if(save_plot){print(plot)}
  if(save_plot){png(filename = paste0(if(is.null(output_folder)){""}else{paste0(output_folder, "/")}, color_value, ".png"), width = 750, height = 600, units = "px")}
  print(plot)
  if(save_plot){dev.off()}
}


#' Appends Data for Progress branding to a Data for Progress map.
#' @param plot_path Path to the plot without Data for Progress branding.
#' @param annotation_text Text to include annotation. 
#' @param font Font of choice with a limited selection to most base fonts. Defaults to Montserrat.
#' @export

append_dfp_branding <- function(plot_path, annotation_text, font="Montserrat"){
  
  # load in the plot from custom location
  plot <- image_read(plot_path) 
  
  # load in the watermark row & whitespace
  watermark_raw <- image_read(system.file("watermark.png", package = "dfp")) 
  whitespace <- image_read(system.file("whitespace.png", package = "dfp")) 
  
  # format the watermark
  watermark <- watermark_raw %>%
    image_scale("500") %>% 
    image_background("white", flatten = TRUE) %>%
    image_border("white","25x26")
  
  # format the annotation
  annotation <- whitespace %>%
    image_border("white","25x0") %>%
    image_annotate(
      toupper(annotation_text), color = "black", size = 12, 
      gravity = "center", font = font
    )
  
  # merge into bottom border
  bottom_border <- image_append(image_scale(c(watermark, annotation), "x100"))
  
  # create final ploy
  final_plot <- image_append(image_scale(c(plot, bottom_border), "750"), stack = TRUE)
  
  # save final plot
  image_write(final_plot, path = plot_path, format = "png")
}