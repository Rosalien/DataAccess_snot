# Ensemble des fonctions CSS, Javascript et ggplot2 pour la mise en forme de l'appli

googleAnalyticsParameter <- function(...){
'<!-- Global site tag (gtag.js) - Google Analytics -->
        <script async src="https://www.googletagmanager.com/gtag/js?id=UA-143674771-1"></script>
        <script>
        window.dataLayer = window.dataLayer || [];
        function gtag(){dataLayer.push(arguments);}
        gtag(\'js\', new Date());
        gtag(\'set\',\'cookie_expires\',395*24*60*60); //Mesure RGPD : IP anonymisées et cookies réduit à 13mois max de conservation
        gtag(\'config\', \'UA-143674771-1\');
        </script>'
}

# Pour aligner des checkbox sur plusieurs colonnes
# https://stackoverflow.com/questions/36898492/aligning-checkbox-elements-in-many-coumns-in-a-shiny-app
tweaks <- function(...){
".multicol { 
height:auto;
-webkit-column-count: 4;
-moz-column-count: 4;
column-count: 4;
}
div.checkbox {margin-top: 0px;}
div.checkbox {margin-left: 0px;}"
}

# Pour aligner des checkbox sur plusieurs colonnes
# https://stackoverflow.com/questions/42742191/align-checkboxgroupinput-vertically-and-horizontally
tweaks2 <- function(...){
".multicol .shiny-options-group{
                            -webkit-column-count: 4; /* Chrome, Safari, Opera */
                            -moz-column-count: 4;    /* Firefox */
                            column-count: 4;
                            -moz-column-fill: balanced;
                            -column-fill: balanced;
                            }
                            .checkbox{
                            margin-top: 0px !important;
                            -webkit-margin-after: 0px !important; 
                            }"
}

# Pour le chargement de la page
# loading-content
appCSS <- function(x){
x <- "
  #loading-content {
    position: absolute;
    padding: 0 0 0 0;
    background: #FFFFFF;
    opacity: 0.9;
    z-index: 100;
    left: 0;
    right: 0;
    height: 100%;
    text-align: center;
    color: #333;
  }
  "
x
}


#https://rstudio.github.io/dygraphs/gallery-custom-plotters.html
dyMultiColumn <- function(dygraph) { 
      dyPlotter(dygraph = dygraph, 
                name = "MultiColumn", 
                path = system.file("plotters/multicolumn.js", 
                                   package = "dygraphs")) 
    }   

#https://rstudio.github.io/dygraphs/gallery-custom-plotters.html
dyBarChart <- function(dygraph) {
  dyPlotter(dygraph = dygraph,
            name = "BarChart",
            path = system.file("plotters/barchart.js",
                               package = "dygraphs"))
}

# Fonction pour le css des graphiques dy_graph
# From https://github.com/rstudio/dygraphs/issues/227
dyCSScool <- function(dygraph){  
  dygraph$x$css <- '
  .dygraph-legend {
  width: auto !important;
  min-width: 150px;
  color: white;
  background-color: #BABABA !important;
  padding-left:5px;
  border-color:#BABABA;
  border-style:solid;
  border-width:thin;
  transition:0s 4s;
  z-index: 80 !important;
  box-shadow: 2px 2px 5px rgba(0, 0, 0, .3);
  border-radius: 3px;
  }
  
  .dygraph-legend:hover{
  transform: translate(-110%);
  transition: 0s;
  }
  
  .dygraph-legend > span {
  color: black;
  padding-left:5px;
  padding-right:2px;
  margin-left:-5px;
  background-color: white !important;
  display: block;
  }
  
  .dygraph-legend > span:first-child {
  margin-top:2px;
  }
  
  .dygraph-legend > span > span{
  display: inline;
  }
  
  .highlight {
  border-left: 2px solid #BABABA;
  padding-left:3px !important;
  }
  '
  dygraph
}

# https://stackoverflow.com/questions/33867301/dynamic-grid-plots-in-shiny-r
# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                    ncol = cols, nrow = ceiling(numPlots/cols))
  }

 if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

# From http://stackoverflow.com/questions/13649473/add-a-common-legend-for-combined-ggplots
grid_arrange_shared_legend <- function(...,nrow = 1, ncol = length(list(...)), position = c("bottom", "right")) {

  plots <- list(...)
  position <- match.arg(position)
  g <- ggplotGrob(plots[[1]] + theme(legend.position = position))$grobs
  legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
  lheight <- sum(legend$height)
  lwidth <- sum(legend$width)
  gl <- lapply(plots, function(x) x + theme(legend.position = "none"))
  gl <- c(gl, nrow = nrow, ncol = ncol)

  combined <- switch(position,
                     "bottom" = arrangeGrob(do.call(arrangeGrob, gl),
                                            legend,
                                            ncol = 1,
                                            heights = unit.c(unit(1, "npc") - lheight, lheight)),
                     "right" = arrangeGrob(do.call(arrangeGrob, gl),
                                           legend,
                                           ncol = 2,
                                           widths = unit.c(unit(1, "npc") - lwidth, lwidth)))
  grid.newpage()
  grid.draw(combined)
  #return(list(grid.newpage(),grid.draw(combined)))
  return(combined)
}

# Thème ggplot2 pour la représentation des données disponibles
theme_chart <- function(position="bottom",...) {
 theme_minimal() +
  theme(
    text = element_text(family="Lato",color = "#22211d"),#22211d
    axis.line = element_blank(),
    axis.text.x = element_text(size = 9,face = "bold"),
    axis.text.y = element_text(size = 9,face = "bold"),
    axis.ticks = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.minor = element_line(color = "#dbdbd9", size = 0.2),
    panel.grid.major = element_line(color = "#dbdbd9", size = 0.2),#Couleur de la grille. Initialement (#ebebe5)
    plot.background = element_rect(fill = "white", color = NA), 
    panel.background = element_rect(fill = "white", color = NA), 
    legend.background = element_rect(fill = "white", color = NA),
    legend.text=element_text(size = 9,colour = "#22211d"),
    legend.title=element_text(colour = "#22211d",face = "bold",size = 9),
    legend.position = position,
    strip.background =element_rect(fill="#dbdbd9",color="#dbdbd9"),#0071b7
    strip.text = element_text(size = 9,face = "bold")
  )
}

# Thème ggplot2 pour la représentation des boxplots
theme_chart2 <- function(position="bottom",...){
theme_classic()+
theme(
      text = element_text(family="Lato",color = "#22211d",size=12),#22211d
      axis.text.x = element_text(size = 11,face = "bold"),#,face = "bold"),
      axis.text.y = element_text(size = 11,face = "bold"),#,face = "bold"),face = "bold"),
      axis.line.x = element_line(colour = "#22211d", size = 0.7),
      axis.line.y = element_line(colour = "#22211d", size = 0.7),
      plot.title = element_text(size = 14, face = "bold"), 
      axis.title = element_text(face="bold"),
      legend.title=element_text(colour = "#22211d",face = "bold",size = 12),
      legend.position = position,
      strip.background =element_rect(fill="#dbdbd9",color="#dbdbd9"),#0071b7
      strip.text = element_text(size = 10,face = "bold")
    )
}

# Pour la création de la rose des vents
# Selon https://stackoverflow.com/questions/17266780/wind-rose-with-ggplot-r
plot.windrose <- function(data,
                      spd,
                      dir,
                      spdres = 2,
                      dirres = 30,
                      spdmin = 2,
                      spdmax = 20,
                      spdseq = NULL,
                      palette = "YlGnBu",
                      countmax = NA,
                      debug = 0,
                      titreLegend = "",
                      titre=""){


# Look to see what data was passed in to the function
  if (is.numeric(spd) & is.numeric(dir)){
    # assume that we've been given vectors of the speed and direction vectors
    data <- data.frame(spd = spd,
                       dir = dir)
    spd = "spd"
    dir = "dir"
  } else if (exists("data")){
    # Assume that we've been given a data frame, and the name of the speed 
    # and direction columns. This is the format we want for later use.    
  }  

  # Tidy up input data ----
  n.in <- NROW(data)
  dnu <- (is.na(data[[spd]]) | is.na(data[[dir]]))
  data[[spd]][dnu] <- NA
  data[[dir]][dnu] <- NA

  # figure out the wind speed bins ----
  if (missing(spdseq)){
    spdseq <- seq(spdmin,spdmax,spdres)
  } else {
    if (debug >0){
      cat("Using custom speed bins \n")
    }
  }
  # get some information about the number of bins, etc.
  n.spd.seq <- length(spdseq)
  n.colors.in.range <- n.spd.seq - 1

  # create the color map
  spd.colors <- colorRampPalette(brewer.pal(min(max(3,
                                                    n.colors.in.range),
                                                min(9,
                                                    n.colors.in.range)),                                               
                                            palette))(n.colors.in.range)

  if (max(data[[spd]],na.rm = TRUE) > spdmax){    
    spd.breaks <- c(spdseq,
                    max(data[[spd]],na.rm = TRUE))
    spd.labels <- c(paste(c(spdseq[1:n.spd.seq-1]),
                          '-',
                          c(spdseq[2:n.spd.seq])),
                    paste(spdmax,
                          "-",
                          round(max(data[[spd]],na.rm = TRUE),2)))
    spd.colors <- c(spd.colors, "grey50")
  } else{
    spd.breaks <- spdseq
    spd.labels <- paste(c(spdseq[1:n.spd.seq-1]),
                        '-',
                        c(spdseq[2:n.spd.seq]))    
  }
  data$spd.binned <- cut(x = data[[spd]],
                         breaks = spd.breaks,
                         labels = spd.labels,
                         ordered_result = TRUE)
  # clean up the data
  data. <- na.omit(data)

  # figure out the wind direction bins
  dir.breaks <- c(-dirres/2,
                  seq(dirres/2, 360-dirres/2, by = dirres),
                  360+dirres/2)  
  dir.labels <- c(paste(360-dirres/2,"-",dirres/2),
                  paste(seq(dirres/2, 360-3*dirres/2, by = dirres),
                        "-",
                        seq(3*dirres/2, 360-dirres/2, by = dirres)),
                  paste(360-dirres/2,"-",dirres/2))
  # assign each wind direction to a bin
  dir.binned <- cut(data[[dir]],
                    breaks = dir.breaks,
                    ordered_result = TRUE)
  levels(dir.binned) <- dir.labels
  data$dir.binned <- dir.binned

  # Run debug if required ----
  if (debug>0){    
    cat(dir.breaks,"\n")
    cat(dir.labels,"\n")
    cat(levels(dir.binned),"\n")       
  }  

  # deal with change in ordering introduced somewhere around version 2.2
  if(packageVersion("ggplot2") > "2.2"){    
    cat("Hadley broke my code\n")
    data$spd.binned = with(data, factor(spd.binned, levels = rev(levels(spd.binned))))
    spd.colors = rev(spd.colors)
  }

  # create the plot ----
  p.windrose <- ggplot(data = data,
                       aes(x = dir.binned,
                           fill = spd.binned)) +
    geom_bar() + 
    scale_x_discrete(drop = FALSE,
                     labels = waiver()) +
    coord_polar(start = -((dirres/2)/360) * 2*pi) +
    scale_fill_manual(name = titreLegend, 
                      values = spd.colors,
                      drop = FALSE) +
    theme(axis.title.x = element_blank()) +
    labs(title=titre)

  # adjust axes if required
  if (!is.na(countmax)){
    p.windrose <- p.windrose +
      ylim(c(0,countmax))
  }

  # print the plot
  print(p.windrose)  

  # return the handle to the wind rose
  return(p.windrose)
}


# Fonction pour créer la fenêtre de dialogue du téléchargement des données

dataModal <- function(failed = FALSE){
      modalDialog(
        title = "Licence & conditions d'utilisation des données du SNO-T",
        span('Sauf mentions contraires, les données du SNO-T sont diffusées sous'),
        tags$a(href="https://creativecommons.org/licenses/by-sa/4.0/deed.fr", "licence Creative Commons Attribution - Attribution - Partage dans les Mêmes Conditions 4.0 International",target="_blank"),
        tags$br(),tags$br(),
        HTML('<center><a rel="license" href="http://creativecommons.org/licenses/by-sa/4.0/" target="_blank"><img alt="Creative Commons License" style="border-width:0" src="https://i.creativecommons.org/l/by-sa/4.0/88x31.png" /></a></center>'),
        tags$hr(),
        tags$a(href="https://data-snot.cnrs.fr/snot/resources/manual/charte.pdf", "Conditions d'utilisation des données du SNO-T",target="_blank"),
        checkboxInput(ns("readConfirmation"), i18n()$t("J'accepte les conditions d'utilisation"), FALSE),
        footer = tagList(
          modalButton(i18n()$t("Annuler")),
          downloadButton(ns("downloadData"), i18n()$t("Télécharger"))
        ),
        easyClose = TRUE
      )
}