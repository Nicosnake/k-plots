# This code is really good
lnP_plot <- function (in_file, a_title="Write your own title") {
  if (!is.character(a_title)) {
    stop("The title must be a string")
  }
  else if (file.exists(in_file)) {
    packages=c("stringr", "data.table", "tools", "ggplot2")
    lapply(packages, function (x){
      if (!require(x, character.only = TRUE)) {
        installed.packages(x, dependencies = TRUE)
        library(x, character.only = TRUE)
      }
    })
    read.file <- readLines(in_file)
    coll.file <- paste0(read.file, collapse = "\n")
    if (str_detect(string = coll.file, pattern = "(?<=#{10}\\n#\\s)")) {
      #the_title=paste(a_title)
      name_file <- file_path_sans_ext(in_file)
      ext.file <- str_extract(string = coll.file, pattern = "(?<=#{10}\\n#\\s)((.|\\n)*)")
      in_table<-fread(text = ext.file)
      integer_breaks <- function(n = length(in_table$K), ...){ ## function by Joshua Cook at  https://joshuacook.netlify.app/post/integer-values-ggplot-axis/
        fxn <- function(x){
          breaks <- floor(pretty(x, n, ...))
          names(breaks) <- attr(breaks, "labels")
          breaks
        }
        return(fxn)
      }
      mean_vec <- in_table$`Mean LnP(K)` # This can be another function??
      SD_vec <- in_table$`Stdev LnP(K)`
      SD_sum <- mean_vec + SD_vec
      SD_diff <- mean_vec - SD_vec
      max_SD <- max(c(SD_sum, SD_diff))
      min_SD <- min(c(SD_sum, SD_diff))
      prob_plot<-ggplot(in_table, aes(x=as.integer(K), y=`Mean LnP(K)`))  + 
        ylim(range(pretty(c(min_SD, max_SD)))) +
        geom_line(linetype="solid", colour = "blue") +
        geom_errorbar(aes(ymin=`Mean LnP(K)`-`Stdev LnP(K)`, ymax=`Mean LnP(K)`+`Stdev LnP(K)`), 
                      colour = "red", width = .1) +
        geom_point(shape=21, size=2, color = "red", fill = "white" ) +
        scale_x_continuous(breaks = integer_breaks()) +
        labs(x = "K", y = "Mean ln P(Data|K)", title = paste(a_title)) +
        theme(panel.background = element_blank(),  
              panel.grid.major.x = element_blank(), 
              panel.grid.major.y = element_line(size=.1, color="black"),
              axis.line = element_line(size = .5, colour = "black"),
              axis.title.x = element_text(face = "italic"),
              plot.title = element_text(hjust = 0.5))
    } else {
      stop("Not an appropriate file")
    }
  } else {
    stop("The file does not exist")
  }
  return(prob_plot)
}
