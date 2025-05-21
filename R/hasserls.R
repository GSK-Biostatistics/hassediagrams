#-------------------------------------------------------------------------------
#' @encoding UTF-8
#' 
#' @title Hasse diagram of the restricted layout structure
#' @description Returns a Hasse diagram of the restricted layout structure of an experimental design
#'
#' @param object An object of class \code{"rls"}. The function \code{\link[hassediagrams]{itemlist}} generates the class \code{"rls"} object. 
#' Printing the "\code{"rls"} object will give a list of structural objects (that define the layout structure) to aid in 
#' defining the randomisation objects in the restricted layout structure. 
#' @param randomisation.objects This argument takes the format of the \code{TransferObject} item from the class \code{"rls"} object. 
#' The first column contains the names of all the structural objects in the layout structure (automatically generated) and the 
#' second column contains the corresponding randomisation objects in the restricted layout structure (manually generated). 
#' To begin with, the function \code{\link[hassediagrams]{itemlist}} should be run to generate the class \code{"rls"} object. 
#' The user will then need to edit the second column of the \code{TransferObject} matrix to define the randomisation objects 
#' that appear in the restricted layout structure. Structural objects that do not occur in the restricted layout structure must be 
#' left as "NULL" in the second column. The names specified in the second column represent the labels of the randomisation objects 
#' on the Hasse diagram of the restricted layout structure.
#' @param random.arrows A matrix of two columns that takes integer entries. The entries in the first column contain the object(s) 
#' at the start of the randomisation arrow and the second column contains the object(s) at the end. 
#' The values correspond to the entry number for the randomisation object in the \code{TransferObject} 
#' item from the class \code{"rls"} object). The first column specifies the randomisation object(s) at the beginning of the 
#' randomisation arrow(s) and the second column specifies the randomisation object(s) at the end of the arrow. 
#' The randomisation arrows must point downwards, hence, in each row of the matrix the second column entry must be larger than the first column entry. 
#' The randomisation object(s) involved in the randomisation arrow(s) must first be specified in the randomisation.objects argument.
#' @param showRLS logical. If "N" then generation of the Hasse diagram of the restricted layout structure is suppressed. The default is "Y".
#' @param showpartialRLS logical. If "N" then the partial crossing between randomisation objects (using dotted connecting lines) is not illustrated  
#' on the Hasse diagram of the restricted layout structure. The default is "Y".
#' @param showdfRLS logical. If "N" then the randomisation object label is not displayed on the Hasse diagram of the restricted layout structure. The default is "Y".
#' @param showrandRLS logical. If "N" then the randomisations are not illustrated (using arrows) on the Hasse diagram of the restricted layout structure. 
#' The default is "Y". If \code{random.arrows=NULL}, then \code{showrandRLS} defaults to "N".
#' @param check.confound.df logical. If "N" then the check for confounded degrees of freedom is not performed. The default is "Y".
#' @param maxlevels.df logical. If "N" then the potential maximum number of levels of a generalised factor is removed from the randomisation object 
#' label on the Hasse diagram of the restricted layout structure. The default is "Y".
#' @param table.out logical. If "Y" then a table that shows the relationships between the randomisation objects in the restricted layout structure is printed. 
#' The default is "N".
#' @param equation.out logical. If "Y" then a recommended mixed model to use in the statistical analysis is printed. The default is "N".
#' @param pdf logical. If "Y" then a pdf file containing the Hasse diagram of the restricted layout structure is generated. 
#' The default is "N", i.e., a pdf file is not generated.
#' @param example character. Filename for the pdf output file containing the Hasse diagram. The default is set to "example".
#' @param outdir character. Location of the pdf output file if \code{pdf="Y"}. The default is set to \code{NULL} and in this case the pdf output file 
#' containing the Hasse diagram will be stored in the working directory of the user's R session (current working directory).
#' @param hasse.font character. The name of the font family used for all text included on the Hasse diagram. 
#' Standard and safe font families to choose are "sans", "serif", and "mono". 
#' If any of the labels of the randomisation objects (as defined in the second column of \code{randomisation.objects} matrix) 
#' contain Unicode characters, a Unicode friendly font family should be selected. 
#' For more details on Unicode friendly family options see the Details section. 
#' If the font family selected fails to render, the font is automatically changed to "sans" instead.
#' The default is "sans".
#' @param produceBWPlot logical. If "Y" then the Hasse diagram will be generated in black and white format. 
#' The default is set to "N", i.e., a coloured version of the plot is produced.
#' @param structural.colour character. The colour of the structural lines that connect randomisation objects on the Hasse diagram. The default colour is "grey".
#' @param structural.width numeric. The width of the structural lines on the Hasse diagram. The default width is 2.
#' @param partial.colour character. The colour of the partial crossing dotted lines of the connecting randomisation objects on the Hasse diagram. 
#' The default colour is "orange".
#' @param partial.width numeric. The width of the partial crossing dotted lines on the Hasse diagram. The default width is 1.5.
#' @param objects.colour character. The colour of the labels of the randomisation objects on the Hasse diagram. The default colour is "mediumblue".
#' @param df.colour character. The colour of the degrees of the freedom labels on the Hasse diagram. The default colour is "red".
#' @param arrow.colour character. The colour of the randomisation arrows on the Hasse diagram. The default colour is "mediumblue".
#' @param arrow.width numeric. The randomisation arrows width on the Hasse diagram. The default width is 1.5.
#' @param arrow.pos numeric. Specifies the position of the randomisation arrows, i.e., how far the randomisation arrows will be from 
#' the objects they point at. The default is 7.5. A smaller number specifies longer arrows and a higher number specifies shorter arrows.
#' @param larger.fontlabelmultiplier numeric. The large font multiplier is the multiplier for the font used for the labels of objects on the 
#' Hasse diagram where there are four or less objects at that level in the diagram. The default is 1.
#' @param middle.fontlabelmultiplier numeric. The medium font multiplier is the multiplier for the font used for the labels of objects on the 
#' Hasse diagram involving a factor that is equivalent to a generalised factor. The default is 1.
#' @param smaller.fontlabelmultiplier numeric. The small font multiplier is the multiplier for the font used for the labels of objects on the 
#' Hasse diagram where there are five or more objects at that level of the diagram. The default is 1.
#'
#' @return The function \code{\link[hassediagrams]{hasserls}} returns:
#' 1. The Hasse diagram of the restricted layout structure (if \code{showRLS = "Y"}).
#' 
#' 2. The restricted layout structure table shows the relationships between the randomisation objects in the restricted layout structure 
#' (if \code{table.out="Y"}). The individual entries in the table consist of blanks on the main diagonal and 0’s, (0)’s or 1’s elsewhere. 
#' If the factor (or generalised factor) corresponding to the ith row and the factor (or generalised factor) corresponding to the jth column are fully crossed, 
#' then a 0 is entered in the (i,j)th entry in the table. If these factors (or generalised factors) are partially crossed, or the ith row factor 
#' (or generalised factor) only has one level and nests the jth column factor (or generalised factor), then the (i,j)th entry is (0). 
#' If the ith row factor (or generalised factor) is nested within the jth column factor (or generalised factor), then a 1 is entered in the 
#' (i,j)th entry. If two factors (or generalised factor) are equivalent, then they share a single row and column in the table, 
#' where the row and column headers include both factor (or generalised factor) names, separated by an "=" sign.
#' 
#' 3. An equation that suggests the mixed model to be fitted (if \code{equation.out="Y"}).
#' 
#' 4. If there are confounded degrees of freedom, a table of the structural objects and a description of the associated degrees of freedom is printed.
#'
#' @details
#' The \code{\link[hassediagrams]{hasserls}} function generates the Hasse diagram of the restricted layout structure. 
#' The Hasse diagram consists of a set of randomisation objects, corresponding to the factors and generalised factors, 
#' and the relationships between the objects (either crossed, nested, partially crossed or equivalent), 
#' as defined by the structure of the experimental design and the randomisation performed, see Bate and Chatfield (2016b).
#' 
#' The function requires an object of class \code{"rls"} containing the structural objects in the layout structure.
#' 
#' Where present, two partially crossed factors are illustrated on the diagram with a dotted line connecting them. 
#' This feature can be excluded using the \code{showpartialRLS} option.
#' 
#' The maximum number of possible levels of each generalised factor, along with the actual number present in the design 
#' and the "skeleton ANOVA" degrees of freedom, can be included in the randomisation object label on the Hasse diagram.
#'
#' The randomisation arrows that illustrate the randomisation performed can be included on the Hasse diagram.
#' 
#' The \code{\link[hassediagrams]{hasserls}} function evaluates the design in order to identify if there are any 
#' confounded degrees of freedom across the design. It is not recommended to perform this evaluation for large designs, 
#' due to the potential high computational cost. This can be controlled using the \code{check.confound.df = "N"} option. 
#' 
#' Objects that contain Unicode characters, e.g., u2192 or u2297 must be handled by Unicode friendly font families. Common font families that work with Unicode characters are: 
#' for Windows: Cambria, Embrima, Segoe UI Symbol, Arial Unicode MS, and 
#' for macOS: AppleMyungjo, .SF Compact Rounded, Arial Unicode MS, .SF Compact, .SF NS Rounded.
#' The aforementioned fonts may not not be available in your R session. The available system fonts can be printed by systemfonts::system_fonts()$family.
#' System available fonts can be imported by running showtext::font_import() or extrafont::font_import().
#' To check which fonts have been successfully imported, run showtext::fonts() or extrafont::fonts().
#' The Arial Unicode MS font can be downloaded from online sources.
#' The Noto Sans Math font can be installed using sysfonts::font_add_google("Noto Sans Math").
#' 
#' @author
#' Damianos Michaelides, Simon Bate, and Marion Chatfield
#'
#' @references
#' Bate, S.T. and Chatfield, M.J. (2016a), Identifying the structure of the experimental design. Journal of Quality Technology, 48, 343-364.
#' 
#' Bate, S.T. and Chatfield, M.J. (2016b), Using the structure of the experimental design and the randomization to construct a mixed model. Journal of Quality Technology, 48, 365-387.
#' 
#' Box, G.E.P., Hunter, J.S., and Hunter, W.G., (1978), Statistics for Experimenters. Wiley.
#' 
#' Joshi, D.D. (1987), Linear Estimation and Design of Experiments. Wiley Eastern, New Delhi.
#' 
#' Williams, E.R., Matheson, A.C. and Harwood, C.E. (2002), Experimental design and analysis for tree improvement. 2nd edition. CSIRO, Melbourne, Australia.
#'
#' @export
#' 
#' @importFrom stats anova aov as.formula model.matrix.default
#' 
#' @examples
#' \dontrun{
#' #' ## In some of the examples, we use encoding "u2297" to print the tensor product symbol
#' ## and "u2192" to print a right arrow symbol. See the Details section and the hasse.font
#' argument in the documentation to find out about fonts that are Unicode friendly.
#' 
#' ## Examples using the package build-in data concrete, dental, human, analytical.
#' 
#' 
#' ## A fractional factorial design for investigating asphalt concrete production
#' 
#' # Generate objects required to create the Hasse diagram of the restricted layout structure 
#' concrete_objects <- itemlist(datadesign=concrete) 
#' print(concrete_objects)
#' 
#' # Define the names of the randomisation objects corresponding to the structural objects
#' concrete_rls <- concrete_objects$TransferObject 
#' concrete_rls[,2] <- concrete_rls[,1]
#' concrete_rls[27,2] <- c("AC^AG^CC^CoT^CuT \u2192 Run")
#' 
#' 
#' hasserls(object=concrete_objects, randomisation.objects=concrete_rls, 
#'          larger.fontlabelmultiplier=1.6, smaller.fontlabelmultiplier=1.3,
#'          table.out="Y", arrow.pos=8, hasse.font="Cambria")
#' 
#' 
#' ## A crossover design for a dental study
#' 
#' # Generate objects required to create the Hasse diagram of the restricted layout structure 
#' dental_objects <- itemlist(datadesign=dental, randomfacsid=c(0,1,0,0,0))
#' summary(dental_objects)
#' 
#' # Define the names of the randomisation objects corresponding to the structural objects
#' dental_rand_arrows <- matrix(c(3, 5, 4, 7), ncol=2, byrow=TRUE) 
#' dental_rls <- dental_objects$TransferObject 
#' dental_rls[c(1:5,7,8), 2] <- c("Mean", "Period", "Sequence", 
#'                                "Treatment", "Subject[Sequence]", 
#'                                "Period \u2297 Sequence", "Observation")
#' 
#' hasserls(object=dental_objects, randomisation.objects=dental_rls, 
#'          random.arrows=dental_rand_arrows, larger.fontlabelmultiplier=1.6,
#'          table.out="Y", equation.out="Y", arrow.pos=15,
#'          hasse.font="Embrima")
#' 
#' 
#' ## A block design for an experiment assessing human-computer interaction
#' 
#' # Generate objects required to create the Hasse diagram of the restricted layout structure 
#' human_objects <- itemlist(datadesign=human, randomfacsid=c(1,1,0,0,0,0,1))
#' print(human_objects)
#' 
#' # Define the names of the randomisation objects corresponding 
#' # to the structural objects
#' 
#' human_rand_arrows <- matrix(c(3, 12, 6, 13), ncol=2, byrow=TRUE) 
#' human_rls <- human_objects$TransferObject 
#' 
#' human_rls[, 2] <- c("Mean", "Day", "Method", "Period", "Room", "Sequence", 
#'                     "NULL", "Subject | Subject \u2192 Day \u2297 Sequence", 
#'                     "NULL", "NULL", "NULL", "Day \u2297 Room", 
#'                     "Period \u2297 Room",  "NULL", 
#'                     "Test = Day^Method^Period^Room^Sequence")
#' 
#' hasserls(object=human_objects, randomisation.objects=human_rls, 
#'          random.arrows=human_rand_arrows, 
#'          larger.fontlabelmultiplier=1.4,
#'          hasse.font="AppleMyungjo")
#'          
#'          
#' ## A cross-nested design for an analytical method investigation
#' 
#' # Generate objects required to create the Hasse diagram of 
#' # the restricted layout structure 
#' analytical_objects <- itemlist(datadesign=analytical, 
#'                                randomfacsid=c(0,0,1,1,1,0,0,0))
#' summary(analytical_objects)
#' 
#' # Define the names of the randomisation objects corresponding to the structural objects
#' analytical_rand_arrows <- matrix(c(2, 19, 19, 20), ncol=2, byrow=TRUE) 
#' analytical_rls <- analytical_objects$TransferObject 
#' analytical_rls[,2] <- c("Mean", "Batch", "Site", 
#'                         "Analyst[Site]", "Column[Site]", 
#'                         "System[Site]", "NULL", 
#'                         "{Analyst^Column}[Site]", 
#'                         "{Analyst^System}[Site]", "NULL", 
#'                         "{Column^System}[Site]", 
#'                         "NULL", "NULL", 
#'                         "{Analyst^Column^System}[Site] \u2192 Run",
#'                         "NULL", "NULL", "NULL", "NULL", 
#'                         "Preparation[Run]", "Injection[Run]") 
#' 
#' hasserls(object=analytical_objects, randomisation.objects=analytical_rls, 
#'          random.arrows=analytical_rand_arrows, showpartialRLS="N", 
#'          check.confound.df="N", larger.fontlabelmultiplier=1, 
#'          smaller.fontlabelmultiplier=1.6, hasse.font="Segoe UI Symbol")
#' 
#' 
#' 
#' ## Examples using data from the dae package (conditionally loaded)
#' 
#' if (requireNamespace("dae", quietly = TRUE)) {
#'   
#' ## Data for a balanced incomplete block experiment, Joshi (1987)
#'   
#'   data(BIBDWheat.dat, package = "dae")
#'   
#'   # remove the response from the dataset
#'   BIBDWheat <- BIBDWheat.dat[, -4]
#'   
#'   # make plots 1:30 to index the rows of the design
#'   BIBDWheat$Plots <- c(1:30)
#'   
#'   BIBDWheat_objects <- itemlist(datadesign=BIBDWheat)
#'   print(BIBDWheat_objects)
#'   
#'   IBDWheat_rand_arrows <- matrix(c(3,4), ncol=2, byrow=TRUE) 
#'   IBDWheat_rls <- BIBDWheat_objects$TransferObject 
#'   IBDWheat_rls[c(1:4), 2] <- c("Mean", "Blocks", "Varieties", "Plot[Block]")
#'   
#'   hasserls(object=BIBDWheat_objects, randomisation.objects=IBDWheat_rls, 
#'            random.arrows=IBDWheat_rand_arrows, equation.out="Y")
#'   
#' 
#'   ## Data for an un-replicated 2^4 factorial experiment to investigate 
#'   ## a chemical process from Table 10.6 of Box, Hunter and Hunter (1978)
#'   
#'  data(Fac4Proc.dat, package = "dae")
#'   
#'   # remove the response from the dataset
#'   Fac4Proc <- Fac4Proc.dat[, -6]
#'   
#'   Fac4Proc_objects <- itemlist(datadesign=Fac4Proc)
#'   summary(Fac4Proc_objects)
#'   
#'   Fac4Proc_rls <- Fac4Proc_objects$TransferObject 
#'   Fac4Proc_rls[,2] <- Fac4Proc_rls[,1]
#'   Fac4Proc_rls[16,2] <- c("Catal^Conc^Press^Temp \u2192 Run")
#'   
#'   hasserls(object=Fac4Proc_objects, randomisation.objects=Fac4Proc_rls, 
#'            showpartialRLS="N", table.out="Y", 
#'            smaller.fontlabelmultiplier = 2,
#'            hasse.font="Noto Sans Math")
#' 
#' 
#'   ## Data for an experiment with rows and columns from p.144 of 
#'   ## Williams, Matheson and Harwood (2002)
#'   
#'   data(Casuarina.dat, package = "dae")
#'   
#'   # remove the response from the dataset
#'   Casuarina <- Casuarina.dat[, -7]
#'
#'   # create unique factor level labels
#'   Casuarina$Row <- paste(Casuarina$Reps, Casuarina$Rows)
#'   Casuarina$Col <- paste(Casuarina$Reps, Casuarina$Columns)
#'   Casuarina <- Casuarina[, -c(2,3)]
#'   
#'   Casuarina_objects <- itemlist(datadesign=Casuarina, 
#'                                 randomfacsid=c(1,0,1,0,0,0))
#'   print(Casuarina_objects)
  
#'   # It is decided to include Countries and Countries^Innoc in the
#'   # restricted layout structure even though they are not involved
#'   # in the randomisation.
#'   Casuarina_rand_objects <- c(1:6, 9, 13)
#'   Casuarina_rand_arrows <- matrix(c(3, 5 , 4, 13), ncol=2, byrow=TRUE) 
#'   
#'   Casuarina_rls <- Casuarina_objects$TransferObject
#'   Casuarina_rls[Casuarina_rand_objects,2] <- c("Mean", "Countries", 
#'                                                "InocTime", "Provences", 
#'                                                "Reps", "Countries^InocTime", 
#'                                                "Inoc^Provences", 
#'                                                "{Row \u2297 Col}[Rep]")
#'   
#'   hasserls(object=Casuarina_objects, randomisation.objects=Casuarina_rls,
#'            random.arrows=Casuarina_rand_arrows, 
#'            check.confound.df="N", showpartialRLS="N", 
#'            arrow.pos=10, smaller.fontlabelmultiplier=1.5,
#'            hasse.font="Arial Unicode MS")
#'   
#' } else {
#'   message("Examples using data from the 'dae' package 
#'   require 'dae' to be installed.")
#' }
#' }
#' 

hasserls <- function(object,
                     randomisation.objects,
                     random.arrows = NULL,
                     showRLS = "Y",
                     showpartialRLS = "Y",
                     showdfRLS = "Y",
                     showrandRLS = "Y",
                     check.confound.df = "Y",
                     maxlevels.df = "Y",
                     table.out = "N",
                     equation.out = "N",
                     pdf = "N",
                     example = "example",
                     outdir = NULL,
                     hasse.font = "sans",
                     produceBWPlot = "N",
                     structural.colour = "grey",
                     structural.width = 2,
                     partial.colour = "orange",
                     partial.width = 1.5,
                     objects.colour = "mediumblue",
                     df.colour = "red",
                     arrow.colour = "mediumblue",
                     arrow.width = 1.5,
                     arrow.pos = 7.5,
                     larger.fontlabelmultiplier = 1,   
                     middle.fontlabelmultiplier = 1,   
                     smaller.fontlabelmultiplier = 1) {
  
  contains_symbols <- grepl("\u2297", randomisation.objects, fixed = TRUE) | grepl("\u2192", randomisation.objects, fixed = TRUE)
  
  if (any(contains_symbols==TRUE)) {
    #fonts <- systemfonts::system_fonts()
    #fontlist <- unic_fonts[unic_fonts %in% fonts]
    warning("The randomisation.objects argument contains Unicode characters, either '\u2297' or '\u2192'. \nThe hasse.font argument must be set to a Unicode friendly font family, \notherwise the Hasse diagram may be misleading, i.e., squares or question marks instead of the requested Unicode symbols. \nFor more details on Unicode friendly family options see the Details section in the documentation.")
  }
  
  
  if (showpartialRLS=="Y" || showdfRLS=="Y"|| showrandRLS=="Y") showRLS <- "Y"
  
  if(is.null(outdir)) {
    data.folder.location <- getwd()
  } else {
    data.folder.location <- outdir
  }
  
  if(pdf=="Y") {
    edgewidth <- structural.width
    dottedline <- 2
    cairo_pdf(filename=paste(data.folder.location, "/", example,"_output.pdf",sep=""), width=11, height=8)
  } else {
    edgewidth <- structural.width
    dottedline <- 2
  }
  
  if (!(table.out %in% c("Y", "N"))) stop("table.out should be Y or N")
  if (!(equation.out %in% c("Y", "N"))) stop("equation.out should be Y or N")
  if (any(random.arrows == 1)) stop("Randomisation arrows should not involve the Mean (should not contain entry 1)")
  
  ceffects.table.final.brief <- object$finalstructure
  finaleffects <- object$finaleffects
  rnames.effects <- object$finaleffectsnames
  finaleffectrandom <- object$finalrandomeffects 
  confound.tab <- object$confoundings
  datadesign <- object$design
  maxfacs <- object$nfactors
  outputlistip1 <- object$outputlistip1
  finalnames.effects <- object$finaleffectsnames
  nested.names <- object$nestednames
  transobject <- object$TransferObject
  
  noall <- length(colnames(outputlistip1$designi))
  
  nrto <- nrow(transobject) 
  ncto <- ncol(transobject)
  
  nreff <- nrow(randomisation.objects) 
  nceff <- ncol(randomisation.objects)
  
  if (randomisation.objects[nreff, 2] == "NULL") {
    randomisation.objects[nreff, 2] <- "Obs Unit"
  } else {
    randomisation.objects[nreff, 2] <- randomisation.objects[nreff, 2]
  }
  
  
  if (nrto != nreff) stop("The number of rows in randomisation.objects should be the same as
                          the number of rows of the TransferObject from the RLS.object 
                          function. If you have manually deleted rows from TransferObject when 
                          creating the randomisation.objects matrix, you should retain them but 
                          leave the RLS effects entry as NULL.")
  
  if (ncto != nceff) stop("The number of columns in randomisation.objects should be as the number
                        of columns of the TransferObject item from the RLS.object function")
  
  rlseff1 <- as.vector(randomisation.objects[,1])
  
  if (!identical(rnames.effects, rlseff1)) {
    randomisation.objects[,1] <- rnames.effects
    warning(" The first column in randomisation.objects has to be the same as the first column of
              the TransferObject item from the RLS.object function.
              Due to the differences between them, the first column in randomisation.objects was 
              replaced with the first column of the TransferObject item from the RLS.object function. 
              If the rows of TransferObject have been manually re-ordered, this will not generate the
              correct Hasse diagram and the original order should be retained.")
  }
  
  fixed <- which(as.vector(randomisation.objects[,2]) != "NULL")
  briefrename <- as.vector(randomisation.objects[fixed,2])
  
  if (!is.null(random.arrows)) {
    if( !( all(random.arrows %in% fixed) ) ) stop("There must not be objects that define randomisation arrows which are NULL in the randomisation.objects matrix")
  } 
  
  if (is.null(random.arrows)) {
    showrandRLS <- "N"
  }
  
  Colourblue<-"mediumblue"
  Colourred<-"red"
  Colourpurple<-"purple"
  Colourorange<-"orange"
  
  if (produceBWPlot == "Y") {	
    Colourblue <- "black"
    Colourred <- "black"
    Colourpurple <- "black"
    Colourorange <- "black"
    structural.colour <- "black"
    partial.colour <- "black"
    objects.colour="black"
    df.colour="black"
    arrow.colour="black"
  }
  
  if (showRLS=="Y") {
    
    finaldesign.effects <- rownames(ceffects.table.final.brief)[fixed]
    brief.finaldesign.effects <- as.vector(colnames(ceffects.table.final.brief)[fixed])
    
    finalnames.wrong <- "N" 
    if (sum(finaldesign.effects %in% rownames(ceffects.table.final.brief)) != length(finaldesign.effects)) {
      cat("\n The selected final terms  ", finaldesign.effects[!(finaldesign.effects %in% rownames(ceffects.table.final.brief))], "is not in the terms
          in the layout structure  \n", rownames(ceffects.table.final.brief))
      finalnames.wrong <- "Y"
    }
    
    if (!is.null(random.arrows)) {
      nr.row <- nrow(random.arrows)
      nr.col <- ncol(random.arrows)
      randomised <- matrix(0, nrow=nr.row, ncol=nr.col)
      for (r in 1:nr.row) {
        for(cc in 1:nr.col) {
          randomised[r,cc] <- rownames(ceffects.table.final.brief)[random.arrows[r,cc]]
        }
      }
    } else{
      randomised <- NULL
    }

    if ((!is.null(randomised))&&(sum(randomised %in% rownames(ceffects.table.final.brief)) != length(randomised))) { 
      cat("\n The selected terms indicating the randomisation  ", randomised[!(randomised %in% rownames(ceffects.table.final.brief))], "is not in the
          objects in the layout structure  \n", rownames(ceffects.table.final.brief))
      finalnames.wrong <- "Y"
    }
    if (finalnames.wrong=="Y") {
      if (pdf=="Y") dev.off()
    }
    
    finalceffects.table.final.brief <- ceffects.table.final.brief[finaldesign.effects,brief.finaldesign.effects]
    
    #select final effects in final design structure
    finalfinaleffects <- finaleffects[finalnames.effects %in% finaldesign.effects]
    
    if (table.out == "Y") {
      cat("\nThe following table shows the relationships between the randomisation objects in the Restricted Layout Structure\n")
      print(finalceffects.table.final.brief)
    } 
    
    names(finalfinaleffects)<- finalnames.effects[finalnames.effects %in% finaldesign.effects]
    
    if (!is.null(random.arrows)) {
      nr.row <- nrow(random.arrows)
      nr.col <- ncol(random.arrows)
      for (aa in 1:nr.row) {
        raa1 <- randomised[aa,1]
        raa2 <- randomised[aa,2]
        level.raa1 <- as.numeric(finalfinaleffects[raa1])
        level.raa2 <- as.numeric(finalfinaleffects[raa2])
        if( identical(level.raa1, level.raa2) ) {
          stop("The two randomisation objects at either end of a randomisation arrow
        cannot be at the same level unless the two objects are equivalent. 
        Make sure that the two entries of the same row in the random.arrows argument
        are not at the same level.")
        }
        
        rbb1 <- random.arrows[aa,1]
        rbb2 <- random.arrows[aa,2]
        if(rbb1 > rbb2) {
          random.arrows[aa,1] <- rbb2
          random.arrows[aa,2] <- rbb1
          warning("In the Hasse diagram the randomisation arrows must point downwards rather than upwards. 
          Such entries in the random.arrows argument have been switched automatically.")
          randomised[aa,1] <- raa2
          randomised[aa,2] <- raa1
        }
      }
    }
    
    
    #Check that nested effects are random
    names(finaleffectrandom)<- finalnames.effects
    typenested <- cbind(finaleffectrandom[names(finalfinaleffects)][names(finalfinaleffects) %in% nested.names[ ,2]],nested.names[ ,2])       
    if(ncol(typenested) > 1) {
      fixednested <- typenested[typenested[ ,1]==0,2]
    } else {
      fixednested <- 0
    }
    
    
    # NEEDS ADAPTING FOR MIXED MODEL CURRENTLY ANOVA  
    #Constructs the right hand side of equation terms with as.factor in front and replacing nested and * parts of effects with :
    model.equation.final <- model.equation.fun(model.effects.fun(rownames(finalceffects.table.final.brief)))
    if (equation.out == "Y") {
      cat("\nThe suggested mixed model to be fitted is: \n", substring(model.equation.final,6))
    }
    
    #Need a matrix indicating which effects are to be linked by lines (indicated by 1s in the matrix)
    #First treat partially crossed and fully crossed as the same
    fadjm <- matrix(NA,nrow=nrow(finalceffects.table.final.brief),ncol=ncol(finalceffects.table.final.brief),dimnames=dimnames(finalceffects.table.final.brief))
    
    colnames(fadjm) <- briefrename

    for (i in 1:nrow(finalceffects.table.final.brief)) {
      for (j in 1:ncol(finalceffects.table.final.brief)) {
        if (finalceffects.table.final.brief[i,j]=="1") fadjm[i,j] <- 1
        if (finalceffects.table.final.brief[i,j]=="0") fadjm[i,j] <- 0
        if (finalceffects.table.final.brief[i,j]=="(0)") fadjm[i,j] <- 0
        if (finalceffects.table.final.brief[i,j]==" ") fadjm[i,j] <- 0
      }
    }
    
    #Then remove lines which link to nested effects lower in the design e.g. A links to A*B and A*B links to A*B*C but A does not link to A*B*C in diagram.
    fadjm.adjust <- fadjm
    for (j in 1:ncol(fadjm)) {
      for (i in 1:nrow(fadjm)) {
        if (fadjm.adjust[i,j]==1) {
          for (k in 1:nrow(fadjm)) {
            fadjm.adjust[k,j] <- max(fadjm.adjust[k,j]-fadjm[k,i],0)
          }
        }
      }
    }
    
    #This adds dummy variables so that whole width of plotting space is used
    fadjm.adjust <- rbind(aaadum1=c(0), Mean=fadjm.adjust[1, ], zzzdum2=c(0), fadjm.adjust[-1, ])
    fadjm.adjust <- cbind(aaadum1=c(0), Mean=fadjm.adjust[ ,1], zzzdum2=c(0), fadjm.adjust[ ,-1])
    
    #This removes a line if it is to be drawn as a randomised arrow  
    if (showrandRLS=="Y") {  
      randomisedmat <- matrix(0,nrow=nrow(fadjm.adjust),ncol=ncol(fadjm.adjust),dimnames=list(rownames(fadjm.adjust),rownames(fadjm.adjust)))
      if (!is.null(randomised)) {
        for (i in 1:nrow(randomised)) {
          randomisedmat[randomised[i,2],randomised[i,1]] <- 1
        }
      }
      
      colnames(randomisedmat) <- colnames(fadjm.adjust) 
      
      if (!is.null(briefrename)) {
        colnames(randomisedmat)[c(-1,-3)] <- briefrename
      }       
      
      fadjm.adjust <- fadjm.adjust - randomisedmat
    } 
    
    # For plot to work need to reverse the order of effects                      
    fadjm.reverse <- fadjm.adjust[nrow(fadjm.adjust):1,ncol(fadjm.adjust):1]
    
    if ( any(fadjm.reverse < 0) ) {
      fadjm.reverse[fadjm.reverse < 0] <- 0
      warning("One or more of the arrows do not correspond to structural lines. You may want to check that your randomisation arrows are defined correctly.")
    }
    
    fg1 <- graph_from_adjacency_matrix(fadjm.reverse, mode="undirected")
    
    fg <- simplify(fg1)
    V(fg)$label <- V(fg)$name
    
    #This section calculates the coordinates for the vertices of the Hasse diagram  
    dscoords <- dscoords.fun(DStype="RLS", feffects=finalfinaleffects, ceffects.table.fb=finalceffects.table.final.brief, 
                             larger.fontlabelmultiplier, smaller.fontlabelmultiplier, middle.fontlabelmultiplier)  
    fg$layout <- dscoords$coords
    
    font_used <- hasse.font
    vertex.label.font <- rep(2,length(colnames(fadjm.reverse)))
    vertex.label.color.objects <- c(rep(objects.colour,length(colnames(fadjm.reverse))-3),"transparent",objects.colour,"transparent")
    vertex.label.color.black <- c(rep("black",length(colnames(fadjm.reverse))-3),"transparent","black","transparent")
    vertex.label.color.df <- c(rep(df.colour,length(colnames(fadjm.reverse))-3),"transparent",df.colour,"transparent")
    
    # Set up plot for underlining random effects
    # Default assumes that interaction of two fixed effects is fixed - should allow user to modify
    # Put list identifying random effects in reverse order
    finalfinaleffectrandom <- finaleffectrandom[finalnames.effects %in% finaldesign.effects]
    finalfinaleffectrandom.reverse <- c(finalfinaleffectrandom[length(finalfinaleffectrandom):1],0,0)
    
    fadjm.reverse.blank <- fadjm.reverse
    #Replace characters by underscores to produce underlines
    for (m in 1:length(colnames(fadjm.reverse.blank))) {
      if (finalfinaleffectrandom.reverse[m]==1) {
        colnames(fadjm.reverse.blank)[m] <- paste("",paste(rep("_",nchar(colnames(fadjm.reverse.blank)[m])),collapse=""))
      } else {
        colnames(fadjm.reverse.blank)[m] <- ""}
    }
    
    fg2 <- graph_from_adjacency_matrix(fadjm.reverse.blank,mode="undirected")
    
    fg2a <- simplify(fg2)
    V(fg2a)$label <- V(fg2a)$name
    fg2a$layout <- dscoords$coords
    vcount(fg2a) 
    fg2a.edges <- get.edges(fg2a,1:ecount(fg2a))[ ,1]-1   
    node.dumg <- c(vcount(fg2a)-3,vcount(fg2a)-1) 
    edge.color<-rep(structural.colour,length(fg2a.edges))
    edge.color[fg2a.edges %in% node.dumg]<-"transparent"
    par(mar=c((2*(max(larger.fontlabelmultiplier,smaller.fontlabelmultiplier)-1)+1)*0.8, (5*(max(larger.fontlabelmultiplier,smaller.fontlabelmultiplier)-1)+1)*0.4, 0.2, (5*(max(larger.fontlabelmultiplier,smaller.fontlabelmultiplier)-1)+1)*0.4))     

    tryCatch({
      plot(fg2a, asp=FALSE, add=F,vertex.label.color=vertex.label.color.black, vertex.label.cex=dscoords$textlabel.size, vertex.label.font=vertex.label.font, vertex.label.degree=pi/2, vertex.label.dist=0.4, vertex.size=5, vertex.color="transparent", vertex.shape="circle", vertex.frame.color="white", edge.color=edge.color, edge.width = edgewidth, vertex.label.family=font_used)
    }, error = function(e) {
      message("The font selected in hasse.font is not available in the system's fonts and rendering failed. See the Details section for more information on fonts. The hasse.font is set to 'sans' instead.")
      font_used <<- "sans"
      plot(fg2a, asp=FALSE, add=F,vertex.label.color=vertex.label.color.black, vertex.label.cex=dscoords$textlabel.size, vertex.label.font=vertex.label.font, vertex.label.degree=pi/2, vertex.label.dist=0.4, vertex.size=5, vertex.color="transparent", vertex.shape="circle", vertex.frame.color="white", edge.color=edge.color, edge.width = edgewidth, vertex.label.family=font_used)
    })
    
    
    #-----------------------------------------------------------------------------------------------------------    
    #Identify effects which are partially crossed
    #I have not checked whether the derivation of partially crossed effects is correct 
    #Another way which may be better is by using E(g)[DEFINE HERE WHICH ONES]$lty <- 2
    #Note for can choose type of dotted/dashed line (2-6) if wish
    if (showpartialRLS=="Y")  {
      fadjm3 <- matrix(0,nrow=nrow(fadjm),ncol=ncol(fadjm),dimnames=dimnames(fadjm))
      for (i in 1:nrow(finalceffects.table.final.brief)) {
        for (j in 1:ncol(finalceffects.table.final.brief)) {
          if (finalceffects.table.final.brief[i,j]=="(0)" && finalceffects.table.final.brief[j,i]=="(0)") fadjm3[i,j] <- 1
        }
      }
      #Then remove lines which link to nested effects lower in the design e.g. A links to A*B and A*B links to A*B*C but A does not link to A*B*C in diagram.
      fadjm3.adjust <- fadjm3
      for (j in 1:ncol(fadjm3)) {
        for (i in 1:nrow(fadjm3)) {
          if (fadjm3.adjust[i,j]==1) {
            for (k in 1:nrow(fadjm3)) {
              fadjm3.adjust[k,j] <- max(fadjm3.adjust[k,j]-fadjm3[k,i],0)
            }
          }
        }
      }
      
      #This adds dummy variables so that whole width of plotting space is used 
      fadjm3.adjust <-rbind(aaadum1=c(0), Mean=fadjm3.adjust[1, ], zzzdum2=c(0), fadjm3.adjust[-1, ])
      fadjm3.adjust <-cbind(aaadum1=c(0), Mean=fadjm3.adjust[ ,1], zzzdum2=c(0), fadjm3.adjust[ ,-1])
      
      # For plot to work need to reverse the order of effects
      fadjm3.reverse <- fadjm3.adjust[nrow(fadjm3.adjust):1,ncol(fadjm3.adjust):1]
      
      fg3 <- graph_from_adjacency_matrix(fadjm3.reverse,mode="undirected")   #Change this to "directed" for randomization arrows                     
      fg3 <- simplify(fg3)
      V(fg3)$label <- V(fg3)$name
      fg3$layout <- dscoords$coords
      
      par(mar=c((2*(max(larger.fontlabelmultiplier,smaller.fontlabelmultiplier)-1)+1)*0.8, (5*(max(larger.fontlabelmultiplier,smaller.fontlabelmultiplier)-1)+1)*0.4, 0.2, (5*(max(larger.fontlabelmultiplier,smaller.fontlabelmultiplier)-1)+1)*0.4))     
      
      plot(fg3, asp=FALSE, add=TRUE, vertex.label.color="transparent",vertex.label.cex=dscoords$textlabel.size, vertex.label.font=2, vertex.size=0, vertex.color="transparent", vertex.frame.color="transparent",  edge.label.color=Colourred, edge.label.font=2, edge.color=partial.colour, edge.lty=dottedline, edge.width = partial.width, vertex.label.family=font_used)
      
    }
    
    #Adds names of effects
    par(mar=c((2*(max(larger.fontlabelmultiplier,smaller.fontlabelmultiplier)-1)+1)*0.8, (5*(max(larger.fontlabelmultiplier,smaller.fontlabelmultiplier)-1)+1)*0.4, 0.2, (5*(max(larger.fontlabelmultiplier,smaller.fontlabelmultiplier)-1)+1)*0.4))     
    
    plot(fg, asp=FALSE, add=T,vertex.label.color=vertex.label.color.objects, vertex.label.cex=dscoords$textlabel.size, vertex.label.font=vertex.label.font, vertex.size=0, vertex.color="transparent", vertex.frame.color="transparent", vertex.shape="circle", edge.lty=0, edge.width = edgewidth, vertex.label.family=font_used)
  }



#Sets up matrix for degrees of freedom, 1st col = Tier, 2nd col=max number of levels, 3rd col = actual number of levels, 4th col = dfs
#Adds degrees of freedom to plot

  if (showdfRLS=="Y") {
    RLS.output <- dfs.fun("RLS", noall, finalfinaleffects, finalceffects.table.final.brief, fadjm, outputlistip1, maxfacs, maxlevels.df, check.confound.df, datadesign, finalnames.effects)
    RLS.output$xdfs
    fg4fix<-fg
    fg4rand<-fg
    dflab4fix<-NULL
    dflab4rand<-NULL
    for (i in 1: length(V(fg2a)$name)){
      if (V(fg2a)$name[i]=="") {
        dflab4fix[i]<-paste(sep="", "[",RLS.output$xdfs.reverse[i,3],RLS.output$maxlevelsf.reverse[i],",",RLS.output$xdfs.reverse[i,4],"]")
        dflab4rand[i]<-""
      } else {
        dflab4rand[i]<-paste(sep="", "[",RLS.output$xdfs.reverse[i,3],RLS.output$maxlevelsf.reverse[i],",",RLS.output$xdfs.reverse[i,4],"]")
        dflab4fix[i]<-""    
      }
    }
    
    V(fg4fix)$label <- dflab4fix
    V(fg4rand)$label <- dflab4rand
    
    vertex.label.dist.df4fix<-(1*(max(larger.fontlabelmultiplier,smaller.fontlabelmultiplier)-1)+1)*1
    vertex.label.dist.df4rand<-(1*(max(larger.fontlabelmultiplier,smaller.fontlabelmultiplier)-1)+1)*1
    
    #Add degrees of freedom
    par(mar=c((2*(max(larger.fontlabelmultiplier,smaller.fontlabelmultiplier)-1)+1)*0.8, (5*(max(larger.fontlabelmultiplier,smaller.fontlabelmultiplier)-1)+1)*0.4, 0.2, (5*(max(larger.fontlabelmultiplier,smaller.fontlabelmultiplier)-1)+1)*0.4))     
    
    plot(fg4fix, asp=FALSE, add=T,vertex.label.color=vertex.label.color.df, vertex.label.cex=dscoords$textlabel.size.df,  vertex.label.font=vertex.label.font, vertex.label.degree=pi/2, vertex.label.dist=vertex.label.dist.df4fix,vertex.size=0, vertex.color="transparent", vertex.frame.color="transparent", vertex.shape="circle", edge.lty=0, edge.width = edgewidth, vertex.label.family=font_used)
    plot(fg4rand, asp=FALSE, add=T,vertex.label.color=vertex.label.color.df, vertex.label.cex=dscoords$textlabel.size.df, vertex.label.font=vertex.label.font, vertex.label.degree=pi/2, vertex.label.dist=vertex.label.dist.df4rand,vertex.size=0, vertex.color="transparent", vertex.frame.color="transparent", vertex.shape="circle", edge.lty=0, edge.width = edgewidth, vertex.label.family=font_used)
  }

  #-----------------------------------------------------------------------------------------------------------    
  #Identify effects which are randomised to others
  #Another way which may be better is by using E(g)[DEFINE HERE WHICH ONES]$lty <- 2
  #Note for can choose type of dotted/dashed line (2-6) if wish
  if (showrandRLS=="Y") {
    
    # For plot to work need to reverse the order of effects
    randomisedmat.reverse <- randomisedmat[nrow(randomisedmat):1,ncol(randomisedmat):1]
    randomisedmat.reverse
    
    fg5 <- graph_from_adjacency_matrix(randomisedmat.reverse,mode="directed")                      
    fg5 <- simplify(fg5)
    V(fg5)$label <- V(fg5)$name
    fg5$layout <- dscoords$coords
    
    par(mar=c((2*(max(larger.fontlabelmultiplier,smaller.fontlabelmultiplier)-1)+1)*0.8, (5*(max(larger.fontlabelmultiplier,smaller.fontlabelmultiplier)-1)+1)*0.4, 0.2, (5*(max(larger.fontlabelmultiplier,smaller.fontlabelmultiplier)-1)+1)*0.4))     
    
    plot(fg5, asp=FALSE, add=TRUE, vertex.label.color="transparent",vertex.label.cex=dscoords$textlabel.size, vertex.label.font=1, vertex.size=arrow.pos, 
         vertex.color="transparent", vertex.frame.color="transparent", edge.color=arrow.colour, edge.lty=2, edge.arrow.mode=1, edge.width=arrow.width, edge.arrow.size=0.4)
  }

  if (pdf=="Y") hidedevoff <- dev.off()
  
}



