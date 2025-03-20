#-------------------------------------------------------------------------------
#' 
#' Print and Summary of \code{rls} objects
#'
#' Print and Summary of objects of class \code{"rls"}.
#'
#' @param x An object of class \code{"rls"}.
#' @param object An object of class \code{"rls"}.
#' @param ... Additional arguments to be passed to other methods.
#'
#' @return The functions return the structural objects of the Layout Structure 
#' and a matrix that the users need to edit with the randomisation 
#' objects to pass in the \code{\link[hassediagram]{hasserls}} function.
#'
#' @note For examples see \code{\link[hassediagram]{itemlist}} and \code{\link[hassediagram]{hasserls}}.
#'
#' @author Damianos Michaelides, Simon Bate, and Marion Chatfield
#'
#' @rdname rlsobjects
#' @method print rls
#' @export
print.rls <- function(x, ...){
  cat("The names of all the structural objects to assist the input choices of the Hasse diagram of the Restricted Layout Structure (RLS) are: \n")
  print(x$finaleffectsnames)
  cat("\n and the matrix you need to fill in with the randomisation objects which are present in the RLS is: \n \n")
  print(x$TransferObject)
}

#' @rdname rlsobjects
#' @method summary rls
#' @export
summary.rls <- function(object, ...){
  cat("The names of all the structural objects to assist the input choices of the Hasse diagram of the Restricted Layout Structure (RLS) are: \n")
  print(object$finaleffectsnames)
  cat("\n and the matrix you need to fill in with the randomisation objects which are present in the RLS is: \n \n")
  print(object$TransferObject)
}
