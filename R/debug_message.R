# Auto-executed functions ------------------------


#' Title
#'
#' @param libname
#' @param pkgname
#'
#' @return
.onAttach <- function(libname = "DebugMessageR", pkgname = "DebugMessageR"){
  debug_message_env <- list(
    DEBUG.LEVEL = 20,
    debug.intent = 0,
    intent.space="                    ",
    intent.depth="---------------------"
  )
  attach(debug_message_env)
  message("DebugMessageR activated")
}



# Low level exported functions ------------------------


#' Title debug_message_reset_level
#'
#' @param NEW.DEBUG.LEVEL
#' @description Overrides the debug message visibility thrshold
#'
#' @return
#' @export
#'
#' @examples
debug_message_reset_level <- function(NEW.DEBUG.LEVEL){
  DEBUG.LEVEL <<- NEW.DEBUG.LEVEL
}


#' Title debug_message
#'
#' @param str.in - string input as
#' @param debug.level.trigger - integer
#' @description Prints intended debug message
#' @description Whether to display the message is conditional on
#' @description   debug.level.trigger <= DEBUG.LEVEL
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' my_fun <- function(some_tex_input){
#'    # Say "Hi!" at the beginning
#'    debug_message(paste0("my_fun(',some_tex_input,') started"))
#'
#'    # Do some work with text
#'    debug_message("CP01: Checkpoint 1 passed", 5)
#'
#'    # Do some work with text
#'    if(faulty condition detected) # Error detected here
#'    {
#'      debug_message("*** COND1 met ***: terminating", 2)
#'
#'      # Say "Bye" at the end
#'      debug_message("my_fun() <- ERROR", -1)
#'    }
#'
#'    debug_message("CP02: Checkpoint 2 passed", 5)
#'    # Do some work with text
#'
#'    # Say "Bye" at the end
#'    debug_message("my_fun() <- OK", -1)
#' }
#' }
debug_message <- function(str.in, debug.level.trigger = 1){

  # Check if it is plausible to show debug info
  if(DEBUG.LEVEL >= abs(debug.level.trigger)){

    if(debug.level.trigger==-1)debug.intent<<-max(debug.intent-1,0)
    pr0 <- if(debug.intent>0)substr(intent.depth,0, debug.intent)else""
    if(debug.level.trigger==+1)debug.intent<<-debug.intent+1

    pr1 <- if (debug.level.trigger==1) "{ " else
      if(debug.level.trigger==-1) "} "
    else ""

    pr2 <- substr(intent.space,0,(abs(debug.level.trigger * 2)))

    message(paste0(pr0,pr1,pr2,str.in))
  }
}



# High level wrappers to predefine level --------------------------------

#' Title Message at the beginning of the debugged function
#'
#' @return
#' @export
#'
#' @examples
#'  debug_message_start("Hi there!")
debug_message_start <- function(...){

  debug_message(paste0(...) , debug.level.trigger = 1)
}


#' Title Message at the end of the debugged function
#'
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
#'  debug_message_start("Bye!")
debug_message_end <- function(...){
  paste0(...)
  debug_message(paste0(...) , debug.level.trigger = -1)
}

#' Title
#'
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
debug_message_l2 <- function(...){
  paste0(...)
  debug_message(paste0(...) , debug.level.trigger = 2)
}

#' Title
#'
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
debug_message_l3 <- function(...){
  paste0(...)
  debug_message(paste0(...) , debug.level.trigger = 3)
}

#' Title
#'
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
debug_message_l4 <- function(...){
  paste0(...)
  debug_message(paste0(...) , debug.level.trigger = 4)
}