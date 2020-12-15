# start with basic function wrappers

env_appdata <- env() # basic function all else will be passed to.
attributes(env_appdata) <- list(name = "kindisperse_appdata")

#' Mount KinPairData Objects for use in kindisperse app
#'
#' @param x An object of class \code{KinPairData} or {KinPairSimulation}
#' @param nm character. A name to store the object as
#'
#' @return invisibly returns x.
#' @export
#'
#' @examples
#' mount_appdata(KinPairData(), "mydata")
mount_appdata <- function(x, nm) {
  if (!is.KinPairData(x)) stop("Object x is not of class KinPairData!")
  env_poke(env_appdata, nm, x)
  invisible(x)
}

#' Unmount a KinPairData Object (clear slot from appdata environment)
#'
#' @param nms A character vector of names of objects to unmount from the appdata environment
#'
#' @return
#' @export
#'
#' @examples
#' mount_appdata(KinPairData(), "mydata")
#'
#' unmount_appdata("mydata")
unmount_appdata <- function(nms) {
  env_unbind(env_appdata, nms)
}

#' Reset kindisperse appdata
#'
#' @return
#' @export
#'
#' @examples
#' reset_appdata()
reset_appdata <- function() {
  env_unbind(env_appdata, env_names(env_appdata))
}

#' Retrieve KinPairData object from appdata (single)
#'
#' @param nm character. Name of item as stored in appdata
#'
#' @return Returns KinPairData object accessible by name nm
#' @export
#'
#' @examples
#' mount_appdata(KinPairData(), "mydata")
#'
#' retrieve_appdata("mydata")
retrieve_appdata <- function(nm) {
  env_get(env_appdata, nm)
}

#' Retrieve all KinPairData objects from appdata (as list)
#'
#' @return
#' @export
#'
#' @examples
#' mount_appdata(KinPairData(), "k1")
#' mount_appdata(KinPairSimulation(), "s1")
#' retrieveall_appdata()
retrieveall_appdata <- function() {
  env_get_list(env_appdata, env_names(env_appdata))
}

#' Reset app tempdata (internal mem)
#'
#' @return
#' @export
#'
#' @examples
#' reset_tempdata()
reset_tempdata <- function() {
  env_poke(app_env, "d1", KinPairSimulation())
  env_poke(app_env, "d2", KinPairSimulation())
  env_poke(app_env, "d3", KinPairSimulation())
  env_poke(app_env, "d4", KinPairSimulation())
  env_poke(app_env, "d5", KinPairSimulation())
  env_poke(app_env, "d6", KinPairSimulation())
  env_poke(app_env, "d7", KinPairSimulation())
  env_poke(app_env, "d8", KinPairSimulation())
  env_poke(app_env, "d9", KinPairSimulation())
  env_poke(app_env, "d10", KinPairSimulation())
}

#' Retrieve all tempdata (internal mem) from app (as list)
#'
#' @return A list of all KinPairData objects in kindisperse app's tempdata
#' @export
#'
#' @examples
#' retrieve_tempdata()
retrieve_tempdata <- function() {
  env_get_list(app_env, env_names(app_env))
}

#' Show printout of named items stored in appdata.
#'
#' @return
#' @export
#'
#' @examples
#' mount_appdata(KinPairData(), "my_kindata")
#' mount_appdata(simulate_kindist_simple(nsims = 10), "my_simdata")
#'
#' display_appdata()
display_appdata <- function() {
  env_print(env_appdata)
}
