# start with basic function wrappers

env_appdata <- env() # basic function all else will be passed to.
attributes(env_appdata) <- list(name = "kindisperse_appdata")

#' Mount \code{KinPairData} Objects for use in kindisperse app
#'
#' @description This function is part of a suite of functions handling the interface between the kindisperse app & R
#' package. Due to how shiny's interactive programming works, ordinary objects are not visible to the reactive functions
#' embedded in the app. The solution implemented here is to construct a custom environment, \code{env_appdata}, that is
#' accessible within the app and outside of it.
#'
#' This function takes an object of class \code{KinPairData} or \code{KinPairSimulation}, assigns it an identifying name,
#' and adds it to the app interface environment, making it accessible within the app. Once added, this object will be
#' accessible under its name from the \code{Load} menue of the app. (The app interface uses the same function
#' internally, enabling objects to be passed to the interface from the app also).
#' @param x An object of class \code{KinPairData} or {KinPairSimulation}
#' @param nm character. A name to store the object as
#'
#' @return invisibly returns x.
#' @export
#' @family app_ports
#'
#' @examples
#' mount_appdata(kin_pair_data(), "mydata")
mount_appdata <- function(x, nm) {
  if (!is.KinPairData(x)) stop("Object x is not of class KinPairData!")
  env_poke(env_appdata, nm, x)
  invisible(x)
}

#' Unmount a \code{KinPairData} Object (clear slot from appdata environment)
#'
#' @param nms A character vector of names of objects to unmount from the appdata environment
#'
#' @description This function is part of a suite of functions handling the interface between the kindisperse app & R
#' package. Due to how shiny's interactive programming works, ordinary objects are not visible to the reactive functions
#' embedded in the app. The solution implemented here is to construct a custom environment, \code{env_appdata}, that is
#' accessible within the app and outside of it.
#'
#' When called, this function clears any objects with names found in the vector \code{nms} from the app interface
#' environment, keeping it from becoming over-cluttered & taking up space.
#' @return No return value, called for side effects
#' @export
#' @family app_ports
#'
#' @examples
#' mount_appdata(kin_pair_data(), "mydata")
#'
#' unmount_appdata("mydata")
unmount_appdata <- function(nms) {
  env_unbind(env_appdata, nms)
}

#' Reset kindisperse appdata
#'
#' @description This function is part of a suite of functions handling the interface between the kindisperse app & R
#' package. Due to how shiny's interactive programming works, ordinary objects are not visible to the reactive functions
#' embedded in the app. The solution implemented here is to construct a custom environment, \code{env_appdata}, that is
#' accessible within the app and outside of it.
#'
#' When called, this function clears all attached objects from the app interface environment, keeping it from becoming
#' over-clutttered & taking up space.
#' @return No return value, called for side effects
#' @export
#' @family app_ports
#'
#' @examples
#' reset_appdata()
reset_appdata <- function() {
  env_unbind(env_appdata, env_names(env_appdata))
}

#' Retrieve \code{KinPairData} object from appdata (single)
#'
#' @param nm character. Name of item as stored in appdata
#'
#' @description This function is part of a suite of functions handling the interface between the kindisperse app & R
#' package. Due to how shiny's interactive programming works, ordinary objects are not visible to the reactive functions
#' embedded in the app. The solution implemented here is to construct a custom environment, \code{env_appdata}, that is
#' accessible within the app and outside of it.
#'
#' This function accesses the app interface environment and retrieves an object (typically of class \code{KinPairData} or
#' \code{KinPairSimulation}) with the name \code{nm}, making it accessible from within our outside the app. This can be
#' used to load simulation objects that were saved from the interface while using the app into the regular R environment
#' (after closing the app). (The app uses this function internally to load objects from the interface into its own internal
#' environment for display & processing.)
#' @return Returns KinPairData object accessible by name nm
#' @export
#' @family app_ports
#'
#' @examples
#' mount_appdata(kin_pair_data(), "mydata")
#'
#' retrieve_appdata("mydata")
retrieve_appdata <- function(nm) {
  env_get(env_appdata, nm)
}

#' Retrieve all \code{KinPairData} objects from appdata (as list)
#'
#' @description This function is part of a suite of functions handling the interface between the kindisperse app & R
#' package. Due to how shiny's interactive programming works, ordinary objects are not visible to the reactive functions
#' embedded in the app. The solution implemented here is to construct a custom environment, \code{env_appdata}, that is
#' accessible within the app and outside of it.
#'
#' This function accesses the app interface environment and retrieves a named list of all objects (typically of classes
#' \code{KinPairData} or \code{KinPairSimulation} contained within it, making them accessible outside of the app). This
#' is used to quickly pass all simulation objects that were saved to this interface environment while using the app to
#' the regular R environment (after closing the app).
#' @return Returns a list of objects stored in the appdata environment
#' @export
#' @family app_ports
#'
#' @examples
#' mount_appdata(kin_pair_data(), "k1")
#' mount_appdata(kin_pair_simulation(), "s1")
#' retrieveall_appdata()
retrieveall_appdata <- function() {
  env_get_list(env_appdata, env_names(env_appdata))
}

#' Reset app tempdata (internal mem)
#'
#' @description This function is part of a suite of functions handling the interface between the kindisperse app & R
#' package. Due to how shiny's interactive programming works, ordinary objects are not visible to the reactive functions
#' embedded in the app. The solution implemented here is to construct a custom environment, \code{env_appdata}, that is
#' accessible within the app and outside of it.
#'
#' This function resets the internal \code{tempdata} environment used by the kindisperse app, keeping it from becoming
#' over-cluttered & freeing up space.
#' @return No return value, called for side effects
#' @export
#' @family app_ports
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
#' @description This function is part of a suite of functions handling the interface between the kindisperse app & R
#' package. Due to how shiny's interactive programming works, ordinary objects are not visible to the reactive functions
#' embedded in the app. The solution implemented here is to construct a custom environment, \code{env_appdata}, that is
#' accessible within the app and outside of it.
#'
#' This function accesses the app internal environment and retrieves a named list of all objects (typically of classes
#' \code{KinPairData} or \code{KinPairSimulation} contained within it, making them accessible outside of the app). This
#' is used to quickly retrieve all objects stored in the app's internal memory. Ordinarily, these would be passed to the
#' interface environment, but this function is useful if the app crashed and important results were only present in the
#' app's internal environment.
#' @return A list of all KinPairData objects in kindisperse app's tempdata
#' @export
#' @family app_ports
#'
#' @examples
#' retrieve_tempdata()
retrieve_tempdata <- function() {
  env_get_list(app_env, env_names(app_env))
}

#' Show printout of named items stored in appdata.
#'
#' @description This function is part of a suite of functions handling the interface between the kindisperse app & R
#' package. Due to how shiny's interactive programming works, ordinary objects are not visible to the reactive functions
#' embedded in the app. The solution implemented here is to construct a custom environment, \code{env_appdata}, that is
#' accessible within the app and outside of it.
#'
#' This function prints a summary of all objects currently stored within the app interface environment, by name and class
#' @export
#' @return No return value, called for side effects
#' @family app_ports
#' @examples
#' mount_appdata(kin_pair_data(), "my_kindata")
#' mount_appdata(simulate_kindist_simple(nsims = 10), "my_simdata")
#'
#' display_appdata()
display_appdata <- function() {
  env_print(env_appdata)
}
