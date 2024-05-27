#' Set and end parallel computation
#'
#' @param num_cores (`integer`) Number of cores.
#' @param cluster_type (`character`) Type of cluster. One of `c("fork", "psock")`.
#'
#' @export
#' @examples
#' \dontrun{
#' parallel_set(num_cores = 2)
#' # now the code will run on parallel with 2 cores
#' parallel_end()
#' # now the code will run sequentially
#' }
parallel_set <- function(num_cores = 10, cluster_type = "fork") {
  checkmate::assert_integerish(num_cores, lower = 1, any.missing = FALSE, len = 1)
  cluster_type <- match.arg(cluster_type, choices = c("fork", "psock"))

  parallel_end()

  if (cluster_type == "fork") {
    cl <- parallel::makeForkCluster(num_cores)
  } else {
    cl <- parallel::makePSOCKcluster(num_cores)
  }

  doParallel::registerDoParallel(cl)

  message(paste("Using", foreach::getDoParWorkers(), "cores")) # should be == num_cores
}

#' @rdname parallel_set
#' @export
parallel_end <- function() {
  if (foreach::getDoParRegistered()) {
    foreach::registerDoSEQ()
  }
}


define_foreach_operand <- function(allow_par = FALSE) {
  if (foreach::getDoParRegistered() && allow_par) {
    foreach::`%dopar%`
  } else {
    foreach::`%do%`
  }
}
