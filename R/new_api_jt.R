compile <- function(g, data, evidence = NULL, propagate = TRUE, flow = sum) {
  jt <- new_jt(g, data, evidence, flow)
  if (!propagate) return(jt)
  m <- send_messages(jt)
  while (attr(m, "direction") != "FULL") m <- send_messages(m)
  return(m)
}

query_belief.jt <- function(x, nodes) UseMethod("query_belief")

query_belief.jt <- function(x, nodes) {
  # All queries are joint probabilities

  # Test if there is a clique for which all nodes are contained
  # - otherwise break!
}

