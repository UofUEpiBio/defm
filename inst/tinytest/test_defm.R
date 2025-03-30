data(valentesnsList)
mymodel <- new_defm(
  id = valentesnsList$id,
  Y = valentesnsList$Y,
  X = valentesnsList$X,
  order = 1
)

term_defm_logit_intercept(mymodel)
term_defm_transition_formula(mymodel, "{y1, 0y2} > {y1, y2}")

init_defm(mymodel)
motif_census(mymodel, locs = 0:1)

mymodel_cs <- new_defm(
  id = valentesnsList$id,
  Y = valentesnsList$Y,
  X = valentesnsList$X,
  order = 0
)

motif_census(mymodel_cs, locs = 0:1)
