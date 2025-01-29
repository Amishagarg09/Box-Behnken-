library(rsm)

bb_design_1 <- bbd(
  k  = 3,            # Number of factors,
  n0 = 3,            # Number of center points,
  block = FALSE,     # Consider blocks or not in the design 
  randomize = FALSE  # Randomize or not the order experiments
)

head(as.data.frame(bb_design_1))
readr::write_csv(bb_design_1, file = "bbd1.csv")
bb_design_2 <- bbd(
  k  = 3,            # Four factors 
  n0 = 3,            # Three central points 
  block = FALSE,     # No blocks 
  randomize = FALSE, # Not randomized
  coding = list(
    x1 ~ (Temperature - 20) / 15,
    x2 ~ (Power - 25)/ 15,
    x3 ~ (time - 1) / 1
  )
)

head(bb_design_2)
yield_data <- readr::read_csv("OR1.csv")

head(yield_data)
View(yield_data)
yield_model <- rsm(Yield ~ SO(x1, x2, x3), data = yield_data)
summary(yield_model)

par(mfrow = c(2,3))       # 2 x 3 pictures on one plot
contour(
  yield_model,            # Our model
  ~ x1 + x2 + x3,    # A formula to obtain the 6 possible graphs 
  image = TRUE,           # If image = TRUE, apply color to each contour
)


par(mfrow = c(2,3))       # 2 x 3 pictures on one plot
persp(
  yield_model,            # Our model 
  ~ x1 + x2 + x3,    # A formula to obtain the 6 possible graphs
  col = topo.colors(100), # Color palette
  contours = "colors"     # Include contours with the same color palette
) 

