library (leem)
x <- rbinom(100,100,0.40)
x |> new_leem() |> tabfreq() |> stickplot()
  