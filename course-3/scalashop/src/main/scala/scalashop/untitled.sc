val slices = 0 to 11 by (11 / 2)
println(slices.toList)
val split = slices.zip(slices.tail)
println(split)