import Circuitos._

val chip_not = crearChipUnario((x: Int) => (1-x))
chip_not(List(1))
chip_not(List(0))

val chip_and = crearChipBinario((x: Int, y: Int) => (x*y))
chip_and(List(1,1))
chip_and(List(1,0))
chip_and(List(0,1))
chip_and(List(0,0))

val chip_or = crearChipBinario((x:Int, y: Int) => (x+y - (x*y)))
chip_or(List(1,1))
chip_or(List(1,0))
chip_or(List(0,1))
chip_or(List(0,0))