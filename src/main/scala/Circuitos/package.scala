package object Circuitos {

  type Chip = List[Int] => List[Int]

  def crearChipBinario(function: (Int, Int) => Int): Chip = {
    (bits: List[Int]) => {
      if (bits.length == 2)
        List(function(bits.head, bits(1)))
      else
        Nil
    }
  }

  def crearChipUnario(function: Int => Int): Chip = {
    (bit: List[Int]) =>{
      if(bit.length == 1)
        List(function(bit.head))
      else
        Nil
    }
  }

  def half_adder: Chip = {
    val s = crearChipBinario((x: Int, y: Int) => x ^ y)
    val c = crearChipBinario((x: Int, y: Int) => x & y)

    (bits: List[Int]) => {
      if(bits.length == 2)
         c(bits) ++ s(bits)
      else
        Nil
    }

  }


  def full_adder: Chip = {

    (bits: List[Int]) =>{

      val inicialC = bits(2)
      val c = crearChipBinario((x: Int, y: Int) => (x & y) | ((x ^ y) & inicialC))
      val s = crearChipBinario((x: Int, y: Int) => (x ^ y) ^ inicialC)

      if(bits.length == 3) {

        val salidaC = c(List(bits(0), bits(1)))
        val salidaS = s(List(bits(0), bits(1)))

        salidaC ++ salidaS

      } else
        Nil

    }

  }

  /*
  def adder(n: Int): Chip = {

  }*/

}
