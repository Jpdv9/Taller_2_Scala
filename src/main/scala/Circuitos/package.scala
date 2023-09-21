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

  /*def half_adder: Chip = {

  }

  def full_adder: Chip = {

  }

  def adder(n: Int): Chip = {

  }*/

}
