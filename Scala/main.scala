package Scala

/**
  * Created by Hassan on 4/16/16.
  */
object main {

  def main(args: Array[String]):Unit = {
    //println("Write a number\n")
    //val target = scala.io.StdIn.readInt()

    val p = Population(.4, .4, 30, 30000)
    val x = p.population.size



    var generations = 0
    while (!p.isDone) {
      //println(generations+" generations")
      p.evolve
      generations = generations + 1
    }
    println(generations+ " generations")
    p.result

    println("done")
  }

}
