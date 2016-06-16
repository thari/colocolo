/**
  * Created by hariharan on 6/15/16.
  */
import kodkod.ast._
import kodkod.engine.Solver
import kodkod.engine.satlab.SATFactory

object KodMain {
  def main(args: Array[String]) {
    val uber = new Uber()
    val solver = new Solver()
    try {
//      val n = Integer.parseInt(args(0))
      val n = 2
      solver.options().setSolver(SATFactory.DefaultSAT4J)
      solver.options().setSymmetryBreaking(n)
      val show = uber.declarations().and(uber.req0()).and(uber.req1())
      val solution = solver.solve(show, uber.bounds(n))
      println(solution)
    }catch {
      case ne : NumberFormatException =>  "Provide small number as argument  : " +ne
    }
  }
}
