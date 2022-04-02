package lab.demo

import it.unibo.scafi.incarnations.BasicAbstractIncarnation
import it.unibo.scafi.simulation.s2.frontend.incarnation.scafi.bridge.ExportEvaluation.EXPORT_EVALUATION
import it.unibo.scafi.simulation.s2.frontend.incarnation.scafi.bridge.ScafiWorldIncarnation.EXPORT
import it.unibo.scafi.simulation.s2.frontend.incarnation.scafi.bridge.SimulationInfo
import it.unibo.scafi.simulation.s2.frontend.incarnation.scafi.configuration.{ScafiProgramBuilder, ScafiWorldInformation}
import it.unibo.scafi.simulation.s2.frontend.incarnation.scafi.world.ScafiWorldInitializer.Random
import it.unibo.scafi.simulation.s2.frontend.view.{ViewSetting, WindowConfiguration}
import it.unibo.scafi.space.graphics2D.BasicShape2D.Circle
import lab.gui.patch.RadiusLikeSimulation

object Incarnation extends BasicAbstractIncarnation
import lab.demo.Incarnation._ //import all stuff from an incarnation

object Simulation extends App {

  val formatter_evaluation: EXPORT_EVALUATION[Any] = (e : EXPORT) => formatter(e.root[Any]())

  val formatter: Any => Any = (e) => e match {
    case (a,b) => (formatter(a),formatter(b))
    case (a,b,c) => (formatter(a),formatter(b),formatter(c))
    case (a,b,c,d) => (formatter(a),formatter(b),formatter(c),formatter(d))
    case l:Iterable[_] => l.map(formatter(_)).toString
    case i: java.lang.Number if (i.doubleValue()>100000) => "Inf"
    case i: java.lang.Number if (-i.doubleValue()>100000) => "-Inf"
    case i: java.lang.Double => f"${i.doubleValue()}%1.2f"
    case x => x.toString
  }

  val programClass = classOf[Step1]
  val nodes = 100
  val neighbourRange = 200
  val (width, height) = (1920, 1080)
  ViewSetting.windowConfiguration = WindowConfiguration(width, height)
  ScafiProgramBuilder (
    Random(nodes, width, height),
    SimulationInfo(programClass,exportEvaluations = List(formatter_evaluation)),
    RadiusLikeSimulation(neighbourRange),
    ScafiWorldInformation(shape = Some(Circle(5,5))),
    neighbourRender = true,
  ).launch()
}

abstract class AggregateProgramSkeleton extends AggregateProgram with StandardSensors {
  def sense1 = sense[Boolean]("sens1")
  def sense2 = sense[Boolean]("sens2")
  def sense3 = sense[Boolean]("sens3")
  def boolToInt(b: Boolean) = mux(b){1}{0}
}

class Main extends AggregateProgramSkeleton {
  override def main() = rep(Double.MaxValue){ d => mux[Double](sense1){0.0}{minHoodPlus( mux(sense2){nbr{d}+(nbrRange * 5)}{nbr{d}+nbrRange} )} }
}

class Main1 extends AggregateProgramSkeleton {
  override def main() = 1
}

class Main2 extends AggregateProgramSkeleton {
  override def main() = 2+3
}

class Main3 extends AggregateProgramSkeleton {
  override def main() = (10,20)
}

class Main4 extends AggregateProgramSkeleton {
  override def main() = Math.random()
}

class Main5 extends AggregateProgramSkeleton {
  override def main() = sense1
}

class Main6 extends AggregateProgramSkeleton {
  override def main() = if (sense1) 10 else 20
}

class Main7 extends AggregateProgramSkeleton {
  override def main() = mid()
}

class Main8 extends AggregateProgramSkeleton {
  override def main() = minHoodPlus(nbrRange)
}

class Main9 extends AggregateProgramSkeleton {
  override def main() = rep(0){_+1}
}

class Main10 extends AggregateProgramSkeleton {
  override def main() = rep(Math.random()){x=>x}
}

class Main11 extends AggregateProgramSkeleton {
  override def main() = rep[Double](0.0){x => x + rep(Math.random()){y=>y}}
}

class Main12 extends AggregateProgramSkeleton {
  import Builtins.Bounded.of_i

  override def main() = maxHoodPlus(boolToInt(nbr{sense1}))
}

class Main13 extends AggregateProgramSkeleton {
  override def main() = foldhoodPlus(0)(_+_){nbr{1}}
}

class Main14 extends AggregateProgramSkeleton {
  import Builtins.Bounded.of_i

  override def main() = rep(0){ x => boolToInt(sense1) max maxHoodPlus( nbr{x}) }
}

class Main15 extends AggregateProgramSkeleton {
  override def main() = rep(Double.MaxValue){ d => mux[Double](sense1){0.0}{minHoodPlus(nbr{d}+1.0)} }
}

class Main16 extends AggregateProgramSkeleton {
  override def main() = rep(Double.MaxValue){ d => mux[Double](sense1){0.0}{minHoodPlus(nbr{d}+nbrRange)} }
}

class Case9 extends AggregateProgramSkeleton {
  override def main() = branch(sense1) (rep(0.0)(v => if (v >= 1000) 1000 else v+0.1))(0)
}

class Case12 extends AggregateProgramSkeleton {
  override def main() = foldhoodPlus[Set[ID]](Set.empty)(_ ++ _)(nbr{Set(mid())})
}

class Case8 extends AggregateProgramSkeleton {
  override def main() = minHoodPlus[(D, ID)]{(nbr{0.0} + nbrRange, nbr{mid()})}._2
}

class Case14 extends AggregateProgramSkeleton {
  override def main() = rep(mid()) (id => maxHoodPlus( nbr{mid() max id} ))
}

class Case16 extends AggregateProgramSkeleton {
  override def main() = rep(Double.MaxValue){ d => mux(sense1){0.0}{minHoodPlus( mux(sense2){nbr{d} + (nbrRange * 5)}{nbr{d} + nbrRange} )} }
}

class Step1 extends AggregateProgramSkeleton {
  import Builtins.Bounded._
  // Initialize all nodes to infinite distance and with their own id.
  // When "sense1" is active the node should be considered as source and it's distance will be set to 0.0
  override def main() = rep((Double.MaxValue, mid())){src => mux(sense1){(0.0,mid())}
  // Otherwise
  {
    // Find the shortest path to a source node
    val min = minHoodPlus((nbr{src._1} + nbrRange, nbr{src._2}))
    // If the shortest path is still infinite the node will get his own id (this is needed for initialization)
    // So minHoodPlus won't set the id of all nodes of a cluster to the lowest one.
    mux(min._1 < Double.MaxValue)
      {min}
      {(Double.MaxValue, mid())}
  }}
}

