package lab.demo2

import it.unibo.scafi.incarnations.BasicAbstractIncarnation
import it.unibo.scafi.lib.StandardLibrary
import it.unibo.scafi.simulation.s2.frontend.incarnation.scafi.bridge.ExportEvaluation.EXPORT_EVALUATION
import it.unibo.scafi.simulation.s2.frontend.incarnation.scafi.bridge.SimulationInfo
import it.unibo.scafi.simulation.s2.frontend.incarnation.scafi.configuration.{ScafiProgramBuilder, ScafiWorldInformation}
import it.unibo.scafi.simulation.s2.frontend.incarnation.scafi.world.ScafiWorldInitializer.Random
import it.unibo.scafi.simulation.s2.frontend.view.{ViewSetting, WindowConfiguration}
import it.unibo.scafi.space.Point3D
import it.unibo.scafi.space.graphics2D.BasicShape2D.Circle
import lab.gui.patch.RadiusLikeSimulation

object Incarnation extends BasicAbstractIncarnation with StandardLibrary {
  override type P = Point3D
  override type Time = Double
  override implicit val idBounded: Incarnation.Builtins.Bounded[Int] = Builtins.Bounded.of_i
}


import lab.demo2.Incarnation._ //import all stuff from an incarnation

object SimulationWithBlocks extends App {

  val formatter_evaluation: EXPORT_EVALUATION[Any] = e => formatter(e.root[Any]())

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

  val programClass = classOf[Main5]
  val nodes = 100
  val neighbourRange = 200
  val (width, height) = (1920, 1080)
  ViewSetting.windowConfiguration = WindowConfiguration(width, height)
  ViewSetting.labelFontSize = 20
  ScafiProgramBuilder (
    Random(nodes, width, height),
    SimulationInfo(programClass,exportEvaluations = List(formatter_evaluation)),
    RadiusLikeSimulation(neighbourRange),
    ScafiWorldInformation(shape = Some(Circle(5,5))),
    neighbourRender = true,
  ).launch()
}

trait AggregateProgramSkeleton extends AggregateProgram with StandardSensors with BlockG with BlockC {
  def sense1 = sense[Boolean]("sens1")
  def sense2 = sense[Boolean]("sens2")
  def sense3 = sense[Boolean]("sens3")
  def boolToInt(b: Boolean) = mux(b){1}{0}
}

class Main extends AggregateProgramSkeleton {
  override def main() = G2(sense1)(0.0)(_+nbrRange)(nbrRange)
}

class Main1 extends AggregateProgramSkeleton {
  override def main() = G[Int](sense1,0,_+1,nbrRange)
}

class Main2 extends AggregateProgramSkeleton {
  override def main() = G[Double](sense1,0.0,_+nbrRange,nbrRange)
}

class Main3 extends AggregateProgramSkeleton {
  override def main() = G2(sense1)(0.0)(_+nbrRange)(nbrRange)
}

class Main4 extends AggregateProgramSkeleton {
  override def main() = G2(sense1)(mid)(x=>x)(nbrRange)
}

class Main5 extends AggregateProgramSkeleton {
  override def main() = G2(sense1)(G2(sense2)(0.0)(_+nbrRange)(nbrRange))(x=>x)(nbrRange)
}

class Main6 extends AggregateProgramSkeleton {
  override def main() = ???
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
    override def main() = maxHoodPlus(boolToInt(nbr{sense1}))
}

class Main13 extends AggregateProgramSkeleton {
  override def main() = foldhoodPlus(true)(_ && _){nbr{sense1}}
}

class Main14 extends AggregateProgramSkeleton {
  override def main() = rep(0){ x => boolToInt(sense1) max maxHoodPlus( nbr{x}) }
}

class Main15 extends AggregateProgramSkeleton {
  def gradient(src: Boolean): Double =
    rep(Double.MaxValue) {
      d => mux[Double](src) {
        0.0
      } {
        minHoodPlus(nbr {
          d
        } + 1.0)
      }
    }

  override def main() = (gradient(sense1), gradient(sense2))
}

class Main16 extends AggregateProgramSkeleton {
  override def main() = rep(Double.MaxValue){ d => mux[Double](sense1){0.0}{minHoodPlus(nbr{d}+nbrRange)} }
}

class Main17 extends AggregateProgramSkeleton {
  def gradient(src: Boolean) = rep(Double.MaxValue) { d => mux[Double](src) {
    0.0
  } {
    minHoodPlus(nbr {
      d
    } + nbrRange)
  }
  }

  override def main() = branch(sense2){Double.MaxValue}{gradient(sense1)}
}
