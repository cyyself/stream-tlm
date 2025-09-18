import mill._
import scalalib._
import mill.bsp._

object streamtlm extends SbtModule { m =>
    override def millSourcePath = super.millSourcePath / os.up
    override def scalaVersion = "2.13.16"
    override def scalacOptions = Seq(
        "-language:reflectiveCalls",
        "-deprecation",
        "-feature",
        "-Xcheckinit",
        "-Ymacro-annotations",
    )
    override def ivyDeps = Agg(
        ivy"org.chipsalliance::chisel:7.0.0-RC1",
        ivy"org.scala-lang::toolkit:0.7.0",
    )
    override def scalacPluginIvyDeps = Agg(
        ivy"org.chipsalliance:::chisel-plugin:7.0.0-RC1",
    )
    object test extends SbtTests with TestModule.ScalaTest {
        override def ivyDeps = m.ivyDeps() ++ Agg(
            ivy"org.scalatest::scalatest::3.2.19"
        )
    }
}
