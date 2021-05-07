import mill._
import scalalib._

object recschemes extends ScalaModule {
  val sv = "2.13.5"
  def scalaVersion = sv

  def ivyDeps = Agg(
    ivy"org.typelevel::paiges-core:0.3.0",
    ivy"org.typelevel::paiges-cats:0.3.0",
    ivy"org.typelevel::cats-core:2.3.0"
  )

  def scalacPluginIvyDeps = Agg(
    ivy"org.typelevel:::kind-projector:0.11.3"
  )
}
