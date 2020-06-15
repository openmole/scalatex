package scalatex.site

import ammonite.ops._

import scalatags.Text.all._
import io.circe._, io.circe.generic.auto._, io.circe.syntax._

object Sidebar {
  def snippet(tree: Seq[Tree[String]]) = script(raw(s"""
    scalatexScrollspyController(
      ${tree.asJson}
    )"""))
  def autoResources = Seq(resource/'scalatex/'scrollspy/"scrollspy.js")
}