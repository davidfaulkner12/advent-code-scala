package aoc2021.days

object Day12 extends AocDay {

  def parse(rawData: String): Map[String, Set[String]] = {
    val lines = rawData.split("\n+").map(_.split("-"))

    val symmetricalEdges = lines.flatMap(x => Seq(
      (x(0), x(1)),
      (x(1), x(0))
    ))

    // Group by the first part of the edge and then extract the second part into a set
    symmetricalEdges.groupBy{_._1}.mapValues(_.map(_._2).toSet)
  }

  sealed trait VisitedState {
    def nodeBlacklist: Set[String]
    def visit(node: String): VisitedState
  }

  case class SmallVisitOnce(visited: Set[String]) extends VisitedState {
    def nodeBlacklist = visited
    def visit(node: String) = if (node.forall(_.isUpper)) this else SmallVisitOnce(visited + node)
  }

  case class SmallVisitTwice(allowed: String, triggered: Boolean, visited: Set[String]) extends VisitedState {
    def nodeBlacklist = visited
    def visit(node: String) = (node, triggered) match {
      case ("start", _) => SmallVisitTwice(allowed, triggered, visited + "start")
      case ("end", _) => SmallVisitTwice(allowed, triggered, visited + "end")
      case (s, _) if s.forall(_.isUpper) => this
      case (s, false) if s == allowed => SmallVisitTwice(allowed, true, visited)
      case (s, _) => this.copy(visited = visited + s)
    }
  }

  object SmallVisitTwice {
    def apply(s: String): SmallVisitTwice = SmallVisitTwice(s, false, Set())
  }

  // Graph is invariant
  // curNode is guranteed to be the last item in the curPath list
  // visited does NOT include curNode
  def dfs(graph: Map[String, Set[String]], acc: Set[List[String]], visited: VisitedState, curNode: String, curPath: List[String]) : Set[List[String]] = {
    if (curNode == "end") {
      acc + (curPath.reverse)
    } else {
      val nextVisited = visited.visit(curNode)
      val nextNodes = graph(curNode) diff visited.nodeBlacklist
      nextNodes.flatMap(node => dfs(graph, acc, nextVisited, node, node :: curPath))
    }
  }

  def problem1(data: String) : String = dfs(parse(data), Set(), SmallVisitOnce(Set()), "start", List("start")).size.toString

  def problem2(data: String) : String = {
    val graph = parse(data)
    val visitors = graph.keys.filter(_.forall(_.isLower)).map(SmallVisitTwice(_))
    visitors.map(dfs(graph, Set(), _, "start", List("start"))).reduce(_ ++ _).size.toString
  }
}
