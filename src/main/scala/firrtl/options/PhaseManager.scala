// See LICENSE for license details.

package firrtl.options

import firrtl.AnnotationSeq
import firrtl.graph.{DiGraph, CyclicException}

import scala.collection.mutable

case class PhaseManagerException(message: String, cause: Throwable = null) extends RuntimeException(message, cause)

/** A [[Phase]] that will ensure that some other [[Phase]]s and their prerequisites are executed.
  *
  * This tries to determine a phase ordering such that an [[AnnotationSeq]] ''output'' is produced that has had all of
  * the requested [[Phase]] target transforms run without having them be invalidated.
  * @param phaseTargets the [[Phase]]s you want to run
  */
case class PhaseManager(phaseTargets: Set[Phase], currentState: Set[Phase] = Set.empty) extends Phase {

  /** Modified breadth-first search that supports multiple starting nodes and a custom extractor that can be used to
    * generate/filter the edges to explore. Additionally, this will include edges to previously discovered nodes.
    */
  private def bfs(start: Set[Phase], blacklist: Set[Phase], extractor: Phase => Set[Phase]): Map[Phase, Set[Phase]] = {
    val queue: mutable.Queue[Phase] = mutable.Queue(start.toSeq:_*)
    val edges: mutable.HashMap[Phase, Set[Phase]] = mutable.HashMap[Phase, Set[Phase]](start.map((_ -> Set[Phase]())).toSeq:_*)
    while (queue.nonEmpty) {
      val u = queue.dequeue
      for (v <- extractor(u)) {
        if (!blacklist.contains(v) && !edges.contains(v)) { queue.enqueue(v)     }
        if (!edges.contains(v))                           { edges(v) = Set.empty }
        edges(u) = edges(u) + v
      }
    }
    edges.toMap
  }

  /** Pull in all registered phases once phase registration is integrated
    * @todo implement this
    */
  private lazy val registeredPhases: Set[Phase] = Set.empty

  /** A directed graph consisting of prerequisite edges */
  lazy val dependencyGraph: DiGraph[Phase] =
    DiGraph(
      bfs(
        start = phaseTargets,
        blacklist = currentState,
        extractor = (p: Phase) => p.prerequisites))

  /** A directed graph consisting of edges derived from invalidation */
  lazy val invalidateGraph: DiGraph[Phase] = {
    val v = dependencyGraph.getVertices
    DiGraph(
      bfs(
        start = phaseTargets,
        blacklist = currentState,
        extractor = (p: Phase) => v.filter(p.invalidates).toSet))
      .reverse
  }

  /** Wrap a possible [[CyclicException]] thrown by a thunk in a [[PhaseManagerException]] */
  private def cyclePossible[A](a: String, thunk: => A): A = try { thunk } catch {
    case e: CyclicException =>
      throw new PhaseManagerException(
        s"No Phase ordering possible due to cyclic dependency in $a at node '${e.node}'.", e)
  }

  /** The ordering of phases to run that respects prerequisites and reduces the number of required re-lowerings resulting
    * from invalidations.
    */
  lazy val phaseOrder: Seq[Phase] = {

    /* Topologically sort the dependency graph using the invalidate graph topological sort as a seed. This has the effect
     * of minimizing the number of repeated [[Phase]]s */
    val sorted = {
      val seed = cyclePossible("invalidates", invalidateGraph.linearize).reverse
      cyclePossible("prerequisites",
                    dependencyGraph
                      .seededLinearize(Some(seed))
                      .reverse
                      .dropWhile(currentState.contains))
    }

    val (state, lowerers) = {
      val (s, l) = sorted.foldLeft((currentState, Array[Phase]())){ case ((state, out), in) =>
        val missing = (in.prerequisites -- state)
        val preprocessing: Option[Phase] = {
          if (missing.nonEmpty) { Some(PhaseManager(missing, state)) }
          else                  { None                               }
        }
        ((state ++ missing + in).filterNot(in.invalidates), out ++ preprocessing :+ in)
      }
      val missing = (phaseTargets -- s)
      val postprocessing: Option[Phase] = {
        if (missing.nonEmpty) { Some(PhaseManager(missing, s)) }
        else                  { None                           }
      }

      (s ++ missing, l ++ postprocessing)
    }

    if (!phaseTargets.subsetOf(state)) {
      throw new PhaseException(s"The final state ($state) did not include the requested targets (${phaseTargets})!")
    }
    lowerers
  }

  def flattenedPhaseOrder: Seq[Phase] = phaseOrder.flatMap {
    case p: PhaseManager => p.flattenedPhaseOrder
    case p: Phase => Some(p)
  }

  final def transform(annotations: AnnotationSeq): AnnotationSeq =
    phaseOrder
      .foldLeft(annotations){ case (a, p) => p.transform(a) }

}
