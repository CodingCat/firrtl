// See LICENSE for license details.

package firrtlTests.options

import org.scalatest.{FlatSpec, Matchers}

import firrtl.AnnotationSeq
import firrtl.options.{Phase, PhaseManager, PhaseManagerException}
import firrtl.annotations.{Annotation, NoTargetAnnotation}

trait IdentityPhase extends Phase {
  def transform(annotations: AnnotationSeq): AnnotationSeq = annotations
}

/** Default [[Phase]] that has no prerequisites and invalidates nothing */
case object A extends IdentityPhase {
  override def invalidates(phase: Phase): Boolean = false
}

/** [[Phase]] that requires [[A]] and invalidates nothing */
case object B extends IdentityPhase {
  override def prerequisites: Set[Phase] = Set(A)
  override def invalidates(phase: Phase): Boolean = false
}

/** [[Phase]] that requires [[B]] and invalidates nothing */
case object C extends IdentityPhase {
  override def prerequisites: Set[Phase] = Set(A)
  override def invalidates(phase: Phase): Boolean = false
}

/** [[Phase]] that requires [[A]] and invalidates [[A]] */
case object D extends IdentityPhase {
  override def prerequisites: Set[Phase] = Set(A)
  override def invalidates(phase: Phase): Boolean = phase match {
    case _: A.type => true
    case _ => false
  }
}

/** [[Phase]] that requires [[B]] and invalidates nothing */
case object E extends IdentityPhase {
  override def prerequisites: Set[Phase] = Set(B)
  override def invalidates(phase: Phase): Boolean = false
}

/** [[Phase]] that requires [[B]] and [[C]] and invalidates [[E]] */
case object F extends IdentityPhase {
  override def prerequisites: Set[Phase] = Set(B, C)
  override def invalidates(phase: Phase): Boolean = phase match {
    case _: E.type => true
    case _ => false
  }
}


/** [[Phase]] that requires [[C]] and invalidates [[F]] */
case object G extends IdentityPhase {
  override def prerequisites: Set[Phase] = Set(C)
  override def invalidates(phase: Phase): Boolean = phase match {
    case _: F.type => true
    case _ => false
  }
}

case object CyclicA extends IdentityPhase {
  override def prerequisites: Set[Phase] = Set(CyclicB)
}

case object CyclicB extends IdentityPhase {
  override def prerequisites: Set[Phase] = Set(CyclicA)
}

class CyclicInvalidateFixture {

  case object A extends IdentityPhase {
    override def invalidates(phase: Phase): Boolean = false
  }
  case object B extends IdentityPhase {
    override def prerequisites: Set[Phase] = Set(A)
    override def invalidates(phase: Phase): Boolean = false
  }
  case object C extends IdentityPhase {
    override def prerequisites: Set[Phase] = Set(A)
    override def invalidates(phase: Phase): Boolean = phase match {
      case _: B.type => true
      case _ => false
    }
  }
  case object D extends IdentityPhase {
    override def prerequisites: Set[Phase] = Set(B)
    override def invalidates(phase: Phase): Boolean = phase match {
      case _: C.type | _: E.type => true
      case _ => false
    }
  }
  case object E extends IdentityPhase {
    override def prerequisites: Set[Phase] = Set(B)
    override def invalidates(phase: Phase): Boolean = false
  }

}

trait AnalysisFixture {

  case object Analysis extends IdentityPhase {
    override def invalidates(phase: Phase): Boolean = false
  }

}

class RepeatedAnalysisFixture extends AnalysisFixture {

  trait InvalidatesAnalysis extends IdentityPhase {
    override def invalidates(phase: Phase): Boolean = phase match {
      case _: Analysis.type => true
      case _ => false
    }
  }

  case object A extends InvalidatesAnalysis {
    override def prerequisites: Set[Phase] = Set(Analysis)
  }
  case object B extends InvalidatesAnalysis {
    override def prerequisites: Set[Phase] = Set(A, Analysis)
  }
  case object C extends InvalidatesAnalysis {
    override def prerequisites: Set[Phase] = Set(B, Analysis)
  }

}

class InvertedAnalysisFixture extends AnalysisFixture {

  case object A extends IdentityPhase {
    override def prerequisites: Set[Phase] = Set(Analysis)
    override def invalidates(phase: Phase): Boolean = phase match {
      case _: Analysis.type => true
      case _ => false
    }
  }
  case object B extends IdentityPhase {
    override def prerequisites: Set[Phase] = Set(Analysis)
    override def invalidates(phase: Phase): Boolean = phase match {
      case _: Analysis.type | _: A.type => true
      case _ => false
    }
  }
  case object C extends IdentityPhase {
    override def prerequisites: Set[Phase] = Set(Analysis)
    override def invalidates(phase: Phase): Boolean = phase match {
      case _: Analysis.type | _: B.type => true
      case _ => false
    }
  }

}

class PhaseManagerSpec extends FlatSpec with Matchers {

  behavior of this.getClass.getName

  it should "do nothing if all targets are reached" in {
    val targets: Set[Phase] = Set(A, B, C, D)
    val pm = new PhaseManager(targets, targets)

    pm.flattenedPhaseOrder should be (empty)
  }

  it should "handle a simple dependency" in {
    val targets: Set[Phase] = Set(B)
    val pm = new PhaseManager(targets)

    pm.flattenedPhaseOrder should be (Seq(A, B))
  }

  it should "handle a simple dependency with an invalidation" in {
    val targets: Set[Phase] = Set(A, B, C, D)
    val pm = new PhaseManager(targets)

    pm.flattenedPhaseOrder should be (Seq(A, D, A, C, B))
  }

  it should "handle a dependency with two invalidates optimally" in {
    val targets: Set[Phase] = Set(A, B, C, E, F, G)
    val pm = new PhaseManager(targets)

    pm.flattenedPhaseOrder.size should be (targets.size)
  }

  it should "throw an exception for cyclic prerequisites" in {
    val targets: Set[Phase] = Set(CyclicA, CyclicB)
    val pm = new PhaseManager(targets)

    intercept[PhaseManagerException]{ pm.flattenedPhaseOrder }
      .getMessage should startWith ("No Phase ordering possible")
  }

  it should "handle invalidates that form a cycle" in {
    val f = new CyclicInvalidateFixture
    val targets: Set[Phase] = Set(f.A, f.B, f.C, f.D, f.E)
    val pm = new PhaseManager(targets)

    info("only one phase was recomputed")
    pm.flattenedPhaseOrder.size should be (targets.size + 1)
  }

  it should "handle repeated recomputed analyses" in {
    val f = new RepeatedAnalysisFixture
    val targets: Set[Phase] = Set(f.A, f.B, f.C)
    val pm = new PhaseManager(targets)

    pm.flattenedPhaseOrder should be (Seq(f.Analysis, f.A, f.Analysis, f.B, f.Analysis, f.C))
  }

  it should "handle inverted repeated recomputed analyses" in {
    val f = new InvertedAnalysisFixture
    val targets: Set[Phase] = Set(f.A, f.B, f.C)
    val pm = new PhaseManager(targets)

    pm.flattenedPhaseOrder should be (Seq(f.Analysis, f.C, f.Analysis, f.B, f.Analysis, f.A))
  }
}
