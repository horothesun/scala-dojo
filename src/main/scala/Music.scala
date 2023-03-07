import cats._
import Music.Chord._
import Music.Length._
import Music.Tone._

object Music {

  sealed trait Song[A] {

    def compile: List[A]

    def append(that: Song[A]): Song[A] = MonoidK[Song].combineK(this, that)

    def overlap(that: Song[A]): Song[A] = ???

  }
  object Song {
    implicit val monoidK: MonoidK[Song] = new MonoidK[Song] {
      override def empty[A]: Song[A] = new Song[A] {
        override def compile: List[A] = MonoidK[List].empty
      }
      override def combineK[A](x: Song[A], y: Song[A]): Song[A] = new Song[A] {
        override def compile: List[A] = MonoidK[List].combineK(x.compile, y.compile)
      }
    }

    implicit val functor: Functor[Song] = new Functor[Song] {
      override def map[A, B](fa: Song[A])(f: A => B): Song[B] = new Song[B] {
        override def compile: List[B] = Functor[List].map(fa.compile)(f)
      }
    }

    implicit val applicative: Applicative[Song] = new Applicative[Song] {
      override def pure[A](x: A): Song[A] = new Song[A] {
        override def compile: List[A] = Applicative[List].pure(x)
      }
      override def ap[A, B](ff: Song[A => B])(fa: Song[A]): Song[B] = new Song[B] {
        override def compile: List[B] = Applicative[List].ap(ff.compile)(fa.compile)
      }
    }

    def pause(length: Length): Song[ChordLength] = new Song[ChordLength] {
      override def compile: List[ChordLength] = List(ChordLength(Chord.pause, length))
    }
  }

  case class Hz(value: Double)

  sealed trait Tone {
    // TODO: fix!!! 🔥🔥🔥
    def baseHz: Hz = Hz(this match {
      case A => 10.0
      case B => 20.0
      case C => 30.0
      case D => 40.0
      case E => 50.0
      case F => 60.0
      case G => 70.0
    })
  }
  object Tone {
    case object A extends Tone
    case object B extends Tone
    case object C extends Tone
    case object D extends Tone
    case object E extends Tone
    case object F extends Tone
    case object G extends Tone
  }

  case class Octave(value: Int)

  case class Note(tone: Tone, octave: Octave) {
    // TODO: fix!!! 🔥🔥🔥
    def hz: Hz = Hz(tone.baseHz.value * octave.value)

    def `+`(that: Note): Chord = this + Chord.of(that)
    def `+`(chord: Chord): Chord = chord + this
  }

  case class Chord(notes: Set[Note]) {

    def `+`(note: Note): Chord = this + Chord.of(note)
    def `+`(that: Chord): Chord = monoid.combine(this, that)

    def sortedNotes: List[Note] = ???

  }
  object Chord {
    implicit val monoid: Monoid[Chord] = new Monoid[Chord] {
      override def empty: Chord = Chord(notes = MonoidK[Set].empty)
      override def combine(x: Chord, y: Chord): Chord = Chord(notes = MonoidK[Set].combineK(x.notes, y.notes))
    }

    val pause: Chord = monoid.empty
    def of(note: Note): Chord = Chord(notes = Set(note))
  }

  case class Rational(num: Int, den: Int = 1)

  sealed trait Length {
    def beats: Rational = this match {
      case Semibreve      => Rational(4)
      case DottedMinimum  => Rational(3)
      case Minimum        => Rational(2)
      case DottedCrotchet => Rational(num = 3, den = 2)
      case Crotchet       => Rational(1)
      case Quaver         => Rational(num = 1, den = 2)
      case SemiQuaver     => Rational(num = 1, den = 4)
    }
  }
  object Length {
    case object Semibreve extends Length
    case object DottedMinimum extends Length
    case object Minimum extends Length
    case object DottedCrotchet extends Length
    case object Crotchet extends Length
    case object Quaver extends Length
    case object SemiQuaver extends Length
  }

  case class ChordLength(chord: Chord, length: Length)

}
