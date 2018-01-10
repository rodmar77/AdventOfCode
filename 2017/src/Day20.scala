import scala.io.Source

/*

  --- Day 20: Particle Swarm ---

  Suddenly, the GPU contacts you, asking for help. Someone has asked it to
  simulate too many particles, and it won't be able to finish them all in time
  to render the next frame at this rate.

  It transmits to you a buffer (your puzzle input) listing each particle in order
  (starting with particle 0, then particle 1, particle 2, and so on). For each
  particle, it provides the X, Y, and Z coordinates for the particle's position
  (p), velocity (v), and acceleration (a), each in the format <X,Y,Z>.

  Each tick, all particles are updated simultaneously. A particle's properties
  are updated in the following order:

    - Increase the X velocity by the X acceleration.
    - Increase the Y velocity by the Y acceleration.
    - Increase the Z velocity by the Z acceleration.
    - Increase the X position by the X velocity.
    - Increase the Y position by the Y velocity.
    - Increase the Z position by the Z velocity.

  Because of seemingly tenuous rationale involving z-buffering, the GPU would
  like to know which particle will stay closest to position <0,0,0> in the long
  term. Measure this using the Manhattan distance, which in this situation is
  simply the sum of the absolute values of a particle's X, Y, and Z position.

  For example, suppose you are only given two particles, both of which stay
  entirely on the X-axis (for simplicity). Drawing the current states of
  particles 0 and 1 (in that order) with an adjacent a number line and
  diagram of current X positions (marked in parenthesis), the following would
  take place:

  +-------------------------------------------------------------------+
  | p=< 3,0,0>, v=< 2,0,0>, a=<-1,0,0>    -4 -3 -2 -1  0  1  2  3  4  |
  | p=< 4,0,0>, v=< 0,0,0>, a=<-2,0,0>                         (0)(1) |
  |                                                                   |
  | p=< 4,0,0>, v=< 1,0,0>, a=<-1,0,0>    -4 -3 -2 -1  0  1  2  3  4  |
  | p=< 2,0,0>, v=<-2,0,0>, a=<-2,0,0>                      (1)   (0) |
  |                                                                   |
  | p=< 4,0,0>, v=< 0,0,0>, a=<-1,0,0>    -4 -3 -2 -1  0  1  2  3  4  |
  | p=<-2,0,0>, v=<-4,0,0>, a=<-2,0,0>          (1)               (0) |
  |                                                                   |
  | p=< 3,0,0>, v=<-1,0,0>, a=<-1,0,0>    -4 -3 -2 -1  0  1  2  3  4  |
  | p=<-8,0,0>, v=<-6,0,0>, a=<-2,0,0>                         (0)    |
  +-------------------------------------------------------------------+

  At this point, particle 1 will never be closer to <0,0,0> than particle 0, and
  so, in the long run, particle 0 will stay closest.

 */
object Day20 {

  case class Particle(p: (Long, Long, Long), v: (Long, Long, Long), a: (Long, Long, Long)) {
    implicit class TupleAdd(t: (Long, Long, Long)) {
      def +(p: (Long, Long, Long)) = (p._1 + t._1, p._2 + t._2, p._3 + t._3)
    }

    def manhattanDistance() = p match {
      case (px, py, pz) => px.abs + py.abs + pz.abs
    }

    def collidesWith(that: Particle) = this != that && this.p == that.p

    def move(): Particle = Particle(p + v + a, v + a, a)
    def move(count: Int): Particle = {
      def move(i: Int, acc: Particle): Particle = {
        if (i == count) acc
        else move(i + 1, acc.move())
      }

      move(0, this)
    }
  }

  def main(args: Array[String]): Unit = {
    val regex = ("p=<(-?\\d+),(-?\\d+),(-?\\d+)>, " +
      "v=<(-?\\d+),(-?\\d+),(-?\\d+)>, " +
      "a=<(-?\\d+),(-?\\d+),(-?\\d+)>").r

    val particles = Source
      .fromFile("inputs/2017/input_day20.txt")
      .getLines
      .flatMap {
        case regex(
                px, py, pz,
                vx, vy, vz,
                ax, ay, az) => List(
          Particle(
            (px.toLong, py.toLong, pz.toLong),
            (vx.toLong, vy.toLong, vz.toLong),
            (ax.toLong, ay.toLong, az.toLong))
        )}
      .toList

    // Which particle will stay closest to position <0,0,0> in the long term?
    println(particles
      .map(_.move(1000))
      .zipWithIndex
      .minBy {
        case (p, _) => p.manhattanDistance()
      } match {
        case (_, idx) => idx
      })

    /*
    To simplify the problem further, the GPU would like to remove any particles
    that collide. Particles collide if their positions ever exactly match.
    Because particles are updated simultaneously, more than two particles can
    collide at the same time and place. Once particles collide, they are removed
    and cannot collide with anything else after that tick.

    For example:

    +----------------------------------------------------------------------+
    | p=<-6,0,0>, v=< 3,0,0>, a=< 0,0,0>                                   |
    | p=<-4,0,0>, v=< 2,0,0>, a=< 0,0,0>    -6 -5 -4 -3 -2 -1  0  1  2  3  |
    | p=<-2,0,0>, v=< 1,0,0>, a=< 0,0,0>    (0)   (1)   (2)            (3) |
    | p=< 3,0,0>, v=<-1,0,0>, a=< 0,0,0>                                   |
    |                                                                      |
    | p=<-3,0,0>, v=< 3,0,0>, a=< 0,0,0>                                   |
    | p=<-2,0,0>, v=< 2,0,0>, a=< 0,0,0>    -6 -5 -4 -3 -2 -1  0  1  2  3  |
    | p=<-1,0,0>, v=< 1,0,0>, a=< 0,0,0>             (0)(1)(2)      (3)    |
    | p=< 2,0,0>, v=<-1,0,0>, a=< 0,0,0>                                   |
    |                                                                      |
    | p=< 0,0,0>, v=< 3,0,0>, a=< 0,0,0>                                   |
    | p=< 0,0,0>, v=< 2,0,0>, a=< 0,0,0>    -6 -5 -4 -3 -2 -1  0  1  2  3  |
    | p=< 0,0,0>, v=< 1,0,0>, a=< 0,0,0>                       X (3)       |
    | p=< 1,0,0>, v=<-1,0,0>, a=< 0,0,0>                                   |
    |                                                                      |
    | ------destroyed by collision------                                   |
    | ------destroyed by collision------    -6 -5 -4 -3 -2 -1  0  1  2  3  |
    | ------destroyed by collision------                      (3)          |
    | p=< 0,0,0>, v=<-1,0,0>, a=< 0,0,0>                                   |
    +----------------------------------------------------------------------+

    In this example, particles 0, 1, and 2 are simultaneously destroyed at the
    time and place marked X. On the next tick, particle 3 passes through unharmed.

    How many particles are left after all collisions are resolved?
     */
    println(removeCollisions(particles, 1000).size)
  }


  def removeCollisions(ll: List[Particle], r: Int) = {
    def removeCollisions(idx: Int, acc: List[Particle]): List[Particle] = {
      if (idx == r) acc
      else {
        val moved = acc.map(_.move())
        removeCollisions(
          idx + 1,
          moved.filterNot(x => moved.exists(_.collidesWith(x))))
      }
    }

    removeCollisions(0, ll)
  }
}
