import common._
import barneshut.conctrees._

package object barneshut {

  class Boundaries {
    var minX = Float.MaxValue

    var minY = Float.MaxValue

    var maxX = Float.MinValue

    var maxY = Float.MinValue

    def width = maxX - minX

    def height = maxY - minY

    def size = math.max(width, height)

    def centerX = minX + width / 2

    def centerY = minY + height / 2

    override def toString = s"Boundaries($minX, $minY, $maxX, $maxY) size=$size"
  }

  sealed abstract class Quad {
    def massX: Float

    def massY: Float

    def mass: Float

    def centerX: Float

    def centerY: Float

    def size: Float

    def total: Int

    def insert(b: Body): Quad
  }

  case class Empty(centerX: Float, centerY: Float, size: Float) extends Quad {
    def massX: Float = centerX
    def massY: Float = centerY
    def mass: Float = 0f
    def total: Int = 0
    def insert(b: Body): Quad = {
      Leaf(massX, massY, size, Seq(b))
    }
  }

  case class Fork(
    nw: Quad, ne: Quad, sw: Quad, se: Quad
  ) extends Quad {
    val centerX: Float = nw.centerX + nw.size/2
    val centerY: Float = nw.centerY + nw.size/2
    val size: Float = nw.size * 2
    val mass: Float = nw.mass + ne.mass + sw.mass + se.mass
    val massX: Float =
      if (mass == 0) centerX else (nw.mass * nw.massX + ne.mass * ne.massX + sw.mass * sw.massX + se.mass * se.massX)/mass
    val massY: Float =
      if (mass == 0) centerY else (nw.mass * nw.massY + ne.mass * ne.massY + sw.mass * sw.massY + se.mass * se.massY)/mass
    val total: Int = nw.total + ne.total + sw.total + se.total

    def insert(b: Body): Fork = {
      if (b.x < centerX && b.y < centerY) {
        Fork(nw.insert(b), ne, sw, se)
      } else if (b.x > centerX && b.y < centerY) {
        Fork(nw, ne.insert(b), sw, se)
      } else if (b.x < centerX && b.y > centerY) {
        Fork(nw, ne, sw.insert(b), se)
      } else {
        Fork(nw, ne, sw, se.insert(b))
      }
    }
  }

  case class Leaf(centerX: Float, centerY: Float, size: Float, bodies: Seq[Body])
  extends Quad {
    val tmass = bodies.map(_.mass).sum
    val tmassX = bodies.foldLeft(0f)((a: Float, b: Body) => a + b.mass * b.x)/tmass
    val tmassY = bodies.foldLeft(0f)((a: Float, b: Body) => a + b.mass * b.y)/tmass
    val (mass, massX, massY) = (tmass : Float, tmassX: Float, tmassY : Float)
    val total: Int = bodies.size
    /*
    If the size of a Leaf is greater than a predefined constant minimumSize,
    inserting an additonal body into that Leaf quadtree creates a Fork quadtree with empty children,
    and adds all the bodies into that Fork (including the new body).
    Otherwise, inserting creates another Leaf with all the existing bodies and the new one.
    */
    def insert(b: Body): Quad = {
      if (size > minimumSize) {
        val z = Fork(Empty(centerX-size/4,centerY-size/4,size/2),
                     Empty(centerX+size/4,centerY-size/4,size/2),
                     Empty(centerX-size/4,centerY+size/4,size/2),
                     Empty(centerX+size/4,centerY+size/4,size/2))
        (bodies :+ b).foldLeft(z)(_.insert(_))
      } else {
        Leaf(centerX, centerY, size, bodies :+ b)
      }
    }
  }

  def minimumSize = 0.00001f

  def gee: Float = 100.0f

  def delta: Float = 0.01f

  def theta = 0.5f

  def eliminationThreshold = 0.5f

  def force(m1: Float, m2: Float, dist: Float): Float = gee * m1 * m2 / (dist * dist)

  def distance(x0: Float, y0: Float, x1: Float, y1: Float): Float = {
    math.sqrt((x1 - x0) * (x1 - x0) + (y1 - y0) * (y1 - y0)).toFloat
  }

  class Body(val mass: Float, val x: Float, val y: Float, val xspeed: Float, val yspeed: Float) {

    def updated(quad: Quad): Body = {
      var netforcex = 0.0f
      var netforcey = 0.0f

      def addForce(thatMass: Float, thatMassX: Float, thatMassY: Float): Unit = {
        val dist = distance(thatMassX, thatMassY, x, y)
        /* If the distance is smaller than 1f, we enter the realm of close
         * body interactions. Since we do not model them in this simplistic
         * implementation, bodies at extreme proximities get a huge acceleration,
         * and are catapulted from each other's gravitational pull at extreme
         * velocities (something like this:
         * http://en.wikipedia.org/wiki/Interplanetary_spaceflight#Gravitational_slingshot).
         * To decrease the effect of this gravitational slingshot, as a very
         * simple approximation, we ignore gravity at extreme proximities.
         */
        if (dist > 1f) {
          val dforce = force(mass, thatMass, dist)
          val xn = (thatMassX - x) / dist
          val yn = (thatMassY - y) / dist
          val dforcex = dforce * xn
          val dforcey = dforce * yn
          netforcex += dforcex
          netforcey += dforcey
        }
      }

      def traverse(quad: Quad): Unit = (quad: Quad) match {
        case Empty(_, _, _) => //empty quadtree does not affect the net force
        case Leaf(_, _, _, bodies) => bodies.map(b => addForce(b.mass,b.x,b.y))
        case Fork(nw, ne, sw, se) =>
          // see if node is far enough from the body,
          // or recursion is needed
          // distance between the center of mass and the particle
          if (quad.size/distance(x,y,quad.centerX,quad.centerY) < theta) {
            addForce(quad.mass,quad.massX,quad.massY)
          } else {
            traverse(nw)
            traverse(ne)
            traverse(sw)
            traverse(se)
          }
      }

      traverse(quad)

      val nx = x + xspeed * delta
      val ny = y + yspeed * delta
      val nxspeed = xspeed + netforcex / mass * delta
      val nyspeed = yspeed + netforcey / mass * delta

      new Body(mass, nx, ny, nxspeed, nyspeed)
    }
    override def toString = s"Body(mass=$mass, x=$x, y=$y)"

  }

  val SECTOR_PRECISION = 8

  class SectorMatrix(val boundaries: Boundaries, val sectorPrecision: Int) {
    val sectorSize = boundaries.size / sectorPrecision
    val matrix = new Array[ConcBuffer[Body]](sectorPrecision * sectorPrecision)
    for (i <- 0 until matrix.length) matrix(i) = new ConcBuffer

    val cellX = (boundaries.maxX - boundaries.minX)/sectorPrecision
    val cellY = (boundaries.maxY - boundaries.minY)/sectorPrecision

    def +=(b: Body): SectorMatrix = {

      // to within the Boundaries
      def shrink(b: Body): Body = {
        val x = if (b.x <= boundaries.minX) boundaries.minX+1
                else if (b.x >= boundaries.maxX) boundaries.maxX-1
                else b.x
        val y = if (b.y <= boundaries.minY) boundaries.minY+1
                else if (b.y >= boundaries.maxY) boundaries.maxY-1
                else b.y

        new Body(b.mass,x,y,b.xspeed,b.yspeed)
      }

      val b1 = shrink(b)  // to within boundaries

      case class Sector(val j: Int, val k: Int)

      def get_sector(x: Float, y: Float): Sector = {
        val x1 = (x-boundaries.minX)/cellX
        val y1 = (y-boundaries.minY)/cellY
        Sector(x1.toInt,y1.toInt)
      }

      val sector = get_sector(b1.x,b1.y)

      val sectorIndex = sector.j + (sector.k * sectorPrecision)

      // NOTE even though we get the sector with the body
      // that was shrunk to with in the boundaries
      // we put the original body into the matrix
      matrix(sectorIndex) += b

      this
    }

    def apply(x: Int, y: Int) = matrix(y * sectorPrecision + x)

    def combine(that: SectorMatrix): SectorMatrix = {
      for (i <- 0 until matrix.length)
        matrix(i) = matrix(i).combine(that.matrix(i))
      this
    }

    def toQuad(parallelism: Int): Quad = {
      def BALANCING_FACTOR = 4
      def quad(x: Int, y: Int, span: Int, achievedParallelism: Int): Quad = {
        if (span == 1) {
          val sectorSize = boundaries.size / sectorPrecision
          val centerX = boundaries.minX + x * sectorSize + sectorSize / 2
          val centerY = boundaries.minY + y * sectorSize + sectorSize / 2
          var emptyQuad: Quad = Empty(centerX, centerY, sectorSize)
          val sectorBodies = this(x, y)
          sectorBodies.foldLeft(emptyQuad)(_ insert _)
        } else {
          val nspan = span / 2
          val nAchievedParallelism = achievedParallelism * 4
          val (nw, ne, sw, se) =
            if (parallelism > 1 && achievedParallelism < parallelism * BALANCING_FACTOR) parallel(
              quad(x, y, nspan, nAchievedParallelism),
              quad(x + nspan, y, nspan, nAchievedParallelism),
              quad(x, y + nspan, nspan, nAchievedParallelism),
              quad(x + nspan, y + nspan, nspan, nAchievedParallelism)
            ) else (
              quad(x, y, nspan, nAchievedParallelism),
              quad(x + nspan, y, nspan, nAchievedParallelism),
              quad(x, y + nspan, nspan, nAchievedParallelism),
              quad(x + nspan, y + nspan, nspan, nAchievedParallelism)
            )
          Fork(nw, ne, sw, se)
        }
      }

      quad(0, 0, sectorPrecision, 1)
    }

    override def toString = s"SectorMatrix(#bodies: ${matrix.map(_.size).sum})"
  }

  class TimeStatistics {
    private val timeMap = collection.mutable.Map[String, (Double, Int)]()

    def clear() = timeMap.clear()

    def timed[T](title: String)(body: =>T): T = {
      var res: T = null.asInstanceOf[T]
      val totalTime = /*measure*/ {
        val startTime = System.currentTimeMillis()
        res = body
        (System.currentTimeMillis() - startTime)
      }

      timeMap.get(title) match {
        case Some((total, num)) => timeMap(title) = (total + totalTime, num + 1)
        case None => timeMap(title) = (0.0, 0)
      }

      println(s"$title: ${totalTime} ms; avg: ${timeMap(title)._1 / timeMap(title)._2}")
      res
    }

    override def toString = {
      timeMap map {
        case (k, (total, num)) => k + ": " + (total / num * 100).toInt / 100.0 + " ms"
      } mkString("\n")
    }
  }
}
