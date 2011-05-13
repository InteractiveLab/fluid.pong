package pong.particles {

  import scala.util.Random
  import java.nio.FloatBuffer
  import msafluid._
  import javax.media.opengl._

  object Particle {

    val MOMENTUM = 0.6f
    val FLUID_FORCE = 1.25f

  }

  class Particle {

    // -----------------------------------------------------------------------
    // properties
    // -----------------------------------------------------------------------

    var x, y, vx, vy, alpha, mass:Float = _

    // -----------------------------------------------------------------------
    // methods
    // -----------------------------------------------------------------------

    def init(x:Float, y:Float) = {
      this.x = x
      this.y = y
      alpha = 0.3f + Random.nextFloat * 0.7f
      mass = 0.1f + Random.nextFloat * 0.9f
    }

    def update(width:Float, invWidth:Float, height:Float, invHeight:Float)(implicit fluidSolver:MSAFluidSolver2D):Unit = {
      if ( alpha == 0 ) return

      val fluidIndex = fluidSolver.getIndexForNormalizedPosition( x * invWidth, y * invHeight )
      vx = fluidSolver.u(fluidIndex) * width * mass * Particle.FLUID_FORCE + vx * Particle.MOMENTUM
      vy = fluidSolver.v(fluidIndex) * height * mass * Particle.FLUID_FORCE + vy * Particle.MOMENTUM

      x += vx
      y += vy

      if ( x < 0 ) {
        x = 0;
        vx *= -1
      } else if ( x > width ) {
        x = width
        vx *= -1
      }

      if ( y < 0 ) {
        y = 0
        vy *= -1
      } else if ( y > height ) {
        y = height
        vy *= -1
      }

      if ( vx*vx + vy*vy < 1) {
        vx = Random.nextFloat * 2f - 1f
        vy = Random.nextFloat * 2f - 1f
      }

      alpha *= 0.999f
      if ( alpha < 0.01 ) alpha = 0
    }

    def updateVertexArrays(i:Int, posBuffer:FloatBuffer, colBuffer:FloatBuffer) = {
      var vi = i * 4
      posBuffer.put( vi, x - vx )
      vi += 1
      posBuffer.put( vi, y - vy )
      vi += 1
      posBuffer.put( vi, x )
      vi += 1
      posBuffer.put( vi, y )

      var ci = i * 6
      colBuffer.put( ci, alpha * Random.nextFloat )
      ci += 1
      colBuffer.put( ci, alpha * Random.nextFloat )
      ci += 1
      colBuffer.put( ci, alpha * Random.nextFloat )
      ci += 1
      colBuffer.put( ci, alpha * Random.nextFloat )
      ci += 1
      colBuffer.put( ci, alpha * Random.nextFloat )
      ci += 1
      colBuffer.put( ci, alpha * Random.nextFloat )
    }

  }

}