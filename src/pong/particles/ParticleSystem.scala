package pong.particles {

  import scala.util.Random
  import java.nio.FloatBuffer
  import com.sun.opengl.util._
  import processing.opengl._
  import javax.media.opengl._
  import msafluid._

  object ParticleSystem {

    val MAX_PARTICLES = 3000

  }

  class ParticleSystem {

    // -----------------------------------------------------------------------
    // private vars
    // -----------------------------------------------------------------------

    private var posArray:FloatBuffer = BufferUtil.newFloatBuffer( ParticleSystem.MAX_PARTICLES * 2 * 2 )
    private var colArray:FloatBuffer = BufferUtil.newFloatBuffer( ParticleSystem.MAX_PARTICLES * 3 * 2 )
    private var curIndex:Int = 0
    private val particles = new Array[Particle](ParticleSystem.MAX_PARTICLES)

    for ( i <- 0 to ParticleSystem.MAX_PARTICLES-1 ) particles(i) = new Particle

    // -----------------------------------------------------------------------
    // methods
    // -----------------------------------------------------------------------

    def updateAndDraw(width:Float, invWidth:Float, height:Float, invHeight:Float)(implicit fluidSolver:MSAFluidSolver2D, pgl:PGraphicsOpenGL) = {
      val gl = pgl.beginGL
      gl.glEnable( GL.GL_BLEND )

      gl.glBlendFunc( GL.GL_ONE, GL.GL_ONE )
      gl.glEnable( GL.GL_LINE_SMOOTH )
      gl.glLineWidth( 4 )

      for ( i <- 0 to ParticleSystem.MAX_PARTICLES-1 ) {
        if ( particles(i).alpha > 0 ) {
          particles(i).update( width, invWidth, height, invHeight )
          particles(i).updateVertexArrays( i, posArray, colArray )
        }
      }
      gl.glEnableClientState( GL.GL_VERTEX_ARRAY )
      gl.glVertexPointer( 2, GL.GL_FLOAT, 0, posArray )
      gl.glEnableClientState( GL.GL_COLOR_ARRAY )
      gl.glColorPointer( 3, GL.GL_FLOAT, 0, colArray )

      gl.glDrawArrays( GL.GL_LINES, 0, ParticleSystem.MAX_PARTICLES * 2 )

      gl.glDisable( GL.GL_BLEND )
      pgl.endGL
    }

    def addParticles(x:Float, y:Float, count:Int) = {
      for ( i <- 0 to count-1 ) addParticle( x + Random.nextInt(30) - 15, y + Random.nextInt(30) - 15 )
    }

    def addParticle(x:Float, y:Float) = {
      particles(curIndex).init( x, y )
      curIndex += 1;
      if ( curIndex >= ParticleSystem.MAX_PARTICLES ) curIndex = 0
    }

  }

}