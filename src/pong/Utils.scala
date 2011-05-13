package pong {

  import javax.media.opengl.GL
  import processing.opengl.PGraphicsOpenGL
  import processing.core.PConstants._
  import math._

  object Utils {

    def drawCircle(x:Float, y:Float, radius:Float, r:Float, g:Float, b:Float, a:Float)(implicit pgl:PGraphicsOpenGL) = {
      val gl = pgl.beginGL
      val increment = 2*PI/50f

      gl.glEnable( GL.GL_BLEND )
      gl.glBlendFunc( GL.GL_SRC_ALPHA, GL.GL_ONE_MINUS_SRC_ALPHA )
      gl.glColor4f( r, g, b, a )
      for ( angle <- Iterator.iterate(0f)(_ + increment) takeWhile (_ < 2*PI) ){
        gl.glBegin( GL.GL_POLYGON )
        gl.glVertex2d( x, y )
        gl.glVertex2d( x + cos(angle)* radius, y + sin(angle) * radius )
        gl.glVertex2d( x + cos(angle + increment) * radius, y + sin(angle + increment) * radius )
        gl.glEnd
      }
      gl.glDisable( GL.GL_BLEND )
      pgl.endGL
    }

  }

}