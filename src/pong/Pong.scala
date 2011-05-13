/**
 * FLUID PONG
 * Multi-touch pong in Scala/Processing based on MSA Fluid and Box2D.
 * Copyright (c) Valentin Simonov, Interactive Lab.
 * Distributed under MIT license.
 */
package pong {

  import effects.GoalEffect
  import processing.core._
  import processing.core.PConstants._
  import processing.opengl._

  import org.jbox2d.common.Vec2
  import org.jbox2d.collision._

  import java.awt.event._
  import collection.mutable.{HashMap, ArrayBuffer}
  import scala.util.Random

  import msafluid._
  import shapes.PolygonShape
  import TUIO._
  import scala.math._

  import pong.elements._
  import particles._
  import org.jbox2d.dynamics._

  object Pong extends PApplet {

    val FLUID_WIDTH = 120f

    val METERS2PIXELS = 30f
    val PIXELS2METERS = 1f/30f
    val SYSTEM_PADDING = 100
    val RACKET_RADIUS = 30f
    val RACKET_CATEGORY = 2
    val BALL_CATEGORY = 4
    val WALL_CATEGORY = 8

    val PADDING = 40
    val GATES = 200f

    val PLAYER_LEFT = 1
    val PLAYER_RIGHT = 2

    implicit var instance:Pong = _

    def main(args: Array[String]) = {
      instance = new Pong

      val frame = new javax.swing.JFrame("Pong")
      frame.removeNotify
      frame.setUndecorated( true )
      frame.setLocation( 0, 0 )
      frame.getContentPane().add( instance )

      instance.init

      frame.pack
      frame.setVisible( true )
      frame.addWindowListener( new WindowAdapter() {
        override def windowClosing(e: WindowEvent) = System.exit(0)
      })
    }

    implicit def Any2TuioCursor(value:Any) = value.asInstanceOf[TuioCursor]

  }

  class Pong extends PApplet {

    import Pong._

    var invWidth, invHeight, aspectRatio, aspectRatio2: Float = _

    implicit var fluidSolver:MSAFluidSolver2D = _
    var particleSystem:ParticleSystem = _

    implicit var graphics:PGraphicsOpenGL = _
    var imgFluid:PImage = _
    var myFont:PFont = _

    implicit var world:World = _
    var ground:Body = _
    var walls = new ArrayBuffer[Wall]
    var leftWall:Wall = _
    var rightWall:Wall = _
    var ball:Ball = _
    var listener:BallContactListener = _

    var leftEffect:GoalEffect = _
    var rightEffect:GoalEffect = _

    var tuioClient:TuioProcessing = _
    var rackets = new ArrayBuffer[Racket]
    var removedRackets = new ArrayBuffer[Racket]
    var cursors = new HashMap[TuioCursor,Racket]

    var logo:PImage = _
    var logo2:PImage = _

    var willDestroy = false

    // -----------------------------------------------------------------------
    // setup
    // -----------------------------------------------------------------------

    override def setup() = {
      size(1280, 800, OPENGL)
      hint( ENABLE_OPENGL_2X_SMOOTH )

      invWidth = 1.0f/width
      invHeight = 1.0f/height
      aspectRatio = width * invHeight
      aspectRatio2 = aspectRatio * aspectRatio
      graphics = g.asInstanceOf[PGraphicsOpenGL]

      leftEffect = new GoalEffect( 0, (height - GATES) * .5f, GATES, 50 )
      rightEffect = new GoalEffect( width, (height - GATES) * .5f, GATES, -50 )

      logo = loadImage( "logo.png" )
      logo2 = loadImage( "logo2.png" )

      initFluids
      initBox2D
      initTUIO

    }

    def initFluids() = {
      fluidSolver = new MSAFluidSolver2D( FLUID_WIDTH.toInt, (FLUID_WIDTH * height/width).toInt )
      fluidSolver.enableRGB( true ).setFadeSpeed( 0.02f ).setDeltaT( 0.9f ).setVisc( 0.00005f )
      imgFluid = createImage( fluidSolver.getWidth, fluidSolver.getHeight, RGB )
      particleSystem = new ParticleSystem
    }

    def initBox2D() = {
      world = new World( new Vec2( 0f, 0f ), false )
      world.setAutoClearForces( true )

      //ground
      val shapeDef = new PolygonShape
      shapeDef.setAsEdge(new Vec2(-40.0f, 0.0f), new Vec2(40.0f, 0.0f));
      val bodyDef = new BodyDef
      bodyDef.`type` = BodyType.STATIC
      bodyDef.position.set( new Vec2(0f, 0f) )
      ground = world.createBody( bodyDef )
      ground.createFixture( shapeDef, 0f )

      val wallWidth = width
      val wallHeight = PADDING * 2
      val gatesHeight = height * .5f - GATES * .5f

      // top/bottom
      walls += new Wall( wallWidth, wallHeight, width * .5f, 0 )
      walls += new Wall( wallWidth, wallHeight, width * .5f, height )
      // left/right
      leftWall = new Wall( wallHeight, wallWidth, -PADDING, wallWidth * .5f )
      rightWall = new Wall( wallHeight, wallWidth, width + PADDING, wallWidth * .5f )
      walls += leftWall
      walls += rightWall
      // gates
      walls += new Wall( wallHeight, gatesHeight, 0, gatesHeight * .5f )
      walls += new Wall( wallHeight, gatesHeight, 0, height - gatesHeight * .5f )
      walls += new Wall( wallHeight, gatesHeight, width, gatesHeight * .5f )
      walls += new Wall( wallHeight, gatesHeight, width, height - gatesHeight * .5f )

      listener = new BallContactListener
      listener.addListener( onBallCollide )
      world.setContactListener( listener )

      resetBall
    }

    def initTUIO = {
      tuioClient = new TuioProcessing( this, 3333 )
    }

    // -----------------------------------------------------------------------
    // draw
    // -----------------------------------------------------------------------

    override def draw() = {
      removeRackets
      updateTUIO
      world.step(1/frameRate, 3, 3)

      fluidSolver.update

      for( i <- 0 to fluidSolver.getNumCells-1 ) {
          val d = 2
          imgFluid.pixels(i) = color(fluidSolver.r(i) * d, fluidSolver.g(i) * d, fluidSolver.b(i) * d)
      }
      imgFluid.updatePixels

      image(imgFluid, 0, 0, width, height)
      image(logo, (width-logo.width)*.5f, height-100, logo.width, logo.height)
      image(logo2, (width-logo2.width)*.5f, 100-logo2.height, logo2.width, logo2.height)

      particleSystem.updateAndDraw(width, invWidth, height, invHeight)
      leftEffect.update
      rightEffect.update

      updateBall
      updateRackets
      updateWalls

      println( frameRate )
    }

    // -----------------------------------------------------------------------
    // update
    // -----------------------------------------------------------------------

    def updateTUIO = {

    }

    // -----------------------------------------------------------------------
    // ball
    // -----------------------------------------------------------------------

    def updateBall():Unit = {
      if ( willDestroy ) {
        try {
          resetBall
        } catch {
            case e =>
              println( e.toString )
              return
        }
        willDestroy = false;
      }

      var r = 0f
      var g = 0f
      var b = 0f
      val nx = (ball.x * invWidth * imgFluid.width).toInt
      val ny = (ball.y * invHeight * imgFluid.height).toInt
      var minNx = nx - 2
      var maxNx = nx + 2
      var minNy = ny - 2
      var maxNy = ny + 2
      if ( minNx < 0 ) minNx = 0
      if ( maxNx > imgFluid.width-1 ) maxNx = imgFluid.width-1
      if ( minNy < 0 ) minNy = 0
      if ( maxNy > imgFluid.height-1 ) maxNy = imgFluid.height-1
      for ( i <- minNx to maxNx ) {
        for ( j <- minNy to maxNy ) {
          val color = imgFluid.pixels( j * imgFluid.width + i )
          val newr = color >> 16 & 0xFF
          val newg = color >> 8 & 0xFF
          val newb = color & 0xFF
          if ( r+g+b < newr+newg+newb ) {
            r = newr
            g = newg
            b = newb
          }
        }
      }

      ball.update(r/255f, g/255f, b/255f)
    }

    def destroyBall() = {
      if ( ball != null ) {
        ball.destroy
        ball = null
      }
    }

    def resetBall():Unit = {
      destroyBall

      ball = new Ball
      listener.setBall( ball )
      ball.moveTo( width * .5f, height * .5f )
      ball.applyForce( (Random.nextFloat - .5f) * 20f, (Random.nextFloat - .5) * 20f )
    }

    // -----------------------------------------------------------------------
    // rackets
    // -----------------------------------------------------------------------

    def addRacket(x:Int, y:Int) = {
      val racket = new Racket( ground, RACKET_RADIUS, x, y )
      rackets += racket
      racket
    }

    def addRacket(x:Float, y:Float):Unit = {
      addRacket( x.toInt, y.toInt )
    }

    def updateRackets() = {
      rackets.foreach( _.update )
    }

    def removeRacket(racket:Racket) = {
      rackets -= racket
      removedRackets += racket
    }

    def removeRackets() = {
      removedRackets.foreach { _.destroy }
      removedRackets.clear
    }

    // -----------------------------------------------------------------------
    // walls
    // -----------------------------------------------------------------------

    def updateWalls() = {
      walls.foreach( _.update(this, fluidSolver) )
    }

    def flashWalls() = {
      walls.foreach( _.flash )
    }

    // -----------------------------------------------------------------------
    // misc
    // -----------------------------------------------------------------------

    def addForce(x:Float, y:Float, dx:Float, dy:Float, numParticles:Int = 10) = {
      val speed = dx * dx  + dy * dy * aspectRatio2

      if( speed > 0 ) {
        var xx = x
        var yy = y

        if ( xx < 0 ) {
          xx = 0
        } else if ( xx > 1 ) {
          xx = 1
        }

        if ( yy < 0 ) {
          yy = 0
        } else if ( yy > 1 ) {
          yy = 1
        }

        val colorMult = 5.0f
        val velocityMult = 100.0f

        val index = fluidSolver.getIndexForNormalizedPosition( xx, yy )

        colorMode( HSB, 360, 1, 1 )
        val hue = ((xx + yy) * 180 + frameCount) % 360
        val drawColor = color( hue, 1, 1 )
        colorMode( RGB, 1 )

        fluidSolver.rOld(index)  += red(drawColor) * colorMult
        fluidSolver.gOld(index)  += green(drawColor) * colorMult
        fluidSolver.bOld(index)  += blue(drawColor) * colorMult

        if ( numParticles > 0 ) particleSystem.addParticles(xx * width, yy * height, numParticles)
        fluidSolver.uOld(index) += dx * velocityMult
        fluidSolver.vOld(index) += dy * velocityMult
      }
    }

    def goal(player:Int) = {
      willDestroy = true
      flashWalls

      var x:Int = 0
      var dir:Int = 0
      player match {
        case PLAYER_LEFT =>
          leftEffect.start
        case PLAYER_RIGHT =>
          rightEffect.start
      }
    }

    // -----------------------------------------------------------------------
    // handlers
    // -----------------------------------------------------------------------

    override def mouseMoved = {
      val mouseNormX = mouseX * invWidth
      val mouseNormY = mouseY * invHeight
      val mouseVelX = (mouseX - pmouseX) * invWidth
      val mouseVelY = (mouseY - pmouseY) * invHeight

      addForce( mouseNormX, mouseNormY, mouseVelX, mouseVelY )
    }

    def onBallCollide(body:Body) = {
      val position = ball.body.getPosition.mul( METERS2PIXELS )
      for ( i <- 0 to 9 ) {
        val angle = PI*2f/10f*i
        val c = cos(angle).toFloat
        val s = sin(angle).toFloat
        addForce( (position.x + 60*c) * invWidth, (position.y + 60*s) * invHeight, c * 10 * invWidth, s * 10  * invHeight )
      }

      if ( body == leftWall.body ) {
        goal( PLAYER_LEFT )
      } else if ( body == rightWall.body ) {
        goal( PLAYER_RIGHT )
      }
    }

    // -----------------------------------------------------------------------
    // TUIO
    // -----------------------------------------------------------------------

    def addTuioCursor(tcur:TuioCursor):Unit = {
      val racket = addRacket( tcur.getScreenX(width), tcur.getScreenY(height) )
      cursors(tcur) = racket
    }

    def updateTuioCursor(tcur:TuioCursor) = {
      val racket = cursors(tcur)
      var x = tcur.getScreenX( width )
      var y = tcur.getScreenY( height )
      if ( x < RACKET_RADIUS ) x = RACKET_RADIUS.toInt
      else if ( x > width - RACKET_RADIUS ) x = width - RACKET_RADIUS.toInt
      if ( y < RACKET_RADIUS ) y = RACKET_RADIUS.toInt
      else if ( y > height - RACKET_RADIUS ) y = height - RACKET_RADIUS.toInt
      racket.moveTo( x, y )
    }

    def removeTuioCursor(tcur:TuioCursor):Unit = {
      val racket = cursors( tcur )
      cursors.remove( tcur )
      removeRacket( racket )
    }

    def addTuioObject(tobj:TuioObject) = {}
    def updateTuioObject(tobj:TuioObject) = {}
    def removeTuioObject(tobj:TuioObject) = {}
    def refresh(bundleTime:TuioTime) = {}

  }
}