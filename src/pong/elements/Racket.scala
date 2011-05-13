package pong.elements {

  import javax.media.opengl.GL
  import msafluid.MSAFluidSolver2D
  import org.jbox2d.common.Vec2
  import org.jbox2d.dynamics.joints.{MouseJointDef, MouseJoint}
  import pong.Pong._
  import math._
  import processing.core.PConstants._
  import processing.opengl.PGraphicsOpenGL
  import util.Random
  import pong.Utils._
  import pong.Pong
  import org.jbox2d.dynamics._
import org.jbox2d.collision.shapes.{PolygonShape, MassData, CircleShape}

object Racket {

    val FORCE = .3f

  }

  class Racket(private val ground:Body, private var _radius:Float, private var _x:Float, private var _y:Float)(implicit world:World) {

    import Racket._

    // -----------------------------------------------------------------------
    // private vars
    // -----------------------------------------------------------------------

    private var _lastFrameX = -1f
    private var _lastFrameY = -1f
    private var _rotation = 0f

    private var _body:Body = _
    private var _joint:MouseJoint = _

    create

    // -----------------------------------------------------------------------
    // properties
    // -----------------------------------------------------------------------

    def body = _body

    def radius = _radius

    def radius_= (value:Float) = {
      _radius = value
      create
    }

    def x = _x

    def x_= (value:Float) = {
      if ( _lastFrameX == -1f ) _lastFrameX = _x
      _x = value
      updatePosition
    }

    def y = _y

    def y_= (value:Float) = {
      if ( _lastFrameY == -1f ) _lastFrameY = _y
      _y = value
      updatePosition
    }

    // -----------------------------------------------------------------------
    // methods
    // -----------------------------------------------------------------------

    def isHit(x:Int, y:Int) = {
      val dx = x - _x
      val dy = y - _y
      if ( dx*dx + dy*dy < _radius*_radius ) {
        true
      } else {
        false
      }
    }

    def moveTo(x:Int, y:Int) = {
      if ( _lastFrameX == -1f ) _lastFrameX = _x
      if ( _lastFrameY == -1f ) _lastFrameY = _y
      _x = x
      _y = y
      updatePosition
    }

    def update(implicit host:Pong, fluidSolver:MSAFluidSolver2D, pgl:PGraphicsOpenGL):Unit = {
      if ( _body == null ) return
      
      _body.setAngularVelocity( 0f )

      if ( _lastFrameX == -1f ) _lastFrameX = _x
      if ( _lastFrameY == -1f ) _lastFrameY = _y
      val dx = (_x - _lastFrameX)
      val dy = (_y - _lastFrameY)
      val forceX = dx * host.invWidth * FORCE
      val forceY = dy * host.invHeight * FORCE
      host.addForce( _x * host.invWidth, _y * host.invHeight, forceX, forceY, 1 )
      _lastFrameX = -1f
      _lastFrameY = -1f

      val increment = PI/1f
      for ( angle <- Iterator.iterate(0f)(_ + increment) takeWhile (_ < 2*PI) ){
        val r = _radius * .5f
        val s = sin(_rotation + angle).toFloat
        val c = cos(_rotation + angle).toFloat
        host.addForce( (_x + r * s) * host.invWidth, (_y + r * c) * host.invHeight, s * host.invWidth * .2f, c * host.invHeight * .3f, 1 )
      }
      _rotation += PI/40f

       drawCircle( _x, _y, _radius, .1f, .1f, .1f, .4f )
    }

    def destroy():Unit = {
      if ( _joint != null ) world.destroyJoint( _joint )
      if ( _body != null ) world.destroyBody( _body )
      _body = null
      _joint = null
    }

    // -----------------------------------------------------------------------
    // private functions
    // -----------------------------------------------------------------------

    private def create() = {
      if ( _body != null ) {
        destroy
      }

      val shapeDef = new CircleShape
      shapeDef.m_radius = _radius*PIXELS2METERS

      val fixtureDef = new FixtureDef
      fixtureDef.shape = shapeDef
      fixtureDef.density = 1f
      fixtureDef.friction = .3f
      fixtureDef.restitution = .5f
      fixtureDef.filter.categoryBits = RACKET_CATEGORY
      fixtureDef.filter.maskBits = BALL_CATEGORY

      val bodyDef = new BodyDef
      bodyDef.`type` = BodyType.DYNAMIC
      bodyDef.position.set( x * PIXELS2METERS, y * PIXELS2METERS )

      _body = world.createBody( bodyDef )
      _body.createFixture( fixtureDef )

      val jointDef = new MouseJointDef
      jointDef.bodyA = ground
      jointDef.bodyB = _body
      jointDef.target.set( _x * PIXELS2METERS, _y * PIXELS2METERS )
      jointDef.maxForce = 30000
      jointDef.frequencyHz = 60

      _joint = world.createJoint( jointDef ).asInstanceOf[MouseJoint]
      _body.setAwake( true )
    }

    private def updatePosition() = {
      if ( _body != null && _joint != null ) {
        _joint.setTarget( new Vec2(_x * PIXELS2METERS, _y * PIXELS2METERS) )
      }
    }

  }

}