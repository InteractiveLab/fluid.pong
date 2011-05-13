package pong.elements {

  import msafluid.MSAFluidSolver2D
  import pong.Pong._
  import processing.opengl.PGraphicsOpenGL
  import pong.Utils._
  import pong.Pong
  import org.jbox2d.common.Vec2
  import org.jbox2d.collision.shapes.CircleShape
  import org.jbox2d.dynamics._

  object Ball {

    val RADIUS = 10f

  }

  class Ball(implicit world:World) {

    import Ball._

    create

    // -----------------------------------------------------------------------
    // private vars
    // -----------------------------------------------------------------------

    private var _body:Body = _
    private var _x = 0
    private var _y = 0

    // -----------------------------------------------------------------------
    // properties
    // -----------------------------------------------------------------------

    def body = _body

    def x = _x

    def y = _y

    // -----------------------------------------------------------------------
    // methods
    // -----------------------------------------------------------------------

    def moveTo(x:Int, y:Int):Unit = {
      body.setTransform( new Vec2(x * PIXELS2METERS, y * PIXELS2METERS), _body.getAngle )
      _body.setLinearVelocity( new Vec2(0, 0) )
      updatePosition
    }

    def moveTo(x:Float, y:Float):Unit = {
      moveTo( x.toInt, y.toInt )
    }

    def applyForce(x:Float, y:Float) = {
      _body.applyForce( new Vec2(x, y), new Vec2(0, 0) )
    }

    def applyForce(x:Double, y:Double):Unit = {
      applyForce( x.toFloat, y.toFloat )
    }

    def updatePosition() = {
      val position = _body.getPosition.mul( METERS2PIXELS )
      _x = position.x.toInt
      _y = position.y.toInt
    }

    def update(r:Float, g:Float, b:Float)(implicit host:Pong, fluidSolver:MSAFluidSolver2D, pgl:PGraphicsOpenGL):Unit = {
      if ( _body == null ) return

      updatePosition
      val velocity = _body.getLinearVelocity

      val index = fluidSolver.getIndexForNormalizedPosition( _x * host.invWidth, _y * host.invHeight )
      host.addForce( _x * host.invWidth, _y * host.invHeight, velocity.x * host.invWidth * .02f, velocity.y * host.invHeight * .02f, 3 )
      velocity.x += fluidSolver.u(index)*20f
      velocity.y += fluidSolver.v(index)*20f
      _body.setLinearVelocity( velocity )

      drawCircle( _x, _y, RADIUS*3f, r, g, b, .3f )
      drawCircle( _x, _y, RADIUS*1.8f, r, g, b, .7f )
      drawCircle( _x, _y, RADIUS*1.2f, r, g, b, 1f )
      drawCircle( _x, _y, RADIUS*1.1f, 0f, 0f, 0f, .1f )
      drawCircle( _x, _y, RADIUS*0.9f, 1f, 1f, 1f, 1f )

    }

    def destroy() = {
      world.destroyBody( _body )
      _body = null
    }

    // -----------------------------------------------------------------------
    // private functions
    // -----------------------------------------------------------------------

    private def create() = {
      val shapeDef = new CircleShape
      shapeDef.m_radius = RADIUS*PIXELS2METERS

      val fixtureDef = new FixtureDef
      fixtureDef.shape = shapeDef
      fixtureDef.density = 1f
      fixtureDef.friction = .3f
      fixtureDef.restitution = .5f
      fixtureDef.filter.categoryBits = BALL_CATEGORY
      fixtureDef.filter.maskBits = RACKET_CATEGORY | WALL_CATEGORY

      val bodyDef = new BodyDef
      bodyDef.`type` = BodyType.DYNAMIC
      bodyDef.position.set( x * PIXELS2METERS, y * PIXELS2METERS )
      bodyDef.bullet = true

      _body = world.createBody( bodyDef )
      _body.createFixture( fixtureDef )
//      _body.setMassFromShapes

    }

  }
}