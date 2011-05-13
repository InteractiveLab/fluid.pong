package pong.elements {

  import msafluid.MSAFluidSolver2D
  import processing.core.PConstants._
  import pong.Pong._
  import pong.Pong
  import org.jbox2d.collision.shapes.PolygonShape
  import org.jbox2d.dynamics._

  object Wall {

    val MIN_COLOR = .2f
    val MAX_COLOR = 1f
    val DELTA_COLOR = .025f;

  }

  class Wall(private var width:Float, private var height:Float, private var x:Float, private var y:Float)(implicit world:World) {

    import Wall._

    private var _body:Body = _
    private var _color = MIN_COLOR

    create

    // -----------------------------------------------------------------------
    // properties
    // -----------------------------------------------------------------------

    def body = _body

    // -----------------------------------------------------------------------
    // methods
    // -----------------------------------------------------------------------

    def flash() = {
      _color = MAX_COLOR
    }

    def update(implicit host:Pong, fluidSolver:MSAFluidSolver2D) = {
      if ( _color > MIN_COLOR ) _color -= DELTA_COLOR

      host.noStroke
      host.fill( _color, _color, _color )
      host.rectMode( CENTER )
      host.rect( x, y, width, height )
    }

    // -----------------------------------------------------------------------
    // private functions
    // -----------------------------------------------------------------------

    private def create() = {
      val shapeDef = new PolygonShape
      shapeDef.setAsBox( width * .5f * PIXELS2METERS, height * .5f * PIXELS2METERS )

      val fixtureDef = new FixtureDef
      fixtureDef.shape = shapeDef
      fixtureDef.density = 1f
      fixtureDef.friction = .3f
      fixtureDef.restitution = .5f
      fixtureDef.filter.categoryBits = WALL_CATEGORY
      fixtureDef.filter.maskBits = BALL_CATEGORY

      val bodyDef = new BodyDef
      bodyDef.`type` = BodyType.STATIC
      bodyDef.position.set( x * PIXELS2METERS, y * PIXELS2METERS )

      _body = world.createBody( bodyDef )
      _body.createFixture( fixtureDef )
    }

  }

}