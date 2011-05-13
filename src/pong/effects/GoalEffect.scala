package pong.effects {

  import pong.Pong
  import math._

  object GoalEffect {

    val FORCE_MUL = .5f

  }

  class GoalEffect(x:Float, y:Float, height:Float, force:Float)(implicit host:Pong) {

    import GoalEffect._

    private var _curForce:Float = _

    def start() = {
      _curForce = force
    }

    def update() = {
      if ( _curForce != 0 ) {
        for ( i <- 0 to 9 ) {
          host.addForce( x * host.invWidth, (y + height / 10 * i) * host.invHeight, _curForce * host.invWidth, 0, 2 )
        }

        _curForce *= FORCE_MUL
        if ( abs(_curForce) < 0.1f ) _curForce = 0f
      }
    }

  }

}