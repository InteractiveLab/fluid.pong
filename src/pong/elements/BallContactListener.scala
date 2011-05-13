package pong.elements {

  import org.jbox2d.dynamics.Body
  import org.jbox2d.dynamics.contacts.Contact
  import org.jbox2d.collision.Manifold
  import org.jbox2d.callbacks.{ContactImpulse, ContactListener}

  class BallContactListener() extends ContactListener {

    // -----------------------------------------------------------------------
    // private vars
    // -----------------------------------------------------------------------

    private var ball:Ball = _

    private var listeners: List[ (Body) => Unit ] = Nil

    // -----------------------------------------------------------------------
    // methods
    // -----------------------------------------------------------------------

    def setBall(value:Ball) = {
      ball = value
    }

    def addListener(listener: (Body) => Unit) {
      listeners ::= listener
    }

    // -----------------------------------------------------------------------
    // ContactListener
    // -----------------------------------------------------------------------

    def beginContact(contact:Contact) = {
      val body1 = contact.m_fixtureA.m_body
      val body2 = contact.m_fixtureB.m_body
      if ( body1 == ball.body ) {
        dispatch( body2 )
      } else if ( body2 == ball.body ) {
        dispatch( body1 )
      }
    }

    def endContact(contact:Contact) = {

    }

    def preSolve(contact:Contact, oldManifold:Manifold) = {

    }

    def postSolve(contact:Contact, impulse:ContactImpulse) = {

    }

    // -----------------------------------------------------------------------
    // private functions
    // -----------------------------------------------------------------------

    private def dispatch(body:Body) = {
      for ( listener <- listeners ) listener( body )
    }

  }

}