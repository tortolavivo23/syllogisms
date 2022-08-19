package syllogisms
package figure1


object Barbari extends Syllogism{

  // All M are P
  type Major = (x: Entity) => x.M => x.P

  // All S are M and some S exists
  trait Minor{
    val f: (x: Entity) => x.S => x.M;
    val x: Entity;
    val value: x.S
  }

  // Some S are P
  trait Conclusion { val x: Entity; val value: (x.S, x.P) }

  def proof(major: Major, minor: Minor): Conclusion = new Conclusion {
    val x: minor.x.type = minor.x
    val value = (minor.value, (major(x) compose minor.f(x))(minor.value))
  }

}
