import scala.io.Source

case class Player(hp: Int, mana: Int, armor: Int) {
  def heal(amount: Int) = Player(hp + amount, mana, armor)
  def hit(amt: Int) = Player(hp - (amt - armor), mana, armor)
}

case class Boss(hp: Int, damage: Int) {
  def hit(amt: Int) = Boss(hp - amt, damage)
}

abstract class Spell(val name: String, val cost: Int)(implicit val rounds: Int = 0) {
  def execute(p: Player, b: Boss): (Player, Boss, List[Spell])
  def damage(hit: Int, b: Boss) = Boss(b.hp - hit, b.damage)
}

object MagicMissile extends Spell("MagicMissile", 53) {
  def execute(p: Player, b: Boss) = (p, b.hit(4), Nil)
}

object Drain extends Spell("Drain", 73) {
  def execute(p: Player, b: Boss) = (p.heal(2), b.hit(2), Nil)
}

case class Shield(r: Int = 7) extends Spell("Shield", 113)(r) {
  def execute(p: Player, b: Boss) = this.rounds match {
    case 7 => (p, b, List(Shield(6)))
    case 0 => (Player(p.hp, p.mana, 0), b, List(Shield(0)))
    case _ => (Player(p.hp, p.mana, 7), b, List(Shield(rounds - 1)))
  }
}

case class Poison(r: Int = 7) extends Spell("Poison", 173)(r) {
  def execute(p: Player, b: Boss) = this.rounds match {
    case 7 => (p, b, List(Poison(6)))
    case _ => (p, b.hit(3), List(Poison(rounds - 1)))
  }
}

case class Recharge(r: Int = 6) extends Spell("Recharge", 229)(r) {
  def execute(p: Player, b: Boss) = this.rounds match {
    case 6 => (p, b, List(Recharge(5)))
    case _ => (Player(p.hp, p.mana + 101, p.armor), b, List(Recharge(rounds - 1)))
  }
}

object Day22 extends App {

  val regex = """(.+?): (\d+)""".r

  val data = Source
    .fromFile("inputs/input_day22.txt")
    .getLines
    .map {
      case regex(name, value) => name -> value.toInt
    }
    .toMap

  def allSpells = List(Recharge(), MagicMissile, Drain, Shield(), Poison())

  def bu() = {
    def canApply(p: Player, spells: List[Spell], spell: Spell) = {
      if (spell.cost > p.mana) false
      else if ((spells.isEmpty) || (spell.rounds == 0))  true
      else {
        val ex = spells.find(_.name == spell.name)
        ex.isEmpty || ex.get.rounds == 0
      }
    }

    def executeRounds(p: Player, b: Boss, spells: List[Spell]) = {
      def executeRound(cp: Player, cb: Boss, cs: List[Spell], acc: List[Spell], f: (Player, Boss, List[Spell]) => (Player, Boss, List[Spell])): (Player, Boss, List[Spell]) = {
        if (cs.isEmpty) f(cp, cb, acc)
        else {
          val (np, nb, ns) = cs.head.execute(cp, cb)
          executeRound(np, nb, cs.tail, acc ++ ns, f)
        }
      }

      val (np, nb, ns) = executeRound(p, b, spells, Nil, (rp, rb, rs) => (rp, rb, rs.filter(_.rounds > 0)))
      if ((np.hp < 0) || (nb.hp < 0)) (np, nb, ns)
      else executeRound(np, nb, ns, Nil, (cp, cb, cs) => {
        val (rp, rb, rs) = (cp.hit(cb.damage), cb, cs)
        val exShield = rs.find(_.name == "Shield")
        if (exShield.isEmpty) (rp, rb, rs.filter(_.rounds > 0))
        else {
          val (rrp, rrb, rrs) = exShield.get.execute(rp, rb)
          (rrp, rrb, rrs ++ rs.filter(_.rounds > 0))
        }
      })
    }

    var minSpent = Int.MaxValue
    def ba(p: Player, b: Boss, spells: List[Spell], acc: Int): (Boolean, Int) = {
      if (acc > minSpent) (false, acc)
      else if (b.hp < 0) { println(s"killed boss spending $acc"); minSpent = acc; (true, acc) }
      else if (p.hp < 0) (false, acc)
      else allSpells
        .filter(canApply(p, spells, _))
        .map(s => (s.cost, executeRounds(Player(p.hp, p.mana - s.cost, p.armor), b, spells :+ s)))
        .map {
          case (cost, nextData) => ba(nextData._1, nextData._2, nextData._3, acc + cost)
        }
        .filter(_._1)
        .foldLeft((false, Int.MaxValue))((a, b) => if (a._2 < b._2) a else b)
    }

    ba(Player(50, 500, 0), Boss(data("Hit Points"), data("Damage")), Nil, 0)._2
  }

  println(bu())

}