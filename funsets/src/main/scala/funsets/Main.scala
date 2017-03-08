package funsets

object Main extends App {
  import FunSets._
  val s1 = singletonSet(1)
  val s2 = singletonSet(2)
  val s3 = singletonSet(3)
  val s = union(s1, union(s2, s3))
  println(contains(s, 1) && contains(s, 2) && contains(s, 3))
  val sf = map(s, x => x * 2)
  println(contains(sf, 1) && contains(sf, 2) && contains(sf, 3))
  println(exists(sf, x => x == 9))
}
