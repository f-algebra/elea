package hoverboard.term

/**
  * A mapping with alpha-equality based lookup. Also preserves insertion order.
  */
class UMap[K <: TermLike[K], V] private(val toSeq: Seq[(K, V)]) {

  def insert(k: K, v: V): UMap[K, V] =
    new UMap(toSeq.filterNot(_._1 =@= k) :+ (k, v))

  def lookup(k: K): Option[V] =
    toSeq.find(_._1 =@= k).map(_._2)

  def +(kv: (K, V)): UMap[K, V] =
    insert(kv._1, kv._2)

  def filterKeys(p: K => Boolean): UMap[K, V] =
    new UMap(toSeq.filter { case (k, _) => p(k) })

  def size: Int = toSeq.size
}

object UMap {
  def empty[K <: TermLike[K], V]: UMap[K, V] =
    new UMap[K, V](Seq.empty)
}
