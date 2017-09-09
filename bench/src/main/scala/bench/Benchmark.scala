package bench

import scala.collection.mutable

case class Benchmark(name: String,
                     cases: Benchmark.Case[_]*)
object Benchmark{
  case class Case[T](name: String,
                     initializer: Int => T)
                    (callback: T => (Any, Int)) {
    def run(n: Int): Int = callback(initializer(n))._2
  }
  def pair[T](t: => T) = (t, t)
  val nullO: Object = null
  def obj: Object = new Object()
  val benchmarks = Seq(
    Benchmark(
      "construct",
      Case("List", n=>n){ n =>
        var b = List.empty[Object]
        var i = 0
        while(i < n){
          b = obj :: b
          i += 1
        }
        (b, n)
      },
      Case("Vector", n=>n){ n =>
        var b = Vector.empty[Object]
        var i = 0
        while(i < n){
          b = obj +: b
          i += 1
        }
        (b, n)
      },
      Case("Set", n=>n){ n =>
        var b = Set.empty[Object]
        var i = 0
        while(i < n){
          b = b + obj
          i += 1
        }
        (b, n)
      },
      Case("Map", n=>n){ n =>
        var b = Map.empty[Object, Object]
        var i = 0
        while(i < n){
          b = b + (obj -> obj)
          i += 1
        }
        (b, n)
      },
      Case("Array:+", n=>n){ n =>
        var b = new Array[Object](0)
        var i = 0
        while(i < n){
          b = b :+ obj
          i += 1
        }
        (b, n)
      },
      Case("Array-prealloc", n=>n){ n =>
        val b = new Array[Object](n)
        var i = 0
        while(i < n){
          b(i) = obj
          i += 1
        }
        (b, n)
      },
      Case("Array.toSet", n=>n){ n =>
        val b = new Array[Object](n)
        var i = 0
        while(i < n){
          b(i) = obj
          i += 1
        }
        (b.toSet, n)
      },
      Case("Array.toVector", n=>n){ n =>
        val b = new Array[Object](n)
        var i = 0
        while(i < n){
          b(i) = obj
          i += 1
        }
        (b.toVector, n)
      },
      Case("Array.toMap", n=>n){ n =>
        val b = new Array[(Object, Object)](n)
        var i = 0
        while(i < n){
          b(i) = (obj, obj)
          i += 1
        }
        (b.toMap, n)
      },
      Case("m.Buffer", n=>n){ n =>
        val b = mutable.Buffer.empty[Object]
        var i = 0
        while(i < n){
          b.append(obj)
          i += 1
        }
        (b, n)
      },
      Case("m.Set", n=>n){ n =>
        val b = mutable.Set.empty[Object]
        var i = 0
        while(i < n){
          b.add(obj)
          i += 1
        }
        (b, n)
      },
      Case("m.Map", n=>n){ n =>
        val b = mutable.Map.empty[Object, Object]
        var i = 0
        while(i < n){
          b.put(obj, obj)
          i += 1
        }
        (b, n)
      }
    ),
//    Benchmark(
//      "deconstruct",
//      Case("List.tail", List.fill(_)(obj)){ a =>
//        var x = a
//        while(x.nonEmpty) x = x.tail
//        x
//      },
//      Case("Vector.tail", Vector.fill(_)(obj)){ a =>
//        var x = a
//        while(x.nonEmpty) x = x.tail
//        x
//      },
//      Case("Vector.init", Vector.fill(_)(obj)){ a =>
//        var x = a
//        while(x.nonEmpty) x = x.init
//        x
//      },
//      Case("Set.-", Array.fill(_)(obj).toSet){ a =>
//        var x = a
//        while(x.nonEmpty) x = x - x.head
//        x
//      },
//      Case("Map.-", Array.fill(_)(obj -> obj).toMap){ a =>
//        var x = a
//        while(x.nonEmpty) x = x.-(x.head._1)
//        x
//      },
//      Case("Array.tail", Array.fill(_)(obj)){ a =>
//        var x = a
//        while(x.nonEmpty) x = x.tail
//        x
//      },
//      Case("m.Buffer.remove", x => mutable.Buffer.fill(x)(obj)){ a =>
//        while (a.nonEmpty) a.remove(a.length-1)
//        a
//      },
//      Case("m.Set.remove", Array.fill(_)(obj).to[mutable.Set]){ a =>
//        while (a.nonEmpty) a.remove(a.head)
//        a
//      },
//      Case("m.Map.remove", x => mutable.Map(Array.fill(x)(obj -> obj):_*)){ a =>
//        while (a.nonEmpty) a.remove(a.head._1)
//        a
//      }
//    ),
//    Benchmark(
//      "concat",
//      Case("List", x => pair(List.fill(x)(obj))){ case (a, b) =>
//        a ++ b
//      },
//      Case("Vector", x => pair(Vector.fill(x)(obj))){ case (a, b) =>
//        a ++ b
//      },
//      Case("Set", x => pair(Array.fill(x)(obj).toSet)){ case (a, b) =>
//        a ++ b
//      },
//      Case("Map", x => pair(Array.fill(x)(obj -> obj).toMap)){ case (a, b) =>
//        a ++ b
//      },
//      Case("Array++", x => pair(Array.fill(x)(obj))){ case (a, b) =>
//        a ++ b
//      },
//      Case("Array-arraycopy", x => pair(Array.fill(x)(obj))){ case (a, b) =>
//        val x = new Array[Object](a.length + b.length)
//        System.arraycopy(a, 0, x, 0, a.length)
//        System.arraycopy(b, 0, x, a.length, b.length)
//        x
//      },
//      Case("m.Buffer", x => pair(mutable.Buffer.fill(x)(obj))){ case (a, b) =>
//        a.appendAll(b)
//      },
//      Case("m.Set", x => pair(Array.fill(x)(obj).to[mutable.Set])){ case (a, b) =>
//        a ++= b
//      },
//      Case("m.Map", x => pair(mutable.Map(Array.fill(x)(obj -> obj):_*))){ case (a, b) =>
//        a ++= b
//      }
//    ),
    Benchmark(
      "foreach",
      Case("List", List.fill(_)(obj)){ a =>
        var last = nullO
        var count = 0
        var i = 0
        while(i < 10) {
          a.foreach{x => {last = x; count += 1}}
          i += 1
        }
        (last, count)
      },
      Case("List-while", List.fill(_)(obj)){ case a =>
        var last = nullO
        var count = 0
        var i = 0
        while(i < 10) {
          var j = a
          while(j.nonEmpty){
            last = j.head
            count += 1
            j = j.tail
          }
          i += 1
        }
        (last, count)
      },
      Case("Vector", Vector.fill(_)(obj)){ a =>
        var last = nullO
        var count = 0
        var i = 0
        //var last = nullO
        while(i < 10) {
          a.foreach{x => {last = x; count += 1}}
          i += 1
        }
        (last, count)
      },
      Case("Set", Array.fill(_)(obj).toSet){ a =>
        var i = 0
        var last = nullO
        var count = 0
        while(i < 10) {
          a.foreach(x => { last = x; count += 1})
          i += 1
        }
        (last, count)
      },
      Case("Map", Array.fill(_)(obj -> obj).toMap){ a =>
        var i = 0
        var last = nullO
        var count = 0
        while(i < 10) {
          a.foreach{x => {last = x; count += 1}}
          i += 1
        }
        (last, count)
      },
      Case("Array", Array.fill(_)(obj)){ a =>
        var i = 0
        var last = nullO
        var count = 0
        while(i < 10) {
          a.foreach{x => {last = x; count += 1}}
          i += 1
        }
        (last, count)
      },
      Case("Array-while", x => x -> Array.fill(x)(obj)){ case (n, a) =>
        var count = 0
        var last = nullO
        var i = 0
        while(i < 10) {
          var j = 0
          while(j < n){
            last = a(j)
            count += 1
            j += 1
          }
          i += 1
        }
        (last, count)
      },
      Case("m.Buffer", x => mutable.Buffer.fill(x)(obj)){ a =>
        var count = 0
        var last = nullO
        var i = 0
        while(i < 10) {
          a.foreach{x => {last = x; count += 1}}
          i += 1
        }
        (last, count)
      },
      Case("m.Set", Array.fill(_)(obj).to[mutable.Set]){ a =>
        var last = nullO
        var count = 0
        var i = 0
        while(i < 10) {
          a.foreach{x => {last = x; count += 1}}
          i += 1
        }
        (last, count)
      },
      Case("m.Map", x => mutable.Map(Array.fill(x)(obj -> obj):_*)){ a =>
        var last = nullO
        var count = 0
        var i = 0
        while(i < 10) {
          a.foreach{x => {last = x; count += 1}}
          i += 1
        }
        (last, count)
      }
    ),
    Benchmark(
        "lookup",
        Case("List", x => x -> List.fill(x)(obj)){ case (n, a) =>
          var i = 0
          var last = nullO
          var count = 0
          while(i < 10) {
            var j = 0
            while (j < n) {
              last = a(j)
              count += i
              j += 1
            }
            i += 1
          }
          (last, count)
        },
      Case("Vector", x => x -> Vector.fill(x)(obj)){ case (n, a) =>
        var count = 0
        var last = nullO
        var i = 0
        while(i < 10) {
          var j = 0
          while (j < n) {
            last = a(j)
            count += i
            j += 1
          }
          i += 1
        }
        (last, count)
      },
      Case("Set", x => {
        val r = Array.fill(x)(obj)
        r -> r.toSet
      }){ case (keys, a) =>
        var count = 0
        var last = false
        var i = 0
        while(i < 10) {
          var j = 0
          val n = keys.length
          while (j < n) {
            last = a(keys(j))
            count += i
            j += 1
          }
          i += 1
        }
        (last, count)
      },
      Case("Map", x => {
        val r = Array.fill(x)(obj -> obj).toMap
        r.keysIterator.toArray -> r
      }){ case (keys, a) =>
        var last = nullO
        var count = 0
        var i = 0
        while(i < 10) {
          var j = 0
          val n = keys.length
          while (j < n) {
            last = a(keys(j))
            count += i
            j += 1
          }
          i += 1
        }
        (last, count)
      },
      Case("Array", x => x -> Array.fill(x)(obj)){ case (n, a) =>
        var last = nullO
        var count = 0
        var i = 0
        while(i < 10) {
          var j = 0
          while(j < n){
            last = a(j)
            count += i
            j += 1
          }
          i += 1
        }
        (last, count)
      },
      Case("m.Buffer", x => x -> mutable.Buffer.fill(x)(obj)){ case (n, a) =>
        var last = nullO
        var i = 0
        var count = 0
        while(i < 10) {
          var j = 0
          while (j < n) {
            last = a(j)
            count += i
            j += 1
          }
          i += 1
        }
        (last, count)
      },
      Case("m.Set", x => {
        val r = Array.fill(x)(obj)
        r -> r.to[mutable.Set]
      }){ case (keys, a) =>
        var last = false
        var i = 0
        var count = 0
        while(i < 10) {
          val n = keys.length
          var j = 0
          while (j < n) {
            last = a(keys(j))
            count += i
            j += 1
          }
          i += 1
        }
        (last, count)
      },
      Case("m.Map", x => {
        val r = mutable.Map(Array.fill(x)(obj -> obj):_*)
        r.keysIterator.toArray -> r
      }){ case (keys, a) =>
        var last = nullO
        var i = 0
        var count = 0
        while(i < 10) {
          var j = 0
          val n = keys.length
          while (j < n) {
            last = a(keys(j))
            count += i
            j += 1
          }
          i += 1
        }
        (last, count)
      }
    ),
    Benchmark(
      "immutable update (replace)",
      Case("Vector", x => x -> Vector.fill(x)(obj)){ case (n, a) =>
        var count = a.length
        var vec = a
        var last = nullO
        var i = 0
        while(i < count) {
          vec = vec.updated(i, obj)
          i += 1
        }
        (vec, count)
      },
      Case("Map", x => {
        var count = 0
        val r = Array.fill(x){val pair = count -> obj; count += 1; pair}.toMap
        r.keysIterator.toArray -> r
      }){ case (keys, a) =>
        var count = a.size
        var map = a
        var i = 0
        while(i < count) {
          map += (i -> obj)
          i += 1
        }
        (map, count)
      },
      Case("Array (copy)", x => x -> Array.fill(x)(obj)){ case (n, a) =>
        var count = a.length
        var array = a
        var i = 0
        while(i < count) {
          array = array.clone
          array(i) = obj
          i += 1
        }
        (array, count)
      },
      Case("Array (in place)", x => x -> Array.fill(x)(obj)){ case (n, a) =>
        var count = a.length
        var array = a
        var i = 0
        while(i < count) {
          array(i) = obj
          i += 1
        }
        (array, count)
      },
    ),
    Benchmark(
      "immutable update (insert)",
      Case("Vector", x => x -> Vector.fill(x)(obj)){ case (n, a) =>
        var count = a.length
        var vec = a
        var last = nullO
        var i = 0
        while(i < count) {
          vec = vec.slice(0, i) ++ Seq(obj) ++ vec.slice(i + 1, count)
          //  vec.updated(i, obj)
          i += 1
        }
        (vec, count)
      },
      Case("Map", x => {
        var count = 0
        val r = Array.fill(x){val pair = count -> obj; count += 2; pair}.toMap
        r.keysIterator.toArray -> r
      }){ case (keys, a) =>
        var count = a.size
        val loop_limit = count * 2
        var map = a
        var i = 1
        while(i < loop_limit) {
          map += (i -> obj)
          i += 2
        }
        (map, count)
      },
//      Case("Array", x => x -> Array.fill(x)(obj)){ case (n, a) =>
//        var count = a.size
//        var array = a
//        var i = 0
//        while(i < count) {
//          array = array.clone
//          array(i) = obj
//          i += 1
//        }
//        (array, count)
//      },
    )
  )

}
