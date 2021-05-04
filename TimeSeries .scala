import scala.io.Source
import scala.collection.mutable
import scala.collection.immutable

class TimeSeries(csvFileName:String) {

  // Read lines from file
  private val source = Source.fromFile(csvFileName)
  private val lines = source.getLines()

  // Split content to header and data
  val features: Array[String] = lines.take(1).next.split(",")
  private val features_map: Map[String, Int] = features.zipWithIndex.map(x=>(x._1,x._2)).toMap[String,Int]

  // Build the VectorMap
  private val tmp_data = Vector.newBuilder[Vector[Double]]

  // Initialize VectorMap builder for each feature

  private val tmp_arr = features.map((_,Vector.newBuilder[Double]))

  // Fill the columns to the sub vector builders
  lines.zipWithIndex.foreach(line =>
    line._1.split(",").map(x=>x.toDouble).zipWithIndex.map(value=>{
      tmp_arr(value._2)._2+=(value._1.toDouble)
    })
  )

  tmp_arr.zipWithIndex.foreach(x => tmp_data+=(
    (features(x._2), x._1._2.result())._2
  ))

  private val data = tmp_data.result()


  source.close()

  // given name of a feature return in O(1) its value series
  def getValues(feature:String):Option[Vector[Double]]= {
    if (!features_map.contains(feature))
      return None

    Option(data(features_map(feature)))
  }

  // given name of a feature return in O(1) its value at the given time step
  def getValue(feature:String,timeStep:Int):Option[Double]= {
    if (!features_map.contains(feature))
      return None

    val f = data(features_map(feature))
    if (timeStep >= f.length)
      return None
    Option(f(timeStep))
  }

  // given name of a feature return its value series in the range of indices
  def getValues(feature:String,r:Range):Option[Vector[Double]]={
    if (!features_map.contains(feature))
      return None

    val f = data(features_map(feature))
    if (r.head < 0 || r.head >= f.length || r.last >= f.length)
      return None

    val len = f.size
    Option(f.drop(r.head).dropRight(len - r.last - 1))
  }
}
