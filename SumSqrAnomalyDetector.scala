
import scala.collection.mutable

object SumSqrAnomalyDetector extends  AnomalyDetector {
  override def learn(normal: TimeSeries): Map[String, String] = {
    val pair_of_cols = Util.get_col_pairs_with_high_correlation_rate(normal)

    pair_of_cols.map(pair => {
      val x = normal.getValues(pair._1).get.toArray
      val y = normal.getValues(pair._2).get.toArray

      val points_arr = (x zip y).map(pair => {
        new Point(pair._1, pair._2)
      })

      val max_dist = points_arr.map(p1=>{
        Util.get_dist_sum_from_point_to_other_points(points_arr, p1)
      }).max

      //println(f"${pair._1},${pair._2}", f"$max_dist")
      (f"${pair._1},${pair._2}", f"$max_dist")
    }).toMap
  }

  override def detect(model: Map[String, String], test: TimeSeries): Vector[(String, Int)] = {
    val returned_vector = Set.newBuilder[(String,Int)]
    model.foreach(item => {
      val feature1 = item._1.split(",")(0)
      val feature2 = item._1.split(",")(1)
      //println(f"features: $feature1, $feature2")
      val max_dist = item._2.toDouble

      val points_arr = (test.getValues(feature1).get zip test.getValues(feature2).get).map(pair =>{
        new Point(pair._1, pair._2)
      })

      points_arr.zipWithIndex.foreach(p1 => {
        val dist = points_arr.zipWithIndex.filter(_._2 != p1._2).map(p2=>{
          Math.sqrt(Math.pow(p1._1.x - p2._1.x, 2) + Math.pow(p1._1.y - p2._1.y, 2))
        }).sum

        if (dist > max_dist)
          returned_vector += ((s"${feature1},${feature2}",p1._2))
      })
    })

    returned_vector.result().toVector
  }
}
