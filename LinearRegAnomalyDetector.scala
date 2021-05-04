import scala.collection.immutable.VectorBuilder
import scala.collection.mutable

object LinearRegAnomalyDetector extends  AnomalyDetector {

  override def learn(normal: TimeSeries): Map[String, String] = {

    val pair_of_cols = Util.get_col_pairs_with_high_correlation_rate(normal)
    val model  = Map.newBuilder[String,String]


    pair_of_cols.foreach(pair => {
      val res = Util.get_linear_reg_between_pair(normal, pair)
      // Add entry to map:
      // "<feature1>,<feature2>" -> "<a>,<b>,<max distance allowed>"
      model += ((f"${pair._1},${pair._2}", f"${res._1},${res._2},${res._3}"))

    })

    model.result()

  }

  override def detect(model: Map[String, String], test: TimeSeries): Vector[(String, Int)] = {
    val returned_vector = Vector.newBuilder[(String,Int)]
    model.foreach(item => {

      // Deserialize data from map entry
      val feature_pair = item._1.split(",")
      val data = item._2.toString.split(",")

      val feature_1 = feature_pair(0)
      val feature_2 = feature_pair(1)

      val a = data(0).toDouble
      val b = data(1).toDouble
      val max_dist = data(2).toDouble

      // Zip X and Y and iterate the values
      (test.getValues(feature_1).get zip test.getValues(feature_2).get).zipWithIndex.foreach(val_pair => {
        val res = Math.abs(Math.abs(val_pair._1._2) - Math.abs(val_pair._1._1 * a + b))
        //println(f"${val_pair._1._2} - ($a * ${val_pair._1._1}) = $res, max_dist: $max_dist")
        if (math.abs(res) > max_dist) {
          //println(f"Found! $feature_1,$feature_2,${val_pair._2}")
          returned_vector += ((f"$feature_1,$feature_2", val_pair._2))
        }

      })
    })
    returned_vector.result()
  }

}
