import scala.collection.mutable

object ZAnomalyDetector extends AnomalyDetector {
  override def learn(normal: TimeSeries): Map[String, String] = {
    normal.features.map(x=>{
      val arr = normal.getValues(x).get.toArray
      val max = arr.map(curr_num =>Util.zscore(arr,curr_num).abs).max
      (x, max.toString)
    }).toMap
  }

  override def detect(model: Map[String, String], test: TimeSeries): Vector[(String, Int)] = {
    val returned_vector = Vector.newBuilder[(String,Int)]
    model.foreach(item=>{
      val feature = item._1
      val max_zscore = item._2.toDouble

      val max_found = Util.calc_max_zscore(test, feature)
      if (max_found._1 > max_zscore)
        returned_vector += ((feature, max_found._2))

    })

    returned_vector.result()
  }
}
