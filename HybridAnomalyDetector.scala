
import scala.collection.mutable

object HybridAnomalyDetector extends  AnomalyDetector {

  override def learn(normal: TimeSeries): Map[String, String] = {
    val results_map = Map.newBuilder[String, String]
    normal.features.zipWithIndex.foreach(feature1 => {
      val pearson_results = normal.features.zipWithIndex.drop(feature1._2 + 1).map(feature2=>{
        //println(s"${feature1._1},${feature2._1},${Util.pearson(normal.getValues(feature1._1).get.toArray, normal.getValues(feature2._1).get.toArray).abs}")
        (feature1,feature2,Util.pearson(normal.getValues(feature1._1).get.toArray, normal.getValues(feature2._1).get.toArray).abs)
      })

      val high_correlation = pearson_results.filter(x=>x._3 >= 0.9)
      val medium_correlation = pearson_results.filter(x=>{x._3 < 0.9 && x._3 > 0.5})

      if (high_correlation.size == 0 && medium_correlation.size == 0) {
        // Low correlation - use z score for x

          val arr = normal.getValues(feature1._1).get.toArray
          val max = arr.map(curr_num =>Util.zscore(arr,curr_num).abs).max

          results_map += ((f"ZAD,${feature1._1}", f"$max"))
          //println(f"ZAD,${feature1._1}", f"$max")
      }

      high_correlation.foreach(curr => {
        val res = Util.get_linear_reg_between_pair(normal, (curr._1._1, curr._2._1, curr._3))
        val a = res._1
        val b = res._2
        val m = res._3

        results_map += ((f"LRAD,${curr._1._1},${curr._2._1}", f"${a},${b},${m}"))
        //println(s"LRAD,${curr._1._1},${curr._2._1}", s"${a},${b},${m}")
      })

      // Medium correlation, use sum square
      medium_correlation.foreach(curr => {
        val X = normal.getValues(curr._1._1).get
        val Y = normal.getValues(curr._2._1).get
        val points_arr = (X zip Y).map(item => new Point(item._1, item._2)).toArray
        val points_with_sum = points_arr.map(point => (point, Util.get_dist_sum_from_point_to_other_points(points_arr, point)))

        val center_point = points_with_sum.minBy(_._2)
        val max_dist = points_arr.filter(_ != center_point._1).map(p2 => {
          Math.sqrt(Math.pow(center_point._1.x - p2.x, 2) + Math.pow(center_point._1.y - p2.y, 2))
        }).max

        results_map += ((f"SSD,${curr._1._1},${curr._2._1}", f"$max_dist"))
      })
    })
    results_map.result()
  }

  override def detect(model: Map[String, String], test: TimeSeries): Vector[(String, Int)] = {
    val result_vec  = Vector.newBuilder[(String,Int)]

    model.foreach(line => {

      val algorithm = line._1.split(",")(0)
      //println(s"Got request ${algorithm} for feature ${line._1.split(",")(1)}")


      // Handle LRAD request
      if (algorithm == "LRAD"){
        val feature1 = line._1.split(",")(1)
        val feature2 = line._1.split(",")(2)

        val a = line._2.split(",")(0).toDouble
        val b = line._2.split(",")(1).toDouble
        val max_dist = line._2.split(",")(2).toDouble

        // Zip X and Y and iterate the values
        (test.getValues(feature1).get zip test.getValues(feature2).get).zipWithIndex.foreach(val_pair => {
          val res = Math.abs(Math.abs(val_pair._1._2) - Math.abs(val_pair._1._1 * a + b))
          //println(f"LRAD: ${val_pair._1._2} - ($a * ${val_pair._1._1}) = $res, max_dist: $max_dist")
          if (res > max_dist) {
            //println(f"Found! $feature1,$feature2,${val_pair._2}")
            result_vec += ((f"$feature1,$feature2", val_pair._2))
          }

        })

      // Handle SSD request
      } else if (algorithm == "SSD") {
        val feature1 = line._1.split(",")(1)
        val feature2 = line._1.split(",")(2)
        val max_dist = line._2.toDouble

        // TODO calc res of SSD
        val X = test.getValues(feature1).get
        val Y = test.getValues(feature2).get
        val points_arr = (X zip Y).map(item => new Point(item._1, item._2)).toArray
        val points_with_sum = points_arr.map(point => (point, Util.get_dist_sum_from_point_to_other_points(points_arr, point)))

        val center_point = points_with_sum.minBy(_._2)
        val max_dist_found = points_arr.zipWithIndex.filter(_._1 != center_point._1).map(p2=>{
          (p2._2, Math.sqrt(Math.pow(center_point._1.x - p2._1.x, 2) + Math.pow(center_point._1.y - p2._1.y, 2)))
        }).maxBy(_._2)

        if (max_dist_found._2 > max_dist){
         //(f"SSD: Adding $feature1,$feature2 ${max_dist_found._1}")
          result_vec += ((s"$feature1,$feature2", max_dist_found._1))
        }

        // Handle ZAD request
      } else if (algorithm == "ZAD") {
        val feature = line._1.split(",")(1)

        val max_zscore = line._2.toDouble

        val max_found = Util.calc_max_zscore(test, feature)
        if (max_found._1 > max_zscore) {
          //println(s"ZAD: Adding $feature, ${max_found._2}")
          result_vec+=((feature, max_found._2))
        }
      }

    })
    result_vec.result()
  }

}
