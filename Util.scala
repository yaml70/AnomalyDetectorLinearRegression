import scala.collection.mutable

object Util {

  def max[A](list : List[A], func : (A, A) => Int) : A={
    list.sortWith((a,b) =>func(a,b) > 0).head
  }

  def map[A,B,C](list : List[A], from_a_to_b : A => B, from_b_to_c : B => C) : List[C]={
    list.map(item => from_b_to_c(from_a_to_b(item)))
  }

  def isSorted[A](list : List[A], func : (A, A) => Boolean) : Boolean= {
    list.length match {
      case 0 | 1 => true
      case 2 => func(list(0), list(1))
      case _ => isSorted(list.take(2), func) && isSorted(list.drop(1), func)
    }
  }

  def probs(xs : Array[Double]) : List[Double] = {
    xs.map(x => 1.0* xs.count(_ == x) / xs.length).toList
  }

  def entropy(xs : Array[Double]) : Double = {
    (xs.toList zip probs(xs)).distinct.map(x => - (x._2 * (math.log(x._2) / math.log(2.0)))).sum
  }

  def mu(array : Array[Double]) : Double = {
    (array.toList zip probs(array)).distinct.map(i=>i._1 * i._2).sum
  }

  def variance(array : Array[Double]) : Double = {
    (probs(array) zip array.map(i=>math.pow(i - mu(array), 2))).distinct.map(j => j._1 * j._2).sum
  }

  def zscore(array : Array[Double], num : Double) : Double = {
    (num - mu(array)) / math.sqrt(variance(array))
  }

  def pearson(xs : Array[Double], ys : Array[Double]) : Double = {
    cov(xs, ys) / (math.sqrt(variance(xs)) * math.sqrt(variance(ys)))
  }

  def cov(xs : Array[Double], ys : Array[Double]) : Double = {
    mu((ys zip xs).map(i => i._1 * i._2)) - (mu(xs) * mu(ys))
  }

  def get_pearson_map_between_cols(normal: TimeSeries) : mutable.HashMap[(String,String),Double] = {
    val f = normal.features.map(a=>a)
    val pearson_map : mutable.HashMap[(String,String),Double] = new mutable.HashMap[(String,String),Double]

    // Iterate all columns
    f.zipWithIndex.foreach(feature => {
      // Current is the feature in the left
      val current = f.map(a => a).slice(feature._2, feature._2 + 1)(0)
      val current_col = normal.getValues(feature._1).get.toArray

      // Iterate cols on the right
      Range(feature._2 + 1, f.length).foreach(other_index => {
        val other = normal.features(other_index)
        val other_col = normal.getValues(other).get.toArray
        val pearson_result = Math.abs(Util.pearson(current_col, other_col))
        //println(f"feature: ${feature._1}, other: $other, result: $pearson_result")
        pearson_map += (((current, other), pearson_result))

      })
    })
    pearson_map
  }

  def get_col_pairs_with_high_correlation_rate(normal: TimeSeries): List[(String,String,Double)] = {

    val pearson_map = get_pearson_map_between_cols(normal)

    val sorted_pearson_seq = pearson_map.toSeq.sortBy(x=>x._2)
    val pair_of_cols = new mutable.ListBuffer[(String,String,Double)]

    // Get best pairs
    sorted_pearson_seq.filter(_._2 > 0.9).foreach(x=>{
      pair_of_cols += ((x._1._1, x._1._2, x._2))
      //println(f"Added ${x._1._1},${x._1._2},${x._2}")
    })

    pair_of_cols.result()
  }

  def get_linear_reg_between_pair(normal : TimeSeries, pair : (String,String,Double)) : (Double,Double,Double) = {

    val x = normal.getValues(pair._1).get.toArray
    val y = normal.getValues(pair._2).get.toArray

    // Create line based on the points
    val points_arr = (x zip y).map(pair => {
      new Point(pair._1, pair._2)
    })

    // Create line from the points
    val line: Line = new Line(points_arr)

    // Calculate max distance TODO with fold
    val max_dist : Double = points_arr.map(p => Math.abs(line.dist(p))).sortWith((a, b) => a >= b).head
    (line.a, line.b, max_dist)
  }

  def get_dist_sum_from_point_to_other_points(points_arr : Array[Point], p1 : Point) = {
    points_arr.filter(_ != p1).map(p2=>{
      //println(s"Points: ${p1._2}, ${p2._2}")
      Math.sqrt(Math.pow(p1.x - p2.x, 2) + Math.pow(p1.y - p2.y, 2))
    }).sum
  }

  def calc_max_zscore(test : TimeSeries, feature : String) : (Double,Int) = {
    val arr = test.getValues(feature).get.toArray
    val tmp_arr = arr.zipWithIndex.map(curr_num =>(Util.zscore(arr,curr_num._1).abs, curr_num._2))
    tmp_arr.maxBy(_._1)
  }


}
