object BMSearch extends App {
  val text = "カワカドカドカドドワンゴカドカドンゴドワドワンゴドワカワカドンゴドワ".toSeq
  val pattern = "ドワンゴ".toSeq

  // ずらし表の作成
  var createMap = Map[Char, Int]()
  var i = 0

  for (i <- 0 to pattern.length - 1) {
    if (createMap.isDefinedAt(pattern(i))) {
      createMap = createMap + (pattern(i) -> (pattern.length - i - 1).toInt)
    } else {
      createMap = createMap.updated(pattern(i), (pattern.length - i - 1).toInt)
    }
  }

  // 探索
  var t = 0
  var ans = -1

  while (t < text.length - pattern.length && ans == -1) {
    var s = pattern.length - 1
    var isMatch = true
    val partText = text.slice(t, t + pattern.length)

    while (s > 0 && isMatch) {
      if (pattern(s) != partText(s)) isMatch = false else s = s - 1
    }

    if (isMatch) ans = t

    var shift = createMap.getOrElse(partText(s), pattern.length)
    t = t + shift
  }

  println(ans)
}