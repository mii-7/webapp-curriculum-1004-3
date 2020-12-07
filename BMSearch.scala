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
  var j = pattern.length - 1
  var p = 0
  var ans = -1

  while (j < text.length && ans == -1) {
    p = pattern.length - 1
    var isMatch = true

    while (p >= 0 && j < text.length && isMatch) {
      if (pattern(p) == text(j)) {
        p = p - 1
        j = j + 1
      } else {
        isMatch = false
      }
    }
    if (p < 0) ans = j

    var shift1 = createMap.getOrElse(text(j), pattern.length)
    var shift2 = pattern.length - p
    j = j + shift1 max j + shift2
  }

  println(ans)

}