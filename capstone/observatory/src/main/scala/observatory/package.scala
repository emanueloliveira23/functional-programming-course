package object observatory {
  type Temperature = Double // °C, introduced in Week 1
  type Year = Int // Calendar year, introduced in Week 1

  val ColorScale: Iterable[(Temperature, Color)] = Iterable(
    (60d,   Color(255,255,255)),
    (32d,   Color(255,0,0)),
    (12d,   Color(255,255,0)),
    (0d,    Color(0,255,255)),
    (-15d,  Color(0,0,255)),
    (-27d,   Color(255,0,255)),
    (-50d,   Color(33,0,107)),
    (-60d,   Color(0,0,0))
  )

}
