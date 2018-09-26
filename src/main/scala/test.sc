object MyChartApp extends scalax.chart.module.Charting {
  val data = for (i <- 1 to 5) yield (i,i)
  val chart = PieChart(data)
}
MyChartApp.chart.show()