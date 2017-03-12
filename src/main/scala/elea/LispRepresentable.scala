package elea

/**
  * Things with a `toLisp(...)` method
  */
trait LispRepresentable {
  def toLisp(settings: LispPrintSettings = LispPrintSettings()): String
}

case class LispPrintSettings(showCaseIndices: Boolean = false)
