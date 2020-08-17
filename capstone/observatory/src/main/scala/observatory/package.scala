package object observatory {
  type Temperature = Double // °C, introduced in Week 1
  type Year = Int // Calendar year, introduced in Week 1

  // PT
  type STN = String
  type WBAN = String
  type StationIdentifier = (STN, WBAN)

}
