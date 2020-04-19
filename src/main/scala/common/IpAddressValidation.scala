package common

object IpAddressValidation extends App {

  val ipAddress = "67.180.203.147"

  val validIpFormat = """^(([0-9]|[1-9][0-9]|1[0-9]{2}|2[0-4][0-9]|25[0-5])\.){3}([0-9]|[1-9][0-9]|1[0-9]{2}|2[0-4][0-9]|25[0-5])$"""

  println(ipAddress.matches(validIpFormat))

}
