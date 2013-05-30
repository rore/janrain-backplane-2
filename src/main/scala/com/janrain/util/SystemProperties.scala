package com.janrain.util

import java.util.Properties
import java.util.concurrent.TimeUnit
import java.net.InetAddress
import com.yammer.metrics.reporting.{ConsoleReporter, GraphiteReporter}
import com.janrain.commons.util.InitSystemProps
import javax.naming.InitialContext
import org.apache.commons.lang.StringUtils

/**
 * @author Johnny Bufu
 */
object SystemProperties extends InitSystemProps("/log4j.xml") with Loggable {

  final val REDIS_HOST = "REDIS_HOST"
  final val REDIS_DB = "REDIS_DB"
  load(REDIS_HOST)
  load(REDIS_DB, false)

  private final val BUILD_PROPERTIES: String = "/build.properties"
  private final val BUILD_VERSION_PROPERTY: String = "build.version"

  val buildProperties = new Properties()
  try {
    buildProperties.load(this.getClass.getResourceAsStream(BUILD_PROPERTIES))
  } catch {
    case e: Exception => {
      val err: String = "Error loading build properties from " + BUILD_PROPERTIES
      logger.error(err, e)
      throw new RuntimeException(err, e)
    }
  }

  final val BUILD_VERSION = buildProperties.getProperty(BUILD_VERSION_PROPERTY)

  load(InitSystemProps.AWS_INSTANCE_ID)
  final val INSTANCE_ID = Utils.getRequiredSystemProperty(InitSystemProps.AWS_INSTANCE_ID)
  logger.info("Configured Identity Service instance: " + INSTANCE_ID)

  final val GRAPHITE_SERVER = "GRAPHITE_SERVER"
  load(GRAPHITE_SERVER, false)
  Utils.getOptionalSystemProperty(GRAPHITE_SERVER).foreach( g => {
    val (server, port) = g.span(_ != ':')
    try {
      GraphiteReporter.enable(10, TimeUnit.SECONDS, server, port.drop(1).toInt, "opx/" + machineName.replace(".", "_") )
      logger.info("graphite reporting initialized for server:port = %s".format(g))
    } catch {
      case e: NumberFormatException => logger.info("graphite reporting init error for server:port = %s : %s".format(g, e.getMessage))
    }
  })

  ConsoleReporter.enable(10, TimeUnit.MINUTES)

  def machineName: String = try {
    InetAddress.getLocalHost.getHostName
  } catch {
    case e: Exception => INSTANCE_ID + "/<unknown>"
  }

  // copied from federate-utils/v1.2.3, since we're stuck for now with InitSystemProps from commons-supersimpledb
  private def load(paramName: String, required: Boolean) {
    var result = System.getProperty(paramName)
    if (StringUtils.isBlank(result)) {
      try {
        val initCtx = new InitialContext
        result = initCtx.lookup("java:comp/env/" + paramName).asInstanceOf[String]
        System.setProperty(paramName, result)
        System.out.println("Parameter " + paramName + " fetched from context and inserted as system property")
      } catch {
        case e: Exception => {
          //continue
          if (required) {
            System.out.println("An error occurred trying to locate required parameter " + paramName + " => " + e.getMessage)
          }
        }
      }
    } else {
      System.out.println("Parameter " + paramName + " exists as a system property")
    }
  }

  private def load(paramName: String) {
    load(paramName, false)
  }

}
