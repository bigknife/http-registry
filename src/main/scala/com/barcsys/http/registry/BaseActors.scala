package com.barcsys.http.registry

import java.util.concurrent.TimeUnit

import akka.actor.Actor
import com.codahale.metrics.{ConsoleReporter, MetricRegistry}
import org.slf4j.LoggerFactory

/**
  * Registry actors
  * Created by bigknife on 16/6/23.
  */
object BaseActors {
  //metrics
  val metrics = new MetricRegistry

  trait ActorStack extends Actor {
    //被封装的偏函数, 子类实现该方法, 完成标准的消息处理
    def wrappedReceive: Receive

    // actor statck, 切面类Actor可以重载该方法, 但记得调用super.receive
    def receive: Receive = {
      case x => if (wrappedReceive.isDefinedAt(x)) wrappedReceive(x) else unhandled(x)
    }
  }

  trait Slf4jLogging extends Actor with ActorStack {
    val logger = LoggerFactory.getLogger(getClass)
    private[this] val myPath = self.path.toString

    logger.info(s"Starting actor of $myPath")

    override def receive: Receive = {
      case x =>
        org.slf4j.MDC.put("akkaSource", myPath)
        super.receive(x)
    }
  }

  trait ActorMetrics extends ActorStack {

    import MetricRegistry._

    val timer = metrics.timer(name(getClass, "message-handler"))

    //console reporter
    val reporter = ConsoleReporter.forRegistry(metrics)
      .convertRatesTo(TimeUnit.SECONDS)
      .convertDurationsTo(TimeUnit.MILLISECONDS)
      .build()
      .start(1, TimeUnit.MINUTES)

    override def receive: Receive = {
      case x =>
        val context = timer.time()
        try {
          super.receive(x)
        } finally {
          context.stop()
        }
    }
  }

  trait ActorMessageTrack extends ActorStack {
    override def receive: Receive = {
      case x =>
        println(s"${sender()} -> $x -> $self")
        super.receive(x)
    }
  }

}
