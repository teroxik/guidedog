package com.guidedog.core

import com.clockworksms.{SMS, InvalidCharacterActionEnum, ClockWorkSmsService}
import com.guidedog.model.Sms

import scala.util.{Failure, Try}

object Clockwork {

  // andrewscs@gmail.com
  private val apiKey = "4132d084353a54fc2793550b305d0e6f645ac408"

  private val service = new ClockWorkSmsService(apiKey)
  service.setLongMessage(true)
  service.setFrom("GuideDog")
  service.setTruncateMessage(false)
  service.setInvalidCharacterAction(InvalidCharacterActionEnum.None)

  /**
   * Send sms.
   *
   * @param sms the sms to be sent
   * @return message delivered successfully wrapped in ``Try``
   */
  def sendSMS(sms: Sms) = {

   /* def bundleInfo(oneMessage: String, remaining: List): Unit = {
      remaining match {

        case head :: tail =>
          if (oneMessage.length + remaining.head.length + 1 < 459)
            bundleInfo(s"$oneMessage ${remaining.head}", tail)
          else {
            sendSms(oneMessage)
            bundleInfo(head, tail)
          }

        case List.empty =>
          sendSms(oneMessage)
      }
    }

    bundleInfo("", ) */

    sms.content.grouped(458).toList.map(sendSms(_))

    def sendSms(text: String): Try[Boolean] = {
      val clockworkSms = new SMS(sms.to, text)
      sms.from.foreach(clockworkSms.setFrom)
      val result = Try(service.send(clockworkSms))
      result.map(_.isSuccess)
    }
  }

}
