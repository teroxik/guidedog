package com.guidedog.core

import com.clockworksms.{SMS, InvalidCharacterActionEnum, ClockWorkSmsService}
import com.guidedog.model.Sms

import scala.util.{Success, Failure, Try}

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

   def sendSms(text: String): Try[Boolean] = {
     val clockworkSms = new SMS(sms.to, text)
     sms.from.foreach(clockworkSms.setFrom)
     val result = Try(service.send(clockworkSms))
     result.map(_.isSuccess)
   }

    sms.content.grouped(458).toList.map(sendSms).head
    
  }

}
