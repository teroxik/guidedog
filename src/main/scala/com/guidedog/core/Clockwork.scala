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
  def sendSMS(sms: Sms): Try[Boolean] = {
    if (sms.content.length > 459) {
      Failure(new Throwable("Message length exceeds 459."))
    } else {
      val clockworkSms = new SMS(sms.to, sms.content)
      sms.from.foreach(clockworkSms.setFrom)
      val result = Try(service.send(clockworkSms))
      result.map(_.isSuccess)
    }
  }

}
