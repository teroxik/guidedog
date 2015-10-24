package com.guidedog.model

case class Sms(from: Option[String] = None, to: String, content: String, messageId: Option[String] = None, keyword: Option[String] = None)
