package com.guidedog.model

case class Sms(from: Option[String], to: String, content: String, messageId: Option[String], keyword: Option[String])
