//
// Copyright 2014 Paytronix Systems, Inc.
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.
//

package com.paytronix.utils.interchange.base.datetime

import org.joda.time.{DateTime, DateTimeZone, LocalDate, LocalDateTime, LocalTime}
import org.scalacheck.{Arbitrary, Gen}
import org.specs2.{ScalaCheck, SpecificationWithJUnit}

import com.paytronix.utils.scala.result.Okay

import Arbitrary.arbitrary

object arbitraries {
    // 1900 to avoid annoying transitions between gregorian and julian calendars as well as other things like the introduction of Standard Railway Time in 1883
    // that show up when converting between Joda and java.util.Date that I don't want to deal with right now
    val minTimestamp = new DateTime("1900-01-01T00:00:00.000Z").getMillis
    // 8099 because java.sql.Date doesn't like dates beyond that year
    val maxTimestamp = new DateTime("8099-12-31T23:59:59.999Z").getMillis

    implicit val arbitraryDateTime:      Arbitrary[DateTime]      = Arbitrary { Gen.choose(minTimestamp, maxTimestamp).map { new DateTime(_) } }
    implicit val arbitraryLocalDate:     Arbitrary[LocalDate]     = Arbitrary { arbitraryDateTime.arbitrary.map(_.toLocalDate) }
    implicit val arbitraryLocalDateTime: Arbitrary[LocalDateTime] = Arbitrary { arbitraryDateTime.arbitrary.map(_.toLocalDateTime) }
    implicit val arbitraryLocalTime:     Arbitrary[LocalTime]     = Arbitrary { arbitraryDateTime.arbitrary.map(_.toLocalTime) }
}

import arbitraries._

class javaDatesTest extends SpecificationWithJUnit with ScalaCheck {
    def is = s2"""
        javaDates bijections should round trip
            java.util.Date     $e1
            java.sql.Date      $e2
            java.sql.Time      $e3
            java.sql.Timestamp $e4
    """

    val roundTripJavaDate         = javaDates.javaDateBijection         <=< javaDates.javaDateBijection.flip
    val roundTripJavaSqlDate      = javaDates.javaSqlDateBijection      <=< javaDates.javaSqlDateBijection.flip
    val roundTripJavaSqlTime      = javaDates.javaSqlTimeBijection      <=< javaDates.javaSqlTimeBijection.flip
    val roundTripJavaSqlTimestamp = javaDates.javaSqlTimestampBijection <=< javaDates.javaSqlTimestampBijection.flip

    def e1 = prop { (dt: DateTime) => roundTripJavaDate.from(dt) ==== Okay(dt) }
    def e2 = prop { (ld: LocalDate) => roundTripJavaSqlDate.from(ld) ==== Okay(ld) }
    def e3 = prop { (lt: LocalTime) => roundTripJavaSqlTime.from(lt) ==== Okay(lt) }
    def e4 = prop { (dt: DateTime) => roundTripJavaSqlTimestamp.from(dt) ==== Okay(dt) }
}

class iso8601Test extends SpecificationWithJUnit with ScalaCheck {
    def is = s2"""
        iso8601 bijections should round trip
            org.joda.time.DateTime          $e1
            org.joda.time.LocalDate         $e2
            org.joda.time.LocalDateTime     $e3
            org.joda.time.LocalTime         $e4
        and accept
            yyyy-mm-ddThh:mm:ss.sssZ        $e5
            yyyy-mm-ddThh:mm:ss.sss+hh:mm   $e6
            yyyy-mm-ddThh:mm:ss.sss-hh:mm   $e7
            yyyy-mm-ddThh:mm:ssZ            $e8
            yyyy-mm-ddThh:mm:ss+hh:mm       $e9
            yyyy-mm-ddThh:mm:ss-hh:mm       $e10
            yyyymmddThhmmss.sssZ            $e11
            yyyymmddThhmmss.sss+hh:mm       $e12
            yyyymmddThhmmss.sss-hh:mm       $e13
            yyyymmddThhmmssZ                $e14
            yyyymmddThhmmss+hh:mm           $e15
            yyyymmddThhmmss-hh:mm           $e16
            yyyy-mm-dd                      $e17
            yyyymmdd                        $e18
            yyyy-mm-ddThh:mm:ss.sss         $e19
            yyyy-mm-ddThh:mm:ss             $e20
            yyyymmddThhmmss.sss             $e21
            yyyymmddThhmmss                 $e22
            hh:mm:ss.sss                    $e23
            hh:mm:ss                        $e24
            hhmmss                          $e25
    """

    val roundTripDateTime      = iso8601.dateTimeBijection      >=> iso8601.dateTimeBijection.flip
    val roundTripLocalDate     = iso8601.localDateBijection     >=> iso8601.localDateBijection.flip
    val roundTripLocalDateTime = iso8601.localDateTimeBijection >=> iso8601.localDateTimeBijection.flip
    val roundTripLocalTime     = iso8601.localTimeBijection     >=> iso8601.localTimeBijection.flip

    def e1 = prop { (dt: DateTime) => roundTripDateTime.to(dt).map(_.getMillis) ==== Okay(dt.getMillis) }
    def e2 = prop { (ld: LocalDate) => roundTripLocalDate.to(ld) ==== Okay(ld) }
    def e3 = prop { (ldt: LocalDateTime) => roundTripLocalDateTime.to(ldt) ==== Okay(ldt) }
    def e4 = prop { (lt: LocalTime) => roundTripLocalTime.to(lt) ==== Okay(lt) }

    def e5  = iso8601.dateTimeBijection.from("2014-07-01T13:14:15.161Z") ==== Okay(new DateTime(2014, 7, 1, 13, 14, 15, 161, DateTimeZone.UTC))
    def e6  = iso8601.dateTimeBijection.from("2014-07-01T13:14:15.161+04:56") ==== Okay(new DateTime(2014, 7, 1, 13, 14, 15, 161, DateTimeZone.forOffsetHoursMinutes(4, 56)))
    def e7  = iso8601.dateTimeBijection.from("2014-07-01T13:14:15.161-04:56") ==== Okay(new DateTime(2014, 7, 1, 13, 14, 15, 161, DateTimeZone.forOffsetHoursMinutes(-4, 56)))
    def e8  = iso8601.dateTimeBijection.from("2014-07-01T13:14:15Z") ==== Okay(new DateTime(2014, 7, 1, 13, 14, 15, DateTimeZone.UTC))
    def e9  = iso8601.dateTimeBijection.from("2014-07-01T13:14:15+04:56") ==== Okay(new DateTime(2014, 7, 1, 13, 14, 15, DateTimeZone.forOffsetHoursMinutes(4, 56)))
    def e10 = iso8601.dateTimeBijection.from("2014-07-01T13:14:15-04:56") ==== Okay(new DateTime(2014, 7, 1, 13, 14, 15, DateTimeZone.forOffsetHoursMinutes(-4, 56)))
    def e11 = iso8601.dateTimeBijection.from("20140701T131415.161Z") ==== Okay(new DateTime(2014, 7, 1, 13, 14, 15, 161, DateTimeZone.UTC))
    def e12 = iso8601.dateTimeBijection.from("20140701T131415.161+04:56") ==== Okay(new DateTime(2014, 7, 1, 13, 14, 15, 161, DateTimeZone.forOffsetHoursMinutes(4, 56)))
    def e13 = iso8601.dateTimeBijection.from("20140701T131415.161-04:56") ==== Okay(new DateTime(2014, 7, 1, 13, 14, 15, 161, DateTimeZone.forOffsetHoursMinutes(-4, 56)))
    def e14 = iso8601.dateTimeBijection.from("20140701T131415Z") ==== Okay(new DateTime(2014, 7, 1, 13, 14, 15, DateTimeZone.UTC))
    def e15 = iso8601.dateTimeBijection.from("20140701T131415+04:56") ==== Okay(new DateTime(2014, 7, 1, 13, 14, 15, DateTimeZone.forOffsetHoursMinutes(4, 56)))
    def e16 = iso8601.dateTimeBijection.from("20140701T131415-04:56") ==== Okay(new DateTime(2014, 7, 1, 13, 14, 15, DateTimeZone.forOffsetHoursMinutes(-4, 56)))
    def e17 = iso8601.localDateBijection.from("2014-07-01") ==== Okay(new LocalDate(2014, 7, 1))
    def e18 = iso8601.localDateBijection.from("20140701") ==== Okay(new LocalDate(2014, 7, 1))
    def e19 = iso8601.localDateTimeBijection.from("2014-07-01T13:14:15.161") ==== Okay(new LocalDateTime(2014, 7, 1, 13, 14, 15, 161))
    def e20 = iso8601.localDateTimeBijection.from("2014-07-01T13:14:15") ==== Okay(new LocalDateTime(2014, 7, 1, 13, 14, 15))
    def e21 = iso8601.localDateTimeBijection.from("20140701T131415.161") ==== Okay(new LocalDateTime(2014, 7, 1, 13, 14, 15, 161))
    def e22 = iso8601.localDateTimeBijection.from("20140701T131415") ==== Okay(new LocalDateTime(2014, 7, 1, 13, 14, 15))
    def e23 = iso8601.localTimeBijection.from("12:13:14.161") ==== Okay(new LocalTime(12, 13, 14, 161))
    def e24 = iso8601.localTimeBijection.from("12:13:14") ==== Okay(new LocalTime(12, 13, 14))
    def e25 = iso8601.localTimeBijection.from("121314") ==== Okay(new LocalTime(12, 13, 14))
}
