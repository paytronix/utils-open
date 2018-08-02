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

//import org.joda.time.{DateTime, DateTimeZone, Instant, LocalDate, LocalDateTime, LocalTime}
import java.time.{Instant, LocalDate, LocalDateTime, LocalTime, ZonedDateTime, ZoneOffset}
import org.scalacheck.{Arbitrary, Gen}
import org.specs2.{ScalaCheck, SpecificationWithJUnit}

import com.paytronix.utils.scala.result.Okay

object arbitraries {
    val minTimestamp = Instant.parse("0100-01-01T00:00:00.000Z").toEpochMilli
    // 8099 because java.sql.Date doesn't like dates beyond that year
    val maxTimestamp = Instant.parse("8099-12-31T23:59:59.999Z").toEpochMilli

    implicit val arbitraryDateTime:      Arbitrary[ZonedDateTime] = Arbitrary { Gen.choose(minTimestamp, maxTimestamp).map { t => ZonedDateTime.ofInstant(Instant.ofEpochMilli(t), ZoneOffset.UTC) } }
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

    def e1 = prop { (zdt: ZonedDateTime) => roundTripJavaDate.from(zdt) ==== Okay(zdt) }
    def e2 = prop { (ld: LocalDate)      => roundTripJavaSqlDate.from(ld) ==== Okay(ld) }
    def e3 = prop { (lt: LocalTime)      => roundTripJavaSqlTime.from(lt) ==== Okay(lt.withNano(0)) } // java.sql.Time does not support nanos or millis
    def e4 = prop { (zdt: ZonedDateTime) => roundTripJavaSqlTimestamp.from(zdt) ==== Okay(zdt) }
}

class iso8601Test extends SpecificationWithJUnit with ScalaCheck {
    def is = s2"""
        iso8601 bijections should round trip
            java.time.ZonedDateTime     $e1
            java.time.LocalDate         $e2
            java.time.LocalDateTime     $e3
            java.time.LocalTime         $e4
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
            hhmmss                          $e25
        and format
            yyyy-mm-ddThh:mm:ss.sssZ        $e26
            yyyy-mm-ddThh:mm:ss.sss+hh:mm   $e27
            yyyy-mm-ddThh:mm:ss.sss-hh:mm   $e28
            yyyy-mm-dd                      $e29
            yyyy-mm-ddThh:mm:ss.sss         $e30
            hh:mm:ss.sss                    $e31
    """

    val roundTripDateTime      = iso8601.dateTimeBijection      >=> iso8601.dateTimeBijection.flip
    val roundTripLocalDate     = iso8601.localDateBijection     >=> iso8601.localDateBijection.flip
    val roundTripLocalDateTime = iso8601.localDateTimeBijection >=> iso8601.localDateTimeBijection.flip
    val roundTripLocalTime     = iso8601.localTimeBijection     >=> iso8601.localTimeBijection.flip

    // Round-trip tests
    def e1 = prop { (zdt: ZonedDateTime) => roundTripDateTime.to(zdt).map(_.toInstant.toEpochMilli) ==== Okay(zdt.toInstant.toEpochMilli) }
    def e2 = prop { (ld: LocalDate)      => roundTripLocalDate.to(ld) ==== Okay(ld) }
    def e3 = prop { (ldt: LocalDateTime) => roundTripLocalDateTime.to(ldt) ==== Okay(ldt) }
    def e4 = prop { (lt: LocalTime)      => roundTripLocalTime.to(lt) ==== Okay(lt) }

    val nano: Int = 1000000

    // Parsing tests
    def e5  = iso8601.dateTimeBijection.from("2014-07-01T13:14:15.161Z") ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 15, 161 * nano, ZoneOffset.UTC))
    def e6  = iso8601.dateTimeBijection.from("2014-07-01T13:14:15.161+04:56") ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 15, 161 * nano, ZoneOffset.ofHoursMinutes(4, 56)))
    def e7  = iso8601.dateTimeBijection.from("2014-07-01T13:14:15.161-04:56") ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 15, 161 * nano, ZoneOffset.ofHoursMinutes(-4, -56)))
    def e8  = iso8601.dateTimeBijection.from("2014-07-01T13:14:15Z") ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 15, 0, ZoneOffset.UTC))
    def e9  = iso8601.dateTimeBijection.from("2014-07-01T13:14:15+04:56") ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 15, 0, ZoneOffset.ofHoursMinutes(4, 56)))
    def e10 = iso8601.dateTimeBijection.from("2014-07-01T13:14:15-04:56") ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 15, 0, ZoneOffset.ofHoursMinutes(-4, -56)))
    def e11 = iso8601.dateTimeBijection.from("20140701T131415.161Z") ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 15, 161 * nano, ZoneOffset.UTC))
    def e12 = iso8601.dateTimeBijection.from("20140701T131415.161+04:56") ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 15, 161 * nano, ZoneOffset.ofHoursMinutes(4, 56)))
    def e13 = iso8601.dateTimeBijection.from("20140701T131415.161-04:56") ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 15, 161 * nano, ZoneOffset.ofHoursMinutes(-4, -56)))
    def e14 = iso8601.dateTimeBijection.from("20140701T131415Z") ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 15, 0, ZoneOffset.UTC))
    def e15 = iso8601.dateTimeBijection.from("20140701T131415+04:56") ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 15, 0, ZoneOffset.ofHoursMinutes(4, 56)))
    def e16 = iso8601.dateTimeBijection.from("20140701T131415-04:56") ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 15, 0, ZoneOffset.ofHoursMinutes(-4, -56)))
    def e17 = iso8601.localDateBijection.from("2014-07-01") ==== Okay(LocalDate.of(2014, 7, 1))
    def e18 = iso8601.localDateBijection.from("20140701") ==== Okay(LocalDate.of(2014, 7, 1))
    def e19 = iso8601.localDateTimeBijection.from("2014-07-01T13:14:15.161") ==== Okay(LocalDateTime.of(2014, 7, 1, 13, 14, 15, 161 * nano))
    def e20 = iso8601.localDateTimeBijection.from("2014-07-01T13:14:15") ==== Okay(LocalDateTime.of(2014, 7, 1, 13, 14, 15, 0))
    def e21 = iso8601.localDateTimeBijection.from("20140701T131415.161") ==== Okay(LocalDateTime.of(2014, 7, 1, 13, 14, 15, 161 * nano))
    def e22 = iso8601.localDateTimeBijection.from("20140701T131415") ==== Okay(LocalDateTime.of(2014, 7, 1, 13, 14, 15, 0))
    def e23 = iso8601.localTimeBijection.from("12:13:14.161") ==== Okay(LocalTime.of(12, 13, 14, 161 * nano))
    def e24 = iso8601.localTimeBijection.from("12:13:14") ==== Okay(LocalTime.of(12, 13, 14, 0))
    def e25 = iso8601.localTimeBijection.from("121314") ==== Okay(LocalTime.of(12, 13, 14, 0))

    // Formatting tests
    def e26 = iso8601.dateTimeBijection.to(ZonedDateTime.of(2014, 7, 1, 13, 14, 15, 0, ZoneOffset.UTC)) ==== Okay("2014-07-01T13:14:15.000Z")
    def e27 = iso8601.dateTimeBijection.to(ZonedDateTime.of(2014, 7, 1, 13, 14, 15, 0, ZoneOffset.ofHoursMinutes(4, 56))) ==== Okay("2014-07-01T13:14:15.000+04:56")
    def e28 = iso8601.dateTimeBijection.to(ZonedDateTime.of(2014, 7, 1, 13, 14, 15, 0, ZoneOffset.ofHoursMinutes(-4, -56))) ==== Okay("2014-07-01T13:14:15.000-04:56")
    def e29 = iso8601.localDateBijection.to(LocalDate.of(2014, 7, 1)) ==== Okay("2014-07-01")
    def e30 = iso8601.localDateTimeBijection.to(LocalDateTime.of(2014, 7, 1, 13, 14, 15, 0)) ==== Okay("2014-07-01T13:14:15.000")
    def e31 = iso8601.localTimeBijection.to(LocalTime.of(12, 13, 14, 0)) ==== Okay("12:13:14.000")
}

class longTest extends SpecificationWithJUnit with ScalaCheck {
    def is = s2"""
        long bijections should round trip
            org.joda.time.DateTime          $e1
            org.joda.time.LocalDate         $e2
            org.joda.time.LocalDateTime     $e3
            org.joda.time.LocalTime         $e4
    """

    val roundTripDateTime      = long.dateTimeBijection      >=> long.dateTimeBijection.flip
    val roundTripLocalDate     = long.localDateBijection     >=> long.localDateBijection.flip
    val roundTripLocalDateTime = long.localDateTimeBijection >=> long.localDateTimeBijection.flip
    val roundTripLocalTime     = long.localTimeBijection     >=> long.localTimeBijection.flip

    def e1 = prop { (zdt: ZonedDateTime)  => roundTripDateTime.to(zdt).map(_.toInstant.toEpochMilli) ==== Okay(zdt.toInstant.toEpochMilli) }
    def e2 = prop { (ld: LocalDate)       => roundTripLocalDate.to(ld) ==== Okay(ld) }
    def e3 = prop { (ldt: LocalDateTime)  => roundTripLocalDateTime.to(ldt) ==== Okay(ldt) }
    def e4 = prop { (lt: LocalTime)       => roundTripLocalTime.to(lt) ==== Okay(lt) }
}
