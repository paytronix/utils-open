//
// Copyright 2014-2018 Paytronix Systems, Inc.
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
import java.time.{Instant, LocalDate, LocalDateTime, LocalTime, ZonedDateTime, ZoneId, ZoneOffset}
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
            java.time.ZonedDateTime         $rtZonedDateTime
            java.time.LocalDate             $rtLocalDate
            java.time.LocalDateTime         $rtLocalDateTime
            java.time.LocalTime             $rtLocalTime
        and accept ZonedDateTime
            yyyy-mm-ddThh:mm:ss.sssZ        $zdtColonsZed
            yyyy-mm-ddThh:mm:ss.sss+0000    $zdtColonsPlusZero
            yyyy-mm-ddThh:mm:ss.sss-0000    $zdtColonsMinusZero
            yyyy-mm-ddThh:mm:ss.sss+hh:mm   $zdtColonsPosColon
            yyyy-mm-ddThh:mm:ss.sss-hh:mm   $zdtColonsNegColon
            yyyy-mm-ddThh:mm:ss.sss +hh:mm  $zdtColonsSpaceColonPos
            yyyy-mm-ddThh:mm:ss.sss -hh:mm  $zdtColonsSpaceColonNeg
            yyyy-mm-ddThh:mm:ss.sss+hhmm    $zdtColonsPos
            yyyy-mm-ddThh:mm:ss.sss-hhmm    $zdtColonsNeg
            yyyy-mm-ddThh:mm:ss.sss +hhmm   $zdtColonsSpacePos
            yyyy-mm-ddThh:mm:ss.sss -hhmm   $zdtColonsSpaceNeg
            yyyy-mm-ddThh:mm:ssZ            $zdtColonsNoMilliZed
            yyyy-mm-ddThh:mm:ss+hhmm        $zdtColonsNoMilliPos
            yyyy-mm-ddThh:mm:ss-hhmm        $zdtColonsNoMilliNeg
            yyyy-mm-ddThh:mm:ss +hhmm       $zdtColonsNoMilliSpacePos
            yyyy-mm-ddThh:mm:ss -hhmm       $zdtColonsNoMilliSpaceNeg
            yyyy-mm-ddThh:mm:ss+hh:mm       $zdtColonsNoMilliColonPos
            yyyy-mm-ddThh:mm:ss-hh:mm       $zdtColonsNoMilliColonNeg
            yyyy-mm-ddThh:mm:ss +hh:mm      $zdtColonsNoMilliSpaceColonPos
            yyyy-mm-ddThh:mm:ss -hh:mm      $zdtColonsNoMilliSpaceColonNeg
            yyyy-mm-ddThh:mm+hhmm           $zdtColonsNoSecPos
            yyyy-mm-ddThh:mm-hhmm           $zdtColonsNoSecNeg
            yyyy-mm-ddThh:mm +hhmm          $zdtColonsNoSecSpacePos
            yyyy-mm-ddThh:mm -hhmm          $zdtColonsNoSecSpaceNeg
            yyyy-mm-ddThh:mm+hh:mm          $zdtColonsNoSecColonPos
            yyyy-mm-ddThh:mm-hh:mm          $zdtColonsNoSecColonNeg
            yyyy-mm-ddThh:mm +hh:mm         $zdtColonsNoSecSpaceColonPos
            yyyy-mm-ddThh:mm -hh:mm         $zdtColonsNoSecSpaceColonNeg
            yyyymmddThhmmss.sssZ            $zdtNoColonsZed
            yyyymmddThhmmss.sss+0000        $zdtNoColonsPlusZero
            yyyymmddThhmmss.sss-0000        $zdtNoColonsMinusZero
            yyyymmddThhmmss.sss+00:00       $zdtNoColonsPosColon
            yyyymmddThhmmss.sss-00:00       $zdtNoColonsNegColon
            yyyymmddThhmmssZ                $zdtNoColonsNoMillisZed
            yyyymmddThhmmss+0000            $zdtNoColonsNoMillisColonPos
            yyyymmddThhmmss-0000            $zdtNoColonsNoMillisColonNeg
        and accept LocalDate
            yyyy-mm-dd                      $localDate
            yyyymmdd                        $basicLocalDate
        and accept LocalDateTime
            yyyy-mm-ddThh:mm:ss             $localDateTime
            yyyy-mm-ddThh:mm:ss.sss         $localDateTimeNoMillis
            yyyy-mm-ddThh:mm                $localDateTimeNoSeconds
            yyyymmddThhmmss.sss             $basicLocalDateTime
            yyyymmddThhmmss                 $basicLocalDateTimeNoMillis
            yyyymmddThhmm                   $basicLocalDateTimeNoSeconds
        and accept LocalTime
            hh:mm:ss.sss                    $localTime
            hh:mm:ss                        $localTimeNoMillis
            hh:mm                           $localTimeNoSeconds
            hhmmss.sss                      $basicLocalTime
            hhmmss                          $basicLocalTimeNoMillis
            hhmm                            $basicLocalTimeNoSeconds
        and format ZonedDateTime
            yyyy-mm-ddThh:mm:ss.sssZ        $formatZonedDateTimeUTC
            yyyy-mm-ddThh:mm:ss.sss+hh:mm   $formatZonedDateTimePos
            yyyy-mm-ddThh:mm:ss.sss-hh:mm   $formatZonedDateTimeNeg
            positive timezone               $formatZonedDateTimeNewYork
            negative timezone               $formatZonedDateTimeMoscow
        and format LocalDate
            yyyy-mm-dd                      $formatLocalDate
        and format LocalDateTime
            yyyy-mm-ddThh:mm:ss.sss         $formatLocalDateTime
        and format LocalTime
            hh:mm:ss.sss                    $formatLocalTime
    """

    val roundTripDateTime      = iso8601.zonedDateTimeBijection >=> iso8601.zonedDateTimeBijection.flip
    val roundTripLocalDate     = iso8601.localDateBijection     >=> iso8601.localDateBijection.flip
    val roundTripLocalDateTime = iso8601.localDateTimeBijection >=> iso8601.localDateTimeBijection.flip
    val roundTripLocalTime     = iso8601.localTimeBijection     >=> iso8601.localTimeBijection.flip

    // Round-trip tests
    def rtZonedDateTime = prop { (zdt: ZonedDateTime) => roundTripDateTime.to(zdt).map(_.toInstant.toEpochMilli) ==== Okay(zdt.toInstant.toEpochMilli) }
    def rtLocalDate = prop { (ld: LocalDate)      => roundTripLocalDate.to(ld) ==== Okay(ld) }
    def rtLocalDateTime = prop { (ldt: LocalDateTime) => roundTripLocalDateTime.to(ldt) ==== Okay(ldt) }
    def rtLocalTime = prop { (lt: LocalTime)      => roundTripLocalTime.to(lt) ==== Okay(lt) }

    val nano: Int = 1000000

    // Parsing tests
    def zdtColonsZed  = iso8601.zonedDateTimeBijection.from("2014-07-01T13:14:15.161Z") ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 15, 161 * nano, ZoneOffset.UTC))
    def zdtColonsPlusZero  = iso8601.zonedDateTimeBijection.from("2014-07-01T13:14:15.161+0000") ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 15, 161 * nano, ZoneOffset.UTC))
    def zdtColonsMinusZero  = iso8601.zonedDateTimeBijection.from("2014-07-01T13:14:15.161-0000") ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 15, 161 * nano, ZoneOffset.UTC))
    def zdtColonsPosColon  = iso8601.zonedDateTimeBijection.from("2014-07-01T13:14:15.161+04:56") ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 15, 161 * nano, ZoneOffset.ofHoursMinutes(4, 56)))
    def zdtColonsNegColon  = iso8601.zonedDateTimeBijection.from("2014-07-01T13:14:15.161-04:56") ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 15, 161 * nano, ZoneOffset.ofHoursMinutes(-4, -56)))
    def zdtColonsSpaceColonPos  = iso8601.zonedDateTimeBijection.from("2014-07-01T13:14:15.161 +04:56") ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 15, 161 * nano, ZoneOffset.ofHoursMinutes(4, 56)))
    def zdtColonsSpaceColonNeg  = iso8601.zonedDateTimeBijection.from("2014-07-01T13:14:15.161 -04:56") ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 15, 161 * nano, ZoneOffset.ofHoursMinutes(-4, -56)))
    def zdtColonsPos  = iso8601.zonedDateTimeBijection.from("2014-07-01T13:14:15.161+0456") ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 15, 161 * nano, ZoneOffset.ofHoursMinutes(4, 56)))
    def zdtColonsNeg  = iso8601.zonedDateTimeBijection.from("2014-07-01T13:14:15.161-0456") ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 15, 161 * nano, ZoneOffset.ofHoursMinutes(-4, -56)))
    def zdtColonsSpacePos  = iso8601.zonedDateTimeBijection.from("2014-07-01T13:14:15.161 +0456") ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 15, 161 * nano, ZoneOffset.ofHoursMinutes(4, 56)))
    def zdtColonsSpaceNeg  = iso8601.zonedDateTimeBijection.from("2014-07-01T13:14:15.161 -0456") ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 15, 161 * nano, ZoneOffset.ofHoursMinutes(-4, -56)))
    def zdtColonsNoMilliZed  = iso8601.zonedDateTimeBijection.from("2014-07-01T13:14:15Z") ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 15, 0, ZoneOffset.UTC))
    def zdtColonsNoMilliPos  = iso8601.zonedDateTimeBijection.from("2014-07-01T13:14:15+0456") ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 15, 0, ZoneOffset.ofHoursMinutes(4, 56)))
    def zdtColonsNoMilliNeg = iso8601.zonedDateTimeBijection.from("2014-07-01T13:14:15-0456") ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 15, 0, ZoneOffset.ofHoursMinutes(-4, -56)))
    def zdtColonsNoMilliSpacePos  = iso8601.zonedDateTimeBijection.from("2014-07-01T13:14:15+0456") ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 15, 0, ZoneOffset.ofHoursMinutes(4, 56)))
    def zdtColonsNoMilliSpaceNeg = iso8601.zonedDateTimeBijection.from("2014-07-01T13:14:15-0456") ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 15, 0, ZoneOffset.ofHoursMinutes(-4, -56)))
    def zdtColonsNoMilliColonPos  = iso8601.zonedDateTimeBijection.from("2014-07-01T13:14:15+04:56") ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 15, 0, ZoneOffset.ofHoursMinutes(4, 56)))
    def zdtColonsNoMilliColonNeg = iso8601.zonedDateTimeBijection.from("2014-07-01T13:14:15-04:56") ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 15, 0, ZoneOffset.ofHoursMinutes(-4, -56)))
    def zdtColonsNoMilliSpaceColonPos = iso8601.zonedDateTimeBijection.from("2014-07-01T13:14:15 +04:56") ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 15, 0, ZoneOffset.ofHoursMinutes(4, 56)))
    def zdtColonsNoMilliSpaceColonNeg = iso8601.zonedDateTimeBijection.from("2014-07-01T13:14:15 -04:56") ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 15, 0, ZoneOffset.ofHoursMinutes(-4, -56)))
    def zdtColonsNoSecPos  = iso8601.zonedDateTimeBijection.from("2014-07-01T13:14+0456") ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 0, 0, ZoneOffset.ofHoursMinutes(4, 56)))
    def zdtColonsNoSecNeg = iso8601.zonedDateTimeBijection.from("2014-07-01T13:14-0456") ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 0, 0, ZoneOffset.ofHoursMinutes(-4, -56)))
    def zdtColonsNoSecSpacePos  = iso8601.zonedDateTimeBijection.from("2014-07-01T13:14 +0456") ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 0, 0, ZoneOffset.ofHoursMinutes(4, 56)))
    def zdtColonsNoSecSpaceNeg = iso8601.zonedDateTimeBijection.from("2014-07-01T13:14 -0456") ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 0, 0, ZoneOffset.ofHoursMinutes(-4, -56)))
    def zdtColonsNoSecColonPos  = iso8601.zonedDateTimeBijection.from("2014-07-01T13:14+04:56") ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 0, 0, ZoneOffset.ofHoursMinutes(4, 56)))
    def zdtColonsNoSecColonNeg = iso8601.zonedDateTimeBijection.from("2014-07-01T13:14-04:56") ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 0, 0, ZoneOffset.ofHoursMinutes(-4, -56)))
    def zdtColonsNoSecSpaceColonPos  = iso8601.zonedDateTimeBijection.from("2014-07-01T13:14 +04:56") ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 0, 0, ZoneOffset.ofHoursMinutes(4, 56)))
    def zdtColonsNoSecSpaceColonNeg = iso8601.zonedDateTimeBijection.from("2014-07-01T13:14 -04:56") ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 0, 0, ZoneOffset.ofHoursMinutes(-4, -56)))
    def zdtNoColonsZed = iso8601.zonedDateTimeBijection.from("20140701T131415.161Z") ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 15, 161 * nano, ZoneOffset.UTC))
    def zdtNoColonsPlusZero = iso8601.zonedDateTimeBijection.from("20140701T131415.161+0000") ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 15, 161 * nano, ZoneOffset.UTC))
    def zdtNoColonsMinusZero = iso8601.zonedDateTimeBijection.from("20140701T131415.161-0000") ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 15, 161 * nano, ZoneOffset.UTC))
    def zdtNoColonsPosColon = iso8601.zonedDateTimeBijection.from("20140701T131415.161+04:56") ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 15, 161 * nano, ZoneOffset.ofHoursMinutes(4, 56)))
    def zdtNoColonsNegColon = iso8601.zonedDateTimeBijection.from("20140701T131415.161-04:56") ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 15, 161 * nano, ZoneOffset.ofHoursMinutes(-4, -56)))
    def zdtNoColonsNoMillisZed = iso8601.zonedDateTimeBijection.from("20140701T131415Z") ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 15, 0, ZoneOffset.UTC))
    def zdtNoColonsNoMillisColonPos = iso8601.zonedDateTimeBijection.from("20140701T131415+04:56") ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 15, 0, ZoneOffset.ofHoursMinutes(4, 56)))
    def zdtNoColonsNoMillisColonNeg = iso8601.zonedDateTimeBijection.from("20140701T131415-04:56") ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 15, 0, ZoneOffset.ofHoursMinutes(-4, -56)))

    def localDate = iso8601.localDateBijection.from("2014-07-01") ==== Okay(LocalDate.of(2014, 7, 1))
    def basicLocalDate = iso8601.localDateBijection.from("20140701") ==== Okay(LocalDate.of(2014, 7, 1))

    def localDateTime = iso8601.localDateTimeBijection.from("2014-07-01T13:14:15.161") ==== Okay(LocalDateTime.of(2014, 7, 1, 13, 14, 15, 161 * nano))
    def localDateTimeNoMillis = iso8601.localDateTimeBijection.from("2014-07-01T13:14:15") ==== Okay(LocalDateTime.of(2014, 7, 1, 13, 14, 15, 0))
    def localDateTimeNoSeconds = iso8601.localDateTimeBijection.from("2014-07-01T13:14") ==== Okay(LocalDateTime.of(2014, 7, 1, 13, 14, 0, 0))
    def basicLocalDateTime = iso8601.localDateTimeBijection.from("20140701T131415.161") ==== Okay(LocalDateTime.of(2014, 7, 1, 13, 14, 15, 161 * nano))
    def basicLocalDateTimeNoMillis = iso8601.localDateTimeBijection.from("20140701T131415") ==== Okay(LocalDateTime.of(2014, 7, 1, 13, 14, 15, 0))
    def basicLocalDateTimeNoSeconds = iso8601.localDateTimeBijection.from("20140701T1314") ==== Okay(LocalDateTime.of(2014, 7, 1, 13, 14, 0, 0))

    def localTime = iso8601.localTimeBijection.from("12:13:14.161") ==== Okay(LocalTime.of(12, 13, 14, 161 * nano))
    def localTimeNoMillis = iso8601.localTimeBijection.from("12:13:14") ==== Okay(LocalTime.of(12, 13, 14, 0))
    def localTimeNoSeconds = iso8601.localTimeBijection.from("12:13") ==== Okay(LocalTime.of(12, 13, 0, 0))
    def basicLocalTime = iso8601.localTimeBijection.from("121314.161") ==== Okay(LocalTime.of(12, 13, 14, 161 * nano))
    def basicLocalTimeNoMillis = iso8601.localTimeBijection.from("121314") ==== Okay(LocalTime.of(12, 13, 14, 0))
    def basicLocalTimeNoSeconds = iso8601.localTimeBijection.from("1213") ==== Okay(LocalTime.of(12, 13, 0, 0))

    // Formatting tests
    def formatZonedDateTimeUTC = iso8601.zonedDateTimeBijection.to(ZonedDateTime.of(2014, 7, 1, 13, 14, 15, 0, ZoneOffset.UTC)) ==== Okay("2014-07-01T13:14:15.000Z")
    def formatZonedDateTimePos = iso8601.zonedDateTimeBijection.to(ZonedDateTime.of(2014, 7, 1, 13, 14, 15, 0, ZoneOffset.ofHoursMinutes(4, 56))) ==== Okay("2014-07-01T13:14:15.000+04:56")
    def formatZonedDateTimeNeg = iso8601.zonedDateTimeBijection.to(ZonedDateTime.of(2014, 7, 1, 13, 14, 15, 0, ZoneOffset.ofHoursMinutes(-4, -56))) ==== Okay("2014-07-01T13:14:15.000-04:56")
    // Gotta make sure it only puts the UTC offset and doesn't append "[America/New_York]" to the end
    def formatZonedDateTimeNewYork = iso8601.zonedDateTimeBijection.to(ZonedDateTime.of(2014, 7, 1, 13, 14, 15, 0, ZoneId.of("America/New_York"))) ==== Okay("2014-07-01T13:14:15.000-04:00")
    def formatZonedDateTimeMoscow = iso8601.zonedDateTimeBijection.to(ZonedDateTime.of(2014, 7, 1, 13, 14, 15, 0, ZoneId.of("Europe/Moscow"))) ==== Okay("2014-07-01T13:14:15.000+04:00")
    def formatLocalDate = iso8601.localDateBijection.to(LocalDate.of(2014, 7, 1)) ==== Okay("2014-07-01")
    def formatLocalDateTime = iso8601.localDateTimeBijection.to(LocalDateTime.of(2014, 7, 1, 13, 14, 15, 0)) ==== Okay("2014-07-01T13:14:15.000")
    def formatLocalTime = iso8601.localTimeBijection.to(LocalTime.of(12, 13, 14, 0)) ==== Okay("12:13:14.000")
}

class longTest extends SpecificationWithJUnit with ScalaCheck {
    def is = s2"""
        long bijections should round trip
            org.joda.time.DateTime          $e1
            org.joda.time.LocalDate         $e2
            org.joda.time.LocalDateTime     $e3
            org.joda.time.LocalTime         $e4
    """

    val roundTripDateTime      = long.zonedDateTimeBijection      >=> long.zonedDateTimeBijection.flip
    val roundTripLocalDate     = long.localDateBijection     >=> long.localDateBijection.flip
    val roundTripLocalDateTime = long.localDateTimeBijection >=> long.localDateTimeBijection.flip
    val roundTripLocalTime     = long.localTimeBijection     >=> long.localTimeBijection.flip

    def e1 = prop { (zdt: ZonedDateTime)  => roundTripDateTime.to(zdt).map(_.toInstant.toEpochMilli) ==== Okay(zdt.toInstant.toEpochMilli) }
    def e2 = prop { (ld: LocalDate)       => roundTripLocalDate.to(ld) ==== Okay(ld) }
    def e3 = prop { (ldt: LocalDateTime)  => roundTripLocalDateTime.to(ldt) ==== Okay(ldt) }
    def e4 = prop { (lt: LocalTime)       => roundTripLocalTime.to(lt) ==== Okay(lt) }
}
