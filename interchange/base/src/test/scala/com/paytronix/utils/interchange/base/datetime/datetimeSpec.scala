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
            yyyy-mm-ddThh:mm:ss.ssssssZ     $zdtColonsFractionalSixZed
            yyyy-mm-ddThh:mm:ss.sssssssssZ  $zdtColonsFractionalNineZed
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
    def zdtColonsZed                  = iso8601.zonedDateTimeBijection.from("2014-07-01T13:14:15.161Z")       ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 15, 161 * nano, ZoneOffset.UTC))
    def zdtColonsFractionalSixZed     = iso8601.zonedDateTimeBijection.from("2014-07-01T13:14:15.123456Z")    ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 15, 123456000, ZoneOffset.UTC))
    def zdtColonsFractionalNineZed    = iso8601.zonedDateTimeBijection.from("2014-07-01T13:14:15.123456789Z") ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 15, 123456789, ZoneOffset.UTC))
    def zdtColonsPlusZero             = iso8601.zonedDateTimeBijection.from("2014-07-01T13:14:15.161+0000")   ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 15, 161 * nano, ZoneOffset.UTC))
    def zdtColonsMinusZero            = iso8601.zonedDateTimeBijection.from("2014-07-01T13:14:15.161-0000")   ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 15, 161 * nano, ZoneOffset.UTC))
    def zdtColonsPosColon             = iso8601.zonedDateTimeBijection.from("2014-07-01T13:14:15.161+04:56")  ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 15, 161 * nano, ZoneOffset.ofHoursMinutes(4, 56)))
    def zdtColonsNegColon             = iso8601.zonedDateTimeBijection.from("2014-07-01T13:14:15.161-04:56")  ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 15, 161 * nano, ZoneOffset.ofHoursMinutes(-4, -56)))
    def zdtColonsSpaceColonPos        = iso8601.zonedDateTimeBijection.from("2014-07-01T13:14:15.161 +04:56") ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 15, 161 * nano, ZoneOffset.ofHoursMinutes(4, 56)))
    def zdtColonsSpaceColonNeg        = iso8601.zonedDateTimeBijection.from("2014-07-01T13:14:15.161 -04:56") ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 15, 161 * nano, ZoneOffset.ofHoursMinutes(-4, -56)))
    def zdtColonsPos                  = iso8601.zonedDateTimeBijection.from("2014-07-01T13:14:15.161+0456")   ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 15, 161 * nano, ZoneOffset.ofHoursMinutes(4, 56)))
    def zdtColonsNeg                  = iso8601.zonedDateTimeBijection.from("2014-07-01T13:14:15.161-0456")   ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 15, 161 * nano, ZoneOffset.ofHoursMinutes(-4, -56)))
    def zdtColonsSpacePos             = iso8601.zonedDateTimeBijection.from("2014-07-01T13:14:15.161 +0456")  ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 15, 161 * nano, ZoneOffset.ofHoursMinutes(4, 56)))
    def zdtColonsSpaceNeg             = iso8601.zonedDateTimeBijection.from("2014-07-01T13:14:15.161 -0456")  ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 15, 161 * nano, ZoneOffset.ofHoursMinutes(-4, -56)))
    def zdtColonsNoMilliZed           = iso8601.zonedDateTimeBijection.from("2014-07-01T13:14:15Z")           ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 15, 0, ZoneOffset.UTC))
    def zdtColonsNoMilliPos           = iso8601.zonedDateTimeBijection.from("2014-07-01T13:14:15+0456")       ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 15, 0, ZoneOffset.ofHoursMinutes(4, 56)))
    def zdtColonsNoMilliNeg           = iso8601.zonedDateTimeBijection.from("2014-07-01T13:14:15-0456")       ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 15, 0, ZoneOffset.ofHoursMinutes(-4, -56)))
    def zdtColonsNoMilliSpacePos      = iso8601.zonedDateTimeBijection.from("2014-07-01T13:14:15+0456")       ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 15, 0, ZoneOffset.ofHoursMinutes(4, 56)))
    def zdtColonsNoMilliSpaceNeg      = iso8601.zonedDateTimeBijection.from("2014-07-01T13:14:15-0456")       ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 15, 0, ZoneOffset.ofHoursMinutes(-4, -56)))
    def zdtColonsNoMilliColonPos      = iso8601.zonedDateTimeBijection.from("2014-07-01T13:14:15+04:56")      ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 15, 0, ZoneOffset.ofHoursMinutes(4, 56)))
    def zdtColonsNoMilliColonNeg      = iso8601.zonedDateTimeBijection.from("2014-07-01T13:14:15-04:56")      ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 15, 0, ZoneOffset.ofHoursMinutes(-4, -56)))
    def zdtColonsNoMilliSpaceColonPos = iso8601.zonedDateTimeBijection.from("2014-07-01T13:14:15 +04:56")     ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 15, 0, ZoneOffset.ofHoursMinutes(4, 56)))
    def zdtColonsNoMilliSpaceColonNeg = iso8601.zonedDateTimeBijection.from("2014-07-01T13:14:15 -04:56")     ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 15, 0, ZoneOffset.ofHoursMinutes(-4, -56)))
    def zdtColonsNoSecPos             = iso8601.zonedDateTimeBijection.from("2014-07-01T13:14+0456")          ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 0, 0, ZoneOffset.ofHoursMinutes(4, 56)))
    def zdtColonsNoSecNeg             = iso8601.zonedDateTimeBijection.from("2014-07-01T13:14-0456")          ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 0, 0, ZoneOffset.ofHoursMinutes(-4, -56)))
    def zdtColonsNoSecSpacePos        = iso8601.zonedDateTimeBijection.from("2014-07-01T13:14 +0456")         ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 0, 0, ZoneOffset.ofHoursMinutes(4, 56)))
    def zdtColonsNoSecSpaceNeg        = iso8601.zonedDateTimeBijection.from("2014-07-01T13:14 -0456")         ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 0, 0, ZoneOffset.ofHoursMinutes(-4, -56)))
    def zdtColonsNoSecColonPos        = iso8601.zonedDateTimeBijection.from("2014-07-01T13:14+04:56")         ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 0, 0, ZoneOffset.ofHoursMinutes(4, 56)))
    def zdtColonsNoSecColonNeg        = iso8601.zonedDateTimeBijection.from("2014-07-01T13:14-04:56")         ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 0, 0, ZoneOffset.ofHoursMinutes(-4, -56)))
    def zdtColonsNoSecSpaceColonPos   = iso8601.zonedDateTimeBijection.from("2014-07-01T13:14 +04:56")        ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 0, 0, ZoneOffset.ofHoursMinutes(4, 56)))
    def zdtColonsNoSecSpaceColonNeg   = iso8601.zonedDateTimeBijection.from("2014-07-01T13:14 -04:56")        ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 0, 0, ZoneOffset.ofHoursMinutes(-4, -56)))
    def zdtNoColonsZed                = iso8601.zonedDateTimeBijection.from("20140701T131415.161Z")           ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 15, 161 * nano, ZoneOffset.UTC))
    def zdtNoColonsPlusZero           = iso8601.zonedDateTimeBijection.from("20140701T131415.161+0000")       ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 15, 161 * nano, ZoneOffset.UTC))
    def zdtNoColonsMinusZero          = iso8601.zonedDateTimeBijection.from("20140701T131415.161-0000")       ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 15, 161 * nano, ZoneOffset.UTC))
    def zdtNoColonsPosColon           = iso8601.zonedDateTimeBijection.from("20140701T131415.161+04:56")      ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 15, 161 * nano, ZoneOffset.ofHoursMinutes(4, 56)))
    def zdtNoColonsNegColon           = iso8601.zonedDateTimeBijection.from("20140701T131415.161-04:56")      ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 15, 161 * nano, ZoneOffset.ofHoursMinutes(-4, -56)))
    def zdtNoColonsNoMillisZed        = iso8601.zonedDateTimeBijection.from("20140701T131415Z")               ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 15, 0, ZoneOffset.UTC))
    def zdtNoColonsNoMillisColonPos   = iso8601.zonedDateTimeBijection.from("20140701T131415+04:56")          ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 15, 0, ZoneOffset.ofHoursMinutes(4, 56)))
    def zdtNoColonsNoMillisColonNeg   = iso8601.zonedDateTimeBijection.from("20140701T131415-04:56")          ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 15, 0, ZoneOffset.ofHoursMinutes(-4, -56)))

    def localDate      = iso8601.localDateBijection.from("2014-07-01") ==== Okay(LocalDate.of(2014, 7, 1))
    def basicLocalDate = iso8601.localDateBijection.from("20140701")   ==== Okay(LocalDate.of(2014, 7, 1))

    def localDateTime               = iso8601.localDateTimeBijection.from("2014-07-01T13:14:15.161") ==== Okay(LocalDateTime.of(2014, 7, 1, 13, 14, 15, 161 * nano))
    def localDateTimeNoMillis       = iso8601.localDateTimeBijection.from("2014-07-01T13:14:15")     ==== Okay(LocalDateTime.of(2014, 7, 1, 13, 14, 15, 0))
    def localDateTimeNoSeconds      = iso8601.localDateTimeBijection.from("2014-07-01T13:14")        ==== Okay(LocalDateTime.of(2014, 7, 1, 13, 14, 0, 0))
    def basicLocalDateTime          = iso8601.localDateTimeBijection.from("20140701T131415.161")     ==== Okay(LocalDateTime.of(2014, 7, 1, 13, 14, 15, 161 * nano))
    def basicLocalDateTimeNoMillis  = iso8601.localDateTimeBijection.from("20140701T131415")         ==== Okay(LocalDateTime.of(2014, 7, 1, 13, 14, 15, 0))
    def basicLocalDateTimeNoSeconds = iso8601.localDateTimeBijection.from("20140701T1314")           ==== Okay(LocalDateTime.of(2014, 7, 1, 13, 14, 0, 0))

    def localTime               = iso8601.localTimeBijection.from("12:13:14.161") ==== Okay(LocalTime.of(12, 13, 14, 161 * nano))
    def localTimeNoMillis       = iso8601.localTimeBijection.from("12:13:14")     ==== Okay(LocalTime.of(12, 13, 14, 0))
    def localTimeNoSeconds      = iso8601.localTimeBijection.from("12:13")        ==== Okay(LocalTime.of(12, 13, 0, 0))
    def basicLocalTime          = iso8601.localTimeBijection.from("121314.161")   ==== Okay(LocalTime.of(12, 13, 14, 161 * nano))
    def basicLocalTimeNoMillis  = iso8601.localTimeBijection.from("121314")       ==== Okay(LocalTime.of(12, 13, 14, 0))
    def basicLocalTimeNoSeconds = iso8601.localTimeBijection.from("1213")         ==== Okay(LocalTime.of(12, 13, 0, 0))

    // Formatting tests
    def formatZonedDateTimeUTC     = iso8601.zonedDateTimeBijection.to(ZonedDateTime.of(2014, 7, 1, 13, 14, 15, 0, ZoneOffset.UTC)) ==== Okay("2014-07-01T13:14:15.000Z")
    def formatZonedDateTimePos     = iso8601.zonedDateTimeBijection.to(ZonedDateTime.of(2014, 7, 1, 13, 14, 15, 0, ZoneOffset.ofHoursMinutes(4, 56))) ==== Okay("2014-07-01T13:14:15.000+04:56")
    def formatZonedDateTimeNeg     = iso8601.zonedDateTimeBijection.to(ZonedDateTime.of(2014, 7, 1, 13, 14, 15, 0, ZoneOffset.ofHoursMinutes(-4, -56))) ==== Okay("2014-07-01T13:14:15.000-04:56")
    // Gotta make sure it only puts the UTC offset and doesn't append "[America/New_York]" to the end
    def formatZonedDateTimeNewYork = iso8601.zonedDateTimeBijection.to(ZonedDateTime.of(2014, 7, 1, 13, 14, 15, 0, ZoneId.of("America/New_York"))) ==== Okay("2014-07-01T13:14:15.000-04:00")
    def formatZonedDateTimeMoscow  = iso8601.zonedDateTimeBijection.to(ZonedDateTime.of(2014, 7, 1, 13, 14, 15, 0, ZoneId.of("Europe/Moscow"))) ==== Okay("2014-07-01T13:14:15.000+04:00")
    def formatLocalDate            = iso8601.localDateBijection.to(LocalDate.of(2014, 7, 1)) ==== Okay("2014-07-01")
    def formatLocalDateTime        = iso8601.localDateTimeBijection.to(LocalDateTime.of(2014, 7, 1, 13, 14, 15, 0)) ==== Okay("2014-07-01T13:14:15.000")
    def formatLocalTime            = iso8601.localTimeBijection.to(LocalTime.of(12, 13, 14, 0)) ==== Okay("12:13:14.000")
}

class classicTest extends SpecificationWithJUnit with ScalaCheck {
    def is = s2"""
        classic bijections should round trip
            java.time.ZonedDateTime         $rtZonedDateTime
            java.time.LocalDate             $rtLocalDate
            java.time.LocalDateTime         $rtLocalDateTime
            java.time.LocalTime             $rtLocalTime
        and accept ZonedDateTime
            yyyy-mm-dd hh:mm:ss.sssZ        $zdtSpcColonsZed
            yyyy-mm-dd hh:mm:ss.ssssssZ     $zdtSpcColonsFractionalSixZed
            yyyy-mm-dd hh:mm:ss.sssssssssZ  $zdtSpcColonsFractionalNineZed
            yyyy-mm-dd hh:mm:ss.sss+0000    $zdtSpcColonsPlusZero
            yyyy-mm-dd hh:mm:ss.sss-0000    $zdtSpcColonsMinusZero
            yyyy-mm-dd hh:mm:ss.sss+hh:mm   $zdtSpcColonsPosColon
            yyyy-mm-dd hh:mm:ss.sss-hh:mm   $zdtSpcColonsNegColon
            yyyy-mm-dd hh:mm:ss.sss +hh:mm  $zdtSpcColonsSpaceColonPos
            yyyy-mm-dd hh:mm:ss.sss -hh:mm  $zdtSpcColonsSpaceColonNeg
            yyyy-mm-dd hh:mm:ss.sss+hhmm    $zdtSpcColonsPos
            yyyy-mm-dd hh:mm:ss.sss-hhmm    $zdtSpcColonsNeg
            yyyy-mm-dd hh:mm:ss.sss +hhmm   $zdtSpcColonsSpacePos
            yyyy-mm-dd hh:mm:ss.sss -hhmm   $zdtSpcColonsSpaceNeg
            yyyy-mm-dd hh:mm:ssZ            $zdtSpcColonsNoMilliZed
            yyyy-mm-dd hh:mm:ss+hhmm        $zdtSpcColonsNoMilliPos
            yyyy-mm-dd hh:mm:ss-hhmm        $zdtSpcColonsNoMilliNeg
            yyyy-mm-dd hh:mm:ss +hhmm       $zdtSpcColonsNoMilliSpacePos
            yyyy-mm-dd hh:mm:ss -hhmm       $zdtSpcColonsNoMilliSpaceNeg
            yyyy-mm-dd hh:mm:ss+hh:mm       $zdtSpcColonsNoMilliColonPos
            yyyy-mm-dd hh:mm:ss-hh:mm       $zdtSpcColonsNoMilliColonNeg
            yyyy-mm-dd hh:mm:ss +hh:mm      $zdtSpcColonsNoMilliSpaceColonPos
            yyyy-mm-dd hh:mm:ss -hh:mm      $zdtSpcColonsNoMilliSpaceColonNeg
            yyyy-mm-dd hh:mm+hhmm           $zdtSpcColonsNoSecPos
            yyyy-mm-dd hh:mm-hhmm           $zdtSpcColonsNoSecNeg
            yyyy-mm-dd hh:mm +hhmm          $zdtSpcColonsNoSecSpacePos
            yyyy-mm-dd hh:mm -hhmm          $zdtSpcColonsNoSecSpaceNeg
            yyyy-mm-dd hh:mm+hh:mm          $zdtSpcColonsNoSecColonPos
            yyyy-mm-dd hh:mm-hh:mm          $zdtSpcColonsNoSecColonNeg
            yyyy-mm-dd hh:mm +hh:mm         $zdtSpcColonsNoSecSpaceColonPos
            yyyy-mm-dd hh:mm -hh:mm         $zdtSpcColonsNoSecSpaceColonNeg
            yyyy-mm-ddThh:mm:ss.sssZ        $zdtTColonsZed
            yyyy-mm-ddThh:mm:ss.sss+0000    $zdtTColonsPlusZero
            yyyy-mm-ddThh:mm:ss.sss-0000    $zdtTColonsMinusZero
            yyyy-mm-ddThh:mm:ss.sss+hh:mm   $zdtTColonsPosColon
            yyyy-mm-ddThh:mm:ss.sss-hh:mm   $zdtTColonsNegColon
            yyyy-mm-ddThh:mm:ss.sss +hh:mm  $zdtTColonsSpaceColonPos
            yyyy-mm-ddThh:mm:ss.sss -hh:mm  $zdtTColonsSpaceColonNeg
            yyyy-mm-ddThh:mm:ss.sss+hhmm    $zdtTColonsPos
            yyyy-mm-ddThh:mm:ss.sss-hhmm    $zdtTColonsNeg
            yyyy-mm-ddThh:mm:ss.sss +hhmm   $zdtTColonsSpacePos
            yyyy-mm-ddThh:mm:ss.sss -hhmm   $zdtTColonsSpaceNeg
            yyyy-mm-ddThh:mm:ssZ            $zdtTColonsNoMilliZed
            yyyy-mm-ddThh:mm:ss+hhmm        $zdtTColonsNoMilliPos
            yyyy-mm-ddThh:mm:ss-hhmm        $zdtTColonsNoMilliNeg
            yyyy-mm-ddThh:mm:ss +hhmm       $zdtTColonsNoMilliSpacePos
            yyyy-mm-ddThh:mm:ss -hhmm       $zdtTColonsNoMilliSpaceNeg
            yyyy-mm-ddThh:mm:ss+hh:mm       $zdtTColonsNoMilliColonPos
            yyyy-mm-ddThh:mm:ss-hh:mm       $zdtTColonsNoMilliColonNeg
            yyyy-mm-ddThh:mm:ss +hh:mm      $zdtTColonsNoMilliSpaceColonPos
            yyyy-mm-ddThh:mm:ss -hh:mm      $zdtTColonsNoMilliSpaceColonNeg
            yyyy-mm-ddThh:mm+hhmm           $zdtTColonsNoSecPos
            yyyy-mm-ddThh:mm-hhmm           $zdtTColonsNoSecNeg
            yyyy-mm-ddThh:mm +hhmm          $zdtTColonsNoSecSpacePos
            yyyy-mm-ddThh:mm -hhmm          $zdtTColonsNoSecSpaceNeg
            yyyy-mm-ddThh:mm+hh:mm          $zdtTColonsNoSecColonPos
            yyyy-mm-ddThh:mm-hh:mm          $zdtTColonsNoSecColonNeg
            yyyy-mm-ddThh:mm +hh:mm         $zdtTColonsNoSecSpaceColonPos
            yyyy-mm-ddThh:mm -hh:mm         $zdtTColonsNoSecSpaceColonNeg
            yyyymmdd hhmmss.sssZ            $zdtSpcNoColonsZed
            yyyymmdd hhmmss.sss+0000        $zdtSpcNoColonsPlusZero
            yyyymmdd hhmmss.sss-0000        $zdtSpcNoColonsMinusZero
            yyyymmdd hhmmss.sss+00:00       $zdtSpcNoColonsPosColon
            yyyymmdd hhmmss.sss-00:00       $zdtSpcNoColonsNegColon
            yyyymmdd hhmmssZ                $zdtSpcNoColonsNoMillisZed
            yyyymmdd hhmmss+0000            $zdtSpcNoColonsNoMillisColonPos
            yyyymmdd hhmmss-0000            $zdtSpcNoColonsNoMillisColonNeg
            yyyymmddThhmmss.sssZ            $zdtTNoColonsZed
            yyyymmddThhmmss.sss+0000        $zdtTNoColonsPlusZero
            yyyymmddThhmmss.sss-0000        $zdtTNoColonsMinusZero
            yyyymmddThhmmss.sss+00:00       $zdtTNoColonsPosColon
            yyyymmddThhmmss.sss-00:00       $zdtTNoColonsNegColon
            yyyymmddThhmmssZ                $zdtTNoColonsNoMillisZed
            yyyymmddThhmmss+0000            $zdtTNoColonsNoMillisColonPos
            yyyymmddThhmmss-0000            $zdtTNoColonsNoMillisColonNeg
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

    val roundTripDateTime      = classic.zonedDateTimeBijection >=> classic.zonedDateTimeBijection.flip
    val roundTripLocalDate     = classic.localDateBijection     >=> classic.localDateBijection.flip
    val roundTripLocalDateTime = classic.localDateTimeBijection >=> classic.localDateTimeBijection.flip
    val roundTripLocalTime     = classic.localTimeBijection     >=> classic.localTimeBijection.flip

    // Round-trip tests
    def rtZonedDateTime = prop { (zdt: ZonedDateTime) => roundTripDateTime.to(zdt).map(_.toInstant.toEpochMilli) ==== Okay(zdt.toInstant.plusNanos(-zdt.getNano).toEpochMilli) }
    def rtLocalDate = prop { (ld: LocalDate)      => roundTripLocalDate.to(ld) ==== Okay(ld) }
    def rtLocalDateTime = prop { (ldt: LocalDateTime) => roundTripLocalDateTime.to(ldt) ==== Okay(ldt.plusNanos(-ldt.getNano)) }
    def rtLocalTime = prop { (lt: LocalTime)      => roundTripLocalTime.to(lt) ==== Okay(lt) }

    val nano: Int = 1000000

/*
    val dateTimeFormatters = NonEmptyList (
        DateTimeFormatters.withOptionalFractionalSecondRange("uuuu-MM-dd HH:mm[:ss]", Some("[ ]XX")),
        DateTimeFormatters.withOptionalFractionalSecondRange("uuuu-MM-dd HH:mm[:ss]", Some("[ ]XXX")),
        DateTimeFormatters.withOptionalFractionalSecondRange("uuuu-MM-dd'T'HH:mm[:ss]", Some("[ ]XX")),
        DateTimeFormatters.withOptionalFractionalSecondRange("uuuu-MM-dd'T'HH:mm[:ss]", Some("[ ]XXX")),
        DateTimeFormatters.withOptionalFractionalSecondRange("E MMM dd HH:mm[:ss]", Some(" XX uuuu")),
        DateTimeFormatters.withOptionalFractionalSecondRange("E MMM dd HH:mm[:ss]", Some(" XXX uuuu")),
        DateTimeFormatters.withOptionalFractionalSecondRange("E, dd MMM yy HH:mm[:ss]", Some(" XX")),
        DateTimeFormatters.withOptionalFractionalSecondRange("E, dd MMM yy HH:mm[:ss]", Some(" XXX")),
        DateTimeFormatter.ISO_OFFSET_DATE_TIME,
        DateTimeFormatters.withOptionalFractionalSecondRange("uuuuMMdd'T'HHmm[ss]", Some("[ ]XX")),
        DateTimeFormatters.withOptionalFractionalSecondRange("uuuuMMdd'T'HHmm[ss]", Some("[ ]XXX")),
        // For some terrible reason our documentation says timezone is optional so we'll fall back to this
        DateTimeFormatters.withOptionalFractionalSecondRange("uuuu-MM-dd HH:mm[:ss]").withZone(ZoneOffset.UTC),
        DateTimeFormatters.withOptionalFractionalSecondRange("uuuu-MM-dd HH:mm[:ss]").withZone(ZoneOffset.UTC),
        DateTimeFormatters.withOptionalFractionalSecondRange("uuuu-MM-dd'T'HH:mm[:ss]", Some("[ ]XX"))
    )
*/

    // Parsing tests
    def zdtSpcColonsZed                  = classic.zonedDateTimeBijection.from("2014-07-01 13:14:15.161Z")       ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 15, 161 * nano, ZoneOffset.UTC))
    def zdtSpcColonsFractionalSixZed     = iso8601.zonedDateTimeBijection.from("2014-07-01 13:14:15.123456Z")    ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 15, 123456000, ZoneOffset.UTC))
    def zdtSpcColonsFractionalNineZed    = iso8601.zonedDateTimeBijection.from("2014-07-01 13:14:15.123456789Z") ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 15, 123456789, ZoneOffset.UTC))
    def zdtSpcColonsPlusZero             = classic.zonedDateTimeBijection.from("2014-07-01 13:14:15.161+0000")   ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 15, 161 * nano, ZoneOffset.UTC))
    def zdtSpcColonsMinusZero            = classic.zonedDateTimeBijection.from("2014-07-01 13:14:15.161-0000")   ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 15, 161 * nano, ZoneOffset.UTC))
    def zdtSpcColonsPosColon             = classic.zonedDateTimeBijection.from("2014-07-01 13:14:15.161+04:56")  ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 15, 161 * nano, ZoneOffset.ofHoursMinutes(4, 56)))
    def zdtSpcColonsNegColon             = classic.zonedDateTimeBijection.from("2014-07-01 13:14:15.161-04:56")  ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 15, 161 * nano, ZoneOffset.ofHoursMinutes(-4, -56)))
    def zdtSpcColonsSpaceColonPos        = classic.zonedDateTimeBijection.from("2014-07-01 13:14:15.161 +04:56") ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 15, 161 * nano, ZoneOffset.ofHoursMinutes(4, 56)))
    def zdtSpcColonsSpaceColonNeg        = classic.zonedDateTimeBijection.from("2014-07-01 13:14:15.161 -04:56") ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 15, 161 * nano, ZoneOffset.ofHoursMinutes(-4, -56)))
    def zdtSpcColonsPos                  = classic.zonedDateTimeBijection.from("2014-07-01 13:14:15.161+0456")   ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 15, 161 * nano, ZoneOffset.ofHoursMinutes(4, 56)))
    def zdtSpcColonsNeg                  = classic.zonedDateTimeBijection.from("2014-07-01 13:14:15.161-0456")   ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 15, 161 * nano, ZoneOffset.ofHoursMinutes(-4, -56)))
    def zdtSpcColonsSpacePos             = classic.zonedDateTimeBijection.from("2014-07-01 13:14:15.161 +0456")  ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 15, 161 * nano, ZoneOffset.ofHoursMinutes(4, 56)))
    def zdtSpcColonsSpaceNeg             = classic.zonedDateTimeBijection.from("2014-07-01 13:14:15.161 -0456")  ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 15, 161 * nano, ZoneOffset.ofHoursMinutes(-4, -56)))
    def zdtSpcColonsNoMilliZed           = classic.zonedDateTimeBijection.from("2014-07-01 13:14:15Z")           ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 15, 0, ZoneOffset.UTC))
    def zdtSpcColonsNoMilliPos           = classic.zonedDateTimeBijection.from("2014-07-01 13:14:15+0456")       ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 15, 0, ZoneOffset.ofHoursMinutes(4, 56)))
    def zdtSpcColonsNoMilliNeg           = classic.zonedDateTimeBijection.from("2014-07-01 13:14:15-0456")       ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 15, 0, ZoneOffset.ofHoursMinutes(-4, -56)))
    def zdtSpcColonsNoMilliSpacePos      = classic.zonedDateTimeBijection.from("2014-07-01 13:14:15+0456")       ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 15, 0, ZoneOffset.ofHoursMinutes(4, 56)))
    def zdtSpcColonsNoMilliSpaceNeg      = classic.zonedDateTimeBijection.from("2014-07-01 13:14:15-0456")       ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 15, 0, ZoneOffset.ofHoursMinutes(-4, -56)))
    def zdtSpcColonsNoMilliColonPos      = classic.zonedDateTimeBijection.from("2014-07-01 13:14:15+04:56")      ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 15, 0, ZoneOffset.ofHoursMinutes(4, 56)))
    def zdtSpcColonsNoMilliColonNeg      = classic.zonedDateTimeBijection.from("2014-07-01 13:14:15-04:56")      ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 15, 0, ZoneOffset.ofHoursMinutes(-4, -56)))
    def zdtSpcColonsNoMilliSpaceColonPos = classic.zonedDateTimeBijection.from("2014-07-01 13:14:15 +04:56")     ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 15, 0, ZoneOffset.ofHoursMinutes(4, 56)))
    def zdtSpcColonsNoMilliSpaceColonNeg = classic.zonedDateTimeBijection.from("2014-07-01 13:14:15 -04:56")     ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 15, 0, ZoneOffset.ofHoursMinutes(-4, -56)))
    def zdtSpcColonsNoSecPos             = classic.zonedDateTimeBijection.from("2014-07-01 13:14+0456")          ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 0, 0, ZoneOffset.ofHoursMinutes(4, 56)))
    def zdtSpcColonsNoSecNeg             = classic.zonedDateTimeBijection.from("2014-07-01 13:14-0456")          ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 0, 0, ZoneOffset.ofHoursMinutes(-4, -56)))
    def zdtSpcColonsNoSecSpacePos        = classic.zonedDateTimeBijection.from("2014-07-01 13:14 +0456")         ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 0, 0, ZoneOffset.ofHoursMinutes(4, 56)))
    def zdtSpcColonsNoSecSpaceNeg        = classic.zonedDateTimeBijection.from("2014-07-01 13:14 -0456")         ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 0, 0, ZoneOffset.ofHoursMinutes(-4, -56)))
    def zdtSpcColonsNoSecColonPos        = classic.zonedDateTimeBijection.from("2014-07-01 13:14+04:56")         ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 0, 0, ZoneOffset.ofHoursMinutes(4, 56)))
    def zdtSpcColonsNoSecColonNeg        = classic.zonedDateTimeBijection.from("2014-07-01 13:14-04:56")         ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 0, 0, ZoneOffset.ofHoursMinutes(-4, -56)))
    def zdtSpcColonsNoSecSpaceColonPos   = classic.zonedDateTimeBijection.from("2014-07-01 13:14 +04:56")        ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 0, 0, ZoneOffset.ofHoursMinutes(4, 56)))
    def zdtSpcColonsNoSecSpaceColonNeg   = classic.zonedDateTimeBijection.from("2014-07-01 13:14 -04:56")        ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 0, 0, ZoneOffset.ofHoursMinutes(-4, -56)))
    def zdtTColonsZed                  = classic.zonedDateTimeBijection.from("2014-07-01T13:14:15.161Z")       ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 15, 161 * nano, ZoneOffset.UTC))
    def zdtTColonsPlusZero             = classic.zonedDateTimeBijection.from("2014-07-01T13:14:15.161+0000")   ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 15, 161 * nano, ZoneOffset.UTC))
    def zdtTColonsMinusZero            = classic.zonedDateTimeBijection.from("2014-07-01T13:14:15.161-0000")   ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 15, 161 * nano, ZoneOffset.UTC))
    def zdtTColonsPosColon             = classic.zonedDateTimeBijection.from("2014-07-01T13:14:15.161+04:56")  ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 15, 161 * nano, ZoneOffset.ofHoursMinutes(4, 56)))
    def zdtTColonsNegColon             = classic.zonedDateTimeBijection.from("2014-07-01T13:14:15.161-04:56")  ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 15, 161 * nano, ZoneOffset.ofHoursMinutes(-4, -56)))
    def zdtTColonsSpaceColonPos        = classic.zonedDateTimeBijection.from("2014-07-01T13:14:15.161 +04:56") ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 15, 161 * nano, ZoneOffset.ofHoursMinutes(4, 56)))
    def zdtTColonsSpaceColonNeg        = classic.zonedDateTimeBijection.from("2014-07-01T13:14:15.161 -04:56") ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 15, 161 * nano, ZoneOffset.ofHoursMinutes(-4, -56)))
    def zdtTColonsPos                  = classic.zonedDateTimeBijection.from("2014-07-01T13:14:15.161+0456")   ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 15, 161 * nano, ZoneOffset.ofHoursMinutes(4, 56)))
    def zdtTColonsNeg                  = classic.zonedDateTimeBijection.from("2014-07-01T13:14:15.161-0456")   ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 15, 161 * nano, ZoneOffset.ofHoursMinutes(-4, -56)))
    def zdtTColonsSpacePos             = classic.zonedDateTimeBijection.from("2014-07-01T13:14:15.161 +0456")  ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 15, 161 * nano, ZoneOffset.ofHoursMinutes(4, 56)))
    def zdtTColonsSpaceNeg             = classic.zonedDateTimeBijection.from("2014-07-01T13:14:15.161 -0456")  ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 15, 161 * nano, ZoneOffset.ofHoursMinutes(-4, -56)))
    def zdtTColonsNoMilliZed           = classic.zonedDateTimeBijection.from("2014-07-01T13:14:15Z")           ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 15, 0, ZoneOffset.UTC))
    def zdtTColonsNoMilliPos           = classic.zonedDateTimeBijection.from("2014-07-01T13:14:15+0456")       ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 15, 0, ZoneOffset.ofHoursMinutes(4, 56)))
    def zdtTColonsNoMilliNeg           = classic.zonedDateTimeBijection.from("2014-07-01T13:14:15-0456")       ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 15, 0, ZoneOffset.ofHoursMinutes(-4, -56)))
    def zdtTColonsNoMilliSpacePos      = classic.zonedDateTimeBijection.from("2014-07-01T13:14:15+0456")       ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 15, 0, ZoneOffset.ofHoursMinutes(4, 56)))
    def zdtTColonsNoMilliSpaceNeg      = classic.zonedDateTimeBijection.from("2014-07-01T13:14:15-0456")       ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 15, 0, ZoneOffset.ofHoursMinutes(-4, -56)))
    def zdtTColonsNoMilliColonPos      = classic.zonedDateTimeBijection.from("2014-07-01T13:14:15+04:56")      ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 15, 0, ZoneOffset.ofHoursMinutes(4, 56)))
    def zdtTColonsNoMilliColonNeg      = classic.zonedDateTimeBijection.from("2014-07-01T13:14:15-04:56")      ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 15, 0, ZoneOffset.ofHoursMinutes(-4, -56)))
    def zdtTColonsNoMilliSpaceColonPos = classic.zonedDateTimeBijection.from("2014-07-01T13:14:15 +04:56")     ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 15, 0, ZoneOffset.ofHoursMinutes(4, 56)))
    def zdtTColonsNoMilliSpaceColonNeg = classic.zonedDateTimeBijection.from("2014-07-01T13:14:15 -04:56")     ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 15, 0, ZoneOffset.ofHoursMinutes(-4, -56)))
    def zdtTColonsNoSecPos             = classic.zonedDateTimeBijection.from("2014-07-01T13:14+0456")          ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 0, 0, ZoneOffset.ofHoursMinutes(4, 56)))
    def zdtTColonsNoSecNeg             = classic.zonedDateTimeBijection.from("2014-07-01T13:14-0456")          ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 0, 0, ZoneOffset.ofHoursMinutes(-4, -56)))
    def zdtTColonsNoSecSpacePos        = classic.zonedDateTimeBijection.from("2014-07-01T13:14 +0456")         ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 0, 0, ZoneOffset.ofHoursMinutes(4, 56)))
    def zdtTColonsNoSecSpaceNeg        = classic.zonedDateTimeBijection.from("2014-07-01T13:14 -0456")         ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 0, 0, ZoneOffset.ofHoursMinutes(-4, -56)))
    def zdtTColonsNoSecColonPos        = classic.zonedDateTimeBijection.from("2014-07-01T13:14+04:56")         ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 0, 0, ZoneOffset.ofHoursMinutes(4, 56)))
    def zdtTColonsNoSecColonNeg        = classic.zonedDateTimeBijection.from("2014-07-01T13:14-04:56")         ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 0, 0, ZoneOffset.ofHoursMinutes(-4, -56)))
    def zdtTColonsNoSecSpaceColonPos   = classic.zonedDateTimeBijection.from("2014-07-01T13:14 +04:56")        ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 0, 0, ZoneOffset.ofHoursMinutes(4, 56)))
    def zdtTColonsNoSecSpaceColonNeg   = classic.zonedDateTimeBijection.from("2014-07-01T13:14 -04:56")        ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 0, 0, ZoneOffset.ofHoursMinutes(-4, -56)))
    def zdtSpcNoColonsZed                = classic.zonedDateTimeBijection.from("20140701 131415.161Z")           ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 15, 161 * nano, ZoneOffset.UTC))
    def zdtSpcNoColonsPlusZero           = classic.zonedDateTimeBijection.from("20140701 131415.161+0000")       ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 15, 161 * nano, ZoneOffset.UTC))
    def zdtSpcNoColonsMinusZero          = classic.zonedDateTimeBijection.from("20140701 131415.161-0000")       ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 15, 161 * nano, ZoneOffset.UTC))
    def zdtSpcNoColonsPosColon           = classic.zonedDateTimeBijection.from("20140701 131415.161+04:56")      ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 15, 161 * nano, ZoneOffset.ofHoursMinutes(4, 56)))
    def zdtSpcNoColonsNegColon           = classic.zonedDateTimeBijection.from("20140701 131415.161-04:56")      ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 15, 161 * nano, ZoneOffset.ofHoursMinutes(-4, -56)))
    def zdtSpcNoColonsNoMillisZed        = classic.zonedDateTimeBijection.from("20140701 131415Z")               ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 15, 0, ZoneOffset.UTC))
    def zdtSpcNoColonsNoMillisColonPos   = classic.zonedDateTimeBijection.from("20140701 131415+04:56")          ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 15, 0, ZoneOffset.ofHoursMinutes(4, 56)))
    def zdtSpcNoColonsNoMillisColonNeg   = classic.zonedDateTimeBijection.from("20140701 131415-04:56")          ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 15, 0, ZoneOffset.ofHoursMinutes(-4, -56)))
    def zdtTNoColonsZed                = classic.zonedDateTimeBijection.from("20140701T131415.161Z")           ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 15, 161 * nano, ZoneOffset.UTC))
    def zdtTNoColonsPlusZero           = classic.zonedDateTimeBijection.from("20140701T131415.161+0000")       ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 15, 161 * nano, ZoneOffset.UTC))
    def zdtTNoColonsMinusZero          = classic.zonedDateTimeBijection.from("20140701T131415.161-0000")       ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 15, 161 * nano, ZoneOffset.UTC))
    def zdtTNoColonsPosColon           = classic.zonedDateTimeBijection.from("20140701T131415.161+04:56")      ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 15, 161 * nano, ZoneOffset.ofHoursMinutes(4, 56)))
    def zdtTNoColonsNegColon           = classic.zonedDateTimeBijection.from("20140701T131415.161-04:56")      ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 15, 161 * nano, ZoneOffset.ofHoursMinutes(-4, -56)))
    def zdtTNoColonsNoMillisZed        = classic.zonedDateTimeBijection.from("20140701T131415Z")               ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 15, 0, ZoneOffset.UTC))
    def zdtTNoColonsNoMillisColonPos   = classic.zonedDateTimeBijection.from("20140701T131415+04:56")          ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 15, 0, ZoneOffset.ofHoursMinutes(4, 56)))
    def zdtTNoColonsNoMillisColonNeg   = classic.zonedDateTimeBijection.from("20140701T131415-04:56")          ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 15, 0, ZoneOffset.ofHoursMinutes(-4, -56)))

    def localDate = classic.localDateBijection.from("2014-07-01") ==== Okay(LocalDate.of(2014, 7, 1))
    def basicLocalDate = classic.localDateBijection.from("20140701") ==== Okay(LocalDate.of(2014, 7, 1))

    def localDateTime = classic.localDateTimeBijection.from("2014-07-01T13:14:15.161") ==== Okay(LocalDateTime.of(2014, 7, 1, 13, 14, 15, 161 * nano))
    def localDateTimeNoMillis = classic.localDateTimeBijection.from("2014-07-01T13:14:15") ==== Okay(LocalDateTime.of(2014, 7, 1, 13, 14, 15, 0))
    def localDateTimeNoSeconds = classic.localDateTimeBijection.from("2014-07-01T13:14") ==== Okay(LocalDateTime.of(2014, 7, 1, 13, 14, 0, 0))
    def basicLocalDateTime = classic.localDateTimeBijection.from("20140701T131415.161") ==== Okay(LocalDateTime.of(2014, 7, 1, 13, 14, 15, 161 * nano))
    def basicLocalDateTimeNoMillis = classic.localDateTimeBijection.from("20140701T131415") ==== Okay(LocalDateTime.of(2014, 7, 1, 13, 14, 15, 0))
    def basicLocalDateTimeNoSeconds = classic.localDateTimeBijection.from("20140701T1314") ==== Okay(LocalDateTime.of(2014, 7, 1, 13, 14, 0, 0))

    def localTime = classic.localTimeBijection.from("12:13:14.161") ==== Okay(LocalTime.of(12, 13, 14, 161 * nano))
    def localTimeNoMillis = classic.localTimeBijection.from("12:13:14") ==== Okay(LocalTime.of(12, 13, 14, 0))
    def localTimeNoSeconds = classic.localTimeBijection.from("12:13") ==== Okay(LocalTime.of(12, 13, 0, 0))
    def basicLocalTime = classic.localTimeBijection.from("121314.161") ==== Okay(LocalTime.of(12, 13, 14, 161 * nano))
    def basicLocalTimeNoMillis = classic.localTimeBijection.from("121314") ==== Okay(LocalTime.of(12, 13, 14, 0))
    def basicLocalTimeNoSeconds = classic.localTimeBijection.from("1213") ==== Okay(LocalTime.of(12, 13, 0, 0))

    // Formatting tests
    def formatZonedDateTimeUTC = classic.zonedDateTimeBijection.to(ZonedDateTime.of(2014, 7, 1, 13, 14, 15, 0, ZoneOffset.UTC)) ==== Okay("2014-07-01 13:14:15 +0000")
    def formatZonedDateTimePos = classic.zonedDateTimeBijection.to(ZonedDateTime.of(2014, 7, 1, 13, 14, 15, 0, ZoneOffset.ofHoursMinutes(4, 56))) ==== Okay("2014-07-01 13:14:15 +0456")
    def formatZonedDateTimeNeg = classic.zonedDateTimeBijection.to(ZonedDateTime.of(2014, 7, 1, 13, 14, 15, 0, ZoneOffset.ofHoursMinutes(-4, -56))) ==== Okay("2014-07-01 13:14:15 -0456")
    // Gotta make sure it only puts the UTC offset and doesn't append "[America/New_York]" to the end
    def formatZonedDateTimeNewYork = classic.zonedDateTimeBijection.to(ZonedDateTime.of(2014, 7, 1, 13, 14, 15, 0, ZoneId.of("America/New_York"))) ==== Okay("2014-07-01 13:14:15 -0400")
    def formatZonedDateTimeMoscow = classic.zonedDateTimeBijection.to(ZonedDateTime.of(2014, 7, 1, 13, 14, 15, 0, ZoneId.of("Europe/Moscow"))) ==== Okay("2014-07-01 13:14:15 +0400")
    def formatLocalDate = classic.localDateBijection.to(LocalDate.of(2014, 7, 1)) ==== Okay("2014-07-01")
    def formatLocalDateTime = classic.localDateTimeBijection.to(LocalDateTime.of(2014, 7, 1, 13, 14, 15, 0)) ==== Okay("2014-07-01 13:14:15")
    def formatLocalTime = classic.localTimeBijection.to(LocalTime.of(12, 13, 14, 0)) ==== Okay("12:13:14")
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
