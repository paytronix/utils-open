//
// Copyright 2014-2020 Paytronix Systems, Inc.
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
        javaDates converters should round trip
            java.util.Date     $e1
            java.sql.Date      $e2
            java.sql.Time      $e3
            java.sql.Timestamp $e4
    """

    val roundTripJavaDate         = javaDates.javaDateConverter.flip.compose(javaDates.javaDateConverter)
    val roundTripJavaSqlDate      = javaDates.javaSqlDateConverter.flip.compose(javaDates.javaSqlDateConverter)
    val roundTripJavaSqlTime      = javaDates.javaSqlTimeConverter.flip.compose(javaDates.javaSqlTimeConverter)
    val roundTripJavaSqlTimestamp = javaDates.javaSqlTimestampConverter.flip.compose(javaDates.javaSqlTimestampConverter)

    def e1 = prop { (zdt: ZonedDateTime) => roundTripJavaDate.from(zdt) ==== Okay(zdt) }
    def e2 = prop { (ld: LocalDate)      => roundTripJavaSqlDate.from(ld) ==== Okay(ld) }
    def e3 = prop { (lt: LocalTime)      => roundTripJavaSqlTime.from(lt) ==== Okay(lt.withNano(0)) } // java.sql.Time does not support nanos or millis
    def e4 = prop { (zdt: ZonedDateTime) => roundTripJavaSqlTimestamp.from(zdt) ==== Okay(zdt) }
}

class iso8601Test extends SpecificationWithJUnit with ScalaCheck {
    def is = s2"""
        iso8601 converters should round trip
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

    val roundTripDateTime      = iso8601.zonedDateTimeConverter.compose(iso8601.zonedDateTimeConverter.flip)
    val roundTripLocalDate     = iso8601.localDateConverter.compose(iso8601.localDateConverter.flip)
    val roundTripLocalDateTime = iso8601.localDateTimeConverter.compose(iso8601.localDateTimeConverter.flip)
    val roundTripLocalTime     = iso8601.localTimeConverter.compose(iso8601.localTimeConverter.flip)

    // Round-trip tests
    def rtZonedDateTime = prop { (zdt: ZonedDateTime) => roundTripDateTime.to(zdt).map(_.toInstant.toEpochMilli) ==== Okay(zdt.toInstant.toEpochMilli) }
    def rtLocalDate = prop { (ld: LocalDate)      => roundTripLocalDate.to(ld) ==== Okay(ld) }
    def rtLocalDateTime = prop { (ldt: LocalDateTime) => roundTripLocalDateTime.to(ldt) ==== Okay(ldt) }
    def rtLocalTime = prop { (lt: LocalTime)      => roundTripLocalTime.to(lt) ==== Okay(lt) }

    val nano: Int = 1000000

    // Parsing tests
    def zdtColonsZed                  = iso8601.zonedDateTimeConverter.from("2014-07-01T13:14:15.161Z")       ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 15, 161 * nano, ZoneOffset.UTC))
    def zdtColonsFractionalSixZed     = iso8601.zonedDateTimeConverter.from("2014-07-01T13:14:15.123456Z")    ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 15, 123456000, ZoneOffset.UTC))
    def zdtColonsFractionalNineZed    = iso8601.zonedDateTimeConverter.from("2014-07-01T13:14:15.123456789Z") ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 15, 123456789, ZoneOffset.UTC))
    def zdtColonsPlusZero             = iso8601.zonedDateTimeConverter.from("2014-07-01T13:14:15.161+0000")   ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 15, 161 * nano, ZoneOffset.UTC))
    def zdtColonsMinusZero            = iso8601.zonedDateTimeConverter.from("2014-07-01T13:14:15.161-0000")   ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 15, 161 * nano, ZoneOffset.UTC))
    def zdtColonsPosColon             = iso8601.zonedDateTimeConverter.from("2014-07-01T13:14:15.161+04:56")  ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 15, 161 * nano, ZoneOffset.ofHoursMinutes(4, 56)))
    def zdtColonsNegColon             = iso8601.zonedDateTimeConverter.from("2014-07-01T13:14:15.161-04:56")  ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 15, 161 * nano, ZoneOffset.ofHoursMinutes(-4, -56)))
    def zdtColonsSpaceColonPos        = iso8601.zonedDateTimeConverter.from("2014-07-01T13:14:15.161 +04:56") ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 15, 161 * nano, ZoneOffset.ofHoursMinutes(4, 56)))
    def zdtColonsSpaceColonNeg        = iso8601.zonedDateTimeConverter.from("2014-07-01T13:14:15.161 -04:56") ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 15, 161 * nano, ZoneOffset.ofHoursMinutes(-4, -56)))
    def zdtColonsPos                  = iso8601.zonedDateTimeConverter.from("2014-07-01T13:14:15.161+0456")   ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 15, 161 * nano, ZoneOffset.ofHoursMinutes(4, 56)))
    def zdtColonsNeg                  = iso8601.zonedDateTimeConverter.from("2014-07-01T13:14:15.161-0456")   ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 15, 161 * nano, ZoneOffset.ofHoursMinutes(-4, -56)))
    def zdtColonsSpacePos             = iso8601.zonedDateTimeConverter.from("2014-07-01T13:14:15.161 +0456")  ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 15, 161 * nano, ZoneOffset.ofHoursMinutes(4, 56)))
    def zdtColonsSpaceNeg             = iso8601.zonedDateTimeConverter.from("2014-07-01T13:14:15.161 -0456")  ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 15, 161 * nano, ZoneOffset.ofHoursMinutes(-4, -56)))
    def zdtColonsNoMilliZed           = iso8601.zonedDateTimeConverter.from("2014-07-01T13:14:15Z")           ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 15, 0, ZoneOffset.UTC))
    def zdtColonsNoMilliPos           = iso8601.zonedDateTimeConverter.from("2014-07-01T13:14:15+0456")       ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 15, 0, ZoneOffset.ofHoursMinutes(4, 56)))
    def zdtColonsNoMilliNeg           = iso8601.zonedDateTimeConverter.from("2014-07-01T13:14:15-0456")       ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 15, 0, ZoneOffset.ofHoursMinutes(-4, -56)))
    def zdtColonsNoMilliSpacePos      = iso8601.zonedDateTimeConverter.from("2014-07-01T13:14:15+0456")       ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 15, 0, ZoneOffset.ofHoursMinutes(4, 56)))
    def zdtColonsNoMilliSpaceNeg      = iso8601.zonedDateTimeConverter.from("2014-07-01T13:14:15-0456")       ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 15, 0, ZoneOffset.ofHoursMinutes(-4, -56)))
    def zdtColonsNoMilliColonPos      = iso8601.zonedDateTimeConverter.from("2014-07-01T13:14:15+04:56")      ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 15, 0, ZoneOffset.ofHoursMinutes(4, 56)))
    def zdtColonsNoMilliColonNeg      = iso8601.zonedDateTimeConverter.from("2014-07-01T13:14:15-04:56")      ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 15, 0, ZoneOffset.ofHoursMinutes(-4, -56)))
    def zdtColonsNoMilliSpaceColonPos = iso8601.zonedDateTimeConverter.from("2014-07-01T13:14:15 +04:56")     ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 15, 0, ZoneOffset.ofHoursMinutes(4, 56)))
    def zdtColonsNoMilliSpaceColonNeg = iso8601.zonedDateTimeConverter.from("2014-07-01T13:14:15 -04:56")     ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 15, 0, ZoneOffset.ofHoursMinutes(-4, -56)))
    def zdtColonsNoSecPos             = iso8601.zonedDateTimeConverter.from("2014-07-01T13:14+0456")          ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 0, 0, ZoneOffset.ofHoursMinutes(4, 56)))
    def zdtColonsNoSecNeg             = iso8601.zonedDateTimeConverter.from("2014-07-01T13:14-0456")          ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 0, 0, ZoneOffset.ofHoursMinutes(-4, -56)))
    def zdtColonsNoSecSpacePos        = iso8601.zonedDateTimeConverter.from("2014-07-01T13:14 +0456")         ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 0, 0, ZoneOffset.ofHoursMinutes(4, 56)))
    def zdtColonsNoSecSpaceNeg        = iso8601.zonedDateTimeConverter.from("2014-07-01T13:14 -0456")         ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 0, 0, ZoneOffset.ofHoursMinutes(-4, -56)))
    def zdtColonsNoSecColonPos        = iso8601.zonedDateTimeConverter.from("2014-07-01T13:14+04:56")         ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 0, 0, ZoneOffset.ofHoursMinutes(4, 56)))
    def zdtColonsNoSecColonNeg        = iso8601.zonedDateTimeConverter.from("2014-07-01T13:14-04:56")         ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 0, 0, ZoneOffset.ofHoursMinutes(-4, -56)))
    def zdtColonsNoSecSpaceColonPos   = iso8601.zonedDateTimeConverter.from("2014-07-01T13:14 +04:56")        ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 0, 0, ZoneOffset.ofHoursMinutes(4, 56)))
    def zdtColonsNoSecSpaceColonNeg   = iso8601.zonedDateTimeConverter.from("2014-07-01T13:14 -04:56")        ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 0, 0, ZoneOffset.ofHoursMinutes(-4, -56)))
    def zdtNoColonsZed                = iso8601.zonedDateTimeConverter.from("20140701T131415.161Z")           ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 15, 161 * nano, ZoneOffset.UTC))
    def zdtNoColonsPlusZero           = iso8601.zonedDateTimeConverter.from("20140701T131415.161+0000")       ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 15, 161 * nano, ZoneOffset.UTC))
    def zdtNoColonsMinusZero          = iso8601.zonedDateTimeConverter.from("20140701T131415.161-0000")       ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 15, 161 * nano, ZoneOffset.UTC))
    def zdtNoColonsPosColon           = iso8601.zonedDateTimeConverter.from("20140701T131415.161+04:56")      ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 15, 161 * nano, ZoneOffset.ofHoursMinutes(4, 56)))
    def zdtNoColonsNegColon           = iso8601.zonedDateTimeConverter.from("20140701T131415.161-04:56")      ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 15, 161 * nano, ZoneOffset.ofHoursMinutes(-4, -56)))
    def zdtNoColonsNoMillisZed        = iso8601.zonedDateTimeConverter.from("20140701T131415Z")               ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 15, 0, ZoneOffset.UTC))
    def zdtNoColonsNoMillisColonPos   = iso8601.zonedDateTimeConverter.from("20140701T131415+04:56")          ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 15, 0, ZoneOffset.ofHoursMinutes(4, 56)))
    def zdtNoColonsNoMillisColonNeg   = iso8601.zonedDateTimeConverter.from("20140701T131415-04:56")          ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 15, 0, ZoneOffset.ofHoursMinutes(-4, -56)))

    def localDate      = iso8601.localDateConverter.from("2014-07-01") ==== Okay(LocalDate.of(2014, 7, 1))
    def basicLocalDate = iso8601.localDateConverter.from("20140701")   ==== Okay(LocalDate.of(2014, 7, 1))

    def localDateTime               = iso8601.localDateTimeConverter.from("2014-07-01T13:14:15.161") ==== Okay(LocalDateTime.of(2014, 7, 1, 13, 14, 15, 161 * nano))
    def localDateTimeNoMillis       = iso8601.localDateTimeConverter.from("2014-07-01T13:14:15")     ==== Okay(LocalDateTime.of(2014, 7, 1, 13, 14, 15, 0))
    def localDateTimeNoSeconds      = iso8601.localDateTimeConverter.from("2014-07-01T13:14")        ==== Okay(LocalDateTime.of(2014, 7, 1, 13, 14, 0, 0))
    def basicLocalDateTime          = iso8601.localDateTimeConverter.from("20140701T131415.161")     ==== Okay(LocalDateTime.of(2014, 7, 1, 13, 14, 15, 161 * nano))
    def basicLocalDateTimeNoMillis  = iso8601.localDateTimeConverter.from("20140701T131415")         ==== Okay(LocalDateTime.of(2014, 7, 1, 13, 14, 15, 0))
    def basicLocalDateTimeNoSeconds = iso8601.localDateTimeConverter.from("20140701T1314")           ==== Okay(LocalDateTime.of(2014, 7, 1, 13, 14, 0, 0))

    def localTime               = iso8601.localTimeConverter.from("12:13:14.161") ==== Okay(LocalTime.of(12, 13, 14, 161 * nano))
    def localTimeNoMillis       = iso8601.localTimeConverter.from("12:13:14")     ==== Okay(LocalTime.of(12, 13, 14, 0))
    def localTimeNoSeconds      = iso8601.localTimeConverter.from("12:13")        ==== Okay(LocalTime.of(12, 13, 0, 0))
    def basicLocalTime          = iso8601.localTimeConverter.from("121314.161")   ==== Okay(LocalTime.of(12, 13, 14, 161 * nano))
    def basicLocalTimeNoMillis  = iso8601.localTimeConverter.from("121314")       ==== Okay(LocalTime.of(12, 13, 14, 0))
    def basicLocalTimeNoSeconds = iso8601.localTimeConverter.from("1213")         ==== Okay(LocalTime.of(12, 13, 0, 0))

    // Formatting tests
    def formatZonedDateTimeUTC     = iso8601.zonedDateTimeConverter.to(ZonedDateTime.of(2014, 7, 1, 13, 14, 15, 0, ZoneOffset.UTC)) ==== Okay("2014-07-01T13:14:15.000Z")
    def formatZonedDateTimePos     = iso8601.zonedDateTimeConverter.to(ZonedDateTime.of(2014, 7, 1, 13, 14, 15, 0, ZoneOffset.ofHoursMinutes(4, 56))) ==== Okay("2014-07-01T13:14:15.000+04:56")
    def formatZonedDateTimeNeg     = iso8601.zonedDateTimeConverter.to(ZonedDateTime.of(2014, 7, 1, 13, 14, 15, 0, ZoneOffset.ofHoursMinutes(-4, -56))) ==== Okay("2014-07-01T13:14:15.000-04:56")
    // Gotta make sure it only puts the UTC offset and doesn't append "[America/New_York]" to the end
    def formatZonedDateTimeNewYork = iso8601.zonedDateTimeConverter.to(ZonedDateTime.of(2014, 7, 1, 13, 14, 15, 0, ZoneId.of("America/New_York"))) ==== Okay("2014-07-01T13:14:15.000-04:00")
    def formatZonedDateTimeMoscow  = iso8601.zonedDateTimeConverter.to(ZonedDateTime.of(2014, 7, 1, 13, 14, 15, 0, ZoneId.of("Europe/Moscow"))) ==== Okay("2014-07-01T13:14:15.000+04:00")
    def formatLocalDate            = iso8601.localDateConverter.to(LocalDate.of(2014, 7, 1)) ==== Okay("2014-07-01")
    def formatLocalDateTime        = iso8601.localDateTimeConverter.to(LocalDateTime.of(2014, 7, 1, 13, 14, 15, 0)) ==== Okay("2014-07-01T13:14:15.000")
    def formatLocalTime            = iso8601.localTimeConverter.to(LocalTime.of(12, 13, 14, 0)) ==== Okay("12:13:14.000")
}

class classicTest extends SpecificationWithJUnit with ScalaCheck {
    def is = s2"""
        classic converters should round trip
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

    val roundTripDateTime      = classic.zonedDateTimeConverter.compose(classic.zonedDateTimeConverter.flip)
    val roundTripLocalDate     = classic.localDateConverter.compose(classic.localDateConverter.flip)
    val roundTripLocalDateTime = classic.localDateTimeConverter.compose(classic.localDateTimeConverter.flip)
    val roundTripLocalTime     = classic.localTimeConverter.compose(classic.localTimeConverter.flip)

    // Round-trip tests
    def rtZonedDateTime = prop { (zdt: ZonedDateTime) => roundTripDateTime.to(zdt).map(_.toInstant.toEpochMilli) ==== Okay(zdt.toInstant.plusNanos(-zdt.getNano).toEpochMilli) }
    def rtLocalDate = prop { (ld: LocalDate)      => roundTripLocalDate.to(ld) ==== Okay(ld) }
    def rtLocalDateTime = prop { (ldt: LocalDateTime) => roundTripLocalDateTime.to(ldt) ==== Okay(ldt.plusNanos(-ldt.getNano)) }
    def rtLocalTime = prop { (lt: LocalTime)      => roundTripLocalTime.to(lt) ==== Okay(lt) }

    val nano: Int = 1000000

    // Parsing tests
    def zdtSpcColonsZed                  = classic.zonedDateTimeConverter.from("2014-07-01 13:14:15.161Z")       ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 15, 161 * nano, ZoneOffset.UTC))
    def zdtSpcColonsFractionalSixZed     = iso8601.zonedDateTimeConverter.from("2014-07-01 13:14:15.123456Z")    ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 15, 123456000, ZoneOffset.UTC))
    def zdtSpcColonsFractionalNineZed    = iso8601.zonedDateTimeConverter.from("2014-07-01 13:14:15.123456789Z") ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 15, 123456789, ZoneOffset.UTC))
    def zdtSpcColonsPlusZero             = classic.zonedDateTimeConverter.from("2014-07-01 13:14:15.161+0000")   ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 15, 161 * nano, ZoneOffset.UTC))
    def zdtSpcColonsMinusZero            = classic.zonedDateTimeConverter.from("2014-07-01 13:14:15.161-0000")   ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 15, 161 * nano, ZoneOffset.UTC))
    def zdtSpcColonsPosColon             = classic.zonedDateTimeConverter.from("2014-07-01 13:14:15.161+04:56")  ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 15, 161 * nano, ZoneOffset.ofHoursMinutes(4, 56)))
    def zdtSpcColonsNegColon             = classic.zonedDateTimeConverter.from("2014-07-01 13:14:15.161-04:56")  ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 15, 161 * nano, ZoneOffset.ofHoursMinutes(-4, -56)))
    def zdtSpcColonsSpaceColonPos        = classic.zonedDateTimeConverter.from("2014-07-01 13:14:15.161 +04:56") ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 15, 161 * nano, ZoneOffset.ofHoursMinutes(4, 56)))
    def zdtSpcColonsSpaceColonNeg        = classic.zonedDateTimeConverter.from("2014-07-01 13:14:15.161 -04:56") ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 15, 161 * nano, ZoneOffset.ofHoursMinutes(-4, -56)))
    def zdtSpcColonsPos                  = classic.zonedDateTimeConverter.from("2014-07-01 13:14:15.161+0456")   ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 15, 161 * nano, ZoneOffset.ofHoursMinutes(4, 56)))
    def zdtSpcColonsNeg                  = classic.zonedDateTimeConverter.from("2014-07-01 13:14:15.161-0456")   ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 15, 161 * nano, ZoneOffset.ofHoursMinutes(-4, -56)))
    def zdtSpcColonsSpacePos             = classic.zonedDateTimeConverter.from("2014-07-01 13:14:15.161 +0456")  ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 15, 161 * nano, ZoneOffset.ofHoursMinutes(4, 56)))
    def zdtSpcColonsSpaceNeg             = classic.zonedDateTimeConverter.from("2014-07-01 13:14:15.161 -0456")  ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 15, 161 * nano, ZoneOffset.ofHoursMinutes(-4, -56)))
    def zdtSpcColonsNoMilliZed           = classic.zonedDateTimeConverter.from("2014-07-01 13:14:15Z")           ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 15, 0, ZoneOffset.UTC))
    def zdtSpcColonsNoMilliPos           = classic.zonedDateTimeConverter.from("2014-07-01 13:14:15+0456")       ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 15, 0, ZoneOffset.ofHoursMinutes(4, 56)))
    def zdtSpcColonsNoMilliNeg           = classic.zonedDateTimeConverter.from("2014-07-01 13:14:15-0456")       ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 15, 0, ZoneOffset.ofHoursMinutes(-4, -56)))
    def zdtSpcColonsNoMilliSpacePos      = classic.zonedDateTimeConverter.from("2014-07-01 13:14:15+0456")       ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 15, 0, ZoneOffset.ofHoursMinutes(4, 56)))
    def zdtSpcColonsNoMilliSpaceNeg      = classic.zonedDateTimeConverter.from("2014-07-01 13:14:15-0456")       ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 15, 0, ZoneOffset.ofHoursMinutes(-4, -56)))
    def zdtSpcColonsNoMilliColonPos      = classic.zonedDateTimeConverter.from("2014-07-01 13:14:15+04:56")      ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 15, 0, ZoneOffset.ofHoursMinutes(4, 56)))
    def zdtSpcColonsNoMilliColonNeg      = classic.zonedDateTimeConverter.from("2014-07-01 13:14:15-04:56")      ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 15, 0, ZoneOffset.ofHoursMinutes(-4, -56)))
    def zdtSpcColonsNoMilliSpaceColonPos = classic.zonedDateTimeConverter.from("2014-07-01 13:14:15 +04:56")     ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 15, 0, ZoneOffset.ofHoursMinutes(4, 56)))
    def zdtSpcColonsNoMilliSpaceColonNeg = classic.zonedDateTimeConverter.from("2014-07-01 13:14:15 -04:56")     ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 15, 0, ZoneOffset.ofHoursMinutes(-4, -56)))
    def zdtSpcColonsNoSecPos             = classic.zonedDateTimeConverter.from("2014-07-01 13:14+0456")          ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 0, 0, ZoneOffset.ofHoursMinutes(4, 56)))
    def zdtSpcColonsNoSecNeg             = classic.zonedDateTimeConverter.from("2014-07-01 13:14-0456")          ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 0, 0, ZoneOffset.ofHoursMinutes(-4, -56)))
    def zdtSpcColonsNoSecSpacePos        = classic.zonedDateTimeConverter.from("2014-07-01 13:14 +0456")         ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 0, 0, ZoneOffset.ofHoursMinutes(4, 56)))
    def zdtSpcColonsNoSecSpaceNeg        = classic.zonedDateTimeConverter.from("2014-07-01 13:14 -0456")         ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 0, 0, ZoneOffset.ofHoursMinutes(-4, -56)))
    def zdtSpcColonsNoSecColonPos        = classic.zonedDateTimeConverter.from("2014-07-01 13:14+04:56")         ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 0, 0, ZoneOffset.ofHoursMinutes(4, 56)))
    def zdtSpcColonsNoSecColonNeg        = classic.zonedDateTimeConverter.from("2014-07-01 13:14-04:56")         ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 0, 0, ZoneOffset.ofHoursMinutes(-4, -56)))
    def zdtSpcColonsNoSecSpaceColonPos   = classic.zonedDateTimeConverter.from("2014-07-01 13:14 +04:56")        ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 0, 0, ZoneOffset.ofHoursMinutes(4, 56)))
    def zdtSpcColonsNoSecSpaceColonNeg   = classic.zonedDateTimeConverter.from("2014-07-01 13:14 -04:56")        ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 0, 0, ZoneOffset.ofHoursMinutes(-4, -56)))
    def zdtTColonsZed                  = classic.zonedDateTimeConverter.from("2014-07-01T13:14:15.161Z")       ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 15, 161 * nano, ZoneOffset.UTC))
    def zdtTColonsPlusZero             = classic.zonedDateTimeConverter.from("2014-07-01T13:14:15.161+0000")   ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 15, 161 * nano, ZoneOffset.UTC))
    def zdtTColonsMinusZero            = classic.zonedDateTimeConverter.from("2014-07-01T13:14:15.161-0000")   ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 15, 161 * nano, ZoneOffset.UTC))
    def zdtTColonsPosColon             = classic.zonedDateTimeConverter.from("2014-07-01T13:14:15.161+04:56")  ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 15, 161 * nano, ZoneOffset.ofHoursMinutes(4, 56)))
    def zdtTColonsNegColon             = classic.zonedDateTimeConverter.from("2014-07-01T13:14:15.161-04:56")  ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 15, 161 * nano, ZoneOffset.ofHoursMinutes(-4, -56)))
    def zdtTColonsSpaceColonPos        = classic.zonedDateTimeConverter.from("2014-07-01T13:14:15.161 +04:56") ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 15, 161 * nano, ZoneOffset.ofHoursMinutes(4, 56)))
    def zdtTColonsSpaceColonNeg        = classic.zonedDateTimeConverter.from("2014-07-01T13:14:15.161 -04:56") ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 15, 161 * nano, ZoneOffset.ofHoursMinutes(-4, -56)))
    def zdtTColonsPos                  = classic.zonedDateTimeConverter.from("2014-07-01T13:14:15.161+0456")   ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 15, 161 * nano, ZoneOffset.ofHoursMinutes(4, 56)))
    def zdtTColonsNeg                  = classic.zonedDateTimeConverter.from("2014-07-01T13:14:15.161-0456")   ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 15, 161 * nano, ZoneOffset.ofHoursMinutes(-4, -56)))
    def zdtTColonsSpacePos             = classic.zonedDateTimeConverter.from("2014-07-01T13:14:15.161 +0456")  ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 15, 161 * nano, ZoneOffset.ofHoursMinutes(4, 56)))
    def zdtTColonsSpaceNeg             = classic.zonedDateTimeConverter.from("2014-07-01T13:14:15.161 -0456")  ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 15, 161 * nano, ZoneOffset.ofHoursMinutes(-4, -56)))
    def zdtTColonsNoMilliZed           = classic.zonedDateTimeConverter.from("2014-07-01T13:14:15Z")           ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 15, 0, ZoneOffset.UTC))
    def zdtTColonsNoMilliPos           = classic.zonedDateTimeConverter.from("2014-07-01T13:14:15+0456")       ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 15, 0, ZoneOffset.ofHoursMinutes(4, 56)))
    def zdtTColonsNoMilliNeg           = classic.zonedDateTimeConverter.from("2014-07-01T13:14:15-0456")       ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 15, 0, ZoneOffset.ofHoursMinutes(-4, -56)))
    def zdtTColonsNoMilliSpacePos      = classic.zonedDateTimeConverter.from("2014-07-01T13:14:15+0456")       ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 15, 0, ZoneOffset.ofHoursMinutes(4, 56)))
    def zdtTColonsNoMilliSpaceNeg      = classic.zonedDateTimeConverter.from("2014-07-01T13:14:15-0456")       ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 15, 0, ZoneOffset.ofHoursMinutes(-4, -56)))
    def zdtTColonsNoMilliColonPos      = classic.zonedDateTimeConverter.from("2014-07-01T13:14:15+04:56")      ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 15, 0, ZoneOffset.ofHoursMinutes(4, 56)))
    def zdtTColonsNoMilliColonNeg      = classic.zonedDateTimeConverter.from("2014-07-01T13:14:15-04:56")      ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 15, 0, ZoneOffset.ofHoursMinutes(-4, -56)))
    def zdtTColonsNoMilliSpaceColonPos = classic.zonedDateTimeConverter.from("2014-07-01T13:14:15 +04:56")     ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 15, 0, ZoneOffset.ofHoursMinutes(4, 56)))
    def zdtTColonsNoMilliSpaceColonNeg = classic.zonedDateTimeConverter.from("2014-07-01T13:14:15 -04:56")     ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 15, 0, ZoneOffset.ofHoursMinutes(-4, -56)))
    def zdtTColonsNoSecPos             = classic.zonedDateTimeConverter.from("2014-07-01T13:14+0456")          ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 0, 0, ZoneOffset.ofHoursMinutes(4, 56)))
    def zdtTColonsNoSecNeg             = classic.zonedDateTimeConverter.from("2014-07-01T13:14-0456")          ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 0, 0, ZoneOffset.ofHoursMinutes(-4, -56)))
    def zdtTColonsNoSecSpacePos        = classic.zonedDateTimeConverter.from("2014-07-01T13:14 +0456")         ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 0, 0, ZoneOffset.ofHoursMinutes(4, 56)))
    def zdtTColonsNoSecSpaceNeg        = classic.zonedDateTimeConverter.from("2014-07-01T13:14 -0456")         ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 0, 0, ZoneOffset.ofHoursMinutes(-4, -56)))
    def zdtTColonsNoSecColonPos        = classic.zonedDateTimeConverter.from("2014-07-01T13:14+04:56")         ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 0, 0, ZoneOffset.ofHoursMinutes(4, 56)))
    def zdtTColonsNoSecColonNeg        = classic.zonedDateTimeConverter.from("2014-07-01T13:14-04:56")         ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 0, 0, ZoneOffset.ofHoursMinutes(-4, -56)))
    def zdtTColonsNoSecSpaceColonPos   = classic.zonedDateTimeConverter.from("2014-07-01T13:14 +04:56")        ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 0, 0, ZoneOffset.ofHoursMinutes(4, 56)))
    def zdtTColonsNoSecSpaceColonNeg   = classic.zonedDateTimeConverter.from("2014-07-01T13:14 -04:56")        ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 0, 0, ZoneOffset.ofHoursMinutes(-4, -56)))
    def zdtSpcNoColonsZed                = classic.zonedDateTimeConverter.from("20140701 131415.161Z")           ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 15, 161 * nano, ZoneOffset.UTC))
    def zdtSpcNoColonsPlusZero           = classic.zonedDateTimeConverter.from("20140701 131415.161+0000")       ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 15, 161 * nano, ZoneOffset.UTC))
    def zdtSpcNoColonsMinusZero          = classic.zonedDateTimeConverter.from("20140701 131415.161-0000")       ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 15, 161 * nano, ZoneOffset.UTC))
    def zdtSpcNoColonsPosColon           = classic.zonedDateTimeConverter.from("20140701 131415.161+04:56")      ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 15, 161 * nano, ZoneOffset.ofHoursMinutes(4, 56)))
    def zdtSpcNoColonsNegColon           = classic.zonedDateTimeConverter.from("20140701 131415.161-04:56")      ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 15, 161 * nano, ZoneOffset.ofHoursMinutes(-4, -56)))
    def zdtSpcNoColonsNoMillisZed        = classic.zonedDateTimeConverter.from("20140701 131415Z")               ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 15, 0, ZoneOffset.UTC))
    def zdtSpcNoColonsNoMillisColonPos   = classic.zonedDateTimeConverter.from("20140701 131415+04:56")          ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 15, 0, ZoneOffset.ofHoursMinutes(4, 56)))
    def zdtSpcNoColonsNoMillisColonNeg   = classic.zonedDateTimeConverter.from("20140701 131415-04:56")          ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 15, 0, ZoneOffset.ofHoursMinutes(-4, -56)))
    def zdtTNoColonsZed                = classic.zonedDateTimeConverter.from("20140701T131415.161Z")           ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 15, 161 * nano, ZoneOffset.UTC))
    def zdtTNoColonsPlusZero           = classic.zonedDateTimeConverter.from("20140701T131415.161+0000")       ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 15, 161 * nano, ZoneOffset.UTC))
    def zdtTNoColonsMinusZero          = classic.zonedDateTimeConverter.from("20140701T131415.161-0000")       ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 15, 161 * nano, ZoneOffset.UTC))
    def zdtTNoColonsPosColon           = classic.zonedDateTimeConverter.from("20140701T131415.161+04:56")      ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 15, 161 * nano, ZoneOffset.ofHoursMinutes(4, 56)))
    def zdtTNoColonsNegColon           = classic.zonedDateTimeConverter.from("20140701T131415.161-04:56")      ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 15, 161 * nano, ZoneOffset.ofHoursMinutes(-4, -56)))
    def zdtTNoColonsNoMillisZed        = classic.zonedDateTimeConverter.from("20140701T131415Z")               ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 15, 0, ZoneOffset.UTC))
    def zdtTNoColonsNoMillisColonPos   = classic.zonedDateTimeConverter.from("20140701T131415+04:56")          ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 15, 0, ZoneOffset.ofHoursMinutes(4, 56)))
    def zdtTNoColonsNoMillisColonNeg   = classic.zonedDateTimeConverter.from("20140701T131415-04:56")          ==== Okay(ZonedDateTime.of(2014, 7, 1, 13, 14, 15, 0, ZoneOffset.ofHoursMinutes(-4, -56)))

    def localDate = classic.localDateConverter.from("2014-07-01") ==== Okay(LocalDate.of(2014, 7, 1))
    def basicLocalDate = classic.localDateConverter.from("20140701") ==== Okay(LocalDate.of(2014, 7, 1))

    def localDateTime = classic.localDateTimeConverter.from("2014-07-01T13:14:15.161") ==== Okay(LocalDateTime.of(2014, 7, 1, 13, 14, 15, 161 * nano))
    def localDateTimeNoMillis = classic.localDateTimeConverter.from("2014-07-01T13:14:15") ==== Okay(LocalDateTime.of(2014, 7, 1, 13, 14, 15, 0))
    def localDateTimeNoSeconds = classic.localDateTimeConverter.from("2014-07-01T13:14") ==== Okay(LocalDateTime.of(2014, 7, 1, 13, 14, 0, 0))
    def basicLocalDateTime = classic.localDateTimeConverter.from("20140701T131415.161") ==== Okay(LocalDateTime.of(2014, 7, 1, 13, 14, 15, 161 * nano))
    def basicLocalDateTimeNoMillis = classic.localDateTimeConverter.from("20140701T131415") ==== Okay(LocalDateTime.of(2014, 7, 1, 13, 14, 15, 0))
    def basicLocalDateTimeNoSeconds = classic.localDateTimeConverter.from("20140701T1314") ==== Okay(LocalDateTime.of(2014, 7, 1, 13, 14, 0, 0))

    def localTime = classic.localTimeConverter.from("12:13:14.161") ==== Okay(LocalTime.of(12, 13, 14, 161 * nano))
    def localTimeNoMillis = classic.localTimeConverter.from("12:13:14") ==== Okay(LocalTime.of(12, 13, 14, 0))
    def localTimeNoSeconds = classic.localTimeConverter.from("12:13") ==== Okay(LocalTime.of(12, 13, 0, 0))
    def basicLocalTime = classic.localTimeConverter.from("121314.161") ==== Okay(LocalTime.of(12, 13, 14, 161 * nano))
    def basicLocalTimeNoMillis = classic.localTimeConverter.from("121314") ==== Okay(LocalTime.of(12, 13, 14, 0))
    def basicLocalTimeNoSeconds = classic.localTimeConverter.from("1213") ==== Okay(LocalTime.of(12, 13, 0, 0))

    // Formatting tests
    def formatZonedDateTimeUTC = classic.zonedDateTimeConverter.to(ZonedDateTime.of(2014, 7, 1, 13, 14, 15, 0, ZoneOffset.UTC)) ==== Okay("2014-07-01 13:14:15 +0000")
    def formatZonedDateTimePos = classic.zonedDateTimeConverter.to(ZonedDateTime.of(2014, 7, 1, 13, 14, 15, 0, ZoneOffset.ofHoursMinutes(4, 56))) ==== Okay("2014-07-01 13:14:15 +0456")
    def formatZonedDateTimeNeg = classic.zonedDateTimeConverter.to(ZonedDateTime.of(2014, 7, 1, 13, 14, 15, 0, ZoneOffset.ofHoursMinutes(-4, -56))) ==== Okay("2014-07-01 13:14:15 -0456")
    // Gotta make sure it only puts the UTC offset and doesn't append "[America/New_York]" to the end
    def formatZonedDateTimeNewYork = classic.zonedDateTimeConverter.to(ZonedDateTime.of(2014, 7, 1, 13, 14, 15, 0, ZoneId.of("America/New_York"))) ==== Okay("2014-07-01 13:14:15 -0400")
    def formatZonedDateTimeMoscow = classic.zonedDateTimeConverter.to(ZonedDateTime.of(2014, 7, 1, 13, 14, 15, 0, ZoneId.of("Europe/Moscow"))) ==== Okay("2014-07-01 13:14:15 +0400")
    def formatLocalDate = classic.localDateConverter.to(LocalDate.of(2014, 7, 1)) ==== Okay("2014-07-01")
    def formatLocalDateTime = classic.localDateTimeConverter.to(LocalDateTime.of(2014, 7, 1, 13, 14, 15, 0)) ==== Okay("2014-07-01 13:14:15")
    def formatLocalTime = classic.localTimeConverter.to(LocalTime.of(12, 13, 14, 0)) ==== Okay("12:13:14")
}

class longTest extends SpecificationWithJUnit with ScalaCheck {
    def is = s2"""
        long converters should round trip
            org.joda.time.DateTime          $e1
            org.joda.time.LocalDate         $e2
            org.joda.time.LocalDateTime     $e3
            org.joda.time.LocalTime         $e4
    """

    val roundTripDateTime      = long.zonedDateTimeConverter.compose(long.zonedDateTimeConverter.flip)
    val roundTripLocalDate     = long.localDateConverter.compose(long.localDateConverter.flip)
    val roundTripLocalDateTime = long.localDateTimeConverter.compose(long.localDateTimeConverter.flip)
    val roundTripLocalTime     = long.localTimeConverter.compose(long.localTimeConverter.flip)

    def e1 = prop { (zdt: ZonedDateTime)  => roundTripDateTime.to(zdt).map(_.toInstant.toEpochMilli) ==== Okay(zdt.toInstant.toEpochMilli) }
    def e2 = prop { (ld: LocalDate)       => roundTripLocalDate.to(ld) ==== Okay(ld) }
    def e3 = prop { (ldt: LocalDateTime)  => roundTripLocalDateTime.to(ldt) ==== Okay(ldt) }
    def e4 = prop { (lt: LocalTime)       => roundTripLocalTime.to(lt) ==== Okay(lt) }
}
