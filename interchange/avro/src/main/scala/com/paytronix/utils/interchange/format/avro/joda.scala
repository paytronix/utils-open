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

package com.paytronix.utils.interchange.format.avro

import org.joda.time.{
    DateTime          => JodaDateTime,
    DateTimeZone      => JodaDateTimeZone,
    LocalDate         => JodaLocalDate,
    LocalDateTime     => JodaLocalDateTime,
    LocalTime         => JodaLocalTime
}

import com.paytronix.utils.interchange.base.datetime

object joda extends joda

trait joda {
    object jodaAsLong {
        implicit lazy val dateTimeAvroCoder      : AvroCoder[JodaDateTime]      = scalar.longAvroCoder.mapWithConverter(datetime.joda.long.dateTimeConverter)
        implicit lazy val localDateAvroCoder     : AvroCoder[JodaLocalDate]     = scalar.longAvroCoder.mapWithConverter(datetime.joda.long.localDateConverter)
        implicit lazy val localDateTimeAvroCoder : AvroCoder[JodaLocalDateTime] = scalar.longAvroCoder.mapWithConverter(datetime.joda.long.localDateTimeConverter)
        implicit lazy val localTimeAvroCoder     : AvroCoder[JodaLocalTime]     = scalar.longAvroCoder.mapWithConverter(datetime.joda.long.localTimeConverter)
    }

    object jodaAsIso8601 {
        implicit lazy val dateTimeAvroCoder      : AvroCoder[JodaDateTime]      = scalar.stringAvroCoder.mapWithConverter(datetime.joda.iso8601.dateTimeConverter)
        implicit lazy val localDateAvroCoder     : AvroCoder[JodaLocalDate]     = scalar.stringAvroCoder.mapWithConverter(datetime.joda.iso8601.localDateConverter)
        implicit lazy val localDateTimeAvroCoder : AvroCoder[JodaLocalDateTime] = scalar.stringAvroCoder.mapWithConverter(datetime.joda.iso8601.localDateTimeConverter)
        implicit lazy val localTimeAvroCoder     : AvroCoder[JodaLocalTime]     = scalar.stringAvroCoder.mapWithConverter(datetime.joda.iso8601.localTimeConverter)
    }

    object jodaAsClassic {
        implicit lazy val dateTimeAvroCoder      : AvroCoder[JodaDateTime]      = scalar.stringAvroCoder.mapWithConverter(datetime.joda.classic.dateTimeConverter)
        implicit lazy val localDateAvroCoder     : AvroCoder[JodaLocalDate]     = scalar.stringAvroCoder.mapWithConverter(datetime.joda.classic.localDateConverter)
        implicit lazy val localDateTimeAvroCoder : AvroCoder[JodaLocalDateTime] = scalar.stringAvroCoder.mapWithConverter(datetime.joda.classic.localDateTimeConverter)
        implicit lazy val localTimeAvroCoder     : AvroCoder[JodaLocalTime]     = scalar.stringAvroCoder.mapWithConverter(datetime.joda.classic.localTimeConverter)
    }
}
