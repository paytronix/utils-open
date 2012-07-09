//
// Copyright 2010-2012 Paytronix Systems, Inc.
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

package com.paytronix.utils.validation

import java.io.File
import java.net.{MalformedURLException, URL, URI, URISyntaxException}
import java.text.SimpleDateFormat
import java.util.regex.PatternSyntaxException
import scala.collection.generic.{CanBuild, CanBuildFrom}
import scala.collection.mutable.{ArrayBuffer, Builder, ListBuffer}
import scala.util.control.Exception.{allCatch, catching}
import scala.util.matching.Regex

import base.{Validated, ValidationError, ValidationFunction, field, missingValueError, nonMissingValueError}

object boolean {
    val invalidBooleanError = ValidationError("invalid_boolean", "invalid boolean (expected true/false, yes/no, on/off, or a number)")

    /**
     * Convert a string to a boolean, appropriate for parsing HTML form input.
     * Any nonempty string that is not "false", "no" or "off" is treated as true
     */
    def boolean(error: ValidationError = null): ValidationFunction[String, Boolean] =
        _.toLowerCase match {
            case ""|"false"|"no"|"off" => Right(false)
            case "true"|"yes"|"on" => Right(true)
            case s =>
                catching(classOf[NumberFormatException]).opt {
                    Integer.parseInt(s)
                } map {
                    n => Right(n != 0)
                } getOrElse Left((if (error != null) error else invalidBooleanError) :: Nil)
        }
}

object date {
    private val databaseEpoch: Long = -6847786800000L // 1753-01-01 00:00:00 UTC

    val invalidDateFormatError = ValidationError("invalid_date", "invalid date") // FIXME? explain what is expected?

    private def dateFormat(format: String): SimpleDateFormat = {
        val df = new SimpleDateFormat(format)
        df.setLenient(false) // really? lenient is the default? I hate Java.
        df
    }

    def dateWithFormat(format: String, error: ValidationError = invalidDateFormatError): ValidationFunction[String, java.util.Date] = in =>
        catching(classOf[Exception]).either(dateFormat(format).parse(in)).left.map(_ => error :: Nil)

    def dateWithFormats(formats: Seq[String], error: ValidationError = invalidDateFormatError): ValidationFunction[String, java.util.Date] =
        in => formats.view map { format => catching(classOf[Exception]).either(dateFormat(format).parse(in)) } find { _.isRight } match {
            case Some(Right(date)) => Right(date)
            case _                 => Left(error :: Nil)
        }

    def isoDateTime(error: ValidationError = invalidDateFormatError): ValidationFunction[String, java.util.Date] =
        dateWithFormats("yyyy-MM-dd'T'HH:mm:ss.SSS" :: "yyyy-MM-dd'T'HH:mm:ss" :: Nil)

    def isoDate(error: ValidationError = invalidDateFormatError): ValidationFunction[String, java.util.Date] =
        dateWithFormat("yyyy-MM-dd")

    def sqlDatetime(error: ValidationError = invalidDateFormatError): ValidationFunction[String, java.sql.Timestamp] =
        isoDateTime() and { d => Right(new java.sql.Timestamp(d.getTime)) }

    def sqlDate(error: ValidationError = invalidDateFormatError): ValidationFunction[String, java.sql.Date] =
        isoDate() and { d => Right(new java.sql.Date(d.getTime)) }

    val tooFarInPastError = ValidationError("invalid_date", "date is too far in the past")
    val pastDateError = ValidationError("invalid_date", "date cannot be in the future")
    val futureDateError = ValidationError("invalid_date", "date cannot be in the past")
    val tooFarInFutureError = ValidationError("invalid_date", "date is too far in the future")

    /** Assert that some date is after the SQL Server database epoch (1753-01-01) */
    def afterSqlServerEpoch[A <: java.util.Date](error: ValidationError = tooFarInPastError): ValidationFunction[A, A] =
        in => if (in.getTime >= databaseEpoch) Right(in)
              else Left(error :: Nil)

    /** Assert that some date is in the past */
    def beforeNow[A <: java.util.Date](error: ValidationError = pastDateError): ValidationFunction[A, A] =
        in => if (new java.util.Date().after(in)) Right(in)
              else Left(error :: Nil)

    /** Assert that some date is in the future */
    def afterNow[A <: java.util.Date](error: ValidationError = futureDateError): ValidationFunction[A, A] =
        in => if (new java.util.Date().before(in)) Right(in)
              else Left(error :: Nil)

    /** Assert that some date is in the future */
    def beforeDaysInFuture[A <: java.util.Date](i: Int, error: ValidationError = tooFarInFutureError): ValidationFunction[A, A] =
        in => {
                val cal = java.util.Calendar.getInstance
                println(i)
                cal.add(java.util.Calendar.DAY_OF_MONTH, i)
                println(cal.getTime)
                println(in)
                println(cal.after(in))
                if (cal.getTime.after(in)) Right(in)
                else Left(error :: Nil)
        }

}

object enumeration {
    def invalidEnumerationError(incorrect: String): ValidationError =
        ValidationError("invalid_enumeration", "value \"" + incorrect + "\" is not an allowed option for this field")

    /**
     * Apply a partial function to the input only if it is defined, yielding the result of the application.
     * Fails validation if the partial function isn't defined.
     */
    def definedIn[A, B](pf: PartialFunction[A, B], error: String => ValidationError = invalidEnumerationError): ValidationFunction[A, B] =
        in => if (pf.isDefinedAt(in)) Right(pf(in)) else Left(error(String.valueOf(in)) :: Nil)

    /**
     * Assert that the value is a member of the given enumeration, and yield the enumeration value if successful.
     * NOTE: Scala's inference of the type of singletons is heinously awful at best.
     * If you get wonky errors like type mismatch MyEnum#Value versus MyEnum.Value, then explicitly pass the enumeration type:
     *    valueOf[MyEnum.type](MyEnum)
     */
    def valueOf[A <: Enumeration](enum: A, error: String => ValidationError = invalidEnumerationError): ValidationFunction[String, A#Value] =
        definedIn(enumeration(enum), error)

    /**
     * Assert that the value is a member of the given enumeration, and yield the enumeration value if successful.
     */
    def valueOfJavaEnum[A <: Enum[A]](enumClass: Class[A], error: String => ValidationError = invalidEnumerationError): ValidationFunction[String, A] =
        definedIn(enumeration(enumClass), error)

    /**
     * Helper implict which converts any Enumeration into a PartialFunction from String.
     */
    def enumeration[A <: Enumeration](enum: A): PartialFunction[String, A#Value] =
        Map(enum.values.map(v => v.toString -> v.asInstanceOf[A#Value]).toSeq: _*)

    /**
     * Helper implict which converts any Java enum into a PartialFunction from String.
     */
    def enumeration[A <: Enum[A]](enumClass: Class[A]): PartialFunction[String, A] = {
        // bah.
        val meth = enumClass.getMethod("values")
        Map(meth.invoke(null).asInstanceOf[Array[A]].map(v => v.name -> v): _*)
    }
}

object numeric {
    val nonPositiveError                  = ValidationError("invalid_negative_or_zero", "positive value required")
    val nonNegativeError                  = ValidationError("invalid_positive_or_zero", "negative value required")
    val negativeError                     = ValidationError("invalid_negative", "non-negative value required")
    val positiveError                     = ValidationError("invalid_positive", "non-positive value required")
    def tooSmallErrorExclusive(s: String) = ValidationError("underflow", "must greater than %s", s)
    def tooSmallError(s: String)          = ValidationError("underflow", "must greater than or equal to %s", s)
    def tooLargeErrorExclusive(s: String) = ValidationError("overflow", "must less than %s", s)
    def tooLargeError(s: String)          = ValidationError("overflow", "must less than or equal to %s", s)

    /** Assert that some number is greater than some value (> x) */
    def greaterThan[A](minimum: A, error: ValidationError = null)(implicit num: Numeric[A]): ValidationFunction[A, A] = in =>
        if (num.gt(in, minimum))   Right(in)
        else if (error == null)    Left(tooSmallErrorExclusive(minimum.toString) :: Nil)
        else                       Left(error :: Nil)

    /** Assert that some number is not less than some value (>= x) */
    def noLessThan[A](minimum: A, error: ValidationError = null)(implicit num: Numeric[A]): ValidationFunction[A, A] = in =>
        if (num.gteq(in, minimum)) Right(in)
        else if (error == null)    Left(tooSmallError(minimum.toString) :: Nil)
        else                       Left(error :: Nil)

    /** Assert that some number is greater than some value (< x) */
    def lessThan[A](maximum: A, error: ValidationError = null)(implicit num: Numeric[A]): ValidationFunction[A, A] = in =>
        if (num.lt(in, maximum))   Right(in)
        else if (error == null)    Left(tooSmallErrorExclusive(maximum.toString) :: Nil)
        else                       Left(error :: Nil)

    /** Assert that some number is not less than some value (<= x) */
    def noGreaterThan[A](maximum: A, error: ValidationError = null)(implicit num: Numeric[A]): ValidationFunction[A, A] = in =>
        if (num.lteq(in, maximum)) Right(in)
        else if (error == null)    Left(tooSmallError(maximum.toString) :: Nil)
        else                       Left(error :: Nil)

    /** Assert that some number is positive (> 0) */
    def positive[A](error: ValidationError = nonPositiveError)(implicit num: Numeric[A]): ValidationFunction[A, A] =
        in => if (num.signum(in) > 0) Right(in)
              else Left(error :: Nil)

    /** Assert that some number is negative (< 0) */
    def negative[A](error: ValidationError = nonNegativeError)(implicit num: Numeric[A]): ValidationFunction[A, A] =
        in => if (num.signum(in) < 0) Right(in)
              else Left(error :: Nil)

    /** Assert that some number is non-negative (>= 0) */
    def nonNegative[A](error: ValidationError = negativeError)(implicit num: Numeric[A]): ValidationFunction[A, A] =
        in => if (num.signum(in) >= 0) Right(in)
              else Left(error :: Nil)

    /** Assert that some number is non-positive (<= 0) */
    def nonPositive[A](error: ValidationError = nonNegativeError)(implicit num: Numeric[A]): ValidationFunction[A, A] =
        in => if (num.signum(in) <= 0) Right(in)
              else Left(error :: Nil)

    import string.{nonBlank, numericWithSign, numericWithSignAndDecimal, nonNumericError}

    /** Assert that some string is a well formatted number, converting it */
    def number[A] (
        parse: String => A, // expected to throw NumberFormatException on overflow and underflow
        minValue: A,
        maxValue: A,
        missingValue: ValidationError,
        malformatted: ValidationError,
        underflow: ValidationError,
        overflow: ValidationError
    ): ValidationFunction[String, A] = nonBlank() and numericWithSign(malformatted) and (
        in => catching(classOf[NumberFormatException]).either(parse(in)).left.map(_ => {
            if (in.startsWith("-")) {
                if (underflow != null) underflow else tooSmallError(minValue.toString)
            } else {
                if (overflow  != null) overflow  else tooLargeError(maxValue.toString)
            }
        } :: Nil)
    )

    /** Assert that some string is a well formatted decimal number, converting it */
    def numberWithDecimal[A] (
        parse: String => A, // expected to throw NumberFormatException on overflow and underflow
        minValue: A,
        maxValue: A,
        missingValue: ValidationError,
        malformatted: ValidationError,
        underflow: ValidationError,
        overflow: ValidationError
    ): ValidationFunction[String, A] = nonBlank() and numericWithSignAndDecimal(malformatted) and (
        in => catching(classOf[NumberFormatException]).either(parse(in)).left.map(_ => {
            if (in.startsWith("-")) {
                if (underflow != null) underflow else tooSmallError(minValue.toString)
            } else {
                if (overflow  != null) overflow  else tooLargeError(maxValue.toString)
            }
        } :: Nil)
    )

    import java.lang.{Byte => JLByte, Short => JLShort, Integer => JLInteger, Long => JLLong, Float => JLFloat, Double => JLDouble}

    /** Assert that a string is a well formatted byte and convert it */
    def byte (
        missingValue: ValidationError = missingValueError,
        malformatted: ValidationError = nonNumericError,
        underflow: ValidationError = null,
        overflow: ValidationError = null
    ): ValidationFunction[String, Byte] =
        number(s => JLByte.parseByte(s): JLByte, JLByte.MIN_VALUE, JLByte.MAX_VALUE, missingValue, malformatted, underflow, overflow)

    /** Assert that a string is a well formatted short and convert it */
    def short (
        missingValue: ValidationError = missingValueError,
        malformatted: ValidationError = nonNumericError,
        underflow: ValidationError = null,
        overflow: ValidationError = null
    ): ValidationFunction[String, Short] =
        number(s => JLShort.parseShort(s): JLShort, JLShort.MIN_VALUE, JLShort.MAX_VALUE, missingValue, malformatted, underflow, overflow)

    /** Assert that a string is a well formatted int and convert it */
    def int (
        missingValue: ValidationError = missingValueError,
        malformatted: ValidationError = nonNumericError,
        underflow: ValidationError = null,
        overflow: ValidationError = null
    ): ValidationFunction[String, Int] =
        number(s => JLInteger.parseInt(s): JLInteger, JLInteger.MIN_VALUE, JLInteger.MAX_VALUE, missingValue, malformatted, underflow, overflow)

    /** Assert that a string is a well formatted long and convert it */
    def long (
        missingValue: ValidationError = missingValueError,
        malformatted: ValidationError = nonNumericError,
        underflow: ValidationError = null,
        overflow: ValidationError = null
    ): ValidationFunction[String, Long] =
        number(s => JLLong.parseLong(s): JLLong, JLLong.MIN_VALUE, JLLong.MAX_VALUE, missingValue, malformatted, underflow, overflow)

    /** Assert that a string is a well formatted float and convert it */
    def float (
        missingValue: ValidationError = missingValueError,
        malformatted: ValidationError = nonNumericError,
        underflow: ValidationError = null,
        overflow: ValidationError = null
    ): ValidationFunction[String, Float] =
        numberWithDecimal(s => JLFloat.parseFloat(s): JLFloat, JLFloat.MIN_VALUE, JLFloat.MAX_VALUE, missingValue, malformatted, underflow, overflow)

    /** Assert that a string is a well formatted double and convert it */
    def double (
        missingValue: ValidationError = missingValueError,
        malformatted: ValidationError = nonNumericError,
        underflow: ValidationError = null,
        overflow: ValidationError = null
    ): ValidationFunction[String, Double] =
        numberWithDecimal(s => JLDouble.parseDouble(s): JLDouble, JLDouble.MIN_VALUE, JLDouble.MAX_VALUE, missingValue, malformatted, underflow, overflow)

    /** Assert that a string is a well formatted integer value */
    def bigInt (
        missingValue: ValidationError = missingValueError,
        malformatted: ValidationError = nonNumericError
    ): ValidationFunction[String, BigInt] = nonBlank() and numericWithSignAndDecimal(malformatted) and (
        in => catching(classOf[NumberFormatException]).either(BigInt(in)).left.map(_ => malformatted :: Nil)
    )

    /** Assert that a string is a well formatted decimal value */
    def bigDecimal (
        missingValue: ValidationError = missingValueError,
        malformatted: ValidationError = nonNumericError
    ): ValidationFunction[String, BigDecimal] = nonBlank() and numericWithSignAndDecimal(malformatted) and (
        in => catching(classOf[NumberFormatException]).either(BigDecimal(in)).left.map(_ => malformatted :: Nil)
    )
}

object string {
    val ValidDomainChars = "a-zA-Z0-9"
    // This is somewhat complicated because we're allowed to have dashes, but not on either end
    val ValidDomainLabel = "[" + ValidDomainChars + "]+(?:[-]+[" + ValidDomainChars + "]+)*"
    val ValidDomainPart = ValidDomainLabel + "(?:[.]" + ValidDomainLabel + ")+"
    // We do not allow quoted-string local part values!
    val ValidLocalChars = ValidDomainChars + "!#$%&'*+\\-/=?^_`{|}~"
    val ValidLocalPart  = "[" + ValidLocalChars + "]+(?:[.][" + ValidLocalChars + "]+)*"
    val ValidEmail = (ValidLocalPart + "@" + ValidDomainPart).r

    def tooShortError(i: Int) = ValidationError("too_short", "must have at least %d character(s)", i)
    def tooLongError(i: Int)  = ValidationError("too_long", "must have no more than %d character(s)", i)
    val patternMatchError     = ValidationError("invalid_format", "not formatted correctly")
    val numericError          = ValidationError("invalid_numeric", "must not be entirely numeric")
    val nonNumericError       = ValidationError("invalid_non_numeric", "must be numeric")
    val nonIntegralError      = ValidationError("invalid_non_integral", "must be an integral value")
    val invalidEmailError     = ValidationError("invalid_email", "invalid email address")

    /** Assert that some string is not empty */
    def nonEmpty(error: ValidationError = missingValueError): ValidationFunction[String, String] =
        in => if (!in.isEmpty) Right(in)
              else Left(error :: Nil)

    /** Trim the string and then assert it is nonempty */
    def nonBlank(error: ValidationError = missingValueError): ValidationFunction[String, String] =
        in => in.trim match {
            case "" => Left(error :: Nil)
            case s => Right(s)
        }

    /** Trim the string and make Some(string) out of it if nonblank, None otherwise */
    val optionalString: ValidationFunction[String, Option[String]] =
        in => Right(in.trim match {
            case "" => None
            case s => Some(s)
        })

    /** Assert that some string is at least some size */
    def noShorterThan(i: Int, error: ValidationError = null): ValidationFunction[String, String] =
        in => if (in.size >= i) Right(in)
              else Left((if (error != null) error else tooShortError(i)) :: Nil)

    /** Assert that some string is no longer than some size */
    def noLongerThan(i: Int, error: ValidationError = null): ValidationFunction[String, String] =
        in => if (in.size <= i) Right(in)
              else Left((if (error != null) error else tooLongError(i)) :: Nil)

    /** Assert that a string matches some pattern Regex */
    def matches(pattern: Regex, message: ValidationError = patternMatchError): ValidationFunction[String, String] =
        in => if (pattern.unapplySeq(in).isDefined) Right(in)
              else Left(message :: Nil)

    /** Assert that a string does not match some pattern Regex */
    def doesNotMatch(pattern: Regex, message: ValidationError = patternMatchError): ValidationFunction[String, String] =
        in => if (!pattern.unapplySeq(in).isDefined) Right(in)
              else Left(message :: Nil)

    /** Assert that the string is comprised only of digits */
    def numeric(error: ValidationError = nonNumericError) = matches("\\d+".r, error)

    /** Assert that the string is comprised only of digits with an optional leading sign */
    def numericWithSign(error: ValidationError = nonIntegralError) = matches("[+-]?\\d+".r, error)

    /** Assert that the string is comprised only of digits with an optional leading sign and optional fractional part after a decimal */
    def numericWithSignAndDecimal(error: ValidationError = nonNumericError) = matches("[+-]?\\d+(?:\\.\\d+)?".r, error)

    /** Assert that the string is not comprised only of digits */
    def nonNumeric(error: ValidationError = numericError) = doesNotMatch("\\d+".r, error)

    /** Assert that the string conforms to the valid email address format */
    def validEmailAddress(message: ValidationError = invalidEmailError): ValidationFunction[String, String] = string.matches(ValidEmail, invalidEmailError)

    /** Return a boolean indicating whether the string conforms to the valid email address format */
    def isValidEmailAddress(email: String): Boolean = validEmailAddress()(email).isRight
}

object file {
    import string.nonBlank

    val nonExistentError = ValidationError("non_existent", "does not exist")
    val nonDirectoryError = ValidationError("non_directory", "not a directory")
    val nonFileError = ValidationError("non_file", "not a file")

    /** Assert that a String is nonblank and convert it to a File */
    def path(missingValue: ValidationError = missingValueError): ValidationFunction[String, File] = nonBlank() and (in => Right(new File(in)))

    /** Assert that a File refers to an existent path (file, directory, or other special file system entity) */
    def exists(error: ValidationError = nonExistentError): ValidationFunction[File, File] =
        in => if (in.exists) Right(in)
              else Left(error :: Nil)

    /** Assert that a File refers to a directory */
    def isDirectory(error: ValidationError = nonDirectoryError): ValidationFunction[File, File] =
        in => if (in.isDirectory) Right(in)
              else Left(error :: Nil)

    /** Assert that a File refers to a file (not a directory or some special file system entity) */
    def isFile(error: ValidationError = nonFileError): ValidationFunction[File, File] =
        in => if (in.isFile) Right(in)
              else Left(error :: Nil)
}

object regex {
    import string.nonBlank

    /** Assert that a String is nonblank and parse it as a regular expression */
    def pattern(missingValue: ValidationError = missingValueError): ValidationFunction[String, Regex] =
        nonBlank() and (
            in => catching(classOf[PatternSyntaxException]).either(in.r).left.map(t => ValidationError("invalid_regex", Option(t.getMessage) getOrElse t.toString) :: Nil)
        )
}

object reflection {
    import string.nonBlank

    val invalidClassName = ValidationError("invalid_class_name", "invalid class name")
    def lookupError(e: Exception): ValidationError = ValidationError("unknown_error", "error while looking up class: " + e.toString)

    /** Assert that a String is nonblank and refers to a loadable class */
    def className[A](bound: Class[A], classLoader: ClassLoader = null, error: ValidationError = invalidClassName): ValidationFunction[String, Class[_ <: A]] =
        in => nonBlank()(in) match {
            case Left(errors) => Left(errors)
            case Right(s) => {
                try {
                    if (classLoader != null) Right(Class.forName(s, true, classLoader).asSubclass(bound))
                    else                     Right(Class.forName(s).asSubclass(bound))
                } catch {
                    case e: ClassNotFoundException => Left(error :: Nil)
                    case e: Exception              => Left(lookupError(e) :: Nil) // FIXME? do we need to be able to customize this one?
                }
            }
        }
}

object column {
    def requiredColumnError(c: String) = ValidationError("missing_required_column", "Missing column " + c)
    def unknownColumnError(c: String) = ValidationError("invalid_column", "Invalid unknown column " + c)

    /** Type of a list of columns zipped with their indices */
    type Columns = List[(String, Int)]

    /** Specific type of validation function that applies to columns and threads some other value through the function */
    type ColumnValidationFunction[A, B] = ValidationFunction[(A, Columns), (B, Columns)]

    /**
     * Validate a header line by applying a chain of column validation functions which prune down the original list of header columns.
     * Any columns remaining unhandled after the function applies cause a validation error.
     * A user-defined value can be threaded through, and usually is used to carry some object to store column indices
     *
     * Example use:
     * <pre>
     *   case class ColumnIndices(column1: Int = -1, column2: Int = -1, column3: Option[Int] = None) {
     *       def column1_= (i: Int) = copy(column1 = i)
     *       def column2_= (i: Int) = copy(column2 = i)
     *       def column3_= (i: Option[Int]) = copy(column3 = i)
     *   }
     *
     *   (ColumnIndices(), headerTokens) are columns (
     *       required("Column 1", _ column1_= _),
     *       required("Column 2", _ column2_= _),
     *       optional("Column 3", _ column3_= _)
     *   )
     * </pre>
     *
     * Each validator passed in is expected to strip any column headings that were handled by it from the list of columns, so that any unrecognized columns are left over
     * and cause an error. E.g. If (Column 1, Column 2, Column 3, Column 4) are passed in as a value to validate, required("Column 1") will yield (Column 2, Column 3,
     * Column 4), required("Column 2") will yield (Column 3, Column 4), optional("Column 3") will yield (Column 4), and finally columns(...) will fail with
     * unknownColumnError("Column 4")
     */
    def columns[A](seed: A, unknownColumn: String => ValidationError = unknownColumnError)(
        fs: ColumnValidationFunction[A, A]*
    ): ValidationFunction[Seq[String], A] = polyColumns(seed, unknownColumn)(fs.reduceLeft(_ and _))

    /** More general type of column validation where the validation function is A => B not A => A. */
    def polyColumns[A, B](seed: A, unknownColumn: String => ValidationError = unknownColumnError)(
        f: ColumnValidationFunction[A, B]
    ): ValidationFunction[Seq[String], B] = cols => {
        f((seed, cols.toList.zipWithIndex)).right.flatMap {
            case (output, Nil)    => Right(output)
            case (_, unknownCols) => Left(unknownCols.map(t => unknownColumn(t._1)))
        }
    }

    /** Require that a column is present in the given input. Applies the given update with the column index to the input value to produce an output. */
    def required[A, B](c: String, update: (A, Int) => B, error: ValidationError = null): ColumnValidationFunction[A, B] = tuple => {
        val (input, cols) = tuple
        cols partition (_._1 == c) match {
            case (Nil, _) if error != null => Left(error :: Nil)
            case (Nil, _)                  => Left(requiredColumnError(c) :: Nil)
            case ((_, idx)::_, rest)       => Right((update(input, idx), rest))
        }
    }

    /**
     * Permit a column name, but don't require it. Applies the given update to the input to produce an output if the column is present.
     * Strips a column from the input, making it so that an enclosing columns(...) application will not fail with unknown column.
     */
    def optional[A](c: String, update: (A, Int) => A): ColumnValidationFunction[A, A] = tuple => {
        val (input, cols) = tuple
        cols partition (_._1 == c) match {
            case (Nil, _)            => Right(tuple)
            case ((_, idx)::_, rest) => Right((update(input, idx), rest))
        }
    }
}

object uri {
    import string.nonBlank

    /** Assert that a String is nonblank and convert it to a URI */
    def uri(missingValue: ValidationError = missingValueError): ValidationFunction[String, URI] =
        nonBlank(missingValue) and (s => catching(classOf[URISyntaxException])
                                         .either(new URI(s))
                                         .left.map(t => ValidationError(Option(t.getMessage) getOrElse t.toString) :: Nil))
}

object url {
    import string.nonBlank

    /** Assert that a String is nonblank and convert it to a URL */
    def url(missingValue: ValidationError = missingValueError): ValidationFunction[String, URL] =
        nonBlank(missingValue) and (s => catching(classOf[MalformedURLException])
                                         .either(new URL(s))
                                         .left.map(t => ValidationError(Option(t.getMessage) getOrElse t.toString) :: Nil))
}

object option {
    /** Apply some validation to the value inside an Option. */
    def optional[A, B](f: ValidationFunction[A, B]): ValidationFunction[Option[A], Option[B]] = (in: Option[A]) => in match {
        case Some(v) => f(v).right.map(Some.apply)
        case None => Right(None)
    }

    /** Require that an optional value be present. */
    def some[A, B](): ValidationFunction[Option[A], A] = some(missingValueError)(x => Right(x))

    /** Require that an optional value be present. */
    def some[A, B](f: ValidationFunction[A, B]): ValidationFunction[Option[A], B] = some(missingValueError)(f)

    /** Require that an optional value be present and specify a particular error message. */
    def some[A, B](error: ValidationError)(f: ValidationFunction[A, B]): ValidationFunction[Option[A], B] = (in: Option[A]) => in match {
        case Some(v) => f(v)
        case None => Left(error :: Nil)
    }

    /** Require that an optional value not be present. */
    val none: ValidationFunction[Option[Any], Unit] = none(nonMissingValueError)

    /** Require that an optional value not be present. */
    def none(error: ValidationError): ValidationFunction[Option[Any], Unit] = _ match {
        case Some(v) => Left(error :: Nil)
        case None => Right(())
    }
}

object sequence {
    def tooShortError(i: Int) = ValidationError("too_short", "must have at least %d element(s)", i)
    def tooLongError(i: Int)  = ValidationError("too_long", "must have no more than %d element(s)", i)

    /** Assert that some iterable is at least some size */
    def noShorterThan[A <: Iterable[_]](i: Int, error: ValidationError = null): ValidationFunction[A, A] =
        in => if (in.size >= i) Right(in)
              else Left((if (error != null) error else tooShortError(i)) :: Nil)

    /** Assert that some iterable is no longer than some size */
    def noLongerThan[A <: Iterable[_]](i: Int, error: ValidationError = null): ValidationFunction[A, A] =
        in => if (in.size <= i) Right(in)
              else Left((if (error != null) error else tooLongError(i)) :: Nil)

    /** Split some input string by a delimiter */
    def delimitedBy(delimiter: String, strict: Boolean = false, maxSplits: Int = 0): ValidationFunction[String, List[String]] =
        s => {
            val builder = List.newBuilder[String]
            val in = if (strict) s else s.trim
            val limit = in.length
            val delimiterLength = delimiter.length
            var count = 0
            var start = 0

            while (start < limit) {
                in.indexOf(delimiter, start) match {
                    case -1 =>
                        builder += in.substring(start, limit)
                        start = limit

                    case pos if strict || pos > start =>
                        if (maxSplits > 0 && count == maxSplits) {
                            builder += in.substring(start, limit)
                            start = limit
                        } else {
                            builder += in.substring(start, pos)
                            count += 1
                            start = pos + delimiterLength
                        }

                    case pos =>
                        start = pos + delimiterLength
                }
            }

            Right(builder.result())
        }

    /**
     * Apply some validation to every value in the given input collection.
     * This is the composable version that is intended to be used with "and".
     * See applyToEach for the version that's more sane to use when applying directly to values
     */
    def eachIs[A, B, InColl <: Iterable[A], OutColl](f: ValidationFunction[A, B])(implicit cbf: CanBuildFrom[InColl, B, OutColl]): ValidationFunction[InColl, OutColl] =
        in => {
            val results = cbf(in)
            val allErrors = List.newBuilder[ValidationError]
            val iter = in.iterator
            while (iter.hasNext) {
                f(iter.next) match {
                    case Left(errors) => errors.foreach(allErrors += _)
                    case Right(result) => results += result
                }
            }
            allErrors.result() match {
                case Nil => Right(results.result())
                case someErrors => Left(someErrors)
            }
        }
    /**
     * Apply some validation to every value in the given input collection.
     * This is the apply-able version that is not intended to be used in composition using "and".
     * See each for the composable version
     */
    def applyToEach[A, B, InColl <: Iterable[A], OutColl](in: InColl, f: ValidationFunction[A, B])(implicit cbf: CanBuildFrom[InColl, B, OutColl]): Validated[OutColl] =
        eachIs[A, B, InColl, OutColl](f)(cbf)(in)

    // undocumented territory! ask me later!

    import numeric.int
    import shapeless.{::, HList, HNil, LeftFolder, Poly2, Reverse}

    val exhaustedInput = ValidationError("exhausted_input", "exhausted input")
    def unexpectedInput(got: String) = ValidationError("unexpected_input", "expected end of input, but got: " + got)

    final case class ListParsingResult[A, +B](rest: List[A], result: B) {
        def mapResult[C](f: B => C): ListParsingResult[A, C] = copy(result = f(result))
    }
    type ListParsingFunction[A, +B] = ValidationFunction[List[A], ListParsingResult[A, B]]

    implicit def validationToSingleListParser[A, B](f: ValidationFunction[A, B]): ListParsingFunction[A, B] = single(f)

    def single[A, B](f: ValidationFunction[A, B]): ListParsingFunction[A, B] = {
        case Nil => Left(exhaustedInput :: Nil)
        case scala.::(a, rest) =>
            f(a) match {
                case Left(errors) if a.isInstanceOf[String] => Left(errors.map(_.withInvalidInput(a.asInstanceOf[String])))
                case Left(errors) => Left(errors)
                case Right(b) => Right(ListParsingResult(rest, b))
            }
    }

    def fixed[A, B](count: Int)(f: ListParsingFunction[A, B]): ListParsingFunction[A, List[B]] =
        repeating[A, B]((index, _) => index >= count)(f)

    def rest[A, B](f: ListParsingFunction[A, B]): ListParsingFunction[A, List[B]] =
        repeating[A, B]((_, rest) => rest.isEmpty)(f)

    def counted[B](f: ListParsingFunction[String, B]): ListParsingFunction[String, List[B]] =
        field("count", single(int())) and { case ListParsingResult(rest, count) => fixed(count)(f)(rest) }

    def repeating[A, B](terminate: (Int, List[A]) => Boolean)(f: ListParsingFunction[A, B]): ListParsingFunction[A, List[B]] =
        in => {
            val results = List.newBuilder[B]

            var index = 0
            var rest = in
            var errors: Option[List[ValidationError]] = None
            while (!errors.isDefined && !terminate(index, rest)) {
                field(index.toString, f(rest)) match {
                    case Left(errs) => errors = Some(errs)
                    case Right(ListParsingResult(newRest, result)) =>
                        results += result
                        rest = newRest
                }
                index += 1
            }

            errors map Left.apply getOrElse Right(ListParsingResult(rest, results.result()))
        }

    // these bits following are a bit clunky, but work. come scala 2.10, we'll have macros and then this mess can be cleaned up.

    object compositionFold extends Poly2 {
        def fold[A, B, L <: HList](prev: ListParsingFunction[A, L], f: ListParsingFunction[A, B]): ListParsingFunction[A, B :: L] =
            in => prev(in).right.flatMap {
                case ListParsingResult(rest, result) => f(rest).right.map { res => ListParsingResult(res.rest, res.result :: result) }
            }

        implicit def validationFunction[A, B, L <: HList] = at[ListParsingFunction[A, L], ValidationFunction[A, B]]((prev, f) => fold[A, B, L](prev, single(f)))
        implicit def listParsingFunction[A, B, L <: HList] = at[ListParsingFunction[A, L], ListParsingFunction[A, B]]((prev, f) => fold[A, B, L](prev, f))
    }

    def listOf[A]: ListParser[A] = new ListParser[A]
    final class ListParser[A] {
        def composedOf[L <: HList](fs: L)(implicit folder: LeftFolder[L, ListParsingFunction[A, HNil], compositionFold.type]): folder.Out =
            folder(fs, (in => Right(ListParsingResult(in, HNil))): ListParsingFunction[A, HNil])
    }

    implicit def resultReverser[A, B <: HList](lpf: ListParsingFunction[A, B]) = ResultReverser(lpf)
    final case class ResultReverser[A, B <: HList](lpf: ListParsingFunction[A, B]) {
        def reverseResult(implicit reverse: Reverse[B]): ListParsingFunction[A, reverse.Out] =
            in => lpf(in).right.map { _ mapResult (l => reverse(l)) }
    }

    implicit def fullyParse[A, B](lpf: ListParsingFunction[A, B]) = FullyParse(lpf)
    final case class FullyParse[A, B](lpf: ListParsingFunction[A, B]) {
        def mustFullyParse: ValidationFunction[List[A], B] =
            in => lpf(in).right.flatMap {
                case ListParsingResult(Nil, result) => Right(result)
                case ListParsingResult(rest, _) => Left(unexpectedInput(rest.toString) :: Nil)
            }
    }
}

