# Paytronix Open-source Utilities

This project contains a set of libraries originally internal to Paytronix, deployed in production for varying amounts of time, which are being released back into the community in the hopes that they will be helpful to others.

To the end of being most helpful to the most people, this code is being released under the terms of the Apache License 2.0, included in the LICENSE file, or available on the web at http://www.apache.org/licenses/LICENSE-2.0

Currently the project is managed using Maven, and as such we don't publish cross-builds for different Scala versions. If someone wanted to contribute an equivalent SBT setup, it would be accepted, though not maintained by Paytronix.

# Paytronix Engineering on Twitter

* @dridus: Ross MacLeod
* @aldiyen: Matt Explosion

There are more of us, but not with Twitter accounts!

## Libraries in the Project

### extendedreflection: Reflection metamodel generation for Java and Scala

Scala 2.10 will include a full fledged reflection library, but unfortunately is not yet released at the time of this writing. To support the automatic coding of arbitrary Scala classes including Java reflection-proof constructs such as Scala Enumeration instances, the Extended Reflection library wraps up the magic of reading the Scala compiler signature metadata using the scalap library melded with traditional Java reflection data to produce a metamodel that is consumed by Interchange.

Come 2.10, the Extended Reflection library will likely be updated (and vastly simplifed) to use Scala reflection to produce its metamodel.

## interchange: An automatic and full featured JSON, Avro, and MongoDB mapping library

Interchange is a JSON, Avro, and MongoDB encoding/decoding library. It is designed under the principle that minimal metadata and configuration should be used; instead it uses extended reflection (above) to model the classes to encode/decode.

It supports numerous data modelling techniques, including POJOs, POSOs, case classes, Java and Scala enumerations, a variety of simple scalar types, Option, Either, Result (see Miscellaneous, below), arrays, maps with both complex and simple keys, secure and insecure coding contexts (e.g. for using the same model classes for both external and internal use) and overridable coding rules per type.

It is usable with only minimal pain from Java as well as Scala.

JSON support is implemented using the Lift JSON library. Avro support is fully implemented, including compile-time generation of Avro schema files directly from classes at compile time or run time. MongoDB support is also implemented to convert BasicDBObjects to your own data model and vice versa.

## lift: Lift Utilities

A collection of utilities and other glue for using Interchange and Validation (below) with Lift's Box type, along with other Lift-related helpers. Does not depend on or help with Lift WebKit, just Lift Utils and Lift Common.

## maven: Maven Plugin for Avro Schema Generation

A plugin is provided for generating Avro schema using the Interchange framework. Annotate your classes with @GenerateSchema and a schema file will be produced at compile time.

## scala: Miscellaneous Scala Utilities

* collection: unfold, zip for 3+ collections, partitioning a collection of Either, and min/max enrichment on collections.
* concurrent: atomic update helper, Scala-friendly wrappers for ThreadLocal, java.util.concurrent.Lock, and java.util.concurrent.ReadWriteLock
* io: read chars or bytes using a function, read entire files as bytes, chars, or a string
* log: enrich SLF4J loggers with functions for logging ResultGs (below) and enrich ResultGs with functions for logging themselves to SLF4J loggers
* reflection: small utilities for making reflection easier to use
* resource: helpers for .close()ing resources upon exit from blocks
* result: ResultG, a monad for error handling isomorphic to Right-leaning Either[(Throwable, E), A] along with type alias Result and many utilities helpful for workflows such as collection-oriented utilities like mapResult, foreachResult, etc, utilities to convert exception-ful expressions to Result ones, etc.
* types: Identity and Const type functions

## validation: Functional validation using Either and HList

A library of standard validation functions and tools to compose them both for single values (e.g. int() and positive()) and multiple values using Miles Sabin's excellent shapeless library:

    import shapeless.{::, HNil}

    val map = Map("foo" -> "123", "bar" -> "a", "baz" -> "-1")

    validate (
        ("foo" from map.get is some(int())) ::
        ("bar" from map.get is some(nonBlank() and matches("[a-z]".r))) ::
        ("baz" from map.get is optional(int() and nonPositive())) ::
        HNil
    )

    => Right(123 :: "a" :: Some(-1) :: HNil)
     : Validated[Int :: String :: Option[Int] :: HNil]

This is probably familiar to those who use scalaz. Once scalaz 7 is released, I'll start working on making it compatible.

Supported validations include:

* boolean: convert String to Boolean, accepting yes/no, true/false, on/off, 1/0
* column: validation of columnar inputs with a header line
* date: parse java.util.Dates (Joda time support welcome, but not there presently)
* enumeration: convert String to enumeration value
* file: String must denote a valid path, existing path, file, or directory
* numeric: convert String to various numeric types, assert positive, negative, less than, greater than, etc.
* option: apply validations inside Option, require an Option to be Some
* reflection: convert String to Class by Class.forName, possibly constraining to some lower bound
* regex: convert String to Regex
* result: apply validations inside ResultG, require a ResultG to be Okay
* sequence: apply validations to each element of a sequence, min/max length of collections, split strings into sequences
* string: must be longer than, shorter, than, conform to a regex, be a valid email address, numeric, non-blank, and so on
* uri, url: convert String to java.net.URI or java.net.URL, respectively

# Paytronix

Paytronix Systems, Inc. is a hosted gift, loyalty, comp, and email provider based out of Waltham, MA US. We're regularly hiring talented and fun engineering and client services people, so if you think you fit that bill and live somewhere nearby or plan to soon, [contact us](http://www.paytronix.com/contact).


