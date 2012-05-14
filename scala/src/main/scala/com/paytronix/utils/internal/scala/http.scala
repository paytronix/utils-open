//
// Copyright 2011-2012 Paytronix Systems, Inc.
// All Rights Reserved
//

package com.paytronix.utils.internal.scala

import com.paytronix.utils.internal.java.Base64
import com.paytronix.utils.scala.result.{Okay, Failed, Result}

/** Helpers pertaining to HTTP */
object http {
    /** Decode a basic Authorization header to its username and password values */
    def decodeBasicAuthorizationValue(authorization: String): Result[(String, String)] =
        try {
            if (!authorization.toLowerCase.startsWith("basic ")) {
                Failed("Unrecognized Authorization style " + authorization.split(' ').headOption.getOrElse("<none>"))
            } else {
                Base64.decode(authorization.substring(6).getBytes("ISO-8859-1")) match {
                    case null => Failed("Failed to Base64 decode Authorization value")
                    case bytes =>
                        val decoded = new String(bytes, "ISO-8859-1")
                        decoded.indexOf(':') match {
                            case -1                               => Failed("could not parse Authorization value - missing : separator in Base64 decoded value")
                            case 0                                => Failed("could not parse Authorization value - username missing")
                            case pos if pos == decoded.length - 1 => Failed("could not parse Authorization value - password missing")
                            case pos                              => Okay((decoded.substring(0, pos), decoded.substring(pos+1)))
                        }
                }
            }
        } catch {
            case e: Exception => Failed("failed to authorize", e)
        }
}
