//
// Copyright 2011-2012 Paytronix Systems, Inc.
// All Rights Reserved
//

package com.paytronix.utils.internal.scala

/**
 * Base32 (ish) encoding and decoding of byte arrays, for a compact but human type-able representation of raw data.
 * (ish) is because this uses a custom table of only characters and avoids easily confused characters, such as o, O, l, and I
 */
object base32 {
    val table = Array[Char]('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'm', 'n', 'p', 'q', 'r',
                            'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'J', 'K', 'L', 'M', 'N', 'P', 'Q', 'R')

    /** Encode the given bytes into a base32-ish string */
    def encode(bytes: Array[Byte]): String =
        if (bytes.length == 0) ""
        else {
            val chars = Array.ofDim[Char](bytes.length * 8 / 5 + 1)
            var byteIndex: Int = 0
            var charIndex: Int = 0
            var startBit: Int = 0
            while (byteIndex < bytes.length) {
                if (startBit < 4) {
                    chars(charIndex) = table((bytes(byteIndex) >>> (3-startBit)) & 0x1f)
                    charIndex += 1
                    startBit += 5
                } else if (bytes.length > byteIndex+1) {
                    val bitsRemaining = 11 - startBit
                    chars(charIndex) = table(((bytes(byteIndex) << -(3-startBit)) & (0x1f & (0xff << -(3-startBit)))) |
                                             ((bytes(byteIndex+1) >>> bitsRemaining) & (0xff >>> bitsRemaining)))
                    byteIndex += 1
                    charIndex += 1
                    startBit = (startBit+5) % 8
                } else {
                    chars(charIndex) = table((bytes(byteIndex) << -(3-startBit)) & (0x1f & (0xff << -(3-startBit))))
                    byteIndex += 1
                }
            }
            new String(chars)
        }

    /** Decode the given base32-ish string into bytes. No validation is performed, so if there's such a concern, make sure to put in a magic number or checksum */
    def decode(s: String): Array[Byte] =
        if (s.size == 0) Array.ofDim[Byte](0)
        else {
            val chars = s.toCharArray
            val bytes = Array.ofDim[Byte](chars.length * 5 / 8)
            var byteIndex: Int = 0
            var charIndex: Int = 0
            var startBit: Int = 0
            while (byteIndex < bytes.length) {
                var fragment = 0
                val char = chars(charIndex)
                while (fragment < 255 && table(fragment) != char) { fragment += 1 }
                if (startBit < 4) {
                    bytes(byteIndex) = (bytes(byteIndex) | (fragment << (3-startBit))).asInstanceOf[Byte]
                    charIndex += 1
                    startBit += 5
                } else if (bytes.length > byteIndex+1) {
                    val bitsRemaining = 11 - startBit
                    bytes(byteIndex) = (bytes(byteIndex) | (fragment >>> -(3-startBit))).asInstanceOf[Byte]
                    bytes(byteIndex+1) = (bytes(byteIndex+1) | (fragment << bitsRemaining)).asInstanceOf[Byte]
                    byteIndex += 1
                    charIndex += 1
                    startBit = (startBit + 5) % 8
                } else {
                    bytes(byteIndex) = (bytes(byteIndex) | (fragment >>> -(3-startBit))).asInstanceOf[Byte]
                    byteIndex += 1
                }
            }
            bytes
        }

    /**
     * Utility method to brute force verify that the encode and decode work.
     * This can take a huge amount of time - ~30 minutes on a Core 2 Duo 3.06GHz just for length 4 all permutations (all permutations of 4 bytes)
     */
    def verify(len: Int, permutations: Long) = {
        import java.util.Arrays
        import net.liftweb.util.SecurityHelpers.hexEncode
        val b = Array.ofDim[Byte](len)
        def increment(i: Int): Boolean =
            if (i >= len) true
            else if (b(i) == -1) {
                b(i) = 0
                increment(i+1)
            } else {
                b(i) = (b(i) + 1).asInstanceOf[Byte]
                false
            }

        val ran = new java.util.Random
        var count = 0L
        val start = System.currentTimeMillis
        var done = false
        while (!done && (if (permutations > 0) count < permutations else true)) {
            if (permutations > 0) {
                for (i <- 0 to (len-1)) {
                    b(i) = (ran.nextInt % 256).asInstanceOf[Byte]
                }
            }

            try {
                val encoded = encode(b)
                val decoded = decode(encoded)

                assert(Arrays.equals(b, decoded), "failed at length " + len + ": encoded = " + encoded + " decoded = " + hexEncode(decoded))

                done = increment(0)
            } catch {
                case e => {
                    println("Failed with permutation " + hexEncode(b))
                    e.printStackTrace(System.out)
                    done = true
                }
            }

            count += 1
        }
        val end = System.currentTimeMillis

        println("finished in " + (end - start) + "ms")
    }
}
