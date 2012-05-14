//
// Copyright 2009-2012 Paytronix Systems, Inc.
// All Rights Reserved
//

package com.paytronix.utils.internal.scala

/**
 * Helpers for control flow
 */
object control
{
    /** Apply a transformation to a value if a given predicate is met, returning the value untransformed otherwise */
    def when[A](b: Boolean)(f: A => A)(a: A): A = if (b) f(a) else a
}
