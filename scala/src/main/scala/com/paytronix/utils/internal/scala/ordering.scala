//
// Copyright 2011-2012 Paytronix Systems, Inc.
// All Rights Reserved
//

package com.paytronix.utils.internal.scala


object ordering {
    /** Implicitly extend an Ordering to ExtendedOrderingOps to give additional operations */
    implicit def extendedOrderingOps[A](in: Ordering[A]): ExtendedOrderingOps[A] = ExtendedOrderingOps(in)

    final case class ExtendedOrderingOps[A](ordering: Ordering[A]) {
        def andThen(other: Ordering[A]): Ordering[A] = new Ordering[A] {
            def compare(l: A, r: A) = ordering.compare(l, r) match {
                case 0 => other.compare(l, r)
                case divergence => divergence
            }
        }
    }
}
