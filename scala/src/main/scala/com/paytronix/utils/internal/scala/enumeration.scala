//
// Copyright 2011-2012 Paytronix Systems, Inc.
// All Rights Reserved
//

package com.paytronix.utils.internal.scala

object enumeration {
    /**
     * Trait of enums that have a numeric ID which is typically stored in the database and not starting at 0.
     * Using the ID field Enumeration#Value for high-valued IDs will result in a large amount of storage being consumed due to the backing BitSet
     */
    trait KeyedEnum {
        self: Enumeration =>

        val valueById: Map[Int, Value]
        lazy val idByValue = Map(valueById.toSeq.map(_.swap): _*)
    }

    /**
     * Trait of enums that have a description.  This is useful for creating a selection list that have an internal value (i.e., the enum name) and
     * a descriptive label for presentation to the user.
     */
    trait LabelEnum {
        self: Enumeration =>

        val labelByValue: Map[Value, String]
    }
}
