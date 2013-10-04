//
// Copyright 2012-2013 Paytronix Systems, Inc.
// All Rights Reserved
//

package com.paytronix.utils.internal.scm.utility

import net.liftweb.json.JsonAST.{JField, JObject, JString}

import com.paytronix.utils.internal.scm.common.NodeContents

object audit {
    def internal(detail: String): NodeContents =
        JObject (
            JField("source", JString("internal")) ::
            JField("detail", JString(detail)) :: Nil
        )

    def jmx(detail: String): NodeContents =
        JObject (
            JField("source", JString("jmx")) ::
            JField("detail", JString(detail)) :: Nil
        )

    val defaultLoading: NodeContents = internal("defaultLoading")
}
