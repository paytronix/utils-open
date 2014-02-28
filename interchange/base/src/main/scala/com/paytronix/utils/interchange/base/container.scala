//
// Copyright 2014 Paytronix Systems, Inc.
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

package com.paytronix.utils.interchange.base.container

import com.paytronix.utils.interchange.base.{InsecureContext, InterchangeClassLoader}
import com.paytronix.utils.scala.reflection.classByName
import com.paytronix.utils.scala.result.{Okay, Result, tryCatch, tryCatching}

object result {
    def instantiateThrowable (
        className: String, message: String, causeOpt: Option[Throwable]
    )(implicit interchangeClassLoader: InterchangeClassLoader): Result[Throwable] =
        for {
            clazz <- if (InsecureContext.get) Okay(classOf[RuntimeException])
                     else classByName[Throwable](interchangeClassLoader.classLoader, className)
            inst <- causeOpt match {
                case Some(cause) =>
                    (tryCatching[NoSuchMethodException].value(clazz.getConstructor(classOf[String], classOf[Throwable]))
                        | ("throwable class " + className + " does not have (String, Throwable) constructor")
                        >>= { ctor => tryCatch.value(ctor.newInstance(message, cause)) })

                case None =>
                    ( (tryCatching[NoSuchMethodException].value(clazz.getConstructor(classOf[String]))
                        | ("throwable class " + className + " does not have a (String) constructor")
                        >>= { ctor => tryCatch.value(ctor.newInstance(message)) })
                    | (tryCatching[NoSuchMethodException].value(clazz.getConstructor(classOf[String], classOf[Throwable]))
                        | ("throwable class " + className + " does not have a (String, Throwable) constructor")
                        >>= { ctor => tryCatch.value(ctor.newInstance(message, null)) })
                    )
            }
        } yield inst
}