//
// Copyright 2012-2020 Paytronix Systems, Inc.
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

package com.paytronix.utils.scala
import result._

import scalaz.Monad

object dresultg {

	type DResultG[-D, +E, +A] = D => ResultG[E, A]
    object DResultG {
        def liftF[E, A](r: => ResultG[E, A]): DResultG[Any, E, A] = _ => r
        def pure[A](a: A): DResultG[Any, Nothing, A] = DOkay(a)
        def apply[D, E, A](f: D => ResultG[E, A]): DResultG[D, E, A] = f

        def fromPureFunction[D, A](f: D => A): DResultG[D, Nothing, A] = 
            d => Okay(f(d))
    }

    type DResult[-D, +A] = DResultG[D, Unit, A]
    /*object DResult {
        def fromVolatileFunction[D, A](f: D => A): DResult[D, A] = 
            d => tryCatchValue(f(d))
    }*/

    type DOkay[+A] = DResultG[Any, Nothing, A]
    object DOkay {
        def apply[A](a: A): DResultG[Any, Nothing, A] = _ => Okay(a)
        def unit: DOkay[Unit] = apply(())
    }

    type DFailedG[+E] = DResultG[Any, E, Nothing]
    object DFailedG {
        def apply[E](e: E): DResultG[Any, E, Nothing] = _ => FailedG[E]("", e)
    }

    implicit class DFailedGCapabilities[+E](val f: DFailedG[E]) extends AnyVal {
        def when[e >: E](b: Boolean): DResultG[Any, e, Unit] = 
            if (b) f.asInstanceOf[DResultG[Any, e, Unit]] else DOkay.unit

        def unless[e >: E](b: Boolean): DResultG[Any, e, Unit] = 
            when(!b)
    }

    implicit class DResultCapabilities[-D, +E, +A](val fa: DResultG[D, E, A]) extends AnyVal {
        def flatMap[DD <: D, e >: E, B](f: A => DResultG[DD, e, B]): DResultG[DD, e, B] = 
            dd => fa(dd).flatMap(a => f(a)(dd))

        def map[B](f: A => B): DResultG[D, E, B] = 
            d => fa(d).map(f)

        def filter[e >: E : FailedParameterDefault](p: A => Boolean): DResultG[D, e, A] = 
            flatMap(a => if (p(a)) (d => Okay(a)) else (d => FailedG("filter failure", implicitly[FailedParameterDefault[e]].default)))

        def flatten[DD <: D, e >: E, B](implicit ev: A => DResultG[DD, e, B]): DResultG[DD, e, B] = 
            flatMap(ev)

        def |[F, a >: A](f: FailedG[E] => ResultG[F, a]): DResultG[D, F, a] = 
            d => fa(d) | f

        def run[DD <: D](dd: DD): ResultG[E, A] = fa(dd)

        def toOption[DD <: D, a >: A]: DD => Option[a] = 
            dd => fa(dd).toOption

        def toList[DD <: D, a >: A]: DD => List[a] = 
            dd => fa(dd).toList
    }

    object instances {

        implicit def monadInstance[D, E] = new Monad[({ type T[A] = DResultG[D, E, A] })#T] {
            def point[A](a: => A): DResultG[D, E, A] = 
                DResultG.pure(a)
            def bind[A, B](fa: DResultG[D, E, A])(f: A => DResultG[D, E, B]): DResultG[D, E, B] = 
                fa flatMap f
        }
    }
}