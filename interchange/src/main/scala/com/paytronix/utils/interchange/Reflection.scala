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

package com.paytronix.utils.interchange

import java.beans.Introspector
import java.lang.reflect.{Constructor, Method}
import java.math.BigInteger
import scala.collection.mutable.WrappedArray
import org.slf4j.{Logger, LoggerFactory}
import net.liftweb.json.JsonAST.{JField, JNothing, JValue}
import com.paytronix.utils.extendedreflection.{Builder, ClassR, ClassTypeR, ConstructorR, MethodLikeR, PropertyR}
import com.paytronix.utils.scala.collection.zip
import com.paytronix.utils.scala.log.resultLoggerOps
import com.paytronix.utils.scala.reflection.{classAndAncestors, getTypeArguments, paranamer}
import com.paytronix.utils.scala.result.{Failed, Okay, Result, catchingException, iterableResultOps, optionOps}

object Reflection {
    private val logger = LoggerFactory.getLogger(getClass)

    /** Generate an ArgumentArrayCoder by introspecting a method's arguments */
    def reflect(classLoader: ClassLoader, meth: Method)(implicit builder: Builder): Result[(ComposableCoder[Array[AnyRef]], ComposableCoder[_])] =
        for {
            classR      <- builder.classRFor(meth.getDeclaringClass)
            methR       <- classR.allMethods.find { _.matches(meth) }.toResult
                               .whenFailed("could not find method " + meth + " in class model " + classR.name.qualified)
            paramCoders <- reflectParameterCoders(classLoader, methR)
            resultCoder <- Coding.forTypeComposable(classLoader, methR.result)
        } yield (paramCoders, resultCoder)

    /** Generate an ArgumentArrayCoder by introspecting a constructor's arguments */
    def reflect(classLoader: ClassLoader, ctor: Constructor[_])(implicit builder: Builder): Result[ComposableCoder[Array[AnyRef]]] =
        for {
            classR      <- builder.classRFor(ctor.getDeclaringClass)
            ctorR       <- classR.constructors.find { _.matches(ctor) }.toResult
                               .whenFailed("could not find constructor " + ctor + " in class model " + classR.name.qualified)
            paramCoders <- reflectParameterCoders(classLoader, ctorR)
        } yield paramCoders

    /** Generate an ArgumentArrayCoder for the given MethodLikeR */
    private def reflectParameterCoders(classLoader: ClassLoader, methLikeR: MethodLikeR)(implicit builder: Builder): Result[ComposableCoder[Array[AnyRef]]] =
        methLikeR.parameters.mapResult { paramR =>
            for {
                name  <- paramR.name.toResult
                             .whenFailed("cannot code parameters of " + methLikeR + " because the parameter names are not known")
                coder <- Coding.forTypeComposable(classLoader, paramR.typeR)
            } yield ArgumentCoding(name, if (paramR.annotations.exists(_.isInstanceOf[Nullable])) NullCoder(coder) else coder)
        } map { argCodings => ArgumentArrayCoder(false, argCodings.toList) }

    /** Reflect a Class to generate an ObjectCoder, by looking for any bean field like things */
    def reflectObjectCoder[T](classLoader: ClassLoader, classTypeR: ClassTypeR)(implicit builder: Builder): Result[ObjectCoder[T]] =
        for {
            classR <- builder.classRFor(classTypeR.reflectionModel)

            val availableProperties = classR.properties.filterNot { _.annotations.exists(_.isInstanceOf[NotCoded]) }

            selectedConstructor <- chooseConstructor(classR, availableProperties)

            val constructorPropertyNames = selectedConstructor.parameters.map(_.name).flatten
            val usableProperties = availableProperties.filter { propR =>
                constructorPropertyNames.contains(propR.name) ||
                propR.setter.isDefined ||
                propR.annotations.exists { _.isInstanceOf[Coded] }
            }

            usablePropertiesOk <-
                if (!usableProperties.isEmpty) Okay(())
                else Failed("couldn't find any usable properties for " + classTypeR + " -- make sure you either have a constructor " +
                            "that takes parameters named identically to your properties, or that your properties are writable")

            fieldCodings <- usableProperties.mapResult { propR =>
                Coding.forTypeComposable(classLoader, propR.typeR) map { coder =>
                    val wrappedCoder =
                        if (propR.annotations.exists(_.isInstanceOf[Nullable]) ||
                            (selectedConstructor.parameters
                             .find(_.name == Some(propR.name))
                             .map(_.annotations.exists(_.isInstanceOf[Nullable])).getOrElse(false)))
                            NullCoder(coder)
                        else
                            coder

                    FieldCoding(propR.name, wrappedCoder, propR.getter.reflectionModel, propR.setter.map(_.reflectionModel))
                }
            }
        } yield ObjectCoder (
            clazz = classTypeR.reflectionModel.asInstanceOf[Class[T]],
            constructor = selectedConstructor.reflectionModel.asInstanceOf[Constructor[T]],
            constructorFieldNames = constructorPropertyNames,
            fieldCodings = fieldCodings.toList,
            flatten = false
        )

    /** Reflect a singleton object looking for readable @Coded properties and generates a SingletonCoder */
    def reflectSingletonCoder[T <: AnyRef](classLoader: ClassLoader, inst: T)(implicit builder: Builder): Result[SingletonCoder[T]] = {
        import ComposableCoder.{atProperty, formatFailedPath}
        for {
            classR <- builder.classRFor(inst.getClass)

            val usableProperties = classR.properties.filter {
                propR => propR.annotations.exists(_.isInstanceOf[Coded]) && !propR.annotations.exists(_.isInstanceOf[NotCoded])
            }

            fields <- formatFailedPath {
                usableProperties.mapResult { propR =>
                    for {
                        coder <- Coding.forTypeComposable(classLoader, propR.typeR) map { coder =>
                            if (propR.annotations.exists(_.isInstanceOf[Nullable]))
                                NullCoder(coder)
                            else
                                coder
                        } withFailureParameter Nil

                        encoded <- atProperty(propR.name) {
                            catchingException(propR.getter.reflectionModel.invoke(inst))
                            .whenFailed("failed to get property " + propR.name + " from singleton " + inst)
                            .withFailureParameter(Nil)
                            .flatMap(coder.forceEncode(classLoader, _))
                        }
                    } yield {
                        encoded match {
                            case JNothing => None
                            case other    => Some(JField(propR.name, other))
                        }
                    }
                }
            }
        } yield SingletonCoder(inst, fields.flatten.toList)
    }

    /** Determine which constructor to use for instantiation of an object via coding */
    private def chooseConstructor(classR: ClassR, availableProperties: List[PropertyR]): Result[ConstructorR] = {
        def constructorHasUnusableParameter(in: ConstructorR): Boolean =
            in.parameters.exists { param =>
                param.name match {
                    case Some(name) => !availableProperties.exists(_.name == name)
                    case None => true // treat unnamed parameters as unusable ones
                }
            }

        classR.constructors.filter { _.annotations.exists(_.isInstanceOf[ConstructorForCoding]) } match {
            case Nil =>
                /*
                 * No specifically annotated constructor, so filter a list of constructors that are valid for coding use and then use the one with the
                 * largest number of parameters. To be valid for coding use, every parameter of the constructor must be named after a known property of
                 * the class.
                 */
                classR.constructors
                    .filterNot(constructorHasUnusableParameter)
                    .sortBy(_.parameters.length)
                    .lastOption.toResult
                    .whenFailed("no constructors found for " + classR.name.qualified + " that can be used for coding")

            case ctor :: Nil =>
                // Check that the specifically annotated constructor makes sense
                if (!constructorHasUnusableParameter(ctor)) Okay(ctor)
                else Failed("Constructor of " + classR.name.qualified + " annotated with @ConstructorForCoding is not suitable for coding -- it has " +
                             "one or more parameters that are not named after available properties, or the parameter names could not be determined.")

            case _ =>
                Failed("More than one constructor of " + classR.name.qualified + " was annotated with @ConstructorForCoding, and I'm not smart " +
                        "enough to pick one.")
        }
    }
}
