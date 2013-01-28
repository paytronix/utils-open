//
// Copyright 2012 Paytronix Systems, Inc.
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

import java.io.{File, Writer}
import scala.collection.JavaConverters.seqAsJavaListConverter
import scala.collection.mutable.ArrayBuffer
import org.apache.avro.Schema
import org.slf4j.{Logger, LoggerFactory}
import com.paytronix.utils.extendedreflection.Builder
import com.paytronix.utils.scala.log.resultLoggerOps
import com.paytronix.utils.scala.result.tryCatch

/** Class which uses coding reflection to generate Avro schemas. Used by the Maven plugin. */
class AvroSchemaGenerator {
	implicit val builder = new Builder(getClass.getClassLoader)

	private implicit val logger = LoggerFactory.getLogger(getClass)

	def scanForSchema(root: File, out: Writer): Unit = {
		val foundSchemas = new ArrayBuffer[Schema]

		def visitClass(className: String): Unit = {
			logger.debug("Visiting " + className)
			val clazz = Class.forName(className)
			if (clazz.getAnnotation(classOf[GenerateSchema]) != null) {
				logger.info("Generating schema for " + className)
				tryCatch.result {
					for {
						typeR <- builder.typeRFor(clazz)
						coder <- Coding.forType(clazz.getClassLoader, typeR)
					} yield foundSchemas += coder.implementation.avroSchema._1
				}.logError("Failed to generate schema for " + className)
			}
		}

		def scanDirectory(directory: File, prefix: List[String]): Unit =
			for (subfile <- directory.listFiles) {
				if (subfile.isDirectory) {
					scanDirectory(new File(directory, subfile.getName), subfile.getName :: prefix)
				} else if (subfile.isFile() && subfile.getName.endsWith(".class")) {
					val pos = subfile.getName.lastIndexOf(".class")
					visitClass(prefix.reverse.mkString("", ".", ".") + subfile.getName.substring(0, pos))
				}
			}

		scanDirectory(root, Nil)

		logger.info("Found " + foundSchemas.size + " classes to generate schema for.")

		out.write(Schema.createUnion(foundSchemas.asJava).toString(true))
	}
}
