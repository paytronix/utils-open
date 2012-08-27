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

package com.paytronix.utils.validation

import java.io.File

import base.{ValidationError, ValidationFunction, missingValueError}

object file {
    import string.nonBlank

    val nonExistentError = ValidationError("non_existent", "does not exist")
    val nonDirectoryError = ValidationError("non_directory", "not a directory")
    val nonFileError = ValidationError("non_file", "not a file")

    /** Assert that a String is nonblank and convert it to a File */
    def path(missingValue: ValidationError = missingValueError): ValidationFunction[String, File] = nonBlank() and (in => Right(new File(in)))

    /** Assert that a File refers to an existent path (file, directory, or other special file system entity) */
    def exists(error: ValidationError = nonExistentError): ValidationFunction[File, File] =
        in => if (in.exists) Right(in)
              else Left(error :: Nil)

    /** Assert that a File refers to a directory */
    def isDirectory(error: ValidationError = nonDirectoryError): ValidationFunction[File, File] =
        in => if (in.isDirectory) Right(in)
              else Left(error :: Nil)

    /** Assert that a File refers to a file (not a directory or some special file system entity) */
    def isFile(error: ValidationError = nonFileError): ValidationFunction[File, File] =
        in => if (in.isFile) Right(in)
              else Left(error :: Nil)
}

