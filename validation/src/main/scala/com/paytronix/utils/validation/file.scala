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

package com.paytronix.utils.validation

import java.io.File

import base.{Validated, ValidationError, predicateE, validationFunctionOps}

object file {
    import string.nonBlank

    val nonExistentError = ValidationError("non_existent", "does not exist")
    val nonDirectoryError = ValidationError("non_directory", "not a directory")
    val nonFileError = ValidationError("non_file", "not a file")

    /** Assert that a String is nonblank and convert it to a File */
    val path: String => Validated[File] =
        nonBlank.map(new File(_: String))

    /** Assert that a File refers to an existent path (file, directory, or other special file system entity) */
    val exists: File => Validated[File] =
        existsE(nonExistentError)

    /** Assert that a File refers to an existent path (file, directory, or other special file system entity) */
    def existsE(error: ValidationError): File => Validated[File] =
        predicateE(error)(_.exists)

    /** Assert that a File refers to a directory */
    val isDirectory: File => Validated[File] =
        isDirectoryE(nonDirectoryError)

    /** Assert that a File refers to a directory */
    def isDirectoryE(error: ValidationError): File => Validated[File] =
        predicateE(error)(_.isDirectory)

    /** Assert that a File refers to a file (not a directory or some special file system entity) */
    val isFile: File => Validated[File] =
        isFileE(nonFileError)

    /** Assert that a File refers to a file (not a directory or some special file system entity) */
    def isFileE(error: ValidationError): File => Validated[File] =
        predicateE(error)(_.isFile)
}

