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

import base.{ValidationError, ValidationFunction}

object column {
    def requiredColumnError(c: String) = ValidationError("missing_required_column", "Missing column " + c)
    def unknownColumnError(c: String) = ValidationError("invalid_column", "Invalid unknown column " + c)

    /** Type of a list of columns zipped with their indices */
    type Columns = List[(String, Int)]

    /** Specific type of validation function that applies to columns and threads some other value through the function */
    type ColumnValidationFunction[A, B] = ValidationFunction[(A, Columns), (B, Columns)]

    /**
     * Validate a header line by applying a chain of column validation functions which prune down the original list of header columns.
     * Any columns remaining unhandled after the function applies cause a validation error.
     * A user-defined value can be threaded through, and usually is used to carry some object to store column indices
     *
     * Example use:
     * <pre>
     *   case class ColumnIndices(column1: Int = -1, column2: Int = -1, column3: Option[Int] = None) {
     *       def column1_= (i: Int) = copy(column1 = i)
     *       def column2_= (i: Int) = copy(column2 = i)
     *       def column3_= (i: Option[Int]) = copy(column3 = i)
     *   }
     *
     *   (ColumnIndices(), headerTokens) are columns (
     *       required("Column 1", _ column1_= _),
     *       required("Column 2", _ column2_= _),
     *       optional("Column 3", _ column3_= _)
     *   )
     * </pre>
     *
     * Each validator passed in is expected to strip any column headings that were handled by it from the list of columns, so that any unrecognized columns are left over
     * and cause an error. E.g. If (Column 1, Column 2, Column 3, Column 4) are passed in as a value to validate, required("Column 1") will yield (Column 2, Column 3,
     * Column 4), required("Column 2") will yield (Column 3, Column 4), optional("Column 3") will yield (Column 4), and finally columns(...) will fail with
     * unknownColumnError("Column 4")
     */
    def columns[A](seed: A, unknownColumn: String => ValidationError = unknownColumnError)(
        fs: ColumnValidationFunction[A, A]*
    ): ValidationFunction[Seq[String], A] = polyColumns(seed, unknownColumn)(fs.reduceLeft(_ and _))

    /** More general type of column validation where the validation function is A => B not A => A. */
    def polyColumns[A, B](seed: A, unknownColumn: String => ValidationError = unknownColumnError)(
        f: ColumnValidationFunction[A, B]
    ): ValidationFunction[Seq[String], B] = cols => {
        f((seed, cols.toList.zipWithIndex)).right.flatMap {
            case (output, Nil)    => Right(output)
            case (_, unknownCols) => Left(unknownCols.map(t => unknownColumn(t._1)))
        }
    }

    /** Require that a column is present in the given input. Applies the given update with the column index to the input value to produce an output. */
    def required[A, B](c: String, update: (A, Int) => B, error: ValidationError = null): ColumnValidationFunction[A, B] = tuple => {
        val (input, cols) = tuple
        cols partition (_._1 == c) match {
            case (Nil, _) if error != null => Left(error :: Nil)
            case (Nil, _)                  => Left(requiredColumnError(c) :: Nil)
            case ((_, idx)::_, rest)       => Right((update(input, idx), rest))
        }
    }

    /**
     * Permit a column name, but don't require it. Applies the given update to the input to produce an output if the column is present.
     * Strips a column from the input, making it so that an enclosing columns(...) application will not fail with unknown column.
     */
    def optional[A](c: String, update: (A, Int) => A): ColumnValidationFunction[A, A] = tuple => {
        val (input, cols) = tuple
        cols partition (_._1 == c) match {
            case (Nil, _)            => Right(tuple)
            case ((_, idx)::_, rest) => Right((update(input, idx), rest))
        }
    }
}
