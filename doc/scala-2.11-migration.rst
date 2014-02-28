Scala 2.11 Migration Guide
==========================

Versions of ``utils-open`` prior to 14.3 supported only Scala 2.9.2. While updating the library to support Scala 2.11.1, many cleanups and improvements were made including an overhaul of Interchange to become compile time safe and significantly faster and more reliable. This guide documents changes between the versions for users of the older library wishing to update their code.

Miscellaneous Scala Utilities
-----------------------------

.. code:: scala

   resultG.isDefined
      ==> resultG.isOkay

``collection`` was removed in its entirety since the functions within are now subsumed by Scala and scalaz:

.. code:: scala

   collection.unfold(init, f)
      ==> scalaz.std.stream.unfold(init, f).toList

.. code:: scala

   collection.zip(as, bs, cs)
      ==> (as, bs, cs).zipped.toList

.. code:: scala

   collection.TraversableOnceOps(coll).minOrNone
      ==> scalaz.Foldable[Coll].minimum(coll)
   // or
   import scalaz.syntax.foldable.ToFoldableOps
   collection.TraversableOnceOps(coll).minOrNone
      ==> coll.minimum

.. code:: scala

   collection.TraversableOnceOps(coll).maxOrNone
      ==> scalaz.Foldable[Coll].maximum(coll)
   // or
   import scalaz.syntax.foldable.ToFoldableOps
   coll.maximum

.. code:: scala

   collection.TraversableOnceOps(coll).minOrElse(default)
      ==> scalaz.Foldable[Coll].minimum(coll).getOrElse(default)
   // or
   import scalaz.syntax.foldable.ToFoldableOps
   coll.minimum.getOrElse(default)

.. code:: scala

   collection.TraversableOnceOps(coll).maxOrElse(default)
      ==> scalaz.Foldable[Coll].maximum(coll).getOrElse(default)
   // or
   import scalaz.syntax.foldable.ToFoldableOps
   coll.maximum.getOrElse(default)

Validation
----------

The type ``ValidationFunction`` was removed, in preference for the shorter and less obtuse equivalent ``A => Validated[B]``.

.. code:: scala

    base.ValidationFunction[A, B]
        ==> A => Validated[B]

All validation functions consistently either don't take error message arguments if they don't end with ``E`` or do take error message arguments if they do end with ``E``. For example, ``boolean`` uses a default error message while ``booleanE`` must be supplied with the error message to use. All non-``E`` functions are implemented in terms of their ``E`` counterpart.

.. code:: scala

    base.pass
        ==> scalaz.Validation.success

Joda now the first-class date type with validations in ``date``. Helper conversion functions from Joda types to ``java.util`` and ``java.sql`` types are provided:

.. code:: scala

    date.dateWithFormat(f)
        ==> date.dateTimeWithFormat(f).map(date.javaUtilDate)

.. code:: scala

    option.some(error)(f)
        ==> option.someE(error)(f)

.. code:: scala

    option.none(error)
        ==> option.noneE(error)

.. code:: scala

    f and { x => Success(g(x)) }
        ==> f.map(g)

The ``"name" from map is f and g`` syntax actually had subtle problems where if ``g`` failed the failure would not have the field name associated. As such, the syntax has been removed in favor of a simple overload of ``field`` which does the same:

.. code:: scala

    "name" from map is f and g
        ==> field("name", map, f)

``field`` used to work for both ``Validated`` and ``ValidationFunction``, but was used rarely so was removed to reduce overloads:

.. code:: scala

    field("name", f)
        ==> in => field("name", f(in))

Interchange
===========

See Interchange generation 2 document.
