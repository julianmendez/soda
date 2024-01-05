****************
Reference Manual
****************


Functions
=========

A simple constant can be defined as:

::

  greeting = "Hello world!"

This could also be defined

::

  greeting : String = "Hello world!"


Functions receive one or multiple parameters:

::

  square (n : Int) : Int = n * n

  plus (n : Int) (m : Int) : Int = n + m

The types ``Boolean``, ``Int``, ``Float``, and ``String`` are already available with the standard basic operations.

It is possible to use ``if``-``then``-``else`` structures

::

  max (x : Int) (y : Int) : Int =
    if x > y
    then x
    else y

Soda accepts lambda expressions, like

::

  plus : Int -> Int -> Int =
    lambda x -->
      lambda y -->
        x + y


Classes
=======

It is possible to define classes like

::

  class Shape

  end

which is an empty class.

It is also possible to add abstract fields to a class.

::

  class RegisteredPerson

    abstract
      first_name : String
      last_name : String

    _separator = " "

    full_name = first_name + _separator + last_name

  end

If a constant or function name starts with an underscore (_), it means that it should not be accessed outside the class.

Every class has a default constructor, which is the name of class followed by an underscore

Classes are grouped in packages.

