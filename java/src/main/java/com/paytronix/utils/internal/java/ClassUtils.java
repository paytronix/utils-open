//
// Copyright 2009-2012 Paytronix Systems, Inc.
// All Rights Reserved
//

package com.paytronix.utils.internal.java;

/**
 * Static utilities for dealing with Class objects
 */
public class ClassUtils
{
    /** Return the unqualified part of the class name (e.g. "Object" for Object.class, rather than "java.lang.Object") */
    public static String getUnqualifiedClassName(Class<?> klass)
    {
        String className = klass.getName();
        int index = className.lastIndexOf('.');
        return className.substring(index+1);
    }
}

