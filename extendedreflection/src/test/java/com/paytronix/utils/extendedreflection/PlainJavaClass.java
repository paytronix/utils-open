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

package com.paytronix.utils.extendedreflection;

import java.util.List;

public class PlainJavaClass
{
    private final int aReadOnlyInt = 0;
    private int aReadWriteInt;
    private boolean aReadOnlyBoolean = false;
    @TestAnnotation private int anAnnotatedInt;
    private List<String> aReadWriteListOfString;


    public PlainJavaClass()
    {
        throw new RuntimeException("instantiation is bad!");
    }

    public PlainJavaClass(int i)
    {
        throw new RuntimeException("instantiation is bad!");
    }

    @TestAnnotation public PlainJavaClass(int i, @TestAnnotation String s)
    {
        this(i);
        System.out.println(s);
    }

    public int getAReadOnlyInt()
    {
        return aReadOnlyInt;
    }

    public int getAReadWriteInt()
    {
        return aReadWriteInt;
    }

    public void setAReadWriteInt(int i)
    {
        aReadWriteInt = i;
    }

    public List<String> getAReadWriteListOfString()
    {
        return aReadWriteListOfString;
    }

    public void setAReadWriteListOfString(List<String> l)
    {
        aReadWriteListOfString = l;
    }

    public boolean isAReadOnlyBoolean()
    {
        return aReadOnlyBoolean;
    }


    public int getAnAnnotatedInt()
    {
        return anAnnotatedInt;
    }

    public void setAnAnnotatedInt(int i)
    {
        anAnnotatedInt = i;
    }

    public void aNormalMethod(int i)
    {
        System.out.println(i);
    }

    @TestAnnotation public int anAnnotatedIntMethod()
    {
        return 1;
    }

    @TestAnnotation public int getIntWithAnnotatedReader()
    {
        return 1;
    }

    public void setIntWithAnnotatedReader(int i)
    {
    }

    public void setMethodWithMisleadingName(int i)
    {
    }
}
