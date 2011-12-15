using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Tests
{
    public class NonGenericClass1
    {
        public NonGenericClass1() { }
        public T InstanceGenericMethod<T>() { return default(T); }
        public T InstanceGenericMethod<T>(T x) { return x; }

        public void InstanceVoidMethod() { }
        public int InstanceNonVoidMethod() { return 0; }

        //to test shadowing
        public static int Stopwatch() { return 0; }

        public static int static_property_int { get { return static_field_int; } set { static_field_int = value; } }
        public static int static_field_int = 0;
        public static decimal static_field_decimal3 = 3M;

        public static NonGenericClass1 static_field_ngc1 = new NonGenericClass1();
        public NonGenericClass2 instance_field_ngc2 = new NonGenericClass2();

        public int instance_property_int { get { return instance_field_int; } set { instance_field_int = value; } }
        public NonGenericStruct1 instance_property_ngs1 { get { return new NonGenericStruct1(); } }

        public int instance_field_int = 0;
        public decimal instance_field_decimal3 = 3.0M;

        public int instance_property_int_without_setter { get { return 0; } }
        public int instance_property_int_without_getter { set { return; } }

        public static int static_property_int_without_setter { get { return 0; } }
        public static int static_property_int_without_getter { set { return; } }

        public int instance_property_string { get { return 0; } }
    }

    public class NonGenericClass2
    {
        public NonGenericClass2() { } //being explicit for clarity
        public int instance_field_int = 0;
        public NonGenericStruct1 instance_property_ngs1 { get { return new NonGenericStruct1(); } }
        public int InstanceNonGenericMethod() { return 0; }
    }

    public struct NonGenericStruct1
    {
        public int instance_field_int; //default is 0
        public int instance_property_int { get { return instance_field_int; } set { instance_field_int = value;  } } 
        public int InstanceNonGenericMethod() { return 0; }
    }

    public static class StaticGenericClass1<T>
    {
        public static R StaticGenericMethod<R>() { return default(R); }
    }
}
