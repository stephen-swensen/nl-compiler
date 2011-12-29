using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Tests
{
    public enum Int32Enum { A=0, B=1, C=2 }

    public class NonGenericClass1
    {
        public static readonly int static_readonly_field_int;

        static NonGenericClass1()
        {
            static_readonly_field_int = 3;
        }

        public readonly int instance_readonly_field_int;

        public NonGenericClass1() {
            instance_readonly_field_int = 3;
        }
        public T InstanceGenericMethod<T>() { return default(T); }
        public T InstanceGenericMethod<T>(T x) { return x; }

        public void InstanceVoidMethod() { }
        public int InstanceNonVoidMethod() { return 0; }

        public int InstanceGenericMethodWithThreeTypeArgs<T,R,S>() { return 0; }

        //to test shadowing
        public static int Stopwatch() { return 0; }

        public static int static_property_int { get { return static_field_int; } set { static_field_int = value; } }
        public static int static_field_int = 0;
        public static decimal static_field_decimal3 = 3M;
        public static double static_field_double = 0.0;
        public static NonGenericStruct1 static_field_ngs1 = new NonGenericStruct1();
        public NonGenericStruct1 instance_field_ngs1 = new NonGenericStruct1();

        public static NonGenericClass1 static_field_ngc1 = new NonGenericClass1();
        public NonGenericClass2 instance_field_ngc2 = new NonGenericClass2();

        public int instance_property_int { get { return instance_field_int; } set { instance_field_int = value; } }
        public NonGenericStruct1 instance_property_ngs1 { get { return new NonGenericStruct1(); } }

        public int instance_field_int = 0;
        public decimal instance_field_decimal3 = 3.0M;
        public double instance_field_double = 0.0;

        public int instance_property_int_without_setter { get { return 0; } }
        public int instance_property_int_without_getter { set { return; } }

        public static int static_property_int_without_setter { get { return 0; } }
        public static int static_property_int_without_getter { set { return; } }

        public int instance_property_string { get { return 0; } }

        public static object static_field_object = new object();
        public object instance_field_object = new object();

        public static object static_property_object { get; set; }
        public object instance_property_object { get; set; }

        public static int same_name_different_value = 1;

        public static int same_name_different_kind = 0;

        public const int const_field_int = 0;
        public const object const_field_object = null;
        public const string const_field_string = "hello world";
        public const long const_field_int64 = 0L;
        public const Int32Enum const_field_int32enum = Int32Enum.A;
    }

    public class NonGenericClass2
    {
        public NonGenericClass2() { } //being explicit for clarity
        public int instance_field_int = 0;
        public NonGenericStruct1 instance_property_ngs1 { get { return new NonGenericStruct1(); } }
        public int InstanceNonGenericMethod() { return 0; }

        public static int same_name_different_value = 2;
        public static int same_name_different_kind() { return 0; }
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
