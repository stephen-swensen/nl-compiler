using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Tests
{
    public class Test1 {
        public Test1() { }
        public T DoIt<T>() { return default(T); }
        public T DoIt2<T>(T x) { return x; }
        public static int Stopwatch() { return 0; }
        public static int sf = 0;

        public static Test1 sfld = new Test1();
        public Test3 ifld = new Test3();

        public int t1_iprop1 { get { return 0; } }
        public Struct1 t1_iprop2 { get { return new Struct1(); } }

        //public int t1_ifld2 = 0;
    }

    public static class Test2<T> {
        public static R DoIt1<R>() { return default(R); }
    }

    public class Test3
    {
        public int ifld = 0;
    }

    public struct Struct1
    {
        public int s1_ifld1;
        public int s1_iprop1 { get { return 0; } }
    }
}
