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
    }

    public static class Test2<T> {
        public static R DoIt1<R>() { return default(R); }
    }
}
