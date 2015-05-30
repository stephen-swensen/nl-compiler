# Type Definitions #

```
//in C#: namespace my.namespace { private class MyType<T> : BaseClass, IInterface } 
private my.namespace.MyType = type[T] : BaseClass, IInterface {
  //instance members
  //default public visibility
  x = 3;;
  private y = 5;;
  //unified syntax fo method and delegates
  f = a[string], b[int] -> a.Count + b;;
  
  //static members
  static {
    z = "2";;
    f = "33";;
  }
}
```