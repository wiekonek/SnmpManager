using System;
using SnmpMib;


namespace Manager
{
  class Program
  {
    static void Main(string[] args)
    {
      Console.WriteLine("Hello World!");
      var parser = new Parsing.Parser("ezasy");
      var asd = new Parsing.RawObjectType();
    }
  }
}
