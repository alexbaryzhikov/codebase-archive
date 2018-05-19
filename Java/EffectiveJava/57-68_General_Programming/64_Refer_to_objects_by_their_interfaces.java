// NOTE If appropriate interface types exist, then parameters, return values, variables, and fields
// should all be declared using interface types.


// Good - uses interface as type
List<Subscriber> subscribers = new Vector<>();

// Bad - uses class as type!
Vector<Subscriber> subscribers = new Vector<>();


// NOTE If you get into the habit of using interfaces as types, your program will be much more
// flexible.


List<Subscriber> subscribers = new ArrayList<>();


// NOTE It is entirely appropriate to refer to an object by a class rather than an interface if no
// appropriate interface exists.
