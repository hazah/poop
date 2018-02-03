# Pure Object Oriented Programming

A thought experiment in a minimalistic, pure language expression.

## Features

1. Everything Is An Object (tm)
   Really! Everything is actually an object

2. All operations are messsages sent to objects

3. Minimalistic syntax inspired by Haskell & Smalltalk

4. Multiparadigm Programming (huh?)

### Examples

The standard `Hello World!` intro

```
create Module
    Main <- put "Hello, POOP!!"
    
```

Getting us some objects...

```
bob <- create Object
    name <- "Bob"
    salary <- 70000
```

The languages does provide a Class object to make it easier to
design prototypes for use as templates for object creation.


```
Employee <- create Class
    name <- Void    -- Default values
    salary <- Void  -- Void ensures that any access results in a fatal error.

bob <- create Employee
    name <- "Bob"
    salary <- 70000
```

Flow control...

```

x <- y

if (x == y) then ->
    print "equal"
else ->
    print "nothing alike"

-- equal

y <- 5
x <- 0

while (x /= y) do ->
    print x
    x <- x + 1

-- 01234
```

### Explanations

POOP follows a terse principle of simplicity and directness. Its philosophy is unsophisticated and is captured by the phrase "sending messages to objects." This is reflected in the syntax `message object <args>`.

To assign an object to a `Variable` (which is itself another object) we use the `<-` operator. The variable will have to be sent the `evaluate` message to retrieve the object. This is usually handled by the runtime. To assign a `Block` of code to a variable the assignment arrow switches from `<-` to `->`. With blocks, `evaluate` is sent first to the variable to get to the block, and then a second time to the retrieved block with the applicable arguments. This is also usually handled by the runtime. However, it is possible to work directly with these objects as well, and therefore knowledge of them is more than an implementation detail.

`main -> print "Hello!"` is a syntatic sugar for
```
main <- create Block
    ...
```
where `...` is an the internal object state responsible for the `evaluate` response of the block.

Blocks assigned to properties of objects will be used the implementation of methods. Methods are created in response to messages sent to objects. A `Method` is potentially chained if the property used to refer to it exists in the prototypes of the object. To refer to the next corresponding method in a prototype, the method provides the `super` property. And to refer to the receiving object it provides the `self` property

Some other cases where `evaluate` is applicable are expressions. The object created from the expression `2 + 2` is *not* `4`. It is an `Expression` object holding a tree representing the expression. In order to "collapse" this tree into an actual result, `evaluate` is used. In short, `evaluate` is used a *lot*. Furthermore, following the expression evaluation pattern enables one to use the language with the functional paradigm.

Though there is a `Class` object, there are no true classes. Instead, prototypes are used. An object has one or more prototypes. The simplest case is just `Object`, but any object can used. `Class` is provided as a convenient means to manage the creation of objects who's purpose is to serve as only as prototypes by making the operations of combining various objects into a single prototype. The main purpose of prototype multiplicity in an object is to facilitate direct management of cross cutting concerns (AOP).

In its simplest form `Class` allows for class based OOP. However, its broader purpose is to capture a particular class of problems into a single object.

Arguments are passed through keywords, for explicit name binding between message and method. This means that control flow can be expressed as simple message sending to objects with blocks as arguments. Evaluating named blocks along with control flow facilitate the procedural pardigm.

Blocks of code, which are objects, can be assigned to variables and then attached to an object. To indicate the block of code that starts the program we send the `create` message to `Module` and assign to that new object's `Main` property a block of code to execute as a method. The runtime system will look through the module objects' properties, when it finds one called `Main` it treats it as a block of code and triggers the beginning of execution of our program. Properties assigned to module objects control what objects are made available to other modules. Otherwise these objects remain private to the module itself. Module objects are used to assemble the execution context object of the whole program. This object gives access to the variables used and chained within blocks as they evaluate. This enables modular programming.

Thus POOP programming is multiparadigm expressed in pure object oriented terms.

## Runtime

The runtime is implemented in Haskell, which means the objects are **not** represented by the underlying hardware, instead they are implemented by an abstract data type. Memory is therefore not managed. Instead, all that is managed is the object graph itself. All that is required is inserting or removing objects from this graph. Though strickly not necessary, especially for smaller programs, this graph can be pruned, both manually and automatically. By default, a simple reference counting algorithm removes objects from the graph as soon as all references to this object dissapear from the context object. It is possible to change this behaviour by implementing a different algorithm. Its even possible to completely disable it. This would mean that objects will remain in the graph but will be inaccessible to the code. This is _**not recommended**_ unless some pruning algorithm is invoked as part of the program manually, which, in theory could potentially restore access to the inaccessible objects instead of removing them.

There are some objects that the runtime lazily creates on the fly. For instance, objects that are represented by literals are created when they are first encountered. The literal then becomes a built in variable like object that represents its value.

Some objects, like methods, and intermediate objects used in expression evaluations, are created for the lifetime of a single expression and are then discarded because they are only relevant for that particular execution context. Since their execution can change this context, new method objects, reflecting these changes, are used for each call. These objects are never inserted into the graph unless assigned to a variable, and therefore do not need management.