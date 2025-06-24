# TraitInterfaces

This library focuses on an `@interface` macro to declare interfaces and an 
`@instance` macro to declare implementations of those interfaces. These 
implementations are identified with Julia values, which we think of as *traits*.

Although Julia works via type-based dispatch under the hood, we will use Holy
traits to have the feel of controlling dispatch via explicit choice of an 
implementation, rather than the types of the arguments.

An interface principally has types and operations. We will allow types in the 
interface to depend on terms. For example, one may have types for wires, ports,
and boxes if one were designing an interface for wiring diagrams.

```
    ⌜--⌝--[]
[]--|  |    ⌜--⌝
    ⌞--⌟----|  |---[]
            ⌞--⌟
```

Now we *could* say there is a single type, `Ports`, 
each of which happens to have a box, and likewise for wires:

```julia 
@interface ThWiringDiagram begin 
  Box::TYPE 
  Port::TYPE
  Wire::TYPE
  box(p::Port)::Box
  src(w::Wire)::Port
  tgt(w::Wire)::Port
end
```

However, we could also characterize this as having, for each box `b`, a type 
`Ports{Val{b}}` which knows which box it is a port of. 

```julia 
@interface ThWiringDiagram begin 
  Box::TYPE 
  Port(box)::TYPE ⊣ [box::Box]
  Wire(src, tgt)::TYPE ⊣ [(b₁,b₂)::Box, src::Port(b₁), tgt::Port(b₂)]
end
```

This is nice expressivity for typechecking expressions, though Julia isn't a
natural fit for value-parameterized types. Julia *does* have nice
type-parameterized types, and sometimes we can take values and promote them to
types (e.g. this works with integers, but not vectors).

# Relation to GATLab 

This repo is the core of GATlab with the GAT (generalized algebraic theory) 
aspects stripped away. Many projects in the AlgebraicJulia ecosystem rely on 
interfaces without using the understanding of interfaces as being the objects 
of category. GATlab will focus on this latter goal.
