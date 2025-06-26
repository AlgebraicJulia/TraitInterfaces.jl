# TraitInterfaces.jl

This library focuses on an `@interface` macro to declare interfaces and an 
`@instance` macro to declare implementations of those interfaces. These 
implementations are identified with Julia values, which we think of as *traits*.

Although Julia ultimately works via type-based dispatch under the hood, we will use [Holy
traits](https://ucidatascienceinitiative.github.io/IntroToJulia/Html/DispatchDesigns#Traits-and-THTT) to have the feel of controlling dispatch via explicit choice of an 
implementation, rather than the types of the arguments.

To first approximation, an interface principally consists in the declarations of **types** and **operations**. 

## The basics: [animal example](test/examples.jl)

Borrowing an example from [Rust docs](https://doc.rust-lang.org/rust-by-example/trait.html): we declare an interface `Animal`, which says any implementation of `Animal` must provide a means (called `name`) of providing a `Base.String` and another means (called `noise`) of providing a `Base.String`. 

```julia
@interface Animal begin
  @import String::TYPE
  name()::String
  noise()::String
end 
```

The `@import` pseudo-macro inside the interface declaration declares that the name `String` should be bound from the external context (the type is "fixed", or *concrete*). Because `name` did not have a `@import`, this is an "abstract" method which needs to implemented by anyone who says they can implement the interface `Animal`. Let's look at such an implementation:

```julia
struct Sheep 
  naked::Bool 
  name::String 
end

joe = Sheep(true, "Joe")
```

Declaring an `@instance` for this interface with `[model::Sheep]` says, if one had a `Sheep` (e.g. `joe`) then we'd have a way of satisfying the `Animal` interface:

```julia
@instance Animal [model::Sheep] begin 
  name()::String = model.name
  noise()::String = model.naked ? "baaaaah?" : "baaaaah!"
end

@test name[joe]() == "Joe"
@test noise[joe]() == "baaaaah?"
```

We could have modeled this another way, using an *abstract type* in our interface to represent the animal. Implementers of the interface must supply a Julia type for these abstract types in addition to the supplying methods for the abstract operations.

```julia
@interface Animal′ begin
  Species::TYPE 
  @import String::TYPE
  name(s::Species)::String
  noise(s::Species)::String
end 

struct SheepImplsAnimalTrait end
trait = SheepImplsAnimalTrait()
```

Below we see the `{...}` syntax for assigning Julia types to the abstract type of the interface: 

```julia
@instance Animal′{Species=Sheep} [model::SheepImplsAnimalTrait] begin 
  name(s::Sheep)::String = s.name
  noise(s::Sheep)::String = s.naked ? "baaaaah?" : "baaaaah!"
end

@test name[trait](joe) == "Joe"
@test noise[trait](joe) == "baaaaah?"
```

This second approach is closer to how most people think of Holy traits: they're usually zero-field structs which simply exist to pick a certain method, rather than having proper data in them. In our first example, the `Sheep` value itself was the trait! It happened to be a more concise solution to this particular problem, but when our interface has methods which involve multiple datatypes, or if we wanted to implement `Animal` for `Sheep` in two different ways, the second strategy (i.e. using abstract types) is preferred.

## Axioms, aliases, extensions, importing: [algebraic theory example](test/Renaming.jl)

Now let's consider the algebraic theory of [monoids](https://en.wikipedia.org/wiki/Monoid): this is an interface that a datatype may or may not implement (or, implement in many different ways). This interface says that some abstract type must be equipped with a multiplication operation and a distinguished unit term. The unit must be a do-nothing element when multiplied with. Furthermore, this multiplication must be associative: this means, even though multiplication is defined as a binary operator, all that matters for evaluating some big multiplication `a ⋅ b ⋅ c ⋅ ... ⋅ z` is the order of the elements, rather than how we chose to parenthesize it as a bunch of binary applications of `⋅`.

```julia 
@interface ThMonoid begin
  X::TYPE

  unit()::X
  @op (e) := unit

  mul(x::X, y::X)::X
  @op (⋅) := mul

  assoc := (x ⋅ y) ⋅ z == x ⋅ (y⋅ z) ⊣ [(x,y,z)::X]

  unitality := ((unit() ⋅ x) == x == (x ⋅ unit())) ⊣ [x::X]
end
```

This interface uses the `@op` syntax to define an **alias**, which is a shorthand for some other operation or type.

We also declared **axioms** which state how the operations of the interface are supposed to relate to each other. Although we cannot verify an arbitrary implementation of the theory satisfies the axioms, it is good for documentation, plus some downstream tooling can take advantage of this information.

We're actually going to define [rings](https://en.wikipedia.org/wiki/Ring_(mathematics)), which requires saying what a [group](https://en.wikipedia.org/wiki/Group_(mathematics)) is. A group is just like an monoid which also has an inverse operation. Rather than copy all of the `ThMonoid` content again, we can just **extend** the interface using the `<:` syntax:

```julia 
@interface ThGroup <: ThMonoid begin
  inv(x::X)::X
  @op (⁻¹) := inv

  ⁻¹(x) ⋅ x == x == x ⋅ ⁻¹(x) ⊣ [x::X]
end
```

If the order of arguments in the multiplication operation doesn't matter, our group is actually an *abelian* group:

```julia
@interface ThAbGroup <: ThGroup begin
  x⋅y == y⋅x ⊣ [x::X, y::X]
end
```

Now we get to the main definition of a ring: we have two monoids on the same set, thought of as addition and multiplication. Here we want to extend two theories (which themselves overlap with each other, as `ThMonoid ⊂ ThAbGroup`). Here we use the `using` syntax to create copies a theory (with optional renaming). Because the abstract type `X` does not get renamed, it ends up being shared by the two subtheories of `ThRing`.

```julia
@interface ThRing begin
  using ThAbGroup: unit => zero, mul => plus,  :⋅ => :+, e => Z, 
                   inv => minus, ⁻¹ => :-
  using ThMonoid:  unit => one,  mul => times, :⋅ => :×, e => I

  left_distrib  := a × (b + c) == (a × b) + (a × c) ⊣ [(a, b, c)::X]
  right_distrib := (b + c) × a == (b × a) + (c × a) ⊣ [(a, b, c)::X]
end
```

Now that we've defined what it means to be a ring, we can try to implement it. The most familiar example of a ring is the natural numbers with our ordinary notions of `+` and `×`. 

```julia
struct NatRingTrait end
NatRing = NatRingTrait()

@instance ThRing{Int} [model::NatRingTrait] begin 
  one()::Int = 1
  zero()::Int = 0 
  plus(a::Int, b::Int)::Int = a + b
  times(a::Int, b::Int)::Int = a * b
  minus(a::Int)::Int = -a
end

@test ThRing.plus[NatRing](3,4) == 7 
@test ThRing.times[NatRing](3,4) == 12
```

However there are many other rings, even rings where the underlying datatype is `Int`! Modular arthimetic provides an example:

```julia
struct Modulo 
  n::UInt8
end

ℤm3 = Modulo(3)

@instance ThRing{Int} [model::Modulo] begin 
  one()::Int = 1
  zero()::Int = 0 
  plus(a::Int, b::Int)::Int = (a + b) % model.n
  times(a::Int, b::Int)::Int = (a * b) % model.n
  minus(a::Int)::Int = (-a) % model.n
end

@test ThRing.plus[ℤm3](2, 2) == 1
@test ThRing.times[ℤm3](2, 2) == 1
```

## Dependent types: wiring diagram example 

One may have types for wires, ports,
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
  InPort::TYPE
  OutPort::TYPE
  Wire::TYPE
  inbox(p::InPort)::Box
  outbox(p::OutPort)::Box
  src(w::Wire)::OutPort
  tgt(w::Wire)::InPort
end
```

However, we could also characterize this as having, for each box `b`, a type 
`Ports{Val{b}}` which knows which box it is a port of. 

```julia 
@interface ThWiringDiagram begin 
  Box::TYPE 
  InPort(box)::TYPE ⊣ [box::Box]
  OutPort(box)::TYPE ⊣ [box::Box]
  Wire(src, tgt)::TYPE ⊣ 
    [(b₁,b₂)::Box, src::OutPort(b₁), tgt::InPort(b₂)]
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
