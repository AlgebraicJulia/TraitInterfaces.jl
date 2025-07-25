# TraitInterfaces.jl

This library focuses on an `@interface` macro to declare interfaces and an 
`@instance` macro to declare implementations of those interfaces. These 
implementations are identified with Julia values, which we think of as *traits*.

Although Julia ultimately works via type-based dispatch under the hood, we will use [Holy
traits](https://ucidatascienceinitiative.github.io/IntroToJulia/Html/DispatchDesigns#Traits-and-THTT) to have the feel of controlling dispatch via explicit choice of an 
implementation, rather than the types of the arguments.

## Usage

To first approximation, an interface principally consists in the declarations of **types** and **operations**. 

### The basics: [animal example](test/examples.jl)

Borrowing an example from [Rust docs](https://doc.rust-lang.org/rust-by-example/trait.html): we declare an interface `AnimalInterface`, which says any implementation of `AnimalInterface` must provide a means (called `name`) of providing a `Base.String` and another means (called `noise`) of providing a `Base.String`. 

```julia
@interface AnimalInterface begin
  @import String::TYPE
  name()::String
  noise()::String
end 
```

The `@import` macro inside the interface declaration declares that the name `String` should be bound from the external context (the type is "fixed", or *concrete*). Because `name` did not have a `@import`, this is an "abstract" method which needs to implemented by anyone who says they can implement the interface `AnimalInterface`. Let's look at such an implementation:

```julia
struct Sheep 
  naked::Bool 
  name::String 
end

joe = Sheep(true, "Joe")
```

Declaring an `@instance` for this interface with `[model::Sheep]` says, if one had a `Sheep` (e.g. `joe`) then we'd have a way of satisfying the `AnimalInterface` interface:

```julia
@instance AnimalInterface [model::Sheep] begin 
  name()::String = model.name
  noise()::String = model.naked ? "baaaaah?" : "baaaaah!"
end

@test name[joe]() == "Joe"
@test noise[joe]() == "baaaaah?"
```

We could have modeled this another way, using an *abstract type* (to be distinguished from Julia's built in notion of `abstract type`) in our interface to represent the animal. Implementers of the interface must supply a Julia type for these abstract types in addition to the supplying methods for the abstract operations.

```julia
@interface AnimalInterface′ begin
  Species::TYPE         # meaning determined by user input at @instance decl
  @import String::TYPE  # meaning determined by context of @interface decl
  name(s::Species)::String
  noise(s::Species)::String
end 

struct SheepImplsAnimalTrait end
trait = SheepImplsAnimalTrait()
```

Below we see the `{...}` syntax for assigning Julia types to the abstract type of the interface: 

```julia
@instance AnimalInterface′{Species=Sheep} [model::SheepImplsAnimalTrait] begin 
  name(s::Sheep)::String = s.name
  noise(s::Sheep)::String = s.naked ? "baaaaah?" : "baaaaah!"
end

@test name[trait](joe) == "Joe" # equivalent to name(Trait(trait), joe)
@test noise[trait](joe) == "baaaaah?"
```

This second approach is closer to how most people think of Holy traits: they're usually zero-field structs which simply exist to pick a certain method, rather than having proper data in them. In our first example, the `Sheep` value itself was the trait! It happened to be a more concise solution to this particular problem, but when our interface has methods which involve multiple datatypes, or if we wanted to implement `AnimalInterface` for `Sheep` in two different ways, the second strategy (i.e. using abstract types) is preferred.

### Writing methods generic over interfaces

The result of the `@interface` macro is the production of a *module* `AnimalInterface`. This comes with its own namespace, with a distinguished submodule `Meta` which has things like the underlying data structure of the interface, as well as a macro `@wrapper`. This is used to create a custom type  which wraps a Julia value and checks (upon [construction](https://wiki.haskell.org/index.php?title=Smart_constructors)) that the trait has been implemented.

```julia
AnimalInterface.Meta.@wrapper Animal # defines the Animal wrapper type
joe_animal = Animal(joe) 

# error because there is no `@instance Animal [model::Int] ...` 
@test_throws MethodError Animal(100) 
```

These wrapper types implicitly pass the trait as a first parameter:

```julia
@test name(joe_animal) == "Joe" # equivalent to name[joe]()
```

These wrapper types allow us to write code which generically depends on some unknown / arbitrary implementation of `AnimalInterface`:

```julia 
two_noises(a::Animal, b::Animal) = noise(a) * " and " * noise(b)

@test two_noises(joe_animal, joe_animal) == "baaaaah? and baaaaah?"

# `joe` is not known at the type level to be an `Animal`, so this errors
# upon trying to call two_noises 
@test_throws MethodError two_noises(joe, joe) 
```

One *could* write some code which *implicitly* expects values which implement a particular interface:

```julia
two_noises_unsafe(a, b) = noise[a]() * " and " * noise(b)

# Works because `joe` happens to implement AnimalInterface
@test two_noises_unsafe(joe, joe) == "baaaaah? and baaaaah?"

# this successfully calls two_noises_unsafe, but now has an error inside
@test_throws MethodError two_noises_unsafe(100, 101)
```

### AbstractArray example

For [AbstractArray](https://docs.julialang.org/en/v1/manual/interfaces/), like above we have the ability to either explicitly put a type parameter in our interface for the array type *or* directly make the array type itself be the trait. Given that the below subset of the interface has exactly one mention of the array type in every method, we choose the latter option.

```julia 
@interface ThAbstractArray begin
  @import Int::TYPE
  @import Tuple::TYPE

  V::TYPE # value type

  size()::Tuple{Vararg{Int}}
  getindex(i::Vararg{Int})::V
  length()::Int

  function length() # default implementation
    prod(size[model]())
  end
end

ThAbstractArray.Meta.@wrapper AbsArray

@instance ThAbstractArray{V=T} [model::Vector{T}] where T begin 
  size()::Tuple{Vararg{Int}} = model.size
  function getindex(i′::Int...)
    i = only(i′)
    @boundscheck checkbounds(model, i)
    Core.memoryrefget(Core.memoryrefnew(model.ref, i, false), 
                      :not_atomic, false)
  end
end

arr = AbsArray([2,4,6])

@test length(arr) == 3
@test arr[2] == 4
```

### Axioms, aliases, extensions, importing: [algebraic theory example](test/Renaming.jl)

To showcase some more features of our interfaces, let's consider the algebraic theory of [monoids](https://en.wikipedia.org/wiki/Monoid): this is an interface that a datatype may or may not implement (or, implement in many different ways). This interface says that some abstract type must be equipped with a multiplication operation and a distinguished unit term. The unit must be a do-nothing element when multiplied with. Furthermore, this multiplication must be associative: this means, even though multiplication is defined as a binary operator, all that matters for evaluating some big multiplication `a ⋅ b ⋅ c ⋅ ... ⋅ z` is the order of the elements, rather than how we chose to parenthesize it as a bunch of binary applications of `⋅`.

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

We can give a name to a fixed composition of operations within a theory:

```julia 
@interface ThRingSquare <: ThRing begin
  square(a::X) := a × a
end
```

The implementation of such operations is automatically derived when creating an instance:

```julia
@instance ThRingSquare{Int} [model::Modulo] begin
  # nothing extra is needed to implement square
end

@test implements(ℤm3, ThRingSquare)

@test square[ℤm3](2) == 1
```

### Dependent types: wiring diagram example 

One may have types for wires, ports, and boxes if one were designing an interface for wiring diagrams.

```
    ⌜--⌝--[]
[]--|  |    ⌜--⌝
    ⌞--⌟----|  |---[]
            ⌞--⌟
```

Now we *could* say there is a single type, `Ports`, each of which happens to have a box, and likewise for wires:

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

This is nice expressivity for typechecking expressions, though Julia isn't a natural fit for value-parameterized types. Julia *does* have nice type-parameterized types, and sometimes we can take values and promote them to types (e.g. this works with integers, but not vectors).

## How it works

In this section, we'll see what `@interface`, `@wrapper`, and `@implements` desugar to in order to gain an understanding of how TraitInterfaces.jl works. These will be slightly editorialized for readability and clarity.

### `@interface` macro
Let's start with an example from above, supposing this is being defined in some module `Foo`:

```julia
@interface AnimalInterface′ begin
  Species::TYPE         # 'abstract type'
  @import String::TYPE  # 'concrete type'
  name(s::Species)::String
  noise(s::Species)::String
end 
```

We start with putting the abstract types and operations into the namespace where `@interface` is being declared. Then these are imported into a newly created module:

```julia
function name end 
function noise end 
function Species end 

module AnimalInterface′
  export name, noise, Species
  import ..Foo: name, noise, Species
  module Meta
    struct T end # A special type associated with the interface

    # Copy of the Julia data structure that stores the content of the interface
    const theory = Interface(:AnimalInterface′, Judgment[...])

    macro wrapper(n)
      ... # to be explained below
    end
  end
end
```

For convenience, we add `getindex` methods so that we can call `my_operation[implementation](args...)` to avoid requiring explicit `Trait()` wrapping. E.g.:

```julia
Base.getindex(::typeof(name), m::Any) = (args...; kw...) -> name(Trait(m), args...; kw...)
Base.getindex(::typeof(noise), m::Any) = (args...; kw...) -> noise(Trait(m), args...; kw...)
```

### `@implements` macro

Now, given the following implementation:

```julia 

struct SheepImplsAnimalTrait end

@instance AnimalInterface′{Species=Sheep} [model::SheepImplsAnimalTrait] begin 
  name(s::Sheep)::String = s.name
  noise(s::Sheep)::String = s.naked ? "baaaaah?" : "baaaaah!"
end
```

We first generate the following methods:

```julia
function AnimalInterface′.name(m::Trait{<:SheepImplsAnimalTrait}, s::Sheep)::String
    let model = m.value
        s.name # code that was explicitly written by user appears here
    end
end

function AnimalInterface′.noise(m::Trait{<:SheepImplsAnimalTrait}, s::Sheep)::String
    let model = (m).value
      s.naked ? "baaaaah?" : "baaaaah!"
    end
end
```

Then we generate code to check whether the interface has been fully implemented:

```julia
if !(hasmethod(AnimalInterface′.noise, Tuple{Trait{SheepImplsAnimalTrait}, Sheep}))
  throw(MissingMethodImplementation(...))
end
# likewise for noise
```

Lastly we store the information of how this implementation assigned concrete types to the abstract type of the interface:

```julia
impl_type(::SheepImplsAnimalTrait, ::typeof(AnimalInterface′.Species)) = Sheep
```

### `@wrapper` macro

Let's look at what happens when we declare that `Bar <: Baz` is a wrapper type for implementations of `AnimalInterface′` via the code `AnimalInterface′.Meta.@wrapper Bar <: Baz`:

```julia
@struct_hash_equal struct Bar <: Baz
    val::Any
    types::Dict{Symbol, Type}
    function Bar(x::Any)
        types = try
          Dict(:Species => impl_type(x, AnimalInterface′.Species))
        catch _
          error("Invalid $AnimalInterface′ model: $x")
        end
        new(x, types)
    end
end

Base.get(x::Bar) = x.val
impl_type(x::Bar, o::Symbol) = x.types[o]
Animal′.noise(x::Bar, args...; kw...) = Animal′.noise(Trait(x.val), args...; kw...)
Animal′.name(x::Bar, args...; kw...) = Animal′.name(Trait(x.val), args...; kw...)
```

## Relation to GATLab 

This repo is the core of [GATlab.jl](https://github.com/AlgebraicJulia/GATlab.jl) ([arXiv](https://arxiv.org/abs/2404.04837)), developed by Owen Lynch and Kris Brown, based on Evan Patterson's original work on GATs (generalized algebraic theories) in Catlab. In this repo, the GAT aspects have been stripped away. Many projects in the AlgebraicJulia ecosystem rely on interfaces without using the understanding of interfaces as being the objects of category. GATlab will focus on this latter goal.

However, it's important that a certain well-behaved subset of interfaces can be reasoned about compositionally, so that, when one (inevitably) wants to *change* ones interface or combine interfaces in nontrivial ways, it is possible to have good automation for the migration of implementations of those interfaces, too. GATlab will aim to provide this.

## Other interface packages

It would be nice to compare the features of TraitInterfaces.jl with the following packages:

- [WhereTraits.jl](https://github.com/jolin-io/WhereTraits.jl)
- [SimpleTraits.jl](https://github.com/mauro3/SimpleTraits.jl)
- [CanonicalTraits.jl](https://github.com/thautwarm/CanonicalTraits.jl)
- [TraitWrappers.jl](https://github.com/xiaodaigh/TraitWrappers.jl)
- [Interfaces.jl](https://github.com/rafaqz/Interfaces.jl)
- [RequiredInterfaces.jl](https://github.com/Seelengrab/RequiredInterfaces.jl)
- [DuckDispatch.jl](https://github.com/mrufsvold/DuckDispatch.jl)
- [MultipleInterfaces](https://github.com/CameronBieganek/MultipleInterfaces.jl)

## Documentation

To locally build the documentation and the literate code examples, run the following in the command line:

```
julia --project=docs -e "using TraitInterfaces, LiveServer; servedocs(literate_dir=\"docs/literate\",skip_dir=\"docs/src/generated\")"
```

## Caveat

This library is currently under active development, and so is not yet at a point where a constant API/behavior can be assumed. That being said, if this project looks interesting/relevant please contact us and [let us know](https://www.algebraicjulia.org/#contributing)!

