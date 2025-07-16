module TestExamples 

using TraitInterfaces, Test
import Base: length, size, getindex

# https://doc.rust-lang.org/rust-by-example/trait.html
#######################################################

@interface Animal begin
  @import String::TYPE
  name()::String
  noise()::String
end

struct Sheep 
  naked::Bool
  name::String 
end

joe = Sheep(true, "Joe")

@instance Animal [model::Sheep] begin 
  name()::String = model.name
  noise()::String = model.naked ? "baaaaah?" : "baaaaah!"
end

@test name[joe]() == "Joe"
@test noise[joe]() == "baaaaah?"

# Another way of doing this 
@interface Animal′ begin
  Species::TYPE 
  @import String::TYPE
  name(s::Species)::String
  noise(s::Species)::String
  function name(s::Species) 
    "John Doe"
  end
end

struct SheepImplsAnimalTrait end
trait = SheepImplsAnimalTrait()

@instance Animal′{Species=Sheep} [model::SheepImplsAnimalTrait] begin 
  name(s::Sheep)::String = s.name
  noise(s::Sheep)::String = s.naked ? "baaaaah?" : "baaaaah!"
end

@test name[trait](joe) == "Joe"
@test noise[trait](joe) == "baaaaah?"

struct NoName end
nntrait = NoName()

@instance Animal′{Species=Sheep} [model::NoName] begin 
  noise(s::Sheep)::String = s.naked ? "baaaaah?" : "baaaaah!"
end

@test name[nntrait](joe) == "John Doe"
@test noise[nntrait](joe) == "baaaaah?"


# No [model::...] is a "default model", is just type dispatch (as if we didn't write @instance)
@instance Animal′{Species=Sheep} begin 
  name(s::Sheep)::String = s.name
  noise(s::Sheep)::String = s.naked ? "baaaaah?" : "baaaaah!"
end

# Disambiguating when the same type implements two interfaces which have 
# (conflicting) methods with the same name.
# https://doc.rust-lang.org/rust-by-example/trait/disambiguating.html
#####################################################################

module NamedModule
  export Named
  using TraitInterfaces: @interface
  @interface Named begin 
    @import String::TYPE
    get()::String 
  end
end
using .NamedModule

@instance Named [model::Sheep] begin 
  get()::String = model.name
end


module NoisedModule 
  export Noised
  using TraitInterfaces: @interface
  @interface Noised begin 
    @import String::TYPE
    get()::String 
  end
end

using .NoisedModule

@instance Noised [model::Sheep] begin 
  get()::String = Animal.noise[model]()
end

@test Named.get[joe]() == "Joe"
@test Noised.get[joe]() == "baaaaah?"

#########################
# Base Julia interfaces #
#########################

using TraitInterfaces, Test

import Base: size, length, iterate

# Iterators
###########

@interface Iterable begin
  @import Nothing::TYPE
  @import Tuple::TYPE
  @import Union::TYPE
  V::TYPE
  State::TYPE
  iterate()::Union{Nothing, Tuple{V,State}}
  iterate(s::State)::Union{Nothing, Tuple{V,State}}

  first()::V

  function first()
    fst = iterate[model]()
    isnothing(fst) && throw(BoundsError(model, 1))
    fst[1]
  end
end

@instance Iterable{V=T, State=Int} [model::Vector{T}] where T begin
  function iterate()
    isempty(model) ? nothing : (model[1], 1)
  end
  function iterate(i::Int)
    length(model) == i ? nothing : (model[i+1],i+1)
  end
end

Tr = Trait([3,4,5])

@test iterate(Tr) == (3,1)
@test iterate(Tr,1) == (4,2)
@test iterate(Tr,2) == (5,3)
@test isnothing(iterate(Tr,3))
@test first(Tr) == 3

# Arrays
########
using TraitInterfaces, Test
import Base: length, size, getindex
@interface ThAbstractArray begin
  @import Int::TYPE
  @import Tuple::TYPE

  V::TYPE # value type
  size()::Tuple{Vararg{Int}}
  getindex(i::Vararg{Int})::V

  length()::Int ⊣ []

  function length()
    prod(size[model]())
  end
end

@instance ThAbstractArray{V=T} [model::Vector{T}] where T begin 
  size()::Tuple{Vararg{Int}} = (length(model),)
  getindex(i::Int...) = model[only(i)] 
end

@test length(Trait([1,2,3])) == 3

struct SquaresVector 
  count::Int 
end

@instance ThAbstractArray{V=Int} [model::SquaresVector] begin 
  size()::Tuple{Vararg{Int}} = (model.count,)
  getindex(i::Int...) = only(i)^2
end

s = SquaresVector(4)

struct SparseArray{T,N}
  data::Dict{NTuple{N,Int}, T}
  dims::NTuple{N,Int}
end


@instance ThAbstractArray{V=T} [model::SparseArray{T,N}] where {T,N} begin 
  size()::Tuple{Vararg{Int}} = model.dims
  getindex(i::Int...) = get(model.data, I, zero(T))

end


end # module
