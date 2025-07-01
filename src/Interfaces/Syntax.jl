""" The building blocks that make up an interface """
module Syntax 

export MethodApp, AlgSort, AlgTerm, AlgType, Judgment, TypeScope, Maybe,
       TypeConstructor, TermConstructor, AlgAxiom, AlgAccessor, signature,
       AlgFunction

using StructEquality

const Maybe{T} = Union{Nothing,T}

# Types and terms
#################

""" Only subtyped by AlgTerm - need this to avoid a circular type definition """
abstract type AbstractAlgTerm end

"""
`MethodApp`

An application of a method of a constructor to arguments. We need a type parameter
`T` because `AlgTerm` hasn't been defined yet, but the only type used for `T` will
in fact be `AlgTerm`.

`method` is the name of a `TermConstructor`, a `TypeConstructor` or an
`AlgAccessor`.
"""
@struct_hash_equal struct MethodApp
  method::Symbol
  args::Vector{AbstractAlgTerm}
end


"""
`AlgSort`

A *sort*, which is essentially a type constructor without arguments
"""
@struct_hash_equal struct AlgSort
  method::Symbol
end

Base.nameof(s::AlgSort) = s.method

"""
A term, which is a variable name (Symbol) in some context or the application 
of an operation to some other terms.
"""
@struct_hash_equal struct AlgTerm <: AbstractAlgTerm
  body::Union{Symbol, MethodApp}
end

Base.get(t::AlgTerm) = t.body

function AlgTerm(fun::Symbol, args::Vector{AlgTerm})
  AlgTerm(MethodApp(fun, args))
end

"""
A (possibly dependent) type, represented as a type constructor applied to some 
terms.
"""
@struct_hash_equal struct AlgType 
  body::MethodApp
end

Base.get(t::AlgType) = t.body

"""
A type constructor, alternatively thought of as a (possibly-dependent) type 
where we have forgotten the arguments it depends on. E.g. given a type 
`Matrix{3,4}` for 3 × 4 matrices, the corresponding sort is `Matrix`.
"""
AlgSort(a::AlgType) = AlgSort(a.body.method) # discard the top level arguments

#############
# Judgments #
#############

""" Judgments constitute the content of an interface """
abstract type Judgment end

Base.nameof(t::Judgment) = t.name

# TypeScopes
#-----------

"""
A context that a term (or type) can live in. It has an (ordered) list of names 
bound to types. Later types can refer to earlier types in the list.

The `lookup` field caches a quick way of getting the type from its name. There 
are no name collisions allowed.
"""
@struct_hash_equal struct TypeScope
  args::Vector{Pair{Symbol, AlgType}}
  kwargs::Vector{Pair{Symbol, AlgType}}
  lookup::Dict{Symbol, AlgType}
  function TypeScope(args,kwargs)
    arg = args ∪ kwargs
    length(unique(first.(arg))) == length(arg) || error("Duplicate names $arg")
    new(args, kwargs, Dict(arg))
  end
end

function Base.show(io::IO, s::TypeScope)   
  print(io, "[")
  join(io, ["$k=$v" for (k,v) in s.args], ",")
  if !isempty(s.kwargs)
    print(io, ";")
    join(io, ["$k=$v" for (k,v) in s.kwargs], ",")
  end
  print(io, "]")
end 

Base.length(t::TypeScope) = length(t.args)

Base.getindex(t::TypeScope, i::Union{Vector{Int},Int}) = t.args[i]

Base.getindex(t::TypeScope, i::Symbol) = t.lookup[i]

function Base.push!(t::TypeScope, x)
  haskey(t.lookup, x[1]) && error("Duplicate key $x")
  t.lookup[x[1]] = x[2]
  push!(t.args, x)
end

TypeScope() = TypeScope(Pair{Symbol, AlgType}[], Pair{Symbol, AlgType}[])

# Type/Term constructors
#-----------------------

# Either a TypeConstructor or a TermConstructor
abstract type TrmTypConstructor <: Judgment end

argsof(t::TrmTypConstructor) = t.args

localcontext(t::TrmTypConstructor) = t.localcontext

signature(t::TrmTypConstructor) = AlgSort.(last.(localcontext(t)[argsof(t)]))

@struct_hash_equal struct TypeConstructor <: TrmTypConstructor
  name::Symbol
  localcontext::TypeScope
  args::Vector{Int} # covering subset of the localcontext
end 

@struct_hash_equal struct TermConstructor <: TrmTypConstructor
  name::Symbol
  localcontext::TypeScope
  args::Vector{Int}
  type::Union{TypeScope,AlgType}
end

@struct_hash_equal struct AlgAxiom <: Judgment
  name::Maybe{Symbol}
  localcontext::TypeScope
  sort::AlgSort
  equands::Vector{AlgTerm}
end

"""
`AlgAccessor`

The arguments to a type/term constructor serve a dual function as both arguments 
and also methods to extract the value of those arguments.

E.g. declaring `Hom(dom::Ob, codom::Ob)::TYPE` implicitly creates projection
operations like `dom(h::Hom)::Ob`.
"""
@struct_hash_equal struct AlgAccessor <: Judgment
  name::Symbol
  typecon::Symbol
  arg::Int
end

"""
A term constructor which is purely derivative on other term constructors, 
such as "square(x) := x * x" (where * is some other term constructor). One need
not specify the implementation of such an operation when declaring an 
implementation.
"""
@struct_hash_equal struct AlgFunction <: TrmTypConstructor
  name::Symbol
  localcontext::TypeScope
  args::Vector{Int}
  value::AlgTerm
end

Base.get(f::AlgFunction) = f.value

end # Module
