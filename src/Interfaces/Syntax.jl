""" The things that constitute an interface """
module Syntax 

export MethodApp, AlgSort, AlgTerm, AlgType, Judgment, TypeScope, 
       TypeConstructor, TermConstructor, AlgAxiom, AlgAccessor, signature

using StructEquality

const Maybe{T} = Union{Nothing,T}

#################################
# Building blocks of interfaces #
#################################

"""
`MethodApp`

An application of a method of a constructor to arguments. We need a type parameter
`T` because `AlgTerm` hasn't been defined yet, but the only type used for `T` will
in fact be `AlgTerm`.

`method` is the name of a `TermConstructor`, a `TypeConstructor` or an
`AlgAccessor`.
"""
@struct_hash_equal struct MethodApp{T}
  method::Symbol
  args::Vector{T}
end


"""
`AlgSort`

A *sort*, which is essentially a type constructor without arguments
"""
@struct_hash_equal struct AlgSort
  method::Symbol
end

Base.nameof(s::AlgSort) = s.method

@struct_hash_equal struct AlgTerm
  body::Union{Symbol, MethodApp{AlgTerm}}
end

function AlgTerm(fun::Symbol, args::Vector{AlgTerm})
  AlgTerm(MethodApp{AlgTerm}(fun, args))
end

@struct_hash_equal struct AlgType 
  body::MethodApp{AlgTerm}
end

AlgSort(a::AlgType) = AlgSort(a.body.method) # discard the top level arguments

abstract type Judgment end

Base.nameof(t::Judgment) = t.name

abstract type TrmTypConstructor <: Judgment end

argsof(t::TrmTypConstructor) = t.args
localcontext(t::TrmTypConstructor) = t.localcontext

signature(t::TrmTypConstructor) = AlgSort.(last.(localcontext(t)[argsof(t)]))

"""
A context that a term or type can live in. It has a list of names bound to 
types. 
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

The arguments to a term constructor serve a dual function as both arguments and
also methods to extract the value of those arguments.

I.e., declaring `Hom(dom::Ob, codom::Ob)::TYPE` implicitly creates projection
operations like `dom(h::Hom)::Ob`.
"""
@struct_hash_equal struct AlgAccessor <: Judgment
  name::Symbol
  typecon::Symbol
  arg::Int
end

end # Module

