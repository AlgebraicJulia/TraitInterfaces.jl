""" Defining the data structure which stores data of an interface """
module InterfaceData

export Interface, lookup, sorts, abstract_sorts, signature

using StructEquality
using DataStructures

using ..Syntax

"""
An interface has types, operations, and equations.

These may be abstract. An *implementation* provides concrete (Julia) types and 
methods for any abstract types and operations in the theory.

We can ascribe aliases to various types or operations.

Operations can be *overloaded*, meaning we can distinguish operations that have 
the same name if they have different type signatures. This should be used with 
caution, as implementations of the interface which assign the same concrete type 
to distinct abstract types can then cause method ambiguities.
"""
@struct_hash_equal struct Interface
  name::Symbol
  judgments::Vector{Judgment}
  aliases::Dict{Symbol, Symbol}
  external::Dict{Symbol, Pair{Module,Vector{Symbol}}} # give a fully-qualified module name
  defaults::Dict{Int, Expr}

  # cached data for easy access
  types::Set{Int}
  ops::Set{Int}
  accessors::Set{Int}
  axioms::Set{Int}
  lookup::DefaultDict{Symbol, Dict{Vector{AlgSort}, Int}}
end

""" Create an empty interface - just provide the name of the interface """
function Interface(s::Symbol) 
  aliases = Dict{Symbol,Symbol}()
  external = Dict{Symbol, Vector{Symbol}}()
  defaults = Dict{Int, Expr}()
  lookup = DefaultDict{Symbol, Dict{Vector{AlgSort}, Int}}(
            () -> Dict{Vector{AlgSort}, Int}())
  Interface(s, Judgment[], aliases, external, defaults,
            Set{Int}(), Set{Int}(), Set{Int}(), Set{Int}(), lookup)
end

# Data access
#############

""" 
From a name, get a type or an operation.

`sorts` not required unless there is overloading 
"""
function lookup(i::Interface, s::Symbol, sorts::Maybe{Vector{AlgSort}}=nothing)
  d = i.lookup[s]
  
  sortkey = if isnothing(sorts)
    isempty(keys(d)) && error("Cannot find any methods for $s")
    only(keys(d)) 
  else 
    sorts
  end

  haskey(d, sortkey) || error("Cannot find $s method with sorts $sorts \n \
    alternatives: $(keys(d))")
  i[d[sortkey]]
end

Base.getindex(f::Interface, i) = f.judgments[i]

Base.copy(f::Interface) = Interface(f.name, deepcopy(f.judgments), 
  copy(f.aliases), copy(f.external), deepcopy(f.defaults), 
  copy(f.types), copy(f.ops), 
  copy(f.accessors), copy(f.axioms), deepcopy(f.lookup))

Base.length(f::Interface) = length(f.judgments)

Base.in(x::Judgment, f::Interface) = x ∈ f.judgments

""" All type/term constructors as well as accessors """
allnames(f::Interface) = nameof.(filter(x->!(x isa AlgAxiom), f.judgments))

sorts(f::Interface) = AlgSort.(nameof.(f[sort(collect(f.types))]))

abstract_sorts(Th::Interface) = 
  filter(s->!haskey(Th.external, nameof(s)), sorts(Th))

# Mutating 
###########

""" Combine judgments from two interfaces, removing duplicates """
function Base.union!(I::Interface, J::Interface)
  for j in J.judgments
    j ∈ I || add_judgment!(I, j)
  end
  for (k, v) in J.aliases
    add_alias!(I, k, v)
  end
  I
end

function add_alias!(i::Interface, alias::Symbol, name::Symbol)
  if haskey(i.aliases, alias)
    i.aliases[alias] == name || error("Cannot add conflicting alias $alias => \
      $name (already bound to $(i.aliases[alias]))")
  else 
    i.aliases[alias] = name
  end
end

function add_judgment!(i::Interface, j::Judgment) 
  push!(i.judgments, j)
  n = length(i.judgments)
  _add_judgment!(i, j, n) # initialization specific to the kind of judgment
  n
end

function _add_judgment!(i::Interface, c::AlgAccessor, n::Int)
  push!(i.accessors, n)
  add_lookup!(i, c.name, [AlgSort(c.typecon)], n)
end

AlgSorts(v::Vector) = Vector{AlgSort}(AlgSort.(v))

function _add_judgment!(i::Interface, c::TypeConstructor, n::Int)
  push!(i.types, n)
  c_args = c.localcontext[c.args]
  add_lookup!(i, c.name, AlgSorts(last.(c_args)), n)

  for (idx, (argname, _)) in enumerate(c_args) # should we also store the type?
    add_judgment!(i, AlgAccessor(argname, c.name, idx))
  end
end

function _add_judgment!(i::Interface, c::TermConstructor, n::Int)
  push!(i.ops, n)
  c_args = c.localcontext[c.args]
  add_lookup!(i, c.name, AlgSorts(last.(c_args)), n)
end

function _add_judgment!(i::Interface, c::AlgFunction, n::Int)
  push!(i.ops, n)
  c_args = c.localcontext[c.args]
  add_lookup!(i, c.name, AlgSorts(last.(c_args)), n)
end

_add_judgment!(i::Interface, ::AlgAxiom, n::Int) = push!(i.axioms, n)

function add_lookup!(f::Interface, n::Symbol, ty::Vector{AlgSort}, i::Int)
  haskey(f.lookup[n], ty) && error(
    "Duplicate name + args combo! $(f.lookup[n]) $ty")
  f.lookup[n][ty] = i
end

end # module
