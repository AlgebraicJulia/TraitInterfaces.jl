"""
*Any* interface can be implemented in some canonical ways: 

1. Just using dispatch
2. Assigning all types in the interface the empty (`Union{}`) type
3. Assigning all types in the interface the singleton (`Nothing`) type
"""
module DispatchImplementations
export Dispatch

using ...Interfaces
import ...Interfaces: Dispatch
import ..Check: _implements, implements, impl_type


# Dispatch
###########

Base.get(d::Dispatch) = d.jltypes

Dispatch(theory_module::Module, types::AbstractVector{<:Type}) = 
  Dispatch(theory_module.Meta.theory, types)

function Dispatch(theory_module::Interface, types::AbstractVector{<:Type}) 
  s = sorts(theory_module)
  length(s) == length(types) || error("Bad length of type vector")
  Dispatch(Dict(zip(s, types)))
end

Base.haskey(d::Dispatch, k::AlgSort) = haskey(d.jltypes, k) 

Base.getindex(d::Dispatch, a::AlgSort)::Type = d.jltypes[a]

function Base.getindex(d::Dispatch, x::Symbol)::Type
  get(d)[AlgSort(x)]
end

# Check if a theory implements one of the special models
########################################################

function implements(::Dispatch, theory_mod::Module, name::Symbol, types=nothing) 
  if isnothing(types)
    !isempty(methods(getfield(theory_mod, name))) # there exists *some* method
  else 
    hasmethod(getfield(theory_mod, name), types) # must be methods with these types
  end
end 

function _implements(::Dispatch, theory::Module, name::Symbol, types::Vector{<:Type}) 
  f = getfield(theory, name)
  any(==(Union{}), types) && return true # no such methods (Julia 1.10 bug)
  hasmethod(f, Tuple{types...})
end


# Impl Type
###########

impl_type(d::Dispatch, x::Symbol) = d[x]

impl_type(d::Dispatch, x::Function) = d[nameof(x)]

end # module
