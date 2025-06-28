module SpecialModels
export Dispatch, InitialModel, TerminalModel, InitialModel′, TerminalModel′

using ...Interfaces
using ...Interfaces.InterfaceModules: Dispatch, InitialModel′, TerminalModel′
import ..Check: _implements, implements, impl_type


# Public constants
##################
""" The unique term of type `InitialModel′` """
const InitialModel = InitialModel′()

""" The unique term of type `TerminalModel′` """
const TerminalModel = TerminalModel′()

# Accessing Dispatch
####################

Base.haskey(d::Dispatch, k::AlgSort) = haskey(d.jltypes, k) 

Base.getindex(d::Dispatch, a::AlgSort)::Type = d.jltypes[a]

function Base.getindex(d::Dispatch, x::Symbol)::Type
  get(d)[AlgSort(x)]
end

# Implements
#############

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

function implements(::InitialModel′, ::Module, ::Symbol, types=nothing) 
  isnothing(types) || all(==(Union{}), types)
end

_implements(::InitialModel′, ::Module, ::Symbol, types::Vector{<:Type}) =
  all(==(Union{}), types)

function implements(::TerminalModel′, ::Module, ::Symbol, types=nothing) 
  isnothing(types) || all(==(Nothing), types)
end

_implements(::TerminalModel′, ::Module, ::Symbol, types::Vector{<:Type}) =
  all(==(Nothing), types)


# Impl Type
###########

impl_type(d::Dispatch, x::Symbol) = d[x]
impl_type(d::Dispatch, x::Function) = d[nameof(x)]

impl_type(::InitialModel′, ::Union{Function, Symbol}) = Union{}

impl_type(::TerminalModel′, ::Union{Function, Symbol}) = Nothing


end # module
