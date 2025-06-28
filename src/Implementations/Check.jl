""" 
Runtime checking of method table to see if a trait has implemented a 
particular interface 
"""
module Check 

export implements, impl_type, impl_types

using ...MetaUtils: fq_eval
using ...Interfaces
import ...Interfaces.InterfaceModules: impl_type, implements

"""
Check whether a model implements a particular theory.

If no types are provided, then we look up whether or not `impl_type` methods 
exist for this model + theory. If not, we will get a MethodError and assume 
that the model does not implement the theory. (WARNING: occasionally one has 
a complex type, such as `foo(Int,String)` which itself leads to a MethodError, 
and this can be confusing because it looks like the model doesn't implement
the theory at all rather than just being an error in how it was implemented).

Once types are provided, we can check whether the theory is implemented by 
checking for each term constructor whether or not the model implements that
(handled by a different `implements` method).
"""
function implements(m, theory_module::Module, types = nothing)
  types = try 
     isnothing(types) ? impl_types(m, theory_module) : types
  catch e
    e isa MethodError && return false
    throw(e)
  end
  T = theory_module.Meta.theory
  type_dict = Dict(zip(abstract_sorts(T), types))
  return all(collect(T.ops)) do o 
    j = T[o]
    implements(m, theory_module, nameof(j), map(signature(j)) do arg 
      haskey(type_dict, arg) && return type_dict[arg]
      n = nameof(arg)
      fq_eval([T.external[n]; n])
    end)
  end
end 

""" User-friendly access to checking if a model implements an operation.

Throws an error if the name is overloaded. Anything programmatic should be 
calling a method which accepts method `Ident`s rather than `Symbol`s.
"""
function implements(m::T, theory_mod::Module, name::Symbol, types=nothing) where T
  isnothing(types) || return _implements(m, theory_mod, name, types)
  theory = theory_mod.Meta.theory
  N = length(lookup(theory, name).args)
  !isempty(methods(getfield(theory_mod, name),
                   (WithModel{T}, fill(Any, N)...)))
end

function _implements(::T, theory::Module, name::Symbol, types::Vector{<:Type}) where T
  f = getfield(theory, name)
  any(==(Union{}), types) && return true # no such methods (Julia 1.10 bug)
  hasmethod(f, Tuple{WithModel{<:T}, types...})
end

""" 
Machine-friendly access to checking if a model implements a particular
operation. The `types` vector is in bijection with the AlgSorts of the
*whole theory*. 
"""  
function implements(m, theory::Module, x::Symbol, args::Vector{<:Type}, 
                    types::Vector{<:Type})
  tc = lookup(theory.Meta.theory, x, args)
  name = nameof(getdecl(tc))
  typedict = Dict(zip(sorts(theory.Meta.theory), types))
  types′ = Type[typedict[AlgSort(get(tc[i]))] for i in tc.args]
  return _implements(m, theory, name, types′)
end

"""
If `m` implements a GAT with a type constructor (identified by ident `id`), 
mapped to a Julia type, this function returns that Julia type.
"""
function impl_types(m, T::Module)
  Th = T.Meta.theory
  map(filter(s->!haskey(Th.external, s), nameof.(sorts(Th)))) do s 
    t = impl_type(m, getproperty(T, s)) 
    t isa Type ? t : error("$s impl_type not a Type: $t")
  end
end 

end # module
