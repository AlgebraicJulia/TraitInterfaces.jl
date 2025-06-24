
"""
Any Julia value can be a trait which implements an interface, `I`. A trait 
`t::T` is considered to be implementing `I` iff, for all operations in `I`
`f(::A,::B,...)`, we have `hasmethod(f, (WithModel{M}, A, B)) == true`. 

Also for each sort `s`, we need `hasmethod(impl_type, (M, I.s) == true`.
"""
module ModelInterface

export implements, impl_type, impl_types, MissingMethodImplementation,
       @instance, @withmodel

using MLStyle
using DataStructures: DefaultDict, OrderedDict

using ...MetaUtils, ...Interfaces
using ...MetaUtils: JuliaFunctionSigNoWhere, fq_eval

import ...Interfaces.InterfaceModules: InterfaceModules, impl_type, implements

"""
An error to throw when an implementation of an interface fails to implement a 
particular method
"""
struct MissingMethodImplementation <: Exception
  theory::Module
  methodname::Function
  argtypes::Vector{String}
  whereparams::Vector
end

function Base.showerror(io::IO, e::MissingMethodImplementation) 
  print(io, "No implementation for $(nameof(e.theory)).$(e.methodname) ") 
  if isempty(e.argtypes) 
    println(io, "(with no arguments)")
  else 
    println(io, "with arg types:\n- "*join(string.(e.argtypes), "\n- "))
    isempty(e.whereparams) || println(io, "where $(join(string.(e.whereparams), ","))")
  end
end

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
function implements(m, theory::Module, x::Symbol, args::Vector{<:Type}, types::Vector{<:Type})
  tc = lookup(theory.Meta.theory, x, args)
  name = nameof(getdecl(tc))
  typedict = Dict(zip(sorts(theory.Meta.theory), types))
  types′ = Type[typedict[AlgSort(getvalue(tc[i]))] for i in tc.args]
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


"""
Usage: (TODO)
"""
macro instance(head, model, body)
  # Parse the head of @instance to get theory and instance types
  (theory_module, instance_types) = @match head begin
    Expr(:curly, ThX, Expr(:(=), a,b),xs...) => begin
      theory = macroexpand(__module__, :($ThX.Meta.@theory))
      abs_sorts = filter(x->!haskey(theory.external, x), nameof.(sorts(theory)))
      nS, nT = length(abs_sorts), length(xs)+1 
      nS == nT || error("$nT types provided ($ThX expected $nS)")
      ThX => map(abs_sorts) do psort
        for x in [Expr(:(=), a,b); xs]
          x.head == :(=) || error("Unexpected type assignment $x")
          n, v = x.args
          n == psort && return v
        end
        error("Sort $psort not found in $xs")
      end
    end
    :($ThX{$(Ts...)}) =>  (ThX, Ts) 
    (ThX::Symbol) =>  (ThX, Expr[]) 
    _ => error("invalid syntax for head of @instance macro: $head")
  end

  # Get the underlying theory
  theory = macroexpand(__module__, :($theory_module.Meta.@theory))
  abs_sorts = filter(x->!haskey(theory.external, nameof(x)), sorts(theory))

  nS, nT = length(abs_sorts), length(instance_types) 
  nS == nT || error("$nT types provided ($theory_module expected $nS)")

  # A dictionary to look up the Julia type of a type constructor from its name (an ident)
  jltype_by_sort = Dict{AlgSort,Expr0}([
    zip(abs_sorts, instance_types)..., 
    [AlgSort(s) => foldl((x,y)->Expr(:., x, QuoteNode(y)), [theory.external[s]; s]) 
     for s in nameof.(sorts(theory)) if haskey(theory.external,s)]...
  ]) 

  # Get the model type that we are overloading for, or nothing if this is the
  # default instance for `instance_types`
  model_type, whereparams = parse_model_param(model)


  # Create the actual instance
  generate_instance(theory, theory_module, jltype_by_sort, model_type, whereparams, body)
end


function generate_instance(
  theory::Interface,
  theory_module::Union{Expr0, Module},
  jltype_by_sort::Dict{AlgSort},
  model_type::Union{Expr0, Nothing},
  whereparams::AbstractVector,
  body::Expr;
  escape=true
)

  # Parse the body into functions defined here and functions defined elsewhere
  typechecked_functions = parse_instance_body(body)

  qualified_functions = []
  for f in typechecked_functions
    f_name = nameof(f) 
    tc = lookup(theory, f_name)  # args for overloaded names?
    tc isa TermConstructor || tc isa AlgAccessor || error("Only implement operations, not $tc")
    qf = qualify_function(f, theory_module, model_type, whereparams)
    push!(qualified_functions, qf)
  end

  impl_type_declarations = []
  for (k, v) in collect(jltype_by_sort)
    haskey(theory.external, nameof(k)) && continue
    decl = impl_type_declaration(theory_module, model_type, whereparams, k, v)
    push!(impl_type_declarations, decl)
  end

  runtime_impl_checks = map(collect(theory.ops)) do i
    typecheck_runtime(theory_module, model_type, whereparams, theory[i], jltype_by_sort)
  end

  docsink = gensym(:docsink)

  code = Expr(:block,
    generate_function.(qualified_functions)...,
    runtime_impl_checks...,
    impl_type_declarations...,
    :(function $docsink end),
    :(Core.@__doc__ $docsink)
  )

  escape ? esc(code) : code
end

macro instance(head, body)
  esc(:(@instance $head $(nothing) $body))
end

function parse_model_param(e)
  paramdecl, whereparams = @match e begin
    Expr(:where, paramdecl, whereparams...) => (paramdecl, whereparams)
    _ => (e, [])
  end

  model_type = @match paramdecl begin
    Expr(:vect, Expr(:(::), :model, model_type)) => model_type
    nothing => nothing
    _ => error("invalid syntax for declaring model type: $paramdecl")
  end

  (model_type, whereparams)
end

"""
Parses the raw julia expression into JuliaFunctions
"""
function parse_instance_body(expr::Expr)::Vector{JuliaFunction}
  @assert expr.head == :block
  Vector{JuliaFunction}(map(strip_lines(expr).args) do elem 
    parse_function(strip_lines(elem))
  end)
end

function args_from_sorts(sorts::Vector{AlgSort}, jltype_by_sort::Dict{AlgSort})
  Expr0[Expr(:(::), gensym(), jltype_by_sort[s]) for s in sorts]
end

# Changed to symbol
function default_typecon_impl(X::Symbol, theory::Interface, jltype_by_sort::Dict{AlgSort})
  typecon = getvalue(theory[X])
  sort = AlgSort(getdecl(typecon), X)
  jltype = jltype_by_sort[sort]
  args = args_from_sorts([sort; sortsignature(typecon)], jltype_by_sort)
  JuliaFunction(
    name = nameof(getdecl(typecon)),
    args = args,
    return_type = jltype,
    impl = :(return $(args[1].args[1])),
  )
end

# Changed to symbol
function default_accessor_impl(x::Symbol, theory::Interface, jltype_by_sort::Dict{AlgSort})
  acc = getvalue(theory[x])
  sort = AlgSort(acc.typecondecl, acc.typecon)
  jltype = jltype_by_sort[sort]
  errormsg = "$(acc) not defined for $(jltype)"
  JuliaFunction(;
    name = nameof(getdecl(acc)),
    args = Expr0[Expr(:(::), jltype)],
    impl = :(error($errormsg * " in model $model"))
  )
end

# Changed to symbol
julia_signature(theory::Interface, x::Symbol, jltype_by_sort::Dict{AlgSort}) = 
  julia_signature(getvalue(theory[x]), jltype_by_sort; X=x)

function julia_signature(
  termcon::TermConstructor,
  jltype_by_sort::Dict{AlgSort};
  oldinstance=false, kw...
)
  sortsig = sortsignature(termcon)
  args = if oldinstance && isempty(sortsig)
    Expr0[Expr(:curly, :Type, jltype_by_sort[AlgSort(termcon.type)])]
  else
    Expr0[jltype_by_sort[sort] for sort in sortsig if !GATs.iseq(sort)]
  end
  JuliaFunctionSig(
    nameof(getdecl(termcon)),
    args
  )
end

function julia_signature(
  typecon::TypeConstructor,
  jltype_by_sort::Dict{AlgSort};
  X, kw...
)
  decl = getdecl(typecon)
  sort = AlgSort(decl, X)
  JuliaFunctionSig(
    nameof(decl),
    Expr0[jltype_by_sort[sort] for sort in [sort, sortsignature(typecon)...]]
  )
end

function julia_signature(
  acc::AlgAccessor,
  jltype_by_sort::Dict{AlgSort};
  kw...
)
  jlargtype = jltype_by_sort[AlgSort(acc.typecondecl, acc.typecon)]
  JuliaFunctionSig(nameof(getdecl(acc)), [jlargtype])
end


function toexpr(sig::JuliaFunctionSig)
  Expr(:call, sig.name, [Expr(:(::), type) for type in sig.types]...)
end
toexpr(sig::JuliaFunctionSigNoWhere) = 
  toexpr(sig |> JuliaFunctionSig)


"""
Add `WithModel` param first, if this is not an old instance.
(it shouldn't have it already). Also qualify method name to be in theory module.
"""
function qualify_function(fun::JuliaFunction, theory_module, model_type::Union{Expr0, Nothing}, whereparams)
  (args, impl) = if !isnothing(model_type)
    args = map(fun.args) do arg
      @match arg begin
        Expr(:(::), argname, ty) => Expr(:(::), argname, ty )
        _ => arg
      end
    end

    m = gensym(:m)
    (
      [Expr(:(::), m, Expr(:curly, InterfaceModules.WithModel, Expr(:<:, model_type))), args...],
      Expr(:let, Expr(:(=), :model, :($m.model)), fun.impl)
    )
  else
    (fun.args, Expr(:let, Expr(:(=), :model, nothing), fun.impl))
  end

  JuliaFunction(
    Expr(:., theory_module, QuoteNode(fun.name)),
    args,
    [],
    vcat(fun.whereparams, whereparams),
    fun.return_type,
    impl,
    fun.doc
  )
end

function impl_type_declaration(theory_module, model_type, whereparams, sort, jltype)
  methd = Expr(:., theory_module, QuoteNode(nameof(sort)))
  quote 
    if !hasmethod($(GlobalRef(ModelInterface, :impl_type)), 
      ($(model_type) where {$(whereparams...)}, typeof($methd)))
      @inline $(GlobalRef(ModelInterface, :impl_type))(
        ::$(model_type), ::typeof($methd)) where {$(whereparams...)} = $(jltype)
    end
  end
end

""" Check if a method exists """ # x is a symbol, was an ident
function typecheck_runtime(theory_name, model_type, whereparams, tc, jltype)
  name = nameof(tc)
  wm = :($(GlobalRef(ModelInterface, :WithModel)){$model_type})
  jltypes = [jltype[AlgSort(tc.localcontext[i][2])] for i in tc.args]

  # For default models, nullary constructors are handled funnily
  isnothing(model_type) && isempty(jltypes) && return :()

  jltypes′ = isnothing(model_type) ? jltypes : [wm, jltypes...]
  jltypes′′ = map(jltypes) do t 
    Expr(:where, t, whereparams...)
  end
  quote
    any(==(Union{}), [$(jltypes′′...)]
       ) || hasmethod($(theory_name).$(name), Tuple{$(jltypes′...)} where {$(whereparams...)}
                     )  || throw($(GlobalRef(ModelInterface, :MissingMethodImplementation)
                                  )($((theory_name)), $name, $(string.(jltypes)), $whereparams)
    )
  end
end

"""
Automatically add `WithModel` trait parameter to some specified methods in a 
code block.
"""
macro withmodel(model, subsexpr, body)
  modelvar = gensym("model")

  # e.g., (ℕ, Z, S) => [ℕ, Z, S]
  subs = @match subsexpr begin
    Expr(:tuple, subs...) => [subs...]
    sub::Symbol => [sub]
  end

  # gensym these subs
  subvars = gensym.(subs) # e.g. #25compose to avoid global method overloading

  # set gensym(ℕ) = ℕ, etc.
  subvardefs = [
    Expr(:(=), var, sub)
    for (sub, var) in zip(subs, subvars)
  ]

  # set ℕ = (args...; kwargs...) -> gensym(ℕ)(MyModel, args...; kwargs...) 
  subdefs = [
    Expr(:(=), sub, :((args...;kwargs...) -> $var($modelvar, args...;kwargs...)))
    for (sub, var) in zip(subs, subvars)
  ] 

  esc(
    Expr(:let,
      Expr(:block, :($modelvar = $(Expr(:call, InterfaceModules.WithModel, model))), subvardefs...),
      Expr(:let,
        Expr(:block, subdefs...),
        body
      )
    )
  )
end


end # module
