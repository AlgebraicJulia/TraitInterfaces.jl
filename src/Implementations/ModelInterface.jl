"""
Any Julia value can be a trait which implements an interface, `I`. A trait 
`t::T` is considered to be implementing `I` iff, for all operations in `I`
`f(::A,::B,...)`, we have `hasmethod(f, (Trait{M}, A, B)) == true`. 
"""
module ModelInterface

export MissingMethodImplementation, @instance, @withmodel

using MLStyle
using DataStructures: DefaultDict, OrderedDict

using ...MetaUtils, ...Interfaces
using ...MetaUtils: fq_eval
import ...MetaUtils: JuliaFunction
using ...Interfaces.Algorithms: sortcheck

import ...Interfaces.InterfaceModules: InterfaceModules, impl_type, implements

# Custom errors 
###############
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

# Fixed recipes for implementations of AlgFunctions
###################################################

function alg_function(theory_module::Expr0, I::Interface, f::AlgFunction,   
                      jltype_by_sort::Dict, whereparams)
  argnames = OrderedDict(map(f.args) do i 
    sym, _ = f.localcontext[i]
    sym => gensym(sym)
  end)
  args = map(zip(f.args, argnames)) do (i, (_, argname))
    ty = jltype_by_sort[AlgSort(f.localcontext[i][2])]
    Expr(:(::), argname, ty)
  end
  ret = jltype_by_sort[sortcheck(I, f.localcontext, get(f))]
  body = term_to_expr(get(f), theory_module, argnames)
  JuliaFunction(nameof(f), args, Expr0[], whereparams, ret, body)
end

term_to_expr(t::AlgTerm, mod, argnames) = term_to_expr(get(t), mod, argnames)

term_to_expr(t::Symbol, _, argnames) = argnames[t]

term_to_expr(m::TermApp, mod, argnames) = :($(mod).$(m.method)[model](  
  $(term_to_expr.(m.args, Ref(mod), Ref(argnames))...)))


# Declaring implementations of interfaces
#########################################

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
  generate_instance(__module__, theory, theory_module, jltype_by_sort, 
                    model_type, whereparams, body)
end

"""
We need to look at the method *after* having forgotten the dependent types
and still be able to tell what method was being implemented. Ambiguity can
arise. Check there is only one possibility given the sorts provided.
"""
function get_judgment_runtime(instance_module,theory, f_name, args, 
                              jltype_by_sort, whereparams)
  where_eval(x) = instance_module.eval(Expr(:where, x, whereparams...))
  j_eval = Dict(k=>where_eval(v) for (k,v) in jltype_by_sort)
  srt_poss = map(args) do argexp 
    @match argexp begin 
      Expr(:(::), _, jtype) => begin 
        srts = []
        T = where_eval(jtype)
        for s in sorts(theory) 
          if eval(j_eval[s]) == T
            push!(srts, s)
          elseif Vararg{j_eval[s]} == T 
            push!(srts, AlgSort(nameof(s), true))
          end
        end
        srts
      end
    end
  end
  combos = if isempty(srt_poss)
    [AlgSort[]]
  else 
    collect(Vector{AlgSort}.(collect.(Iterators.product(srt_poss...))))
  end
  tcs = map(combos) do combo
    try lookup(theory, f_name, combo) catch e nothing end
  end
  # TODO also potentially disambiguate using the return type.
  only(filter(x->!isnothing(x), tcs))
end


function generate_instance(
  instance_module,
  theory::Interface,
  theory_module::Expr0,
  jltype_by_sort::Dict{AlgSort},
  model_type::Union{Expr0, Nothing},
  whereparams::AbstractVector,
  body::Expr;
  escape=true
)

  # Parse the body into functions defined here and functions defined elsewhere
  typechecked_functions = parse_instance_body(body)

  fixed_functions = map(filter(j -> j isa AlgFunction, theory.judgments)) do j 
    alg_function(theory_module, theory, j, jltype_by_sort, whereparams)
  end

  qualified_functions = map(typechecked_functions ∪ fixed_functions) do f 
    f_name = nameof(f)
    tc = get_judgment_runtime(instance_module, theory, f_name, f.args, 
                              jltype_by_sort, vcat(whereparams, f.whereparams))

    tc isa TermConstructor || tc isa AlgAccessor || tc isa AlgFunction || error(
      "Only implement operations, not $tc")
    qualify_function(f, theory_module, model_type, whereparams)
  end

  impl_type_declarations = []
  for (k, v) in collect(jltype_by_sort)
    haskey(theory.external, nameof(k)) && continue
    decl = impl_type_declaration(theory_module, model_type, whereparams, k, v)
    push!(impl_type_declarations, decl)
  end
  
  runtime_impl_checks = map(collect(theory.ops)) do i
    typecheck_runtime(theory_module, theory, model_type, whereparams, theory[i], jltype_by_sort)
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

""" Parses a raw julia block expression in @instance into JuliaFunctions """
function parse_instance_body(expr::Expr)::Vector{JuliaFunction}
  @assert expr.head == :block
  Vector{JuliaFunction}(map(strip_lines(expr).args) do elem 
    parse_function(strip_lines(elem))
  end)
end

function args_from_sorts(sorts::Vector{AlgSort}, jltype_by_sort::Dict{AlgSort})
  Expr0[Expr(:(::), gensym(), jltype_by_sort[s]) for s in sorts]
end

"""
Does some preprocessing on the user-written method:

1. Add `Trait` param first (it shouldn't have it already). 
2. qualify method name to be in theory module.

If an interface `ThCategory` with operation `id(x::Ob)::Hom` is implemented via:

```
@instance ThCategory{Ob=Foo, Hom=Bar} [model::Baz] begin 
  id(x::Foo)::Bar = ...
end
```

... then we syntactically modify this method to be:

```
ThCategory.id(model::Trait{<:Bar}, x::Foo) = ...
```
"""
function qualify_function(fun::JuliaFunction, theory_module, 
                          model_type::Union{Expr0, Nothing}, whereparams)
  (args, impl) = if !isnothing(model_type)
    m = gensym(:m)
    (
      [Expr(:(::), m, Expr(:curly, InterfaceModules.Trait, Expr(:<:, model_type))), fun.args...],
      Expr(:let, Expr(:(=), :model, :($m.value)), fun.impl)
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

"""
Register the concrete Julia type associated with a given interface, type in that
interface, and model which implements that type.

E.g. if the interface `ThCategory` has a type `Ob` then 

`@instance ThCategory{Ob=Foo, Hom=Bar} [model::Baz]` should generate the 
following code:

```
impl_type(::Baz, ThCategory.Ob) = Foo
```
"""
function impl_type_declaration(theory_module, model_type, whereparams, sort, jltype)
  isnothing(model_type) && return :() # this only makes sense for explicit models
  methd = Expr(:., theory_module, QuoteNode(nameof(sort)))
  quote 
    if !hasmethod($(GlobalRef(ModelInterface, :impl_type)), 
      ($(model_type) where {$(whereparams...)}, typeof($methd)))
      @inline $(GlobalRef(ModelInterface, :impl_type))(
        ::$(model_type), ::typeof($methd)) where {$(whereparams...)} = $(jltype)
    end
  end
end

""" 
Check that an instance properly implements some interface method.

E.g. if the interface ThCategory has a method `compose(f::Hom, g::Hom)`

then `@instance ThCategory{Ob=Foo, Hom=Bar} [model::Baz]` should generate the 
following code, to throw an error if the body of the macro didn't provide a 
method of the expected type:

```
hasmethod(ThCategory.compose, (Trait{<:Baz}, Bar, Bar)) || throw(
  MissingMethodImplementation(...))
```
"""
function typecheck_runtime(theory_module, theory, model_type,
                           whereparams, tc, jltype)
  name = nameof(tc)
  wm = :($(GlobalRef(ModelInterface, :Trait)){$model_type})
  jltypes = map(tc.args) do i
    ty = tc.localcontext[i][2]
    runtime_type(theory, ty, jltype)
  end

  # For default models, nullary constructors are handled funnily
  isnothing(model_type) && isempty(jltypes) && return :()

  jltypes′ = isnothing(model_type) ? jltypes : [wm, jltypes...]
  jltypes′′ = map(jltypes) do t 
    Expr(:where, t, whereparams...)
  end
  err = :($(GlobalRef(ModelInterface, :MissingMethodImplementation)
           )($((theory.name)), $name, $(string.(jltypes)), $whereparams))
  i = findfirst(==(tc), theory.judgments)

  # what to do if we don't find a method with the right types
  no_method = if !haskey(theory.defaults, i)
    Expr(:call, :throw, err)
  else
    args = map(tc.args) do argᵢ
      argname, argtype = tc.localcontext.args[argᵢ]
      argtype = jltype[AlgSort(argtype)]
      Expr(:(::), argname, argtype)
    end
    return_type = jltype[AlgSort(tc.type)]
    f = JuliaFunction(name, args, Expr0[], whereparams, return_type, 
                      theory.defaults[i], nothing)
    q = qualify_function(f, theory_module, model_type, Expr0[])
    generate_function(q)
  end
  
  quote
    if !any(==(Union{}), [$(jltypes′′...)]) 
      if !hasmethod($(theory_module).$(name), 
                      Tuple{$(jltypes′...)} where {$(whereparams...)})
        $(no_method)
      end
    end
  end
end

function runtime_type(theory::Interface, t::TypeApp, jltypes::Dict)
  base = jltypes[AlgSort(t)]
  params = runtime_type.(Ref(theory), t.params, Ref(jltypes))
  isempty(params) ? base : Expr(:curly, base, params...)
end

function runtime_type(theory::Interface, t::VarArgType, jltypes::Dict)
  Expr(:curly, :Vararg, runtime_type(theory, get(t), jltypes))
end


"""
Automatically add `Trait` trait parameter to some specified methods in a 
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
      Expr(:block, :($modelvar = $(Expr(:call, InterfaceModules.Trait, model))), subvardefs...),
      Expr(:let,
        Expr(:block, subdefs...),
        body
      )
    )
  )
end


end # module
