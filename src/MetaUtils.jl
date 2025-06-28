""" General-purpose tools for metaprogramming in Julia.
"""
module MetaUtils
export JuliaFunction, setimpl, setname, JuliaFunctionSig, parse_docstring, 
       parse_function, parse_function_sig, generate_docstring, 
       generate_function, strip_lines, Expr0, fqmn

using Base.Meta: ParseError
using StructEquality
using MLStyle: @match

# Data types
############

const Expr0 = Union{Symbol,Expr}

"""
An explicit representation of the data of a Julia method
"""
@struct_hash_equal struct JuliaFunction
  name::Expr0
  args::Vector{Expr0}
  kwargs::Vector{Expr0}
  whereparams::Vector{Expr0}
  return_type::Union{Expr0,Nothing}
  impl::Union{Expr,Nothing}
  doc::Union{String,Nothing}
  
  function JuliaFunction(name::Expr0, args=Expr0[], kwargs=Expr0[], whereparams=Expr0[],
                         return_type=nothing, impl=nothing, doc=nothing)
    new(name, args, kwargs, whereparams, return_type, impl, doc)
  end

  function JuliaFunction(;name::Expr0, args=Expr0[], kwargs=Expr0[], whereparams=Expr0[],
                         return_type=nothing, impl=nothing, doc=nothing)
    new(name, args, kwargs, whereparams, return_type, impl, doc)
  end
end

Base.nameof(f::JuliaFunction) = f.name

setimpl(f::JuliaFunction, impl) =
  JuliaFunction(f.name, f.args, f.kwargs, f.whereparams, f.return_type, impl, f.doc)

setname(f::JuliaFunction, name) =
  JuliaFunction(name, f.args, f.kwargs, f.whereparams, f.return_type, f.impl, f.doc)

has_empty_arg_type(fun::JuliaFunction) = any(parse_function_sig(fun).types) do x 
  x == :(Union{})
end

""" Just the type signature of a method """
@struct_hash_equal struct JuliaFunctionSig
  name::Expr0
  types::Vector{Expr0}
  whereparams::Vector{Expr0}
  function JuliaFunctionSig(name::Expr0, types::Vector, 
                            whereparams::Vector{<:Expr0}=Expr0[])
    new(name, types, whereparams)
  end
end

# Parsing Julia functions
#########################

""" Parse Julia expression that is (possibly) annotated with docstring.
"""
function parse_docstring(expr::Expr)::Tuple{Union{String,Nothing},Expr}
  expr = strip_lines(expr)
  if expr.head == :macrocall && (
      # XXX: It seems that the @doc macro can show up in two forms.
      expr.args[1] == GlobalRef(Core, Symbol("@doc")) ||
      expr.args[1] == Expr(:core, Symbol("@doc")))
    (expr.args[2], expr.args[3])
  else
    (nothing, expr)
  end
end

""" Parse Julia function definition into standardized form.
"""
function parse_function(expr::Expr)::JuliaFunction
  doc, expr = parse_docstring(expr)
  fun_expr, impl = @match expr begin
    Expr(:(=), args...) => args
    Expr(:function, args...) => args
    _ => throw(ParseError("Ill-formed function definition $expr"))
  end
  fun_head, whereparams = @match fun_expr begin
    Expr(:where, fun_head, whereparams...) => (fun_head, whereparams)
    _ => (fun_expr, Expr0[])
  end
  (call_expr, return_type, impl, doc) = @match fun_head begin
    Expr(:(::), Expr(:call, args...), return_type) =>
      (Expr(:call, args...), return_type, impl, doc)
    Expr(:call, args...) =>
      (Expr(:call, args...), nothing, impl, doc)
    _ => throw(ParseError("Ill-formed function header $fun_head"))
  end
  (name, args, kwargs) = @match call_expr begin
    Expr(:call, name, Expr(:parameters, kwargs...), args...) => (name, args, kwargs)
    Expr(:call, name, args...) => (name, args, Expr0[])
    _ => throw(ParseError("Ill-formed call expression $call_expr"))
  end
  args = map(args) do arg
    @match arg begin
      Expr(:(::), x, T) => arg
      x::Symbol => :($x::Any)
      Expr(:(::), T) => Expr(:(::), gensym(), T)
      _ => throw(ParseError("Ill-formed argument expression $arg"))
    end
  end
  JuliaFunction(name, args, kwargs, whereparams, return_type, impl, doc)
end

""" Parse signature of Julia function.
"""
function parse_function_sig(fun::JuliaFunction)::JuliaFunctionSig
  types = map(fun.args) do expr
    @match expr begin
      Expr(:(::), _, type) => type
      Expr(:(::), type) => type
      _ => :Any
    end
  end
  JuliaFunctionSig(fun.name, types, fun.whereparams)
end

""" Wrap Julia expression with docstring.
"""
function generate_docstring(expr::Expr, doc::Union{String,Nothing})::Expr
  if isnothing(doc)
    expr
  else
    Expr(:macrocall, GlobalRef(Core, Symbol("@doc")),
         LineNumberNode(0), doc, expr)
  end
end

""" Generate Julia expression for function definition.
"""
function generate_function(fun::JuliaFunction; rename=n->n)::Expr
  has_empty_arg_type(fun) && return :() # workaround for Julia bug
  kwargsblock = if !isempty(fun.kwargs)
    [Expr(:parameters, fun.kwargs...)]
  else
    []
  end
  call_expr = Expr(:call, rename(fun.name), kwargsblock..., fun.args...)

  if !isnothing(fun.return_type)
    call_expr = Expr(:(::), call_expr, fun.return_type)
  end

  head = if isempty(fun.whereparams)
    call_expr
  else
    Expr(:where, call_expr, fun.whereparams...)
  end

  body = if isnothing(fun.impl)
    Expr(:block)
  else
    # Wrap implementation inside block if not already.
    impl = fun.impl
    impl.head == :block ? impl : Expr(:block, impl)
  end
  expr = Expr(:function, head, body)
  generate_docstring(expr, fun.doc)
end

# Operations on Julia expressions
#################################

""" Remove all LineNumberNodes from a Julia expression.
"""
function strip_lines(expr::Expr; recurse::Bool=false)::Expr
  args = [ x for x in expr.args if !isa(x, LineNumberNode) ]
  if recurse
    args = [ isa(x, Expr) ? strip_lines(x; recurse=true) : x for x in args ]
  end
  Expr(expr.head, args...)
end

""" Fully Qualified Module Name """
function fqmn(mod::Module)
  names = Symbol[]
  while parentmodule(mod) != mod
    push!(names, nameof(mod))
    mod = parentmodule(mod)
  end
  push!(names, nameof(mod))
  reverse(names)
end

""" 
Evaluate an expression `A.B.C.Foo` by calling `eval` on `A` and then
iteratively calling `getproperty`
"""
fq_eval(v::Vector{Symbol}) = foldl(getproperty, [eval(first(v)), v[2:end]...])

end # module
