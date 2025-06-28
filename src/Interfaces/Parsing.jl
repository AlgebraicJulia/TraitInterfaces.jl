"""
Code for parsing the @interface DSL
"""
module Parsing

using MLStyle
using Markdown
using ...MetaUtils: fqmn, fq_eval
using ..Algorithms: rename
using ..Interfaces, ..Algorithms, ..Syntax
using ..InterfaceData: add_judgment!, add_alias!, allnames

""" Parse markdown coming out of @doc programmatically. """
mdp(::Nothing) = ""
mdp(x::Markdown.MD) = x
function mdp(x::Base.Docs.DocStr)
  Markdown.parse(only(x.text))
end

unquote(s::Symbol) = s 
unquote(s::QuoteNode) = s.value

function parse_line!(theory::Interface, e::Expr, linenumber, 
                     current_module::Vector{Symbol})
  try
    @match e begin
      Expr(:tuple, arg1, args...) => begin
        @match arg1 begin 
          Expr(:call, :(=>), Expr(:using, Expr(:(:), Expr(:(.), base_theory), 
               Expr(:(.), first_key))), first_val) => begin
            args′ = map(args) do arg
              (arg.head, arg.args[1]) == (:call, :(=>)) || error("Bad")
              unquote(arg.args[2]) => unquote(arg.args[3])
            end
            rename_dict = Dict{Symbol, Symbol}([
              unquote(first_key) => unquote(first_val); args′])
            I = fq_eval([current_module; base_theory]).Meta.theory
            union!(theory, rename(I, rename_dict))
          end
          _ => error("Cannot parse")
        end 
      end
      Expr(:macrocall, mac, _, aliasexpr) => begin
        lines = @match aliasexpr begin
          Expr(:block, lines...) => lines
          _ => [aliasexpr]
        end
        if mac == Symbol("@op")
          for line in lines
            @switch line begin
              @case (_::LineNumberNode)
                nothing
              @case :($alias := $name)
                # check if already a declaration for name, if not, create decl
                alias ∈ allnames(theory) && error(
                  "Cannot declare alias $alias for $name: $alias already exists")
                name ∈ allnames(theory) || error(
                  "Cannot declare alias $alias for $name: $name doesn't exist")

                add_alias!(theory, alias, name)
              @case _
                error("could not match @op expression $line")
            end
          end
        elseif mac == Symbol("@import")
          line = only(lines)
          iⱼ = parse_binding_line!(theory, line, linenumber)
          n = nameof(theory[iⱼ])
          val = fq_eval([current_module; n])
          theory.external[n] = fqmn(parentmodule(val))
        else
          error("Unexpected pseudomacro $mac")
        end
      end
      _ => parse_binding_line!(theory, e, linenumber)
    end
  catch _
    error("error parsing expression $e at line $linenumber")
  end
end

""" Results in a new judgment. Return the idx of this judgment """
function parse_binding_line!(theory::Interface, e, linenumber)::Int
  e = normalize_judgment(e)

  (binding, localcontext) = @match e begin
    Expr(:call, :(⊣), binding, ctxexpr) && if ctxexpr.head == :vect end =>
      (binding, fromexpr(theory, ctxexpr, TypeScope))
    e => (e, TypeScope())
  end

  (head, type_expr) = @match binding begin
    Expr(:(::), head, type_expr) => (head, type_expr)
    _ => (binding, nothing)
  end

  @match head begin
    Expr(:(:=), name, equation) => @match equation begin 
      Expr(:call, :(==), a,b) => begin 
        parseaxiom!(theory, localcontext, type_expr, [a,b]; name)
      end
      Expr(:comparison, args...) => begin 
          parseaxiom!(theory, localcontext, type_expr, args[1:2:end]; name)
      end
      _ => parsefunction!(theory, localcontext, type_expr, name, equation)
    end
    Expr(:call, :(==), t1, t2) => 
      parseaxiom!(theory, localcontext, type_expr, [t1,t2])
    Expr(:comparison, args...) => 
      parseaxiom!(theory, localcontext, type_expr, args[1:2:end])
    _ => parseconstructor!(theory, localcontext, type_expr, head)
  end
end

function parseconstructor!(theory::Interface, localcontext, type_expr, e)::Int
  (name, arglist) = @match e begin
    Expr(:call, name, args...) => (name, args)
    name::Symbol => (name, [])
    _ => error("failed to parse head of term constructor $e")
  end
  args = parseargs!(theory, arglist, localcontext)
  @match type_expr begin
    :TYPE => add_judgment!(theory, TypeConstructor(name, localcontext, args))
    _ => begin
      type = @match type_expr begin 
        Expr(:vect, _...) => fromexpr(theory, localcontext, type_expr, TypeScope)
        _ => parsetype(theory, localcontext, type_expr)
      end
      add_judgment!(theory, TermConstructor(name, localcontext, args, type))
    end
  end
end

fromexpr(theory::Interface, e, ::Type{TypeScope}) = 
  fromexpr(theory, TypeScope(), e, TypeScope)

function fromexpr(theory::Interface, scope, e, ::Type{TypeScope})
  ts = scope

  p!(name, type_expr) = 
    push!(ts, name => parsetype(theory, scope, type_expr))

  for a in e.args 
    @match a begin
      a::Symbol => p!(a, :Default)
      Expr(:tuple, names...) => p!.(names, Ref(:Default))
      Expr(:(::), Expr(:tuple, names...), T) => p!.(names, Ref(T))
      Expr(:(::), name, T) => p!(name, T)
      _ => error("invalid binding expression $e")
    end
  end
  ts
end


function parseargs!(theory::Interface, exprs::AbstractVector, scope::TypeScope)
  linenumber = nothing
  Vector{Int}(filter(x->x isa Int, map(exprs) do expr
    binding_expr = @match expr begin
      a::Symbol => getlid(ident(scope; name=a))
      l::LineNumberNode => begin linenumber = l end
      :($a :: $T) => begin
        binding = parse_binding(theory, scope, expr)
        push!(scope, binding)
        length(scope)
      end
      _ => error("invalid argument expression $expr")
    end
  end))
end

function parse_methodapp(theory, scope, fun::Symbol, argexprs)
  args = Vector{AlgTerm}(parseterm.(Ref(theory), Ref(scope), argexprs))
  MethodApp(get(theory.aliases, fun, fun), args)
end

function parse_binding(theory, scope, e)
  @match e begin
    Expr(:(::), name::Symbol, type_expr) =>
        name => parsetype(theory, scope, type_expr)
    _ => error("could not parse binding of name to type from $e")
  end
end

function parsetype(theory, scope, e)::AlgType
  @match e begin
    s::Symbol => AlgType(parse_methodapp(theory, scope, s, []))
    Expr(:call, head, args...) && if head != :(==) end =>
      AlgType(parse_methodapp(theory, scope, head, args))
    _ => error("could not parse AlgType from $e")
  end
end

function parseterm(theory::Interface, localcontext, e)
  @match e begin
    s::Symbol => begin
      value = localcontext[s]
      AlgTerm(s)
    end
    Expr(:call, head::Symbol, argexprs...) => 
      AlgTerm(parse_methodapp(theory, localcontext, head, argexprs))
    Expr(:(::), val, type) => 
      AlgTerm(Constant(val, fromexpr(theory, localcontext, type, AlgType)))
    e::Expr => error("could not parse AlgTerm from $e")
    _ => error("Cannot parse $e")
  end
end

function parseaxiom!(theory::Interface, localcontext, sort_expr, terms; name=nothing)
  equands = parseterm.(Ref(theory), Ref(localcontext), terms)
  sorts = sortcheck.(Ref(theory), Ref(localcontext), equands)
  @assert allequal(sorts)
  sort = if isnothing(sort_expr)
    first(sorts)
  else
    fromexpr(c, sort_expr, AlgSort)
  end
  ax = AlgAxiom(name, localcontext, sort, equands)
  add_judgment!(theory, ax)
end


"""
This is necessary because the intuitive precedence rules for the symbols that we
use do not match the Julia precedence rules. In theory, this could in principle
be written with some algorithm that recalculates precedence.
"""
function normalize_judgment(e)
  @match e begin
    :($name := $lhs == $rhs :: $typ ⊣ $ctx) => :((($name := ($lhs == $rhs)) :: $typ) ⊣ $ctx)
    :($name := $lhs == $ms == $rhs :: $typ ⊣ $ctx) => :((($name := ($lhs == $ms == $rhs)) :: $typ) ⊣ $ctx)
    :($lhs == $rhs :: $typ ⊣ $ctx) => :((($lhs == $rhs) :: $typ) ⊣ $ctx)
    :($lhs == $ms == $rhs :: $typ ⊣ $ctx) => :((($lhs == $ms == $rhs) :: $typ) ⊣ $ctx)
    :(($lhs == $rhs :: $typ) ⊣ $ctx) => :((($lhs == $rhs) :: $typ) ⊣ $ctx)
    :($trmcon :: $typ ⊣ $ctx) => :(($trmcon :: $typ) ⊣ $ctx)
    :($name := $lhs == $rhs ⊣ $ctx) => :((($name := ($lhs == $rhs))) ⊣ $ctx)
    :($name := $fun ⊣ $ctx) => :(($name := $fun) ⊣ $ctx)
    :($lhs == $rhs ⊣ $ctx) => :(($lhs == $rhs) ⊣ $ctx)
    :($lhs == $ms == $rhs ⊣ $ctx) => :(($lhs == $ms == $rhs) ⊣ $ctx)
    :($(trmcon::Symbol) ⊣ $ctx) => :(($trmcon :: default) ⊣ $ctx)
    :($f($(args...)) ⊣ $ctx) && if f ∉ [:(==), :(⊣)] end => :(($f($(args...)) :: default) ⊣ $ctx)
    trmcon::Symbol => :($trmcon :: default)
    :($f($(args...))) && if f ∉ [:(==), :(⊣)] end => :($e :: default)
    _ => e
  end
end

end # module
