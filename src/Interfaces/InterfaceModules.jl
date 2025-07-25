module InterfaceModules
export @interface, @theory, Trait, Dispatch

using StructEquality
using MLStyle
using Markdown

using ...MetaUtils: fqmn
using ..Interfaces, ..Syntax, ..Parsing

function impl_type end # to be defined in ModelInterface

function implements end # to be defined in ModelInterface

"""
`Trait` is a wrapper around a Julia value in order to signify it is being 
treated as a trait, i.e. a Holy trait, to be used as the first parameter in 
order to control dispatch.
"""
@struct_hash_equal struct Trait{M}
  value::M
end

Base.get(w::Trait) = w.value

# TODO actually use this
construct(::Trait, A, args...) = A(args...) # default construct

function Base.getindex(::typeof(construct), m::Any)
  cons(A,args...) = construct(Trait(m), A, args...)
end


###########################
# Special Implementations #
###########################
"""
The `Dispatch` implementation of an interface defers to type-dispatch.
"""
@struct_hash_equal struct Dispatch
  jltypes::Dict{AlgSort, Type}
end


####################################
# Constructing an interface module #
####################################

""" Same thing as `@interface` """
macro theory(head, body) 
  theory_impl(head, body, __module__)
end

""" Declare an interface (creates a module), possibly by extending an old one """
macro interface(head, body)
  theory_impl(head, body, __module__)
end

function theory_impl(head, body, __module__)
  (name, parentname) = @match head begin
    (name::Symbol) => (name, nothing)
    Expr(:(<:), name, parent) => (name, parent)
    _ => error("could not parse head of @theory: $head")
  end

  theory = if !isnothing(parentname)
    copy(macroexpand(__module__, :($parentname.Meta.@theory)))
  else
    Interface(name)
  end

  current_module=fqmn(__module__) # UNUSED!!!?

  docstr = repr(theory)
  body.head == :block || error("expected a block to parse into a GAT, got: $e")
  linenumber = nothing

  for line in body.args
    @match line begin
      l::LineNumberNode => begin
        linenumber = l
      end
      _ => Parsing.parse_line!(theory, line, linenumber, current_module)
    end
  end
  theory


  lines = Any[]
  newnames = Symbol[]
  for judgment in theory.judgments
    bname = nameof(judgment)
    if judgment isa TermConstructor || judgment isa AlgAccessor || judgment isa AlgFunction
      push!(lines, juliadeclaration(bname))
      push!(newnames, bname)
    elseif judgment isa TypeConstructor 
      push!(lines, :(function $(bname) end))
      push!(newnames, bname)
    end
  end
  for (k,v) in theory.aliases
    push!(lines, :(const $k = $v))
    push!(newnames, k)
  end

  modulelines = Any[]

  # this exports the names of the module, e.g. :default, :⋅, :e 
  push!(modulelines, :(export $(InterfaceData.allnames(theory)...)))

  push!(
    modulelines,
    Expr(:import,
      Expr(:(:),
        Expr(:(.), :(.), :(.), nameof(__module__)),
        Expr.(Ref(:(.)), newnames)...
      )
    )
  )

  if !isnothing(parentname)
    push!(modulelines, Expr(:using, Expr(:(.), :(.), :(.), parentname)))
  end
  
  doctarget = gensym()
  wrapper_expr = wrapper(name, theory, current_module)
  push!(modulelines, Expr(:toplevel, :(module Meta
    struct T end
   
    const theory = $theory

    macro theory() $theory end
    macro theory_module() parentmodule(@__MODULE__) end

    $wrapper_expr
  end)))

  esc(
    Expr(
      :toplevel,
      :("We start with putting the type and term constructors (as well as \
         accessors) into the namespace where @interface is being declared, if \
         they aren't already."),
      :("If this is the first time we've seen a type/term constructor with this\
          name, then we do some preprocessing:"),
      :("This defines various methods for `getindex`, so that we can call \
         `my_function[implementation]` to access a specific method"),
      lines...,
      :(const $(doctarget) = nothing),
      :("Next we define a module for the interface."),
      :(Core.@__doc__ $(doctarget)),
      :(
        module $name
          $(modulelines...)
        end
      ),
      :(@doc ($(Markdown.MD)($(Parsing.mdp)(@doc $doctarget), $docstr)) $name)
    )
  )
end

# WARNING: if any other package play with indexing methodnames with their own 
# structs, then this code could be broken because it assumes we are the only  
# ones to use this trick.
function juliadeclaration(name::Symbol)
  funname = gensym(name)
  quote

    function $name end

    # we expect just one method because of Dispatch type
    if isempty(Base.methods(Base.getindex, [typeof($name), Any]))

      Base.getindex(f::typeof($name), 
                    ::$(GlobalRef(InterfaceModules, :Dispatch))
                   ) = f

      $name(::$(GlobalRef(InterfaceModules, :Trait)
               ){$(GlobalRef(InterfaceModules, :Dispatch))}, 
            args...
           ) = $name(args...)

      function Base.getindex(::typeof($name), m::Any)
        function $funname(args...) 
          $name($(GlobalRef(InterfaceModules, :Trait))(m), args...)
        end
      end

    end
  end
end

################
# Wrapper type #
################

"""
Given an interface module, take a name (and an abstract type, optionally) and
construct a type with that name which plays the role of wrapping a model of the
interface.
"""
function wrapper(name::Symbol, t::Interface, mod)
  use = Expr(:using, Expr(:., :., :., name))
  quote
    $use
    macro wrapper(n)
      x, y = $(parse_wrapper_input)(n)
      esc(:($($(name)).Meta.@wrapper $x $y))
    end

    macro typed_wrapper(n)
      x, y = $(parse_wrapper_input)(n)
      esc(:($($(name)).Meta.@typed_wrapper $x $y))
    end

    macro wrapper(n, abs)
      doctarget = gensym()
      t = $(name).Meta.theory
      Ts = map($(nameof), $(abstract_sorts)(t))
      Xs = map(Ts) do s 
        :($(GlobalRef($(InterfaceModules), :impl_type))(x, $s))
      end

      Xdict = :(Dict(zip($Ts, [$(Xs...)])))
      it = :($(GlobalRef($(InterfaceModules), :impl_type)))
      esc(quote 
        # Catch any potential docs above the macro call
        const $(doctarget) = nothing
        Core.@__doc__ $(doctarget)

        # Declare the wrapper struct
        struct $n <: $abs
          val::Any
          types::Dict{Symbol, Type}
          function $n(x::Any)
            # TODO opt into checking whether the methods are defined
            # right now we just implicitly check whether the types are defined
            types = try 
              $Xdict
            catch MethodError 
              error("Invalid $($($(name))) model: $x")
            end
            new(x, types)
          end
        end
        # Apply the caught documentation to the new struct
        @doc $($(Parsing.mdp))(@doc $doctarget) $n

        # Define == and hash
        $(Expr(:macrocall, $(GlobalRef(StructEquality, 
                                       Symbol("@struct_hash_equal"))), 
                $(mod), $(:n)))

        Base.get(x::$n) = x.val

        $it(x::$n, o::Symbol) = x.types[o]

        # Dispatch on model value for all declarations in theory
        $(map(collect($(t).ops ∪ $(t).accessors)) do (i)
          j = $(t)[i]
          op = nameof(j)
          :(@inline function $($(name)).$op(x::$(($(:n))), args...; kw...) 
              $($(name)).$op(Trait(x.val), args...; kw...)
          end)
      end...)

      nothing
      end)
    end

    macro typed_wrapper(n, abs)
      doctarget = gensym()
      t = $(name).Meta.theory
      Ts = nameof.($(abstract_sorts)(t))
      Tnames = QuoteNode.(Ts)
      Xs = map(Ts) do s 
        :($(GlobalRef($(InterfaceModules), :impl_type))(x, $s))
      end
      XTs = map(zip(Ts,Xs)) do (T,X)
        :($X <: $T || error("Mismatch $($($(Meta.quot)(T))): $($X) ⊄ $($T)"))
      end

      Xdict = :(Dict(zip($Ts, [$(Xs...)])))

      it = :($(GlobalRef($(InterfaceModules), :impl_type)))

      esc(quote 
        # Catch any potential docs above the macro call
        const $(doctarget) = nothing
        Core.@__doc__ $(doctarget)

        # Declare the wrapper struct
        struct $n{$(Ts...)} <: $abs
          val::Any
          types::Dict{Symbol, Type}

          function $n{$(Ts...)}(x::Any) where {$(Ts...)}
            types = try 
               $Xdict
            catch MethodError 
              error("Invalid $($($(name))) model: $x")
            end
            all(zip([$(Ts...)], [$(Tnames...)])) do (T1,k)
              types[k] <: T1 || error("Bad type for $k: $(types[k]) ⊄ $T1 ")
            end
            new{$(Ts...)}(x, types)
          end

          function $n(x::Any) 
            $($(GlobalRef(InterfaceModules, :implements)))(
              x, $($name), [$(Xs...)]) || error("Invalid $($($(name))) model: $x"
             )
            types = $Xdict
            new{$(Xs...)}(x, types)
          end
        end

        # Apply the caught documentation to the new struct
        @doc $($(Parsing.mdp))(@doc $doctarget) $n

        # Define == and hash
        $(Expr(:macrocall, $(GlobalRef(StructEquality, 
                                       Symbol("@struct_hash_equal"))), 
                $(mod), $(:n)))

        Base.get(x::$n) = x.val

        $it(x::$n, o::Symbol) = x.types[o]

        # Dispatch on model value for all declarations in theory
        $(map(collect($(t).ops ∪ $(t).accessors)) do (i)
          j = $(t)[i]
          op = nameof(j)
          :(@inline function $($(name)).$op(x::$(($(:n))), args...; kw...) 
              $($(name)).$op(Trait(x.val), args...; kw...)
          end)
        end)

      nothing

      end)
    end
  end
end

parse_wrapper_input(n::Symbol) = n, Any

parse_wrapper_input(n::Expr) =
  n.head == :<: ? n.args : error("Bad input $n for wrapper")

end # module
