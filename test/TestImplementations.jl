module TestImplementations 

using TraitInterfaces, Test, StructEquality, REPL, StaticArrays
import LinearAlgebra

# Helper functions
##################

get_doc_string(mod::Module, f) = 
  string(Base.Docs.doc(Base.Docs.Binding(mod, Symbol(f))))

test_doc_string(var::Symbol, starts::String) = 
  startswith(get_doc_string(@__MODULE__, var), starts)

# Example interface 
###################

module MyModule
  struct SpecialType end 
end

import .MyModule: SpecialType

# Defining a theory with abstract and fixed types, aliases, an operation

""" A reflexive graph """
@interface ThReflGraphBool begin
  Ob::TYPE

  @import Bool::TYPE
  @import SpecialType::TYPE

  Hom(dom::Ob, codom::Ob)::TYPE
  @op (→) := Hom # alias for Hom

  id(A::Ob)::(A → A)
end

@test test_doc_string(:ThReflGraphBool, "A reflexive graph")

# Extending a theory
@interface ThTest <: ThReflGraphBool begin
  
  one(a::SpecialType)::Ob
  compose(f::(A → B), g::(B → C))::(A → C) ⊣ [A::Ob, B::Ob, C::Ob]
  @op (⋅) := compose

  f⋅id(B) == f ⊣ [A::Ob, B::Ob, f::(A → B)]
  id(A)⋅f == f ⊣ [A::Ob, B::Ob, f::(A → B)]

  assoc := (f⋅g)⋅h == f⋅(g⋅h) ⊣ [(A,B,C,D)::Ob, f::(A → B), g::(B → C), h::(C → D)]
end

T = ThTest.Meta.theory;
@test length(T) == length(T.judgments)


# Wrapper types
###############

abstract type MyAbstractType end

ThTest.Meta.@wrapper TestWrapper <: MyAbstractType

ThTest.Meta.@typed_wrapper TestTypedWrapper


# Example models
#################

@struct_hash_equal struct FinSetC end

@struct_hash_equal struct MatC{T<:Number} end

wm = WithModel(FinSetC())
@test wm isa WithModel{FinSetC}
@test get(wm) == FinSetC()


# Bad implementations
######################

# Note these will *not* throw exceptions once the Good Implementation code 
# below is run.

@test_throws MissingMethodImplementation @instance ThTest{
    Ob=Int, Hom=Vector{Int}} [model::FinSetC] begin
  compose(f::Vector{Int}, g::Vector{Int}) = g[f]
  dom(f::Vector{Int}) = length(f)
end

@test_throws MissingMethodImplementation @instance ThTest{
    Ob=Int, Hom=Matrix{T}} [model::MatC{T}] where T begin
  id(n::Int) = Matrix{T}(LinearAlgebra.I,n,n)
end


# Good implementations
#######################

# FinSetC
#--------

@instance ThTest{Ob=Int, Hom=SVector} [model::FinSetC] begin
  # Hom(dom::Int, codom::Int) = SVector
  """The identity vector on n is [1, ..., n] """ 
  id(m::Int) = SVector{m}(collect(1:m))
  one(::MyModule.SpecialType)::Int = 1
  function compose(f::SVector{N}, g::SVector{M})::SVector{N} where {N, M} 
    g[f]
  end
  function dom(::SVector{N})::Int where N 
    N
  end
end

@test test_doc_string(:id, "The identity vector on n is [1, ..., n]")

# Calling implementation methods with explicit WithModel holy trait.
@test ThTest.id(WithModel(FinSetC()), 2) == SVector{2}([1,2])

# Indirectly doing the above with index notation
@test ThTest.one[FinSetC()](SpecialType()) == 1

@test impl_type(FinSetC(), ThTest.Hom) == SVector

# Test whether a trait has been implemented
@test implements(FinSetC(), ThTest)

@test !implements(3, ThTest)

@withmodel FinSetC() (⋅, id) begin
  s21, s12 = SVector{2}.([[2,1],[1,2]])
  @test s21 ⋅ s21 ⋅ id(2) == s12
end

# MatC
#-----

@instance ThTest{Ob=Int, Hom=Matrix{T}} [model::MatC{T}] where T begin
  compose(m::Matrix{T}, n::Matrix{T}) = m * n
  id(n::Int) = Matrix{T}(LinearAlgebra.I,n,n)
  one(::SpecialType) = 1
end

# The implementation type can depend on type parameters of the model.
@test impl_type(MatC{Float64}(), ThTest.Hom) == Matrix{Float64}
@test impl_type(MatC{Int}(), ThTest.Hom) == Matrix{Int}

# Test wrapper types
w = TestWrapper(MatC{Int}())
@test w isa MyAbstractType
@test typeof(w) == TestWrapper
@test id(w, 2) |> vec == [1,0,0,1]
@test impl_type(w, :Ob) == Int

x = TestTypedWrapper(MatC{Int}())
@test x isa TestTypedWrapper{Int, Matrix{Int}}


end # module
